fuzzy_less_than <- function(observed, rounded, tolerance = 1) {
  rounded.UB <- rounded + 29 * tolerance
  return(observed < rounded.UB)
}

fuzzy_greater_than <- function(observed, rounded, tolerance = 1) {
  rounded.LB <- rounded - 30 * tolerance
  return(observed > rounded.LB)
}

fuzzy_equals <- function(observed, rounded, tolerance = 1) {
  rounded.LB <- rounded - 30 * tolerance
  rounded.UB <- rounded + 29 * tolerance
  return(observed > rounded.LB & observed < rounded.UB)
}

pair_observations <- function(observed, timetable) {
  # Identify Duplicates
  duplicates <- timetable %>%
    group_by(.data$train_header, .data$geo, .data$event) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(.data$n > 1)
  # Remove Duplicates
  timetable <- timetable %>%
    anti_join(duplicates, by = "train_header")

  timetable_wider <- timetable %>%
    select("train_header", "geo", "wtt", "t", "event") %>%
    filter(.data$event %in% c("Pass", "Arrive", "Depart")) %>%
    pivot_wider(
      id_cols = c("train_header", "geo"),
      values_from = c("wtt", "t"),
      names_from = "event"
    ) %>%
    left_join(timetable %>%
                select("train_header", "geo", "day", "start_hour", "end_hour",
                       "group") %>%
                group_by(.data$train_header, .data$geo) %>%
                filter(row_number() == 1) %>%
                ungroup(),
              by = c("train_header", "geo"))

  aligned_observations <- observed %>%
    semi_join(timetable,
              by = c("day", "start_hour")) %>%
    semi_join(timetable,
              by = c("day", "end_hour")) %>%
    inner_join(get_network_map() %>%
                 select("signal", "geo"),
               by = "signal",
               relationship = "many-to-many") %>%
    left_join(timetable_wider %>%
                select(-"end_hour", -"group"),
              by = c("geo", "day", "start_hour"),
              relationship = "many-to-many") %>%
    left_join(timetable_wider %>%
                select(-"start_hour", -"group"),
              by = c("geo", "day", "end_hour"),
              relationship = "many-to-many") %>%
    pivot_longer(
      cols = matches("\\w\\W\\w"),
      names_to = c(".value", "name"),
      names_sep = -2L,
      names_repair = "minimal",
    ) %>%
    select("signal", "geo", "train_id", "train_header", "t_enters", "t_vacates",
           "t_Arrive", "t_Depart", "t_Pass", "group") %>%
    filter(!is.na(.data$train_id) & !is.na(.data$train_header)) %>%
    arrange(.data$train_id, .data$train_header, .data$t_enters) %>%
    group_by(.data$train_id, .data$train_header, .data$geo) %>%
    filter(row_number() == 1) %>%
    ungroup()

  paired_observations <- aligned_observations
  #    group_by(.data$train_id, .data$train_header, .data$geo) %>%
  #    mutate(geo = str_split_fixed(.data$geo, " |-", n = 3)[[1]])

  return(paired_observations)
}

match_pass <- function(t_enters, t_vacates, t_Pass, lb, ub,
                       operators, fuzzy_tolerance, tolerance) {
  if (operators == "fuzzy") {
    return(fuzzy_less_than(t_enters + tolerance*lb, t_Pass, fuzzy_tolerance) &
             fuzzy_greater_than(t_vacates + tolerance*ub, t_Pass, fuzzy_tolerance))
  } else {
    return(t_enters + tolerance*lb < t_Pass & t_vacates + tolerance*ub > t_Pass)
  }
}

match_stop <- function(t_enters, t_vacates, t_Arrive, t_Depart, lb, ub,
                       operators, fuzzy_tolerance, tolerance) {
  if (operators == "fuzzy") {
    return(fuzzy_less_than(t_enters + tolerance*lb, t_Arrive, fuzzy_tolerance) &
             fuzzy_greater_than(t_vacates + tolerance*ub, t_Depart,
                                fuzzy_tolerance))
  } else {
    return(t_enters + tolerance*lb < t_Arrive &
             t_vacates + tolerance*ub > t_Depart)
  }
}

match_group <- function(observed, timetable, match_map,
                        operators, fuzzy_tolerance, tolerance) {
  paired_observations <- pair_observations(observed, timetable)

  match_at <- paired_observations %>%
    inner_join(match_map, by = "geo")

  is_matched <- match_at %>%
    mutate(is_match = if_else(
      .data$event == "Pass",
      match_pass(.data$t_enters, .data$t_vacates, .data$t_Pass, .data$lb,
                 .data$ub, operators, fuzzy_tolerance, tolerance),
      match_stop(.data$t_enters, .data$t_vacates, .data$t_Arrive,
                 .data$t_Depart, .data$lb, .data$ub, operators, fuzzy_tolerance,
                 tolerance)
    )) %>%
    select(train_id, train_header, t_enters, t_vacates, t_Pass, t_Arrive,
           t_Depart, is_match, group)

  matched <- is_matched %>%
    filter(is_match) %>%
    group_by(.data$train_id) %>%
    mutate(tdiff = .data$t_Pass - .data$t_enters) %>%
    slice_min(order_by = .data$tdiff) %>%
    group_by(.data$train_header) %>%
    slice_min(order_by = .data$tdiff) %>%
    group_by(.data$train_id) %>%
    slice_min(order_by = .data$train_header) %>%
    arrange(.data$train_id) %>%
    select(-"is_match", -"tdiff") %>%
    distinct(train_id, train_header, .keep_all = TRUE)

  return(matched)
}

#' @export
preprocess_berths <- function(berth_events, train_classes) {
  names_berths <- c("signal", "train_id", "t_enters", "t_vacates")
  types_berths <- list(character(), integer(), lubridate::POSIXct(),
                       lubridate::POSIXct())
  check_df(berth_events, names_berths, types_berths)

  names_classes <- c("train_id", "group")
  types_classes <- list(integer(), character())
  check_df(train_classes, names_classes, types_classes)

  berth_groups <- berth_events %>%
    inner_join(
      train_classes %>%
        select(train_id, group),
      by = "train_id"
    )

  berth_groups <- berth_groups %>%
    group_by(.data$train_id) %>%
    mutate(day = lubridate::date(first(.data$t_enters)),
           start_hour = lubridate::hour(first(.data$t_enters)),
           end_hour = lubridate::hour(last(.data$t_enters))) %>%
    ungroup()

  return(berth_groups)
}

#' @export
preprocess_timetable <- function(timetable, train_classes) {
  names_tt <- c("train_header", "geo", "event", "t")
  types_tt <- list(character(), character(), character(), lubridate::POSIXct())
  check_df(timetable, names_tt, types_tt)

  names_classes <- c("train_header", "group")
  types_classes <- list(character(), character())
  check_df(train_classes, names_classes, types_classes)

  timetable_groups <- timetable %>%
    inner_join(train_classes %>%
                 select("train_header", "group"),
               by = "train_header")
    filter(!is.na(.data$t)) %>%
    group_by(.data$train_header) %>%
    mutate(day = lubridate::date(first(.data$t))) %>%
    mutate(start_hour = lubridate::hour(first(.data$t)),
           end_hour = lubridate::hour(last(.data$t))) %>%
    ungroup() %>%
    arrange(.data$dt_origin)

  return(timetable_groups)
}

#' Match Train IDs to Train Headers based on Group Information
#'
#' This function matches train IDs dervied from Centrix data to corresponding
#' timetable train headers. It performs the matching for each unique group in
#' the data.
#'
#' @param berth_groups A data frame containing observed Centrix data that has
#'   been pre-processed for ID matching. See [preprocess_berths]. This data
#'   should contain the following columns:
#'   \itemize{
#'     \item \code{signal}: Character vector, signal ID.
#'     \item \code{train_id}: Integer vector, train ID.
#'     \item \code{t_enters}: [POSIXct], the time when the train enters the
#'                      berth.
#'     \item \code{t_vacates}: [POSIXct], the time when the train vacates the
#'                      berth.
#'     \item \code{group}: Character vector, the group name.
#'     \item \code{day}: [POSIXct], the day of the journey.
#'     \item \code{start_hour}: Integer, the start hour of the journey.
#'     \item \code{end_hour}: Integer, the end hour of the journey.
#'   }
#'
#' @param timetable_groups A data frame containing pre-processed timetable data
#'   (see [preprocess_timetable]) with the following columns:
#'   \itemize{
#'     \item \code{train_header}: Character vector, the train header.
#'     \item \code{geo}: Character vector, the geographical location.
#'     \item \code{event}: Character vector, the type of event.
#'     \item \code{wtt}: [POSIXct], the time according to the working timetable.
#'     \item \code{t}: [POSIXct], the actual time of the event.
#'     \item \code{group}: Character vector, the group name.
#'     \item \code{day}: [POSIXct], the day of the journey. Used to narrow down
#'                      the number of potential matches.
#'     \item \code{start_hour}: Integer, the start hour of the journey. Used to
#'                      narrow down the number of potential matches.
#'     \item \code{end_hour}: Integer, the end hour of the journey. Used to
#'                      narrow down the number of potential matches.
#'   }
#'
#' @param group_map A data frame containing group-specific information about
#'   which geographical locations to perform ID matching:
#'   \itemize{
#'     \item \code{group}: Character vector, the group name.
#'     \item \code{geo}: Character vector, the geographical location.
#'     \item \code{event}: Character vector, the type of event. Should be either
#'                      'Pass' if the train does not stop, or 'Stop' if the
#'                      train does stop.
#'     \item \code{lb}: Numeric, the lower bound (in seconds) for what is
#'                      considered a match. Together with `ub`, this defines a
#'                      time range based on `berth_groups$t_enters`. If the
#'                      actual timetable time `t` occurs within this range, it
#'                      is considered a match.
#'     \item \code{ub}: Numeric, the upper bound (in seconds) for what is
#'                      considered a match. Together with `lb`, this defines a
#'                      time range based on `berth_groups$t_enters`. If the
#'                      actual timetable time `t` occurs within this range, it
#'                      is considered a match.
#'   }
#'
#' @param operators The type of matching operator to be used. It can be one of
#'   "fuzzy" (default) or "exact". Using "exact" will use normal mathematical
#'   operators `<` and `>`. Using "fuzzy" accounts for the rounding in timetable
#'   data.
#'
#' @param fuzzy_tolerance The tolerance value for fuzzy matching. This parameter
#'   is used when "operators" is set to "fuzzy". A higher fuzzy_tolerance will
#'   produce more matches.
#'
#' @param tolerance The tolerance value for matching. This parameter acts as a
#'   scalar for the bounds (`lb` and `ub`) defined in the `group_map`. A higher
#'   tolerance will produce more matches.
#'
#' @return A data frame containing the matched train IDs and their corresponding
#'   train headers. The data frame is sorted by train IDs in ascending order.
#'
#' @importFrom lubridate POSIXct
#' @importFrom dplyr bind_rows arrange filter
#'
#' @export
#'
#' @examples
#' #' # First we have to set the network map
#' network_map <- dplyr::tribble(
#'   ~signal, ~berth, ~track, ~event, ~geo,
#'   "A", "1", "1", "pass", "LocationX",
#'   "B", "1", "1", "pass", "LocationY",
#'   "C", "1", "1", "pass", "LocationZ"
#' )
#'
#' set_network_map(network_map)
#'
#' # Example data frames for berth_groups, timetable_groups, and group_map
#' berth_groups <- data.frame(signal = c("A", "B", "C"),
#'                            train_id = c(1, 1, 1),
#'                            t_enters = as.POSIXct(c("2023-07-19 12:00:00",
#'                                                    "2023-07-19 13:00:00",
#'                                                    "2023-07-19 14:00:00")),
#'                            t_vacates = as.POSIXct(c("2023-07-19 12:01:00",
#'                                                     "2023-07-19 13:01:00",
#'                                                     "2023-07-19 14:01:00")),
#'                            group = c("X", "X", "X"),
#'                            day = as.POSIXct(c("2023-07-19",
#'                                               "2023-07-19",
#'                                               "2023-07-19")),
#'                            start_hour = c(12, 13, 14),
#'                            end_hour = c(12, 13, 14))
#' timetable_groups <- data.frame(train_header = c("TrainA", "TrainA", "TrainA"),
#'                                geo = c("LocationX", "LocationY", "LocationZ"),
#'                                event = c("Pass", "Pass", "Pass"),
#'                                wtt = as.POSIXct(c("2023-07-19 12:00:00",
#'                                                   "2023-07-19 13:00:00",
#'                                                   "2023-07-19 14:00:00")),
#'                                t = as.POSIXct(c("2023-07-19 12:01:00",
#'                                                 "2023-07-19 13:08:00",
#'                                                 "2023-07-19 14:00:30")),
#'                                group = c("X", "X", "X"),
#'                                day = as.POSIXct(c("2023-07-19",
#'                                                   "2023-07-19",
#'                                                   "2023-07-19")),
#'                                start_hour = c(12, 13, 14),
#'                                end_hour = c(12, 13, 14))
#' group_map <- data.frame(group = c("X", "X", "X"),
#'                         geo = c("LocationX", "LocationY", "LocationZ"),
#'                         event = c("Pass", "Pass", "Pass"),
#'                         lb = c(30, 30, 30),
#'                         ub = c(30, 30, 30))
#'
#' # Perform the matching
#' matched_data <- match_ids(berth_groups, timetable_groups, group_map)
#'
#' # Print the matched data
#' print(matched_data)
match_ids <- function(berth_groups,
                      timetable_groups,
                      group_map,
                      operators = "fuzzy",
                      fuzzy_tolerance = 1,
                      tolerance = 1) {
  names_berths <- c("signal", "train_id", "t_enters", "t_vacates", "group",
                    "day", "start_hour", "end_hour")
  types_berths <- list(character(), integer(), lubridate::POSIXct(),
                       lubridate::POSIXct(), character(), lubridate::POSIXct(),
                       integer(), integer())
  check_df(berth_groups, names_berths, types_berths)

  names_tt <- c("train_header", "geo", "event", "wtt", "t", "group", "day",
                "start_hour", "end_hour")
  types_tt <- list(character(), character(), character(), lubridate::POSIXct(),
                   lubridate::POSIXct(), character(), lubridate::POSIXct(),
                   integer(), integer())
  check_df(timetable_groups, names_tt, types_tt)

  names_map <- c("group", "geo", "event", "lb", "ub")
  types_map <- list(character(), character(), character(), numeric(), numeric())
  check_df(group_map, names_map, types_map)
  groups <- unique(group_map$group)

  matched_ids <- dplyr::tribble(~train_id, ~train_header)

  for (g in groups) {
    map <- group_map %>%
      filter(.data$group == g)

    observed_group <- berth_groups %>%
      filter(.data$group == g)
    timetable_group <- timetable_groups %>%
      filter(.data$group == g)

    matched <- match_group(observed_group, timetable_group, map, operators,
                           fuzzy_tolerance, tolerance)

    matched_ids <- dplyr::bind_rows(matched_ids, matched)
  }

  return(matched_ids %>% arrange(train_id))
}

#' @export
combine_data <- function(ids, observed, timetable) {
  duplicates <- timetable %>%
    group_by(.data$train_header, .data$geo, .data$event) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)

  timetable_wider <- timetable %>%
    select("train_header", "geo", "t", "event", "group") %>%
    anti_join(duplicates, by = c("train_header", "geo", "event")) %>%
    filter(event %in% c("Pass", "Arrive", "Depart")) %>%
    tidyr::pivot_wider(
      id_cols = c("train_header", "geo", "group"),
      values_from = "t",
      names_from = "event",
      names_glue = "t_{.name}"
    )

  observed_geos <- observed %>%
    select("train_id", "signal", "t_enters") %>%
    inner_join(get_network_map() %>%
                 select("signal", "geo"),
               by = "signal",
               relationship = "many-to-many")

  combined_data <- timetable_wider %>%
    inner_join(ids %>%
                 select("train_id", "train_header"),
               by = "train_header") %>%
    inner_join(observed_geos,
               by = c("geo", "train_id")) %>%
    arrange(train_id) %>%
    select("train_id", "train_header", "group", "geo", "t_enters", "t_vacates",
           "t_Pass", "t_Arrive", "t_Depart")

  return(combined_data)
}

#' @export
accuracy <- function(ids) {
  ids <- ids %>%
    mutate(tdiff = as.integer(.data$t_enters - .data$t),
           tdiff2 = .data$tdiff * .data$tdiff)

  ma = summary(ids$tdiff)[["Mean"]]
  msa = summary(ids$tdiff2)[["Mean"]]

  print(glue::glue(
    "Mean Accuracy: {ma}s      Mean Squared Accuracy: {msa}s^2"
  ))
}
