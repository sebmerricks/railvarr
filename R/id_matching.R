#' @export
#' @importFrom dplyr filter select inner_join anti_join mutate if_else arrange
#'   group_by distinct
match_ids <- function(berth_events_classes, timetable_groups, match_mapping) {
  #validate_berth_eents(berth_events)
  #validate_timetable_subset(timetable_subset)
  #validate_match_mapping(match_mapping)
  berth_matching <- berth_events_classes %>%
    preprocess_berth_matching(match_mapping)

  timetable_matching <-  timetable_groups %>%
    preprocess_timetable_matching(match_mapping)

  timetable_wider <- timetable_matching %>%
    filter(.data$event %in% c("Pass", "Arrive", "Depart")) %>%
    tidyr::pivot_wider(
      id_cols = c("train_header", "dt_origin", "group", "geo", "date", "start_hour", "end_hour", "berth", "lb", "ub"),
      values_from = c("wtt", "t"),
      names_from = "event"
    )

  pair_start <- berth_matching %>%
    select(-"end_hour") %>%
    inner_join(timetable_wider %>%
                 select(-"end_hour"),
               by = c("group", "berth", "lb", "ub", "date", "start_hour", "geo"),
               relationship = "many-to-many")

  pair_end <- berth_matching %>%
    select(-"start_hour") %>%
    inner_join(timetable_wider %>%
                 select(-"start_hour"),
               by = c("group", "berth", "lb", "ub", "date", "end_hour", "geo"),
               relationship = "many-to-many")

  paired <- bind_rows(
    pair_start,
    pair_end %>% anti_join(pair_start,
                           by = c("train_id", "train_header", "dt_origin"))
  )

  matched <- paired %>%
    mutate(is_match = if_else(
      is.na(t_Pass),
      (t_enters + lb < t_Arrive & t_vacates + ub > t_Depart),
      (t_enters + lb < t_Pass & t_enters + ub > t_Pass)
    )) %>%
    select(train_id, train_header, dt_origin, berth, geo, t_enters, t_Pass, t_Arrive,
           t_Depart, t_vacates, lb, ub, is_match) %>%
    arrange(train_id, dt_origin, train_header) %>%
    group_by(train_id, train_header, dt_origin) %>%
    filter(any(is_match)) %>%
    distinct(train_id, train_header, dt_origin) %>%
    ungroup()

  return(matched)
}

#' @importFrom dplyr select inner_join group_by mutate first last ungroup
preprocess_berth_matching <- function(berth_events_classes, match_mapping) {
  berth_events_mapping <- berth_events_classes %>%
    select("train_id", "group", "berth", "t_enters", "t_vacates") %>%
    inner_join(match_mapping,
               by = c("group", "berth"))

  berth_events_times <- berth_events_mapping %>%
    group_by(.data$train_id) %>%
    mutate(first = first(.data$t_enters),
           last = last(.data$t_vacates)) %>%
    ungroup() %>%
    mutate(date = lubridate::date(.data$first),
           start_hour = lubridate::hour(.data$first),
           end_hour = lubridate::hour(.data$last)) %>%
    select(-"first", -"last")

  return(berth_events_times)
}

#' @importFrom dplyr select inner_join group_by mutate first last ungroup filter
#'   arrange
preprocess_timetable_matching <- function(timetable_groups, match_mapping) {
  timetable_subset_mapping <- timetable_groups %>%
    select("train_header", "dt_origin", "group", "geo", "event", "wtt", "t") %>%
    inner_join(match_mapping,
               by = c("group", "geo"))

  timetable_subset_times <- timetable_subset_mapping %>%
    filter(!is.na(.data$t)) %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    mutate(first = first(.data$t),
           last = last(.data$t)) %>%
    ungroup() %>%
    mutate(date = lubridate::date(.data$first),
           start_hour = lubridate::hour(.data$first),
           end_hour = lubridate::hour(.data$last)) %>%
    arrange(.data$date, .data$t) %>%
    select(-"first", -"last")

  return(timetable_subset_times)
}
