env$station_mapping <- dplyr::tribble(
  ~signal, ~geo
)

get_station_mapping <- function() {
  return(env$station_mapping)
}

set_station_mapping <- function(station_mapping) {
  names <- c("signal", "geo")
  types <- list(character(), character())
  check_df(station_mapping, names, types, allow_extra = FALSE)
  env$station_mapping <- station_mapping
}

env$group_details <- dplyr::tribble(
  ~group, ~n_stops
)

get_group_details <- function() {
  return(env$group_details)
}

set_group_details <- function(group_details) {
  names <- c("group", "n_stops")
  types <- list(character(), integer())
  check_df(group_details, names, types)
  env$group_details <- group_details
}

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
  #is.null(get_station_mapping())?

  duplicates <- timetable %>%
    group_by(.data$train_header, .data$geo, .data$event) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(.data$n > 1)

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
    inner_join(get_station_mapping(),
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

  paired_observations <- aligned_observations %>%
    group_by(.data$train_id, .data$train_header, .data$geo) %>%
    mutate(geo = str_split_fixed(.data$geo, " |-", n = 3)[[1]])

  return(paired_observations)
}

match_fast <- function(observed, timetable, pass, operators, fuzzy_tolerance,
                       tolerance) {
  paired_observations <- pair_observations(observed, timetable) %>%
    filter(.data$geo == str_split_fixed(pass, " |-", n = 3)[[1]])

  if (operators == "fuzzy") {
    filtered_observations <- paired_observations %>%
      filter(fuzzy_less_than(.data$t_enters - tolerance*240, .data$t_Pass,
                             fuzzy_tolerance) &
               fuzzy_greater_than(.data$t_enters + tolerance*60, .data$t_Pass,
                                  fuzzy_tolerance))
  } else {
    filtered_observations <- paired_observations %>%
      filter((.data$t_enters - tolerance*240 < .data$t_Pass) &
               (.data$t_enters + tolerance*60 < .data$t_Pass))
  }

  matched <- filtered_observations %>%
    group_by(.data$train_id) %>%
    mutate(tdiff = .data$t_Pass - .data$t_enters) %>%
    slice_min(order_by = .data$tdiff) %>%
    group_by(.data$train_header) %>%
    slice_min(order_by = .data$tdiff) %>%
    group_by(.data$train_id) %>%
    slice_min(order_by = .data$train_header) %>%
    arrange(.data$train_id) %>%
    select("train_id", "train_header")

  return(matched)
}

match_stopping <- function(observed, timetable, operators, match_at, expected_n,
                           fuzzy_tolerance) {
  paired_observations <- pair_observations(observed, timetable)

  if (operators == "fuzzy") {
    filtered_observations <- paired_observations %>%
      filter(fuzzy_less_than(.data$t_enters, .data$t_Arrive, fuzzy_tolerance) &
               fuzzy_greater_than(.data$t_vacates, .data$t_Depart,
                                  fuzzy_tolerance))
  } else {
    filtered_observations <- paired_observations %>%
      filter(.data$t_enters < .data$t_Arrive &
               .data$t_vacates > .data$t_Depart)
  }

  summarised <- filtered_observations %>%
    group_by(.data$train_id, .data$train_header) %>%
    summarise(n = n(),
              .groups = "drop")

  if (match_at == "all") {
    summarised <- summarised %>%
      filter(.data$n == expected_n)
  }

  matched <- summarised %>%
    select("train_id", "train_header")

  return(matched)
}

combine_ids <- function(ids, observed, timetable) {
  duplicates <- timetable %>%
    group_by(train_header, geo, event) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)

  ids_together <- timetable %>%
    select(train_header, geo, t, event) %>%
    anti_join(duplicates) %>%
    filter(event %in% c("Pass", "Arrive", "Depart")) %>%
    pivot_wider(
      id_cols = c("train_header", "geo"),
      values_from = "t",
      names_from = "event",
      names_glue = "t_{.name}"
    ) %>%
    inner_join(ids,
               by = "train_header") %>%
    inner_join(observed %>%
                 select(train_id, signal, t_enters) %>%
                 inner_join(get_station_mapping())) %>%
    arrange(train_id) %>%
    mutate(t = if_else(is.na(t_Pass), t_Arrive, t_Pass)) %>%
    select(train_id, train_header, geo, t_enters, t) %>%
    left_join(train_classes %>%
                select(train_id, group))
  return(ids_together)
}

match_ids <- function(berth_groups,
                      timetable_groups,
                      operators = "fuzzy",
                      match_at = "any",
                      fuzzy_tolerance = 1,
                      fast_tolerance = 1) {
  berth_groups <- berth_groups %>%
    group_by(.data$train_id) %>%
    mutate(day = lubridate::date(first(.data$t_enters)),
           start_hour = lubridate::hour(first(.data$t_enters)),
           end_hour = lubridate::hour(last(.data$t_enters))) %>%
    ungroup()

  timetable_groups <- timetable_groups %>%
    filter(!is.na(.data$t)) %>%
    group_by(.data$train_header) %>%
    mutate(day = lubridate::date(first(.data$t))) %>%
    mutate(start_hour = lubridate::hour(first(.data$t)),
           end_hour = lubridate::hour(last(.data$t))) %>%
    ungroup() %>%
    arrange(.data$dt_origin)

  groups <- inner_join(
    timetable_groups %>% distinct(.data$group),
    berth_groups %>% distinct(.data$group),
    by = "group"
  )

  matched_ids <- dplyr::tribble(~train_id, ~train_header)

  for (i in 1:nrow(groups)) {
    g <- groups[[i,1]]
    observed_group <- berth_groups %>%
      filter(.data$group == g)
    timetable_group <- timetable_groups %>%
      filter(.data$group == g)

    if (grepl("fast", g)) {
      pass <- (get_group_details() %>% filter(.data$group == g))$pass_at
      matched <- match_fast(observed_group, timetable_group,
                            pass, operators, fuzzy_tolerance, fast_tolerance)
    } else {
      expected_n <- (get_group_details() %>% filter(.data$group == g))$n_stops
      matched <- match_stopping(observed_group, timetable_group,
                                operators, match_at, expected_n,
                                fuzzy_tolerance)
    }

    matched_ids <- dplyr::bind_rows(matched_ids, matched)
  }

  return(combine_ids(matched_ids, berth_groups, timetable_groups))
}

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
