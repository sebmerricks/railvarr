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

pair_observations <- function(timetable, observed, stations) {
  #is.null(get_station_mapping())?

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
                ungroup())

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
    mutate(geo = str_split_fixed(.data$geo, " |-", n = 3)[[1]]) %>%
    pivot_wider(
      id_cols = c("train_id", "train_header"),
      values_from = c("t_enters", "t_vacates", "t_Arrive", "t_Depart", "t_Pass"),
      names_from = c("geo")
    ) %>%
    select_if(~ !all(is.na(.)))

  return(paired_observations)
}

match_ids <- function(berth_events_groups,
                      timetable_groups,
                      operators = "fuzzy",
                      match_at = "any",
                      fuzzy_tolerance = 1) {
  berth_events_groups <- berth_events_groups %>%
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
    timetable_groups %>% distinct("group"),
    berth_groups %>% distinct("group")
  )

  for (i in 1:nrow(groups)) {
    group <- groups[[i,1]]
    observed_group <- berth_events_groups %>%
      filter(.data$group == group)
    timetable_group <- timetable_groups %>%
      filter(.data$group == group)

    match_group(observed_group, timetable_group)
  }
}
