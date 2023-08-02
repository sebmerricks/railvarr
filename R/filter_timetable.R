environment$start_station <- NULL
environment$end_station <- NULL
environment$forwards <- NULL
environment$backwards <- NULL

find_relevant_services <- function(timetable) {
  services_either_direction <- timetable %>%
    mutate(is_first = .data$geo %in% environment$start_station,
           is_last = .data$geo %in% environment$end_station) %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    mutate(is_subset = any(is_first) & any(is_last)) %>%
    ungroup() %>%
    filter(.data$is_subset) %>%
    select(-"is_first", -"is_last", -"is_subset")

  return(services_either_direction)
}

filter_forward_services <- function(services_either_direction) {
  services_direction <- services_either_direction %>%
    filter(geo %in% c(environment$start_station, environment$end_station)) %>%
    filter(event == "Pass" | event == "Arrive" | event == "Originate") %>%
    mutate(t_order = dplyr::if_else(
      is.na(t),
      wtt,
      t
    )) %>%
    group_by(train_header, dt_origin) %>%
    arrange(dt_origin, train_header, t_order) %>%
    mutate(forwards = first(geo) %in% environment$start_station) %>%
    ungroup() %>%
    filter(forwards) %>%
    mutate(direction = environment$forwards) %>%
    distinct(train_header, dt_origin, direction) %>%
    left_join(services_either_direction,
              by = c("train_header", "dt_origin"))

  return(services_direction)
}

remove_edge_cases <- function(services_direction) {
  edge_cases <- services_direction %>%
    filter(geo %in% unlist(get_stations())) %>%
    group_by(train_header, dt_origin) %>%
    filter(!(first(geo) %in% environment$start_station) |
             !(last(geo) %in% environment$end_station))

  no_edges <- services_direction %>%
    anti_join(edge_cases, by = c("train_header", "dt_origin"))

  return(no_edges)
}

subset_timetable <- function(services_direction) {
  timetable_subset <- services_direction %>%
    filter(geo %in% unlist(get_stations())) %>%
    filter(direction == environment$forwards) %>%
    select("train_header", "geo", "dt_origin", "event", "wtt", "t", "delay") %>%
    mutate(
      across(c(dt_origin, wtt, t),
             ~lubridate::with_tz(.x, tzone = "Europe/London"))
    )

  return(timetable_subset)
}

find_calling_patterns <- function(timetable) {
  dummy_geo <- timetable %>%
    distinct(.data$train_header, .data$dt_origin) %>%
    mutate(geo = "None", event = "Arrive")

  train_patterns <- timetable %>%
    group_by(.data$train_header, .data$geo) %>%
    filter(geo %in% unlist(get_stations())) %>%
    filter(event %in% c("Originate", "Arrive")) %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    distinct(.data$train_header, .data$dt_origin, .data$geo) %>%
    bind_rows(dummy_geo) %>%
    summarise(
      pattern = stringr::str_c(geo, collapse = ","),
      .groups = "drop"
    ) %>%
    mutate(pattern = stringr::str_replace(pattern, ",None", ""))

  calling_patterns <- timetable %>%
    inner_join(train_patterns, by = c("train_header", "dt_origin"))

  return(calling_patterns)
}

filter_timetable <- function() {
  timetable <- get_timetable()
  stopifnot(!is.null(timetable))
  stopifnot(!is.null(get_event_mapping()))

  stations <- get_stations()
  stopifnot(!is.null(stations))

  environment$start_station <- first(stations)
  environment$end_station <- last(stations)

  environment$forwards <- glue::glue(
    "{environment$start_station[1]}-{environment$end_station[1]}")
  environment$backwards <- glue::glue(
    "{environment$end_station[1]}-{environment$start_station[1]}")

  timetable_subset <- timetable %>%
    find_relevant_services() %>%
    filter_forward_services() %>%
    remove_edge_cases() %>%
    subset_timetable() %>%
    find_calling_patterns()

  return(timetable_subset)
}
