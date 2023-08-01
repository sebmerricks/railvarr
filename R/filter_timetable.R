environment$start_station <- NULL
environment$end_station <- NULL
environment$forwards <- NULL
environment$backwards <- NULL

find_relevant_services <- function(timetable) {
  services_either_direction <- timetable %>%
    select("train_header", "dt_origin", "geo") %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    tidyr::chop("geo") %>%
    ungroup() %>%
    mutate(is_subset = purrr::map_lgl(.data$geo, ~{
      first <- any(.x %in% environment$start_station)
      last <- any(.x %in% environment$end_station)
      return(first & last)
    })) %>%
    filter(.data$is_subset) %>%
    select(-"geo", -"is_subset")

  return(services_either_direction)
}

filter_forward_services <- function(services_either_direction, timetable) {
  services_direction <- inner_join(
    timetable,
    services_either_direction,
    by = c("train_header", "dt_origin")
  ) %>%
    filter(geo %in% c(environment$start_station, environment$end_station)) %>%
    filter(event == "Pass" | event == "Arrive" | event == "Originate") %>%
    mutate(t_order = case_when(
      is.na(wtt) ~ t,
      is.na(t) ~ wtt,
      T ~ t
    )) %>%
    group_by(train_header, dt_origin) %>%
    arrange(t_order) %>%
    mutate(direction = if_else(
      first(geo) %in% environment$start_station,
      environment$forwards,
      environment$backwards
    )) %>%
    ungroup() %>%
    distinct(train_header, dt_origin, direction) %>%
    filter(direction == environment$forwards)

  return(services_direction)
}

remove_edge_cases <- function(services_direction, timetable) {
  edge_cases <- inner_join(
    timetable,
    services_direction,
    by = c("train_header", "dt_origin")
  ) %>%
    filter(geo %in% get_stations()) %>%
    filter(direction == environment$forwards) %>%
    group_by(train_header, dt_origin) %>%
    filter((!first(geo) %in% environment$start_station) |
             (!last(geo) %in% environment$end_station))

  no_edges <- services_direction %>%
    filter(!train_header %in% edge_cases$train_header)

  return(no_edges)
}

subset_timetable <- function(services_direction, timetable) {
  timetable_subset <- inner_join(
    timetable,
    services_direction,
    by = c("train_header", "dt_origin")
  ) %>%
    filter(geo %in% get_stations()) %>%
    filter(direction == environment$forwards) %>%
    select("train_header", "geo", "dt_origin", "event", "wtt", "t", "delay") %>%
    mutate(
      across(c(dt_origin, wtt, t),
             ~lubridate::with_tz(.x, tzone = "Europe/London"))
    )

  return(timetable_subset)
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
    filter_forward_services(timetable) %>%
    remove_edge_cases(timetable) %>%
    subset_timetable(timetable)

  return(timetable_subset)
}
