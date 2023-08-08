#' Wrangle Timetable Data
#'
#' @param timetable Data frame containing timetable data.
#' @param stations List containing station names.
#'
#' @export
wrangle_timetable <- function(timetable, stations) {
  validate_timetable(timetable)
  validate_stations(stations)

  timetable_subset <- timetable %>%
    filter_relevant_services(stations) %>%
    filter_relevant_direction(stations) %>%
    find_calling_patterns()

  return(timetable_subset %>%
           mutate(
             across(c(dt_origin, wtt, t),
                    ~lubridate::with_tz(.x, tzone = "UTC"))
           ))
}

#' @importFrom dplyr filter mutate across
filter_relevant_services <- function(timetable, stations) {
  return(timetable %>%
           filter(geo %in% unlist(stations)))
}

#' @importFrom dplyr filter mutate first last group_by select
filter_relevant_direction <- function(timetable, stations) {
  return(timetable %>%
    filter(.data$geo %in% unlist(stations)) %>%
    mutate(is_first = .data$geo %in% first(stations),
           is_last = .data$geo %in% last(stations)) %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    filter(first(.data$is_first) & last(.data$is_last)) %>%
    select(-"is_first", -"is_last"))
}

#' @importFrom dplyr distinct mutate filter bind_rows summarise inner_join
#'   ungroup
find_calling_patterns <- function(timetable) {
  dummy_geo <- timetable %>%
    distinct(train_header, dt_origin) %>%
    mutate(geo = "None", event = "Arrive")

  calling_patterns <- timetable %>%
    filter(event %in% c("Originate", "Arrive")) %>%
    distinct(train_header, dt_origin, geo) %>%
    bind_rows(dummy_geo) %>%
    summarise(
      pattern = stringr::str_c(geo, collapse = ","),
      .groups = "drop"
    ) %>%
    mutate(pattern = stringr::str_replace(pattern, ",None", ""))

  return(timetable %>%
           inner_join(calling_patterns, by = c("train_header", "dt_origin")) %>%
           ungroup())
}
