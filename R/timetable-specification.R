#' Calculate journey specifications
#' @param timetable Timetable
#' @param stations Stations
#' @export
calculate_journey_specifications <- function(timetable, stations) {
  timetable_filtered <- timetable %>%
    select("train_header", "dt_origin", "geo", "event", "wtt", "group") %>%
    filter(.data$geo %in% unlist(stations))

  journeys <- timetable_filtered %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    mutate(
      xi = lag(.data$geo),
      xk = .data$geo
    ) %>%
    ungroup()

  durations <- journeys %>%
    arrange(train_header, dt_origin) %>%
    mutate(
      duration = if_else(
        is.na(.data$xi),
        NA,
        lubridate::as.duration(.data$wtt - lag(.data$wtt))
      )
    )

  journey_types <- durations %>%
    filter(!is.na(.data$duration)) %>%
    mutate(
      type = if_else(.data$xi == .data$xk, "dwell", "moving")
    ) %>%
    select(-"geo", -"event")

  timetable_specification <- journey_types %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    mutate(
      j = row_number(),
      T_journey = sum(.data$duration)
    ) %>%
    ungroup()

  return(timetable_specification)
}
