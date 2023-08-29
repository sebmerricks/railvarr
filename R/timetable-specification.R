#' Calculate journey specifications
#'
#' Uses scheduled times in the timetable to calculate how long it should take to
#' travel along the track. Calculates journey times between stations specified
#' in the `spec_stations` parameter.
#'
#' @inheritParams wrangle_timetable
#' @param spec_stations List of stations for which to calculate journey times.
#'
#' @export
calculate_journey_specifications <- function(timetable, spec_stations) {
  timetable_filtered <- timetable %>%
    select("train_header", "dt_origin", "geo", "event", "wtt", "group") %>%
    filter(.data$geo %in% unlist(spec_stations))

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
