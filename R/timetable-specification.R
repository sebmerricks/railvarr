#' Calculate journey specifications
#'
#' Uses scheduled times in the timetable to calculate how long it should take to
#' travel along the track. Calculates journey times between stations specified
#' in the `spec_stations` parameter.
#'
#' @inheritParams wrangle_timetable
#' @param spec_stations List of stations for which to calculate journey times.
#'   Calculates the total journey time from the first station in the list to the
#'   last station in the list, as well as every component of that journey.
#'
#' @return Journey times as specified by `spec_stations`. A data frame with 10
#'   columns:
#'   \itemize{
#'     \item{`train_header`}{Train identifier}
#'     \item{`dt_origin`}{Datetime at which the train originated}
#'     \item{`wtt`}{Scheduled datetime of event}
#'     \item{`group`}{Calling pattern}
#'     \item{`xi`}{Start of journey component}
#'     \item{`xk`}{End of journey component}
#'     \item{`duration`}{Duration of journey component}
#'     \item{`type`}{Type of component, either `moving` for sections in-between
#'                   stations, or `dwell` when the component represents a stop
#'                   at a station}
#'     \item{`j`}{Component ID}
#'     \item{`T_journey`}{Total journey time across all components}
#'   }
#'
#' @examples
#' data(timetable_subset)
#' timetable_subset
#' # spec_stations contains stations that completely encompass the Centrix track
#' spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")
#' calculate_journey_specifications(timetable_subset, spec_stations)
#'
#' @seealso [wrangle_timetable()] [timetable_specification]
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
