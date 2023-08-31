#'Wrangle Timetable Data
#'
#'A wrapper for timetable processing functions [filter_relevant_services()],
#'[filter_relevant_direction()], and [find_calling_patterns()].
#'
#'@param timetable Data frame containing timetable data. Strict input validation
#'  is performed. Must be a data frame with the following columns:
#'   \itemize{
#'     \item{`train_header`}{[character()] Train identifier}
#'     \item{`dt_origin`}{[lubridate::POSIXct()] Datetime at which the train
#'        originated}
#'     \item{`geo`}{[character()] Name of event location}
#'     \item{`event`}{[character()] Type of event, must only contain the
#'        following values: `Pass`, `Arrive`, `Depart`, `Originate`,
#'        `Terminate`}
#'     \item{`wtt`}{[lubridate::POSIXct()] Scheduled datetime of event}
#'     \item{`t`}{[lubridate::POSIXct()] Actual datetime of event}
#'     \item{`delay`}{[numeric()] Difference between actual time and scheduled
#'        time of event}
#'     \item{`allow`}{[numeric()] Timetabled delay allowance}
#'   }
#'@param stations You should provide a list of stations that you are interested
#'  in. For example, a train may stop at many stations on its journey, but your
#'  Centrix data only covers a small subset of that journey. Any trains which do
#'  not pass through any of the stations specified will be discarded. It is
#'  important to note that timetables do not only include station stops, they
#'  also include junctions and other `'Pass'` events. These locations must also
#'  be included in `stations`, even though they are not actually stations.
#'
#'  Therefore, your list should completely encompasses your Centrix data. This
#'  ensures that all relevant services are included in the processed timetable.
#'  However, you may need to ensure that no irrelevant services are accidentally
#'  included.
#'
#'  The order in which you define this list is important if you are using
#'  [wrangle_timetable()] or [filter_relevant_direction()], because it specifies
#'  the direction in which trains should be travelling. This allows for the
#'  filtering of services by direction. You can avoid this by using the lower
#'  level functions directly and not using [filter_relevant_direction()], but
#'  these functions do not currently come with input validation, so discretion
#'  is advised.
#'@param stopping_stations A subset of `stations` that specifies which stations
#'  trains can stop at. This is used for calculating calling patterns.
#'  Therefore, any train which does not stop at any of the stations specified in
#'  `stopping_stations` will be labeled as a 'fast' train. Trains which do stop
#'  at these stations will be labeled with 'stopping-', with the specific
#'  stations at which they stop attached as suffixes. For example, if a train
#'  stops at 'geo10', its calling pattern will be labeled as 'stopping-geo10'.
#'@return A subset of the timetable which contains trains that pass through the
#'  specified stations in the given order. Time zone is set to "UTC".
#'
#' @examples
#' data(timetable, stations, stopping_stations)
#' timetable
#' timetable_subset <- wrangle_timetable(timetable, stations, stopping_stations)
#' timetable_subset
#'
#'@seealso [filter_relevant_services()] [find_calling_patterns()]
#'  [calculate_journey_specifications()] [match_ids()] [timetable]
#'  [timetable_subset]
#'
#'@export
wrangle_timetable <- function(timetable, stations, stopping_stations) {
  validate_timetable(timetable)
  validate_stations(stations)

  timetable_subset <- timetable %>%
    filter_relevant_services(stations) %>%
    filter_relevant_direction(stations) %>%
    find_calling_patterns(stopping_stations)

  return(timetable_subset %>%
           mutate(
             across(c(dt_origin, wtt, t),
                    ~lubridate::with_tz(.x, tzone = "UTC"))
           ))
}

#' Filter timetable to relevant trains
#'
#' @description
#'   `filter_relevant_services()` filters only by station, ignoring
#'   direction.
#'
#'   `filter_relevant_direction()` filters by both station and direction. It is
#'   assumed that `stations` is in the forward direction, i.e., the first
#'   element is at the start of the track section and the last element is at the
#'   end.
#'
#' @inheritParams wrangle_timetable
#'
#' @returns
#'   `filter_relevant_services` returns a (usually) smaller timetable
#'   only containing trains which pass through or stop at the specified
#'   stations.
#'
#'   `filter_relevant_direction` returns a (usually) smaller timetable only
#'   containing trains which pass through or stop at the specified stations in
#'   the given order.
#'
#' @examples
#' data(timetable, stations)
#' timetable
#' timetable_services <- filter_relevant_services(timetable, stations)
#' timetable_services
#' timetable_direction <- filter_relevant_direction(timetable, stations)
#' timetable_direction
#'
#' @importFrom dplyr filter mutate across
#' @export
#' @seealso [wrangle_timetable()] [find_calling_patterns()]
filter_relevant_services <- function(timetable, stations) {
  return(timetable %>%
           filter(geo %in% unlist(stations)))
}

#' @rdname filter_relevant_services
#' @importFrom dplyr filter mutate first last group_by select
#' @export
filter_relevant_direction <- function(timetable, stations) {
  return(timetable %>%
           filter(.data$geo %in% unlist(stations)) %>%
           mutate(is_first = .data$geo %in% first(stations),
                  is_last = .data$geo %in% last(stations)) %>%
           group_by(.data$train_header, .data$dt_origin) %>%
           filter(first(.data$is_first) & last(.data$is_last)) %>%
           select(-"is_first", -"is_last"))
}

#' Find calling patterns from timetable
#' @inheritParams wrangle_timetable
#' @returns The timetable with calling patterns added.
#'
#' @examples
#' data(timetable, stations, stopping_stations)
#' calling_patterns <- timetable |>
#'                       filter_relevant_direction(stations) |>
#'                       find_calling_patterns(stopping_stations)
#' calling_patterns[,c("train_header", "dt_origin", "geo", "event", "group")]
#'
#' @importFrom dplyr distinct mutate filter bind_rows summarise inner_join
#'   ungroup arrange first left_join if_else
#' @export
#' @seealso [filter_relevant_services()] [wrangle_timetable()]
find_calling_patterns <- function(timetable, stopping_stations) {
  dummy_geo <- timetable %>%
    distinct(.data$train_header, .data$dt_origin) %>%
    mutate(geo = "None", event = "Arrive")

  calling_patterns <- timetable %>%
    filter(.data$event %in% c("Originate", "Arrive")) %>%
    bind_rows(dummy_geo) %>%
    distinct(.data$train_header, .data$dt_origin, .data$geo) %>%
    mutate(stopping = any(.data$geo %in% unlist(stopping_stations))) %>%
    filter(!.data$stopping | .data$geo %in% unlist(stopping_stations)) %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    summarise(
      pattern = stringr::str_flatten(.data$geo, collapse = "-"),
      stopping = first(.data$stopping),
      .groups = "drop"
    ) %>%
    arrange(.data$dt_origin) %>%
    mutate(pattern = stringr::str_replace(.data$pattern, "-None", ""))

  fast_trains <- dplyr::tribble(
    ~stopping, ~group,
    FALSE, "fast"
  )

  stopping_all <- dplyr::tribble(
    ~pattern, ~group,
    stringr::str_flatten(stopping_stations, collapse = "-"), "stopping-all"
  )

  groups <- calling_patterns %>%
    left_join(fast_trains, by = "stopping") %>%
    left_join(stopping_all, by = "pattern") %>%
    mutate(group.z = if_else(
      is.na(.data$group.x) & is.na(.data$group.y),
      paste0("stopping-", .data$pattern),
      NA
    )) %>%
    tidyr::unite("group",
                 c("group.x", "group.y", "group.z"),
                 remove = FALSE,
                 na.rm = TRUE) %>%
    select(-"pattern", -"stopping", -"group.x", -"group.y", -"group.z")

  return(timetable %>%
           inner_join(groups, by = c("train_header", "dt_origin")) %>%
           ungroup())
}
