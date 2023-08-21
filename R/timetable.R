#' Wrangle Timetable Data
#'
#' A wrapper for timetable processing functions [filter_relevant_services()],
#' [filter_relevant_direction()], and [find_calling_patterns()].
#'
#' @param timetable Data frame containing timetable data.
#' @param stations List containing station names. Trains which do not pass
#'   through any of these stations will be discarded.
#' @param stopping_stations a character vector containing the names of stations
#'   at which trains stop. Any trains which do not stop at any of these stations
#'   will be labeled as 'fast' (non-stopping) trains. Trains which do stop at
#'   these stations will either be labeled as 'stopping-all' or they will be
#'   labeled with the specific stations at which they stop.
#' @return A subset of the timetable which contains trains that pass through the
#'   specified stations in the given order. Time zone is set to "UTC".
#'
#' @examples
#' data(timetable, stations, stopping_stations)
#' timetable
#' timetable_subset <- wrangle_timetable(timetable, stations, stopping_stations)
#' timetable_subset
#'
#' @seealso [filter_relevant_services()] [find_calling_patterns()]
#'
#' @export
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
find_calling_patterns <- function(timetable_raw, stopping_stations) {
  dummy_geo <- timetable_raw %>%
    distinct(.data$train_header, .data$dt_origin) %>%
    mutate(geo = "None", event = "Arrive")

  calling_patterns <- timetable_raw %>%
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

  return(timetable_raw %>%
           inner_join(groups, by = c("train_header", "dt_origin")) %>%
           ungroup())
}
