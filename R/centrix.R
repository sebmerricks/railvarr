#' Wrangle Raw Centrix Data
#'
#' This function takes in raw Centrix data as an input and returns a fully
#' processed data frame containing berth-level signal and track observations,
#' along with calculations of TSAR and all its subcomponents.
#'
#' @param centrix A data frame containing raw Centrix data. Strict input
#'   validation is #' applied to ensure the data adhere to the expected
#'   structure. The data should be in a data frame with the following columns:
#'   \itemize{
#'     \item{\code{asset}}: A character vector containing the asset ID. Tracks
#'        are expected to follow the regex pattern `"T[A-Z]+(-[0-9])?"`. E.g.,
#'        'TABC' 'TABC-1' 'TABC-2'.
#'       Signals are expected to follow the regex pattern `"S[0-9]+\s[A-Z]+"`.
#'        E.g., 'S123 HGE' 'S123 I'.
#'       The second part of the signal ID should be a state code referring to
#'        the signal aspect, e.g. 'RGE' for red or 'DGE' for green. For other
#'        codes see:
#'    [https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals].
#'     \item{\code{dt}}: A [lubridate::POSIXct] object representing the date
#'       and time at which the observation was made.
#'     \item{\code{transition}}: A character vector representing the state
#'       transition that caused the observation. A transition can be one of
#'       "UP to DN" or "DN to UP", anything else will be ignored. For tracks,
#'       "UP to DN" signifies a train entering the track, while "DN to UP" is a
#'       train vacating the track. For signals, "UP to DN" represents an aspect
#'       change. Therefore, any signals with a "DN to UP" transition will be
#'       ignored.
#'   }
#' @param asset_map A data frame containing a 1-1 mapping from signals to berths
#'   and a 1-many mapping from berths to tracks. The map also contains a 1-many
#'   mapping from tracks to events, reflecting the fact that there is an 'enter'
#'   and a 'vacate' event for each track. Therefore, the data frame should
#'   adhere to the following structure:
#'   \itemize{
#'     \item{\code{signal}}: A character vector containing the signal ID. It
#'       should follow the regex pattern `"S[0-9]+"`.
#'     \item{\code{berth}}: A character vector containing the berth ID. It
#'       should follow the regex pattern `"[A-Z]+"`.
#'     \item{\code{track}}: A character vector containing the track ID. It
#'       should follow the regex pattern `"T[A-Z]+(-[0-9])?"`.
#'     \item{\code{event}}: A character vector containing the event type. It
#'       must be one of either "enters" or "vacates".
#'    }
#' @param state_mapping A data frame containing a 1-1 mapping from signal state
#'   to signal aspect. See [state_mapping] (the default) for the expected
#'   structure.
#'
#' @returns A data frame containing the fully processed Centrix data, containing
#'   berth-level information about signal and track events. This includes TSARs
#'   and all sub-components, e.g. 'T_travel' and 'T_offset'.
#'
#' @seealso All lower-level functions that this function wraps.
#'
#' @examples
#' # This will require some example data
#'
#' @export
wrangle_centrix <- function(raw_centrix, asset_map, state_mapping = NULL) {
  validate_centrix(raw_centrix)
  validate_asset_map(asset_map)
  validate_state_mapping(state_mapping)

  separated_events <- split_signal_track_events(raw_centrix)
  raw_aspect_events <- separated_events[[1]]
  raw_track_events <- separated_events[[2]]

  aspect_events <- preprocess_signal_events(raw_aspect_events)
  track_events <- preprocess_track_events(raw_track_events)

  time_windows <- calculate_time_windows(aspect_events, track_events)

  valid_track_events <- validate_track_events(track_events, time_windows)
  valid_red_events <- validate_aspect_events(aspect_events, time_windows)

  berth_events <- calculate_tsars(valid_track_events, valid_red_events)

  return(berth_events)
}

#' Calculate Centrix Time Windows
#'
#' Calculates a set of time windows containing valid Centrix observations. This
#' function acts as a wrapper for a number of functions involved in the
#' calculation. Returns a data frame containing the window ID and time interval.
#'
#' @param aspect_events A data frame containing pre-processed aspect events from
#'   Centrix data. No input validation is performed. It is assumed that the data
#'   frame will follow the structure returned by the function
#'   [preprocess_signal_events()].
#' @param track_events A data frame containing pre-processed track events from
#'   Centrix data. No input validation is performed. It is assumed that the data
#'   frame will follow the structure return by the function
#'   [preprocess_track_events()].
#' @param asset_map A data frame containing a map of the track section. No input
#'   validation is performed. See [wrangle_centrix()] for the expected
#'   structure.
#'
#' @returns A data frame containing window IDs and time intervals in the
#'   columns:
#'  * window: (numeric) window ID,
#'  * interval: (lubridate::interval) window time interval.
#'   Each window indicates a time interval in which the data contain valid
#'   observations.
#'
#' @export
calculate_time_windows <- function(aspect_events, track_events, asset_map) {
  time_windows <- find_time_windows(track_events, asset_map)
  valid_track_windows <- validate_track_windows(track_events, time_windows)
  valid_aspect_windows <- validate_aspect_windows(aspect_events, time_windows)
  valid_windows <- find_good_windows_in_common(valid_track_windows,
                                               valid_aspect_windows)
  return(valid_windows)
}

find_time_windows <- function(track_events, asset_map) {
  start_track <- dplyr::first(asset_map$track)
  end_track <- dplyr::last(asset_map$track)

  track_events_start_end <- track_events %>%
    dplyr::filter((.data$track == start_track & .data$event == "enters") |
             (.data$track == end_track & .data$event == "vacates")) %>%
    dplyr::select("track", "dt") %>%
    dplyr::arrange(.data$dt)

  track_intervals <- track_events_start_end %>%
    dplyr::mutate(next_dt = dplyr::lead(.data$dt)) %>%
    dplyr::filter(.data$track != dplyr::lead(.data$track) &
                    .data$track == end_track) %>%
    dplyr::mutate(diff = as.integer(
      lubridate::as.duration(.data$next_dt - .data$dt))) %>%
    dplyr::filter(.data$diff > 0) %>%
    dplyr::mutate(interval = lubridate::interval(start = .data$dt + 1,
                                          end = .data$next_dt - 1)) %>%
    dplyr::select("interval")

  has_no_events <- track_intervals %>%
    dplyr::mutate(start = lubridate::int_start(.data$interval),
           end = lubridate::int_end(.data$interval)) %>%
    dplyr::full_join(track_events,
              by = dplyr::join_by(between(y$dt, x$start, x$end))) %>%
    dplyr::mutate(has_events = !is.na(.data$dt)) %>%
    dplyr::select("interval", "has_events") %>%
    dplyr::filter(!.data$has_events)

  windows <- has_no_events %>%
    dplyr::mutate(
      left = lubridate::int_end(.data$interval) + 1,
      right = lubridate::int_start(dplyr::lead(.data$interval)) - 1
    ) %>%
    dplyr::mutate(right = if_else(
      is.na(.data$right),
      .data$left + lubridate::days(1),
      .data$right
    ))

  first_window <- has_no_events %>%
    dplyr::mutate(left = lubridate::int_end(dplyr::lag(interval)) + 1,
           right = lubridate::int_start(interval) - 1) %>%
    dplyr::mutate(left = dplyr::if_else(
      is.na(left),
      right - lubridate::days(1),
      left
    ))

  time_windows <- bind_rows(windows, first_window) %>%
    dplyr::select("left", "right") %>%
    dplyr::mutate(interval = lubridate::interval(
      start = .data$left, end = .data$right)) %>%
    dplyr::distinct(.data$interval) %>%
    dplyr::arrange(.data$interval) %>%
    dplyr::mutate(window = dplyr::row_number()) %>%
    dplyr::select("window", "interval")

  return(time_windows)
}

validate_track_windows <- function(track_events, time_windows) {
  # window track events (window_track_events())

  # find valid track windows (find_valid_track_events())

  return(valid_track_windows)
}

validate_aspect_windows <- function(aspect_events, time_windows) {
  # window aspect events (window_aspect_events())

  # find valid aspect windows (find_valid_aspect_events())

  return(valid_aspect_windows)
}

find_good_windows_in_common <- function(valid_track_windows,
                                        valid_aspect_windows) {
  # find_good_windows()
  return(good_windows)
}
