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

  good_windows <- find_good_windows(time_windows)

  valid_track_events <- validate_track_events(track_events, good_windows)
  valid_red_events <- validate_aspect_events(aspect_events, good_windows)

  berth_events <- calculate_tsars(valid_track_events, valid_red_events)

  return(berth_events)
}

#' Split Signal and Track Events
#'
#' Splits a single Centrix data frame containing both signal and track events
#' into a list containing two data frames.
#'
#' @param raw_centrix A data frame containing raw Centrix data with both signal
#'   and track events. No input validation is performed, see [wrangle_centrix()]
#'   for expected structure.
#'
#' @returns A list containing two data frames. The first data frame contains raw
#'   signal events, the second contains raw track events.
#'
#' @seealso [wrangle_centrix()]
#'
#' @export
split_signal_track_events <- function(raw_events) {
  events <- raw_events %>%
    mutate(is_track = stringr::str_starts(asset, "T")) %>%
    group_by(.data$is_track) %>%
    group_split(.keep = F)
  return(events)
}

#' Preprocess Raw Signal Events
#'
#' Performs preprocessing on raw signal events. Splits raw assets into signal
#' IDs and states. Converts signal state to aspect using the state mapping.
#'
#' @param raw_signal_events Data frame containing raw signal events from Centrix
#'   data. No input validation is performed. The data should match the structure
#'   of Centrix data as described in [wrangle_centrix()].
#' @param asset_map Data frame containing a map of the track section, used to
#'   filter the raw signal events down to only the signals specified in the
#'   asset map. No input validation is performed. See [wrangle_centrix()] for
#'   the expected structure.
#' @param state_mapping Data frame containing a 1-1 mapping from signal state to
#'   aspect. See [state_mapping] for the expected structure.
#'
#' @returns A data frame containing aspect events with columns:
#'  * 'signal': (character) signal ID,
#'  * 'dt': (lubridate::POSIXct) datetime,
#'  * 'aspect': (factor) signal aspect after train enters the signal section.
#'  * 'past_aspect': (factor) signal aspect before train enters the signal
#'                            section.
#'
#' @export
preprocess_signal_events <- function(raw_signal_events, asset_map,
                                     state_mapping) {
  signal_events <- raw_signal_events %>%
    mutate(
      signal = stringr::str_split_i(.data$asset, " ", i = 1),
      state = stringr::str_split_i(.data$asset, " ", i = 2)
    ) %>%
    filter(.data$transition == "DN to UP") %>%
    select("signal", "dt", "state")

  signals <- asset_map %>%
    select("signal")

  aspect_events <- signal_events %>%
    semi_join(signals, by = "signal") %>%
    inner_join(
      state_mapping,
      by = "state"
    ) %>%
    arrange(.data$signal, .data$dt) %>%
    select("signal", "dt", "aspect") %>%
    group_by(.data$signal) %>%
    mutate(past_aspect = lag(.data$aspect)) %>%
    ungroup()

  return(aspect_events)
}

#' Preprocess Raw Track Events
#'
#' Performs preprocessing on raw track events. Converts transition data to
#' whether the train enters or vacates the track.
#'
#' @param raw_track_events Data frame containing raw track events from Centrix
#'   data. No input validation is performed. The data should matche the
#'   structure of Centrix data as described in [wrangle_centrix()].
#' @param asset_map Data frame containing a map of the track section, used to
#'   filter the raw track events down to only the tracks specified in the asset
#'   map. No input validation is performed, see [wrangle_centrix()] for the
#'   expected structure.
#'
#' @returns A data frame containing track events with columns:
#'  * 'track': (character) track ID,
#'  * 'dt': (lubridate::POSIXct) datetime,
#'  * 'occupied': (logical) TRUE if train enters track, else FALSE,
#'  * 'event': (character) 'enters' if train enters track, else 'vacates'.
#'
#' @seealso [wrangle_centrix()]
#'
#' @export
preprocess_track_events <- function(raw_track_events, asset_map) {
  tracks <- asset_map %>%
    select("track")

  track_activations <- raw_track_events %>%
    mutate(occupied = if_else(.data$transition == "UP to DN", T, F)) %>%
    select(-"transition") %>%
    rename(track = "asset") %>%
    mutate(track = stringr::str_extract(.data$track, "^([A-z])+(-[0-9])?")) %>%
    select("track", "dt", "occupied") %>%
    arrange(.data$track, .data$dt) %>%
    mutate(event = if_else(.data$occupied, "enters", "vacates")) %>%
    semi_join(tracks, by = "track")

  return(track_activations)
}
