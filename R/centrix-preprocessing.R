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
#' @importFrom dplyr filter mutate group_by group_split
#'
#' @export
split_signal_track_events <- function(raw_events) {
  events <- raw_events %>%
    filter(stringr::str_starts(.data$asset, "T|S")) %>%
    mutate(is_track = stringr::str_starts(.data$asset, "T")) %>%
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
#'   section.
#'
#' @importFrom dplyr mutate filter select semi_join inner_join arrange group_by
#'   ungroup lag
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
#' @importFrom dplyr select mutate rename arrange semi_join if_else
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
