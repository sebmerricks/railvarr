#' Split raw events into separate signal and track events
#'
#' @param raw_events Data frame containing raw data.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate group_by group_split
split_signal_track_events <- function(raw_events) {
  events <- raw_events %>%
    mutate(is_track = stringr::str_starts(asset, "T")) %>%
    group_by(.data$is_track) %>%
    group_split(.keep = F)

  return(events)
}

#' Preprocess raw signal events
#'
#' @param raw_signal_events Raw signal data.
#'
#' @export
#'
#' @import dplyr
#'
preprocess_signal_events <- function(raw_signal_events) {
  signal_events <- raw_signal_events %>%
    mutate(
      signal = stringr::str_split_i(.data$asset, " ", i = 1),
      state = stringr::str_split_i(.data$asset, " ", i = 2)
    ) %>%
    filter(.data$transition == "DN to UP") %>%
    select("signal", "dt", "state", "period")

  signals <- get_asset_mapping() %>%
    select("signal")

  aspect_events <- signal_events %>%
    semi_join(signals, by = "signal") %>%
    inner_join(
      get_state_mapping(),
      by = "state"
    ) %>%
    arrange(.data$signal, .data$dt) %>%
    select("period", "signal", "dt", "aspect") %>%
    group_by(.data$signal) %>%
    mutate(past_aspect = lag(.data$aspect)) %>%
    ungroup()

  return(aspect_events)
}

#' Preprocess raw track data
#'
#' @param raw_track_events Data frame containing raw track data
#'
#' @export
#'
#' @importFrom dplyr %>% mutate if_else select rename arrange semi_join
#'
preprocess_track_events <- function(raw_track_events) {
  tracks <- get_asset_mapping() %>%
    select("track")

  track_activations <- raw_track_events %>%
    mutate(occupied = if_else(.data$transition == "UP to DN", T, F)) %>%
    select(-"transition") %>%
    rename(track = "asset") %>%
    mutate(track = stringr::str_extract(.data$track, "^([A-z])+(-[0-9])?")) %>%
    select("period", "track", "dt", "occupied") %>%
    arrange(.data$track, .data$dt) %>%
    mutate(event = if_else(.data$occupied, "enters", "vacates")) %>%
    semi_join(tracks, by = "track")

  return(track_activations)
}
