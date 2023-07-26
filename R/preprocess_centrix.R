#' Split raw events into separate signal and track events
#'
#' @param raw_events Data frame containing raw data.
#' @param is_track Expression determining how to split tracks and signals.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate group_by group_split
split_signal_track_events <- function(raw_events) {
  stopifnot(is_centrix(raw_events))

  events <- raw_events %>%
    mutate(is_track = startsWith(.data$asset, "T")) %>%
    group_by(.data$is_track) %>%
    group_split(.keep = F)

  return(list(as_centrix(events[[1]]), as_centrix(events[[2]])))
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
  stopifnot(is_centrix(raw_signal_events))

  signal_events <- raw_signal_events %>%
    mutate(
      signal = stringr::str_split_i(.data$asset, " ", i = 1),
      state = stringr::str_split_i(.data$asset, " ", i = 2)
    ) %>%
    filter(.data$transition == "DN to UP") %>%
    select("signal", "state", "dt", "period")

  signals <- get_map() %>%
    select("signal")

  aspect_events <- signal_events %>%
    semi_join(signals, by = "signal") %>%
    inner_join(
      env$state_mapping,
      by = "state"
    ) %>%
    arrange(.data$signal, .data$dt) %>%
    select("signal", "aspect", "dt", "period") %>%
    group_by(.data$signal) %>%
    mutate(past_aspect = lag(.data$aspect)) %>%
    ungroup() %>%
    mutate(signal = as_signal(.data$signal),
           aspect = as_aspect4(.data$aspect),
           past_aspect = as_aspect4(.data$past_aspect)) %>%
    select("signal", "aspect", "past_aspect", "dt", "period")

  return(aspect_event(aspect_events))
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
  stopifnot(is_centrix(raw_track_events))

  tracks <- get_map() %>%
    select("track")

  track_activations <- raw_track_events %>%
    mutate(occupied = if_else(.data$transition == "UP to DN", T, F)) %>%
    select(-"transition") %>%
    rename(track = "asset") %>%
    mutate(track = stringr::str_extract(.data$track, "^([A-z])+(-[0-9])?")) %>%
    select("period", "track", "dt", "occupied") %>%
    arrange(.data$track, .data$dt) %>%
    mutate(event = if_else(.data$occupied, "enters", "vacates")) %>%
    semi_join(tracks, by = "track") %>%
    mutate(track = as_track(track)) %>%
    select("track", "occupied", "dt", "event", "period")

  return(track_event(track_activations))
}
