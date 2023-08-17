#' Preprocess raw Centrix data
#'
#' @description `preprocess_signal_events()` handles only signals, defined by
#'   `raw_centrix$asset` starting with the character 'S'.
#'
#'   `preprocess_track_events()` handles only tracks, defined by
#'   `raw_centrix$asset` starting with the character 'T'.
#'
#' @details `preprocess_signal_events()` converts `raw_centrix$asset` into
#'   signal ID and signal state, the latter of which is converted to signal
#'   aspect using the `state_map`..
#'
#'   `preprocess_track_events()` converts `raw_centrix$transition` into track
#'   entry and exit events.
#'
#' @inheritParams wrangle_centrix
#'
#' @returns `preprocess_signal_events` returns a data frame containing aspect
#'   events with columns:
#' \itemize{
#'   \item{\code{signal}} [character()] signal ID.
#'   \item{\code{dt}} [lubridate::POSIXct()] datetime.
#'   \item{\code{aspect}} [factor()] signal aspect caused by the event.
#'   \item{\code{past_aspect}} [factor()] signal aspect preceding the event.
#' }
#'
#' @examples
#' data(raw_centrix, asset_map, state_mapping)
#' raw_centrix
#' aspect_events <- preprocess_signal_events(raw_centrix,
#'                                           asset_map,
#'                                           state_mapping)
#' aspect_events
#'
#' #----------------------------------------------------------------------------
#'
#' @seealso [wrangle_centrix()]
#'
#' @importFrom dplyr mutate filter select semi_join inner_join arrange group_by
#'   ungroup lag if_else
#'
#' @export
preprocess_signal_events <- function(raw_centrix,
                                     asset_map,
                                     state_map) {
  validate_centrix(raw_centrix)
  validate_asset_map(asset_map)
  validate_state_mapping(state_map)

  raw_signal_events <- raw_centrix %>%
    filter(stringr::str_starts(.data$asset, "S"))

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
      state_map,
      by = "state"
    ) %>%
    arrange(.data$signal, .data$dt) %>%
    select("signal", "dt", "aspect") %>%
    group_by(.data$signal) %>%
    mutate(past_aspect = lag(.data$aspect)) %>%
    ungroup()

  return(aspect_events)
}

#' @rdname preprocess_signal_events
#' @inherit preprocess_signal_events
#'
#' @returns `preprocess_track_events` returns a data frame containing track
#'   events with columns:
#' \itemize{
#'   \item{\code{track}} [character()] track ID.
#'   \item{\code{dt}} [lubridate::POSIXct()] datetime.
#'   \item{\code{occcupied}} [logical()] TRUE if train enters track, else
#'    FALSE.
#'   \item{\code{event}} [character()] 'enters' if train enters track, else
#'    'vacates'.
#' }
#'
#' @examples
#' data(raw_centrix, asset_map)
#' raw_centrix
#' track_events <- preprocess_track_events(raw_centrix,
#'                                         asset_map)
#' track_events
#'
#' @importFrom dplyr select mutate rename arrange semi_join if_else
#'
#' @export
preprocess_track_events <- function(raw_centrix, asset_map) {
  validate_centrix(raw_centrix)
  validate_asset_map(asset_map)

  raw_track_events <- raw_centrix %>%
    filter(stringr::str_starts(.data$asset, "T"))

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
