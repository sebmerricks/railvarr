#' Calculate TSARs from Raw Centrix
#'
#' @export
calculate_tsars <- function() {
  asset_map <- get_asset_mapping()
  stopifnot(!is.null(asset_map))

  centrix <- get_centrix()
  stopifnot(!is.null(centrix))

  stopifnot(!is.null(get_state_mapping()))

  split_events <- split_signal_track_events(centrix)

  aspect_events <- split_events[[1]] %>%
    preprocess_signal_events
  track_events <- split_events[[2]] %>%
    preprocess_track_events

  time_windows <- track_events %>%
    find_intervals() %>%
    find_time_windows(track_events)

  track_events_windowed <- track_events %>%
    window_track_events(time_windows)

  red_events_windowed <- aspect_events %>%
    window_aspect_events(time_windows) %>%
    filter_red_events()

  good_windows <- find_good_windows(track_events_windowed,
                                    red_events_windowed)

  valid_track_events <- track_events_windowed %>%
    validate_track_events(good_windows)

  valid_red_events <- red_events_windowed %>%
    validate_red_events(good_windows)

  berth_events <- combine_track_aspect_events(valid_track_events,
                                              valid_red_events)

  environment$berth_events <- berth_events

  return(get_berth_events())
}

#' @export
get_berth_events <- function() {
  environment$berth_events
}
