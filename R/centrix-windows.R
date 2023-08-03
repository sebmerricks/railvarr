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
  valid_track_windows <- validate_track_windows(track_events, time_windows,
                                                asset_map)
  valid_aspect_windows <- validate_aspect_windows(aspect_events, time_windows,
                                                  asset_map)
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

validate_track_windows <- function(track_events, time_windows, asset_map) {
  track_events_windowed <- dplyr::inner_join(
    track_events,
    time_windows %>%
      mutate(start = lubridate::int_start(.data$interval),
             end = lubridate::int_end(.data$interval)),
    by = dplyr::join_by(between(x$dt, y$start, y$end))
  ) %>%
    dplyr::select("window", "track", "dt", "event")

  track_activation_counts <- track_events_windowed %>%
    dplyr::arrange(.data$window, .data$track, .data$dt) %>%
    dplyr::group_by(.data$window, .data$track) %>%
    dplyr::mutate(past_event = dplyr::lag(.data$event)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(past_event = tidyr::replace_na(.data$past_event, "first")) %>%
    dplyr::group_by(.data$window, .data$track) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_enters = sum(.data$event == "enters"),
      n_vacates = sum(.data$event == "vacates"),
      n_same_as_prior = sum(.data$event == .data$past_event),
      .groups = "drop"
    )

  track_count <- asset_map %>%
    dplyr::distinct(.data$track) %>%
    nrow()

  track_events_summarised <- track_activation_counts %>%
    dplyr::group_by(.data$window) %>%
    dplyr::summarise(
      ntrains = dplyr::first(.data$n_enters),
      ntracks = dplyr::n_distinct(.data$track),
      distinct_track_counts = dplyr::n_distinct(.data$n),
      any_different_enters_vacates = any(
        (.data$n_enters - .data$n_vacates) != 0
      ),
      any_not_interlaced = any(.data$n_same_as_prior > 0)
    )

  valid_track_events_windowed <- track_events_summarised %>%
    dplyr::filter(
      .data$ntracks == track_count &
        .data$distinct_track_counts == 1 &
        !.data$any_not_interlaced &
        !.data$any_different_enters_vacates
    ) %>%
    dplyr::ungroup()

  valid_windows <- valid_track_events_windowed %>%
    dplyr::select("window", "ntrains")

  return(valid_windows)
}

validate_aspect_windows <- function(aspect_events, time_windows, asset_map) {
  signals <- asset_map %>%
    dplyr::group_by(.data$signal) %>%
    dplyr::filter(dplyr::n() == 2) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$signal)

  aspect_events_windowed <- dplyr::inner_join(
    aspect_events,
    time_windows %>%
      mutate(start = lubridate::int_start(.data$interval),
             end = lubridate::int_end(.data$interval) + 10), # + 10 to account for signal offset
    dplyr::join_by(between(x$dt, y$start, y$end))
  ) %>%
    dplyr::select("window", "signal", "dt", "aspect", "past_aspect")

  # find valid aspect windows (find_valid_aspect_events())
  red_events_windowed <- aspect_events_windowed %>%
    dplyr::semi_join(signals, by = "signal") %>%
    dplyr::arrange(.data$window, .data$signal, .data$dt) %>%
    dplyr::filter(.data$aspect == "R" | .data$past_aspect == "R") %>%
    dplyr::mutate(event = dplyr::if_else(.data$aspect == "R",
                                         "red_on",
                                         "red_off"))

  red_events_counts <- red_events_windowed %>%
    dplyr::group_by(.data$window) %>%
    dplyr::summarise(
      n_red_on = sum(.data$aspect == "R"),
      n_red_off = sum(.data$past_aspect == "R"),
      .groups = "drop"
    )

  signal_count <- signals %>%
    dplyr::distinct(.data$signal) %>%
    nrow()

  valid_red_events_windowed <- red_events_counts %>%
    dplyr::mutate(ntrains = as.integer(.data$n_red_on / signal_count)) %>%
    dplyr::filter(
      (.data$n_red_on == .data$n_red_off) &
        (.data$n_red_on %% signal_count == 0)
    )

  valid_windows <- valid_red_events_windowed %>%
    dplyr::select("window", "ntrains")

  return(valid_windows)
}

find_good_windows_in_common <- function(valid_track_windows,
                                        valid_aspect_windows) {
  good_windows <- dplyr::left_join(
    valid_track_windows,
    valid_aspect_windows,
    by = "window"
  ) %>%
    dplyr::filter(!is.na(.data$ntrains.x) &
                    !is.na(.data$ntrains.y) &
                    .data$ntrains.x == .data$ntrains.y) %>%
    dplyr::select("window")
  return(good_windows)
}
