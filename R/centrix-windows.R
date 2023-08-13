#' Calculate train journey intervals
#'
#' Calculates a set of time intervals containing valid Centrix observations.
#' These observations correspond to distinct / interlaced train journeys.
#'
#' @param aspect_events A data frame containing pre-processed aspect events with
#' columns:
#' \itemize{
#'   \item{\code{signal}} (character) signal ID.
#'   \item{\code{dt}} ([lubridate::POSIXct]) datetime.
#'   \item{\code{aspect}} (factor) signal aspect after train enters the signal
#'    section.
#'   \item{\code{past_aspect}} (factor) signal aspect before train enters the
#'    signal section.
#' }
#' @param track_events A data frame containing pre-processed track events with
#' columns:
#' \itemize{
#'   \item{\code{track}} (character) track ID.
#'   \item{\code{dt}} ([lubridate::POSIXct]) datetime.
#'   \item{\code{occcupied}} (logical) TRUE if train enters track, else FALSE.
#'   \item{\code{event}} (character) 'enters' if train enters track, else
#'    'vacates'.
#' }
#' @inheritParams wrangle_centrix
#'
#' @returns A data frame containing window IDs and time intervals in the
#'   columns:
#'   \itemize{
#'     \item{\code{window}} (numeric) window ID.
#'     \item{\code{interval}} ([lubridate::interval]) time intervals.
#'   }
#'   Each window indicates a time interval in which the data contain valid
#'   observations.
#'
#' @seealso [preprocess_signal_events()]
#'
#' @importFrom dplyr semi_join
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
  valid_windows_with_intervals <- time_windows %>%
    semi_join(valid_windows, by = "window")
  return(valid_windows_with_intervals)
}

#' Filter Centrix events by time intervals
#'
#' @description
#' Filter Centrix events to only those that occur within the calculated valid
#' time intervals.
#'
#' `filter_track_events()` filters to only the tracks specified in the asset
#' map.
#'
#' `filter_aspect_events()` filters to only the signals specified in the asset
#' map.
#'
#' @param aspect_events A data frame containing pre-processed aspect events with
#' columns:
#' \itemize{
#'   \item{\code{signal}} (character) signal ID.
#'   \item{\code{dt}} ([lubridate::POSIXct]) datetime.
#'   \item{\code{aspect}} (factor) signal aspect after train enters the signal
#'    section.
#'   \item{\code{past_aspect}} (factor) signal aspect before train enters the
#'    signal section.
#' }
#' @param time_windows A data frame containing window IDs and time intervals in
#'   the columns:
#'   \itemize{
#'     \item{\code{window}} (numeric) window ID.
#'     \item{\code{interval}} ([lubridate::interval]) time intervals.
#'   }
#'   Each window indicates a time interval in which the data contain valid
#'   observations.
#' @inheritParams calculate_time_windows
#'
#' @returns A data frame containing filtered events, with additional columns:
#'   \itemize{
#'     \item{\code{window}} (numeric) the window ID.
#'     \item{\code{interval}} ([lubridate::interval]) the time interval.
#'   }
#'
#' @seealso [preprocess_track_events()] [calculate_time_windows()]
#'
#' @importFrom dplyr inner_join mutate join_by select
#'
#' @export
filter_aspect_events <- function(aspect_events, time_windows, asset_map) {
  signals <- asset_map %>%
    group_by(.data$signal) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    distinct(.data$signal)

  valid_aspect_events <- inner_join(
    aspect_events %>% semi_join(signals, by = "signal"),
    time_windows %>%
      mutate(start = lubridate::int_start(.data$interval),
             end = lubridate::int_end(.data$interval) + 10),
    by = join_by(between(x$dt, y$start, y$end))
  ) %>%
    select(-"start", -"end")
  return(valid_aspect_events)
}

#' @rdname filter_aspect_events
#' @inherit filter_aspect_events
#' @importFrom dplyr inner_join mutate join_by select
#' @export
filter_track_events <- function(track_events, time_windows, asset_map) {
  tracks <- asset_map %>%
    distinct(.data$track)

  valid_track_events <- inner_join(
    track_events %>% semi_join(tracks, by = "track"),
    time_windows %>%
      mutate(start = lubridate::int_start(.data$interval),
             end = lubridate::int_end(.data$interval)),
    by = join_by(between(x$dt, y$start, y$end))
  ) %>%
    select(-"start", -"end")
  return(valid_track_events)
}

#' @importFrom dplyr first last filter select arrange mutate lead full_join
#'   join_by lag bind_rows distinct row_number
find_time_windows <- function(track_events, asset_map) {
  start_track <- first(asset_map$track)
  end_track <- last(asset_map$track)

  track_events_start_end <- track_events %>%
    filter((.data$track == start_track & .data$event == "enters") |
                    (.data$track == end_track & .data$event == "vacates")) %>%
    select("track", "dt") %>%
    arrange(.data$dt)

  track_intervals <- track_events_start_end %>%
    mutate(next_dt = lead(.data$dt)) %>%
    filter(.data$track != lead(.data$track) &
                    .data$track == end_track) %>%
    mutate(diff = as.integer(
      lubridate::as.duration(.data$next_dt - .data$dt))) %>%
    filter(.data$diff > 0) %>%
    mutate(interval = lubridate::interval(start = .data$dt + 1,
                                                 end = .data$next_dt - 1)) %>%
    select("interval")

  has_no_events <- track_intervals %>%
    mutate(start = lubridate::int_start(.data$interval),
                  end = lubridate::int_end(.data$interval)) %>%
    full_join(track_events,
                     by = join_by(between(y$dt, x$start, x$end))) %>%
    mutate(has_events = !is.na(.data$dt)) %>%
    select("interval", "has_events") %>%
    filter(!.data$has_events)

  windows <- has_no_events %>%
    mutate(
      left = lubridate::int_end(.data$interval) + 1,
      right = lubridate::int_start(lead(.data$interval)) - 1
    ) %>%
    mutate(right = if_else(
      is.na(.data$right),
      .data$left + lubridate::days(1),
      .data$right
    ))

  first_window <- has_no_events %>%
    mutate(left = lubridate::int_end(lag(interval)) + 1,
                  right = lubridate::int_start(interval) - 1) %>%
    mutate(left = if_else(
      is.na(left),
      right - lubridate::days(1),
      left
    ))

  time_windows <- bind_rows(windows, first_window) %>%
    select("left", "right") %>%
    mutate(interval = lubridate::interval(
      start = .data$left, end = .data$right)) %>%
    distinct(.data$interval) %>%
    arrange(.data$interval) %>%
    mutate(window = row_number()) %>%
    select("window", "interval")

  return(time_windows)
}

#' @importFrom dplyr inner_join mutate join_by select arrange group_by ungroup
#'   summarise n distinct first n_distinct filter
validate_track_windows <- function(track_events, time_windows, asset_map) {
  track_events_windowed <- inner_join(
    track_events,
    time_windows %>%
      mutate(start = lubridate::int_start(.data$interval),
             end = lubridate::int_end(.data$interval)),
    by = join_by(between(x$dt, y$start, y$end))
  ) %>%
    select("window", "track", "dt", "event")

  track_activation_counts <- track_events_windowed %>%
    arrange(.data$window, .data$track, .data$dt) %>%
    group_by(.data$window, .data$track) %>%
    mutate(past_event = lag(.data$event)) %>%
    ungroup() %>%
    mutate(past_event = tidyr::replace_na(.data$past_event, "first")) %>%
    group_by(.data$window, .data$track) %>%
    summarise(
      n = n(),
      n_enters = sum(.data$event == "enters"),
      n_vacates = sum(.data$event == "vacates"),
      n_same_as_prior = sum(.data$event == .data$past_event),
      .groups = "drop"
    )

  track_count <- asset_map %>%
    distinct(.data$track) %>%
    nrow()

  track_events_summarised <- track_activation_counts %>%
    group_by(.data$window) %>%
    summarise(
      ntrains = first(.data$n_enters),
      ntracks = n_distinct(.data$track),
      distinct_track_counts = n_distinct(.data$n),
      any_different_enters_vacates = any(
        (.data$n_enters - .data$n_vacates) != 0
      ),
      any_not_interlaced = any(.data$n_same_as_prior > 0)
    )

  valid_track_events_windowed <- track_events_summarised %>%
    filter(
      .data$ntracks == track_count &
        .data$distinct_track_counts == 1 &
        !.data$any_not_interlaced &
        !.data$any_different_enters_vacates
    ) %>%
    ungroup()

  valid_windows <- valid_track_events_windowed %>%
    select("window", "ntrains")

  return(valid_windows)
}

#' @importFrom dplyr group_by filter ungroup distinct inner_join mutate join_by
#'   select semi_join arrange if_else summarise
validate_aspect_windows <- function(aspect_events, time_windows, asset_map) {
  signals <- asset_map %>%
    group_by(.data$signal) %>%
    filter(n() == 2) %>%
    ungroup() %>%
    distinct(.data$signal)

  aspect_events_windowed <- inner_join(
    aspect_events,
    time_windows %>%
      mutate(start = lubridate::int_start(.data$interval),
             end = lubridate::int_end(.data$interval) + 10), # + 10 to account for signal offset
    join_by(between(x$dt, y$start, y$end))
  ) %>%
    select("window", "signal", "dt", "aspect", "past_aspect")

  red_events_windowed <- aspect_events_windowed %>%
    semi_join(signals, by = "signal") %>%
    arrange(.data$window, .data$signal, .data$dt) %>%
    filter(.data$aspect == "R" | .data$past_aspect == "R") %>%
    mutate(event = if_else(.data$aspect == "R",
                                         "red_on",
                                         "red_off"))

  red_events_counts <- red_events_windowed %>%
    group_by(.data$window) %>%
    summarise(
      n_red_on = sum(.data$aspect == "R"),
      n_red_off = sum(.data$past_aspect == "R"),
      .groups = "drop"
    )

  signal_count <- signals %>%
    distinct(.data$signal) %>%
    nrow()

  valid_red_events_windowed <- red_events_counts %>%
    mutate(ntrains = as.integer(.data$n_red_on / signal_count)) %>%
    filter(
      (.data$n_red_on == .data$n_red_off) &
        (.data$n_red_on %% signal_count == 0)
    )

  valid_windows <- valid_red_events_windowed %>%
    select("window", "ntrains")

  return(valid_windows)
}

#' @importFrom dplyr left_join filter select
find_good_windows_in_common <- function(valid_track_windows,
                                        valid_aspect_windows) {
  good_windows <- left_join(
    valid_track_windows,
    valid_aspect_windows,
    by = "window"
  ) %>%
    filter(!is.na(.data$ntrains.x) &
                    !is.na(.data$ntrains.y) &
                    .data$ntrains.x == .data$ntrains.y) %>%
    select("window")
  return(good_windows)
}
