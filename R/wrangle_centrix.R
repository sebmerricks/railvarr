#' Wrangle raw Centrix data
#'
#' @param signal_events Data frame containing pre-processed signal data.
#' @param track_events Data frame containing pre-processed track data.
#'
#' @export
#'
wrangle_centrix <- function(signal_events, track_events) {
  map <- get_map()
  start_track <- (map %>% first())$track
  end_track <- (map %>% last())$track

  track_activations_start_end <- track_events %>%
    filter((track == start_track & event == "enters") |
             (track == end_track & event == "vacates")) %>%
    select(period, track, dt) %>%
    group_by(track) %>%
    group_split()

  track_activations_intervals <- track_activations_start_end %>%
    mutate(next_dt = lead(dt)) %>%
    filter(track != lead(track) &
             track == end_track &
             period == lead(period)) %>%
    mutate(diff = as.integer(lubridate::as.duration(next_dt - dt))) %>%
    filter(diff > 0) %>%
    mutate(interval = lubridate::interval(start = dt + 1,
                                          end = next_dt - 1)) %>%
    select(period, interval)

  has_no_activations <- track_activations_intervals %>%
    mutate(start = int_start(interval),
           end = int_end(interval)) %>%
    full_join(track_events,
              by = join_by(between(y$dt, x$start, x$end))) %>%
    mutate(has_activations = !is.na(dt),
           period = period.x) %>%
    select(period, interval, has_activations) %>%
    filter(!has_activations)

  time_windows <- has_no_activations %>%
    group_by(period) %>%
    mutate(
      left = int_end(interval) + 1,
      right = int_start(lead(interval)) - 1
    ) %>%
    mutate(right = if_else(
      is.na(right),
      left + days(1),
      right
    )) %>%
    select(period, left, right) %>%
    mutate(interval = lubridate::interval(start = left, end = right)) %>%
    ungroup() %>%
    mutate(window = row_number()) %>%
    select(period, window, interval)
}


