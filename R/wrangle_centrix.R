check_inputs <- function(aspect_events, track_events) {
  aspect_names <- c("period", "signal", "dt", "aspect", "past_aspect")
  aspect_types <- list(numeric(), character(), lubridate::POSIXct(),
                       factor(), factor())
  check_df(aspect_events, aspect_names, aspect_types)

  track_names <- c("period", "track", "dt", "occupied", "event")
  track_types <- list(numeric(), character(), lubridate::POSIXct(), logical(),
                      character())
  check_df(track_events, track_names, track_types)
}

find_intervals <- function(track_events, start_track, end_track) {
  start_track <- (get_map() %>% first())$track
  end_track <- (get_map() %>% last())$track

  track_activations_start_end <- track_events %>%
    filter((track == start_track & event == "enters") |
             (track == end_track & event == "vacates")) %>%
    select(period, track, dt) %>%
    arrange(period, dt)

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

  return(track_activations_intervals)
}

find_time_windows <- function(track_activations_intervals, track_events) {
  has_no_activations <- track_activations_intervals %>%
    mutate(start = lubridate::int_start(interval),
           end = lubridate::int_end(interval)) %>%
    full_join(track_events,
              by = join_by(between(y$dt, x$start, x$end))) %>%
    mutate(has_activations = !is.na(dt),
           period = period.x) %>%
    select(period, interval, has_activations) %>%
    filter(!has_activations)

  time_windows <- has_no_activations %>%
    group_by(period) %>%
    mutate(
      left = lubridate::int_end(interval) + 1,
      right = lubridate::int_start(lead(interval)) - 1
    ) %>%
    mutate(right = if_else(
      is.na(right),
      left + lubridate::days(1),
      right
    )) %>%
    select(period, left, right) %>%
    mutate(interval = lubridate::interval(start = left, end = right)) %>%
    ungroup() %>%
    mutate(window = row_number()) %>%
    select(period, window, interval)

  return(time_windows)
}

window_track_activations <- function(track_events, time_windows) {
  windows_times <- time_windows %>%
    mutate(start_time = lubridate::int_start(interval),
           end_time = lubridate::int_end(interval)) %>%
    select(-period)

  track_activations_windows <- inner_join(
    track_events,
    windows_times,
    join_by(between(dt, start_time, end_time))
  ) %>%
    select(period, window, track, dt, event)

  return(track_activations_windows)
}

find_valid_track_activations <- function(track_activations_windows) {
  track_activation_counts <- track_activations_windows %>%
    arrange(period, window, track, dt) %>%
    group_by(period,window,track) %>%
    mutate(past_event = lag(event)) %>%
    ungroup() %>%
    # the next statement is too slow if done per grouping
    mutate(past_event = tidyr::replace_na(past_event, "first")) %>%
    group_by(period,window,track) %>%
    summarise(
      n = n(),
      n_enters = sum(event == "enters"),
      n_vacates = sum(event == "vacates"),
      n_same_as_prior = sum(event == past_event),
      .groups = "drop"
    )

  track_count <- get_map() %>%
    distinct(track) %>%
    nrow()

  track_activations_summarised <- track_activation_counts %>%
    group_by(period, window) %>%
    summarise(
      ntrains_track = first(n_enters),
      ntracks = n_distinct(track),
      distinct_track_counts = n_distinct(n),
      any_different_enters_vacates = any((n_enters - n_vacates) != 0),
      any_not_interlaced = any(n_same_as_prior > 0)
    )

  valid_track_activations_windowed <- track_activations_summarised %>%
    filter(
      ntracks == track_count &
        distinct_track_counts == 1 &
        !any_not_interlaced &
        !any_different_enters_vacates
    ) %>%
    ungroup()

  return(valid_track_activations_windowed)
}

window_aspect_events <- function(aspect_events, time_windows) {
  valid_signals <- get_map() %>%
    group_by(signal) %>%
    filter(n() == 2) %>%
    ungroup()

  signals <- valid_signals %>%
    distinct(signal)

  aspect_events_windowed <- inner_join(
    aspect_events,
    time_windows %>%
      mutate(start_time = lubridate::int_start(interval),
             end_time = lubridate::int_end(interval) + 10), # + 10 to account for signal offset
    join_by(between(dt, start_time, end_time))
  ) %>%
    select(window, signal, dt, aspect, past_aspect)

  red_events_windowed <- aspect_events_windowed %>%
    semi_join(signals, by = "signal") %>%
    arrange(window, signal, dt) %>%
    filter(aspect == "R" | past_aspect == "R") %>%
    mutate(event = ifelse(aspect == "R", "red_on", "red_off"))

  return(red_events_windowed)
}

find_valid_aspect_events <- function(red_events_windowed) {
  red_events_counts <- red_events_windowed %>%
    group_by(window) %>%
    summarise(
      n_red_on = sum(aspect == "R"),
      n_red_off = sum(aspect != "R"),
      .groups = "drop"
    )

  valid_signals <- get_map() %>%
    group_by(signal) %>%
    filter(n() == 2) %>%
    ungroup()

  signal_count <- valid_signals %>%
    distinct(signal) %>%
    nrow()

  valid_red_events_windowed <- red_events_counts %>%
    filter((n_red_on == n_red_off) &  (n_red_on %% signal_count == 0)) %>%
    mutate(ntrains = as.integer(n_red_on / signal_count)) %>%
    select(window, ntrains)

  return(valid_red_events_windowed)
}

find_good_windows <- function(track_activations_windows, red_events_windowed) {
  good_windows <- left_join(
    track_activations_windows %>%
      find_valid_track_activations(),
    red_events_windowed %>%
      find_valid_aspect_events(),
    by = "window"
  ) %>%
    filter(!is.na(ntrains) & ntrains_track == ntrains)
}

validate_track_activations <- function(track_activations_windows,
                                       good_windows) {
  valid_track_activations <- track_activations_windows %>%
    semi_join(good_windows, by = c("period", "window"))
  return(valid_track_activations)
}

validate_red_events <- function(red_events_windowed, good_windows) {
  valid_red_events <- red_events_windowed %>%
    semi_join(good_windows, by = c("window")) %>%
    group_by(window, signal) %>%
    mutate(window_train_id = cumsum(event == "red_on")) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c(window,signal, window_train_id),
      values_from = c(dt, past_aspect),
      names_from = event,
      names_glue = "dt_{.name}"
    ) %>%
    select(-dt_past_aspect_red_off) %>%
    rename(
      aspect = dt_past_aspect_red_on,
      t_red_on = dt_dt_red_on,
      t_red_off = dt_dt_red_off
    ) %>%
    mutate(TSAR = lubridate::as.duration(t_red_off - t_red_on))

  return(valid_red_events)
}

combine_track_aspect_events <- function(valid_track_activations,
                                        valid_red_events) {
  add_signal_berth <- valid_track_activations %>%
    inner_join(
      get_map(), by = c("track", "event")
    ) %>%
    select(window, signal, berth, dt, event)

  windowed_train_ids <- add_signal_berth %>%
    arrange(window, signal, dt) %>%
    group_by(window, signal) %>%
    mutate(window_train_id = cumsum(event == "enters")) %>%
    ungroup()

  calculate_timings <- windowed_train_ids %>%
    tidyr::pivot_wider(
      id_cols = c(window,signal,berth,window_train_id),
      values_from = dt,
      names_from = event,
      names_glue = "t_{.name}"
    ) %>%
    group_by(window, window_train_id) %>%
    mutate(t_enters_next = lead(t_enters)) %>%
    ungroup()

  add_red_events <- calculate_timings %>%
    inner_join(
      valid_red_events, by = c("window", "signal", "window_train_id")
    )

  train_ids <- add_red_events %>%
    group_by(window, window_train_id) %>%
    mutate(train_id = cur_group_id()) %>%
    ungroup()

  calculate_durations <- train_ids %>%
    mutate(
      T_onset  = t_red_on - t_enters,
      T_offset = t_red_off - t_vacates,
      T_travel = t_enters_next - t_enters,
      T_coach  = t_vacates - t_enters_next,
      T_clear = t_vacates - t_enters,
    )

  berth_events <- calculate_durations %>%
    select(signal, berth, train_id, aspect, t_enters, t_red_on, t_enters_next,
           t_vacates, t_red_off, TSAR, T_onset, T_clear, T_offset, T_travel,
           T_coach) %>%
    mutate(across(TSAR:last_col(), lubridate::as.duration),
           across(TSAR:last_col(), as.double))

  return(berth_events)
}

#' Wrangle Centrix Data
#'
#' This function performs data wrangling on aspect and track events data to
#' create a data frame containing berth level information including TSAR (Time
#' Signal At Red) and its sub-components. The data produced by this function is
#' suitable for analysis.
#'
#' @param aspect_events A data frame representing aspect events data. Please
#'    describe expected data structure.
#' @param track_events A data frame representing track events data. Please
#'    describe expected data structure.
#'
#' @return A data frame containing information about train berths, signals, and
#'   times.
#'
#' @details The function performs data wrangling on the aspect and track events
#'   data to create a data frame with comprehensive information about train
#'   berths, signals, and various time durations, including TSAR and its
#'   sub-components. The process involves several steps of data manipulation,
#'   filtering, and validation to obtain the relevant information. The resulting
#'   data frame includes columns related to signals, berths, train IDs, aspects,
#'   times of signal changes, and various durations.
#'
#' @importFrom dplyr %>%
#'
#' @export
wrangle_centrix <- function(aspect_events, track_events) {
  check_inputs(aspect_events, track_events)

  time_windows <- track_events %>%
    find_intervals() %>%
    find_time_windows(track_events)

  track_activations_windowed <- track_events %>%
    window_track_activations(time_windows)

  red_events_windowed <- aspect_events %>%
    window_aspect_events(time_windows)

  good_windows <- find_good_windows(track_activations_windowed,
                                    red_events_windowed)

  valid_track_activations <- track_activations_windowed %>%
    validate_track_activations(good_windows)

  valid_red_events <- red_events_windowed %>%
    validate_red_events(good_windows)

  berth_events <- combine_track_aspect_events(valid_track_activations,
                                              valid_red_events)

  return(berth_events)
}
