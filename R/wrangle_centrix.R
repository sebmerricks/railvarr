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

validate_track_events <- function(track_events_windowed,
                                       good_windows) {
  valid_track_events <- track_events_windowed %>%
    semi_join(good_windows, by = c("period", "window"))
  return(valid_track_events)
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

combine_track_aspect_events <- function(valid_track_events,
                                        valid_red_events) {
  add_signal_berth <- valid_track_events %>%
    inner_join(
      get_asset_mapping(),
      by = c("track", "event"),
      relationship = "many-to-many"
    ) %>%
    select("window", "signal", "berth", "dt", "event")

  windowed_train_ids <- add_signal_berth %>%
    arrange(.data$window, .data$signal, .data$dt) %>%
    group_by(.data$window, .data$signal) %>%
    mutate(window_train_id = cumsum(.data$event == "enters")) %>%
    ungroup()

  calculate_timings <- windowed_train_ids %>%
    tidyr::pivot_wider(
      id_cols = c("window", "signal", "berth", "window_train_id"),
      values_from = "dt",
      names_from = "event",
      names_glue = "t_{.name}"
    ) %>%
    group_by(.data$window, .data$window_train_id) %>%
    mutate(t_enters_next = lead(.data$t_enters)) %>%
    ungroup()

  add_red_events <- calculate_timings %>%
    inner_join(
      valid_red_events, by = c("window", "signal", "window_train_id")
    )

  train_ids <- add_red_events %>%
    group_by(.data$window, .data$window_train_id) %>%
    mutate(train_id = cur_group_id()) %>%
    ungroup()

  calculate_durations <- train_ids %>%
    mutate(
      T_onset  = .data$t_red_on - .data$t_enters,
      T_offset = .data$t_red_off - .data$t_vacates,
      T_travel = .data$t_enters_next - .data$t_enters,
      T_coach  = .data$t_vacates - .data$t_enters_next,
      T_clear = .data$t_vacates - .data$t_enters,
    )

  berth_events <- calculate_durations %>%
    select("signal", "berth", "train_id", "aspect", "t_enters", "t_red_on",
           "t_enters_next", "t_vacates", "t_red_off", "TSAR", "T_onset",
           "T_clear", "T_offset", "T_travel", "T_coach") %>%
    mutate(across(TSAR:last_col(), lubridate::as.duration),
           across(TSAR:last_col(), as.double))

  return(berth_events)
}
