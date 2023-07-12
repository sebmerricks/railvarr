#' Wrangle raw Centrix data
#'
#' @param aspect_events Data frame containing pre-processed signal data.
#' @param track_events Data frame containing pre-processed track data.
#'
#' @export
#'
#' @import dplyr
#'
wrangle_centrix <- function(aspect_events, track_events) {
  map <- get_map()
  start_track <- (map %>% first())$track
  end_track <- (map %>% last())$track

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

  track_activations_windows <- inner_join(
    track_events,
    time_windows %>%
      mutate(start_time = lubridate::int_start(interval),
             end_time = lubridate::int_end(interval)) %>%
      select(-period),
    join_by(between(dt, start_time, end_time))
  ) %>%
    select(period, window, track, dt, event)

  track_activation_counts <-
    track_activations_windows %>%
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

  track_count <- map %>%
    distinct(track) %>%
    nrow()

  valid_track_activations_windowed <- track_activation_counts %>%
    group_by(period, window) %>%
    summarise(
      ntrains_track = first(n_enters),
      ntracks = n_distinct(track),
      distinct_track_counts = n_distinct(n),
      any_different_enters_vacates = any((n_enters - n_vacates) != 0),
      any_not_interlaced = any(n_same_as_prior > 0)
    ) %>%
    filter(
      ntracks == track_count &
        distinct_track_counts == 1 &
        !any_not_interlaced &
        !any_different_enters_vacates
    ) %>%
    ungroup()

  aspect_events_windowed <- inner_join(
    aspect_events,
    time_windows %>%
      mutate(start_time = lubridate::int_start(interval),
             end_time = lubridate::int_end(interval) + 10), # + 10 to account for signal offset
    join_by(between(dt, start_time, end_time))
  ) %>%
    select(window, signal, dt, aspect, past_aspect)

  signals <- map %>%
    filter(event == "vacates") %>%
    distinct(signal)

  red_events_windowed <- aspect_events_windowed %>%
    semi_join(signals, by = "signal") %>%
    arrange(window, signal, dt) %>%
    filter(aspect == "R" | past_aspect == "R") %>%
    mutate(event = ifelse(aspect == "R", "red_on", "red_off"))

  red_events_counts <-
    red_events_windowed %>%
    group_by(window) %>%
    summarise(
      n_red_on = sum(aspect == "R"),
      n_red_off = sum(aspect != "R"),
      .groups = "drop"
    )

  signal_count <- map %>%
    filter(event == "vacates") %>%
    distinct(signal) %>%
    nrow()

  valid_red_events_windowed <-
    red_events_counts %>%
    filter((n_red_on == n_red_off) &  (n_red_on %% signal_count == 0)) %>%
    mutate(ntrains = as.integer(n_red_on / signal_count)) %>%
    select(window, ntrains)

  good_windows <- left_join(
    valid_track_activations_windowed,
    valid_red_events_windowed,
    by = "window"
  ) %>%
    filter(
      !is.na(ntrains) &
        ntrains_track == ntrains
    )

  bad_windows <- anti_join(
    time_windows,
    good_windows,
    by = "window"
  )

  valid_track_activations <- track_activations_windows %>%
    semi_join(good_windows, by = c("period", "window"))

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

  berth_events <-
    valid_track_activations %>%
    inner_join(
      map, by = c("track", "event")
    ) %>%
    select(window, signal, berth, dt, event) %>%
    arrange(window, signal, dt) %>%
    group_by(window, signal) %>%
    mutate(window_train_id = cumsum(event == "enters")) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c(window,signal,berth,window_train_id),
      values_from = dt,
      names_from = event,
      names_glue = "t_{.name}"
    ) %>%
    mutate(T_clear = lubridate::as.duration(t_vacates - t_enters)) %>%
    group_by(window, window_train_id) %>%
    mutate(t_enters_next = lead(t_enters)) %>%
    ungroup() %>%
    inner_join(
      valid_red_events, by = c("window", "signal", "window_train_id")
    ) %>%
    group_by(window, window_train_id) %>%
    mutate(train_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(
      T_onset  = t_red_on - t_enters,
      T_offset = t_red_off - t_vacates,
      T_travel = t_enters_next - t_enters,
      T_coach  = t_vacates - t_enters_next,
    ) %>%
    select(signal, berth, train_id, aspect, t_enters, t_red_on, t_enters_next,
           t_vacates, t_red_off, TSAR, T_onset, T_clear, T_offset, T_travel,
           T_coach) %>%
    mutate(across(TSAR:last_col(), as.double))

  return(berth_events)
}


