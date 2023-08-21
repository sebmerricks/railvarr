#' Calculate TSAR and Sub-Components.
#'
#' Calculate berth-level information including TSAR and all sub-components,
#' e.g., T_onset, T_clear, T_offset, etc.
#'
#' Combines valid track events from [filter_track_events()] with valid red
#' signal events from [filter_aspect_events()] to calculate TSAR and its
#' sub-components. The data frame returned by this function contains the first
#' data that can be used for analysis.
#'
#' @param track_events A data frame containing windowed track events. No input
#'   validation is performed. It is assumed that the data frame will follow the
#'   structure returned by the function [filter_track_events()]
#' @param red_events A data frame containing windowed red events. No input
#'   validation is performed. It is assumed that the data frame will follow the
#'   structure returns by the function [filter_aspect_events()]
#' @inheritParams wrangle_centrix
#'
#' @return A data frame containing berth level observations including TSAR and
#'   its sub-components. The following columns are included:
#'   \itemize{
#'     \item{\code{signal}} Signal ID.
#'     \item{\code{berth}} Berth ID.
#'     \item{\code{train_id}} Train ID.
#'     \item{\code{aspect}} Signal aspect on berth entry.
#'     \item{\code{t_enters}} Date and time of berth entry.
#'     \item{\code{t_red_on}} Date and time of signal changing to red.
#'     \item{\code{t_enters_next}} Date and time of entry into next berth.
#'     \item{\code{t_vacates}} Date and time of vacating the berth.
#'     \item{\code{t_red_off}} Date and time of signal changing from red.
#'     \item{\code{TSAR}} Amount of time the signal spends on red.
#'     \item{\code{T_onset}} Signal onset time between train entry and signal
#'       changing to red.
#'     \item{\code{T_clear}} Amount of time the train takes to clear the berth.
#'     \item{\code{T_offset}} Signal offset time between train exit and signal
#'       changing from red.
#'     \item{\code{T_travel}} Amount of time the train takes to travel the
#'       length of the berth.
#'     \item{\code{T_coach}} Amount of time the train takes to travel its own
#'       length.
#'   }
#'
#' @examples
#' data(valid_track_events, valid_aspect_events, asset_map)
#' valid_aspect_events
#' valid_track_events
#'
#' berth_events <- calculate_tsars(valid_track_events,
#'                                 valid_aspect_events,
#'                                 asset_map)
#' berth_events
#'
#' @importFrom dplyr inner_join select arrange group_by mutate ungroup lead
#'   cur_group_id
#'
#' @export
calculate_tsars <- function(track_events, red_events, asset_map) {
  # track_events have columns track, dt, event, window, interval
  add_signal_berth <- track_events %>%
    inner_join(
      asset_map,
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

  calculated_tsars <- red_events %>%
    filter(.data$aspect == "R" | .data$past_aspect == "R") %>%
    mutate(event = if_else(.data$aspect == "R", "red_on", "red_off")) %>%
    group_by(.data$window, .data$signal) %>%
    mutate(window_train_id = cumsum(.data$event == "red_on")) %>%
    ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c("window", "signal", "window_train_id"),
      values_from = c("dt", "past_aspect"),
      names_from = "event",
      names_glue = "dt_{.name}"
    ) %>%
    select(-"dt_past_aspect_red_off") %>%
    rename(
      aspect = dt_past_aspect_red_on,
      t_red_on = dt_dt_red_on,
      t_red_off = dt_dt_red_off
    )

  add_red_events <- calculate_timings %>%
    inner_join(calculated_tsars, by = c("window", "signal", "window_train_id"))

  train_ids <- add_red_events %>%
    group_by(.data$window, .data$window_train_id) %>%
    mutate(train_id = cur_group_id()) %>%
    ungroup()

  calculate_durations <- train_ids %>%
    mutate(
      TSAR = t_red_off - t_red_on,
      T_onset = .data$t_red_on - .data$t_enters,
      T_offset = .data$t_red_off - .data$t_vacates,
      T_travel = .data$t_enters_next - .data$t_enters,
      T_coach  = .data$t_vacates - .data$t_enters_next,
      T_clear = .data$t_vacates - .data$t_enters
    )

  berth_events <- calculate_durations %>%
    select("signal", "berth", "train_id", "aspect", "t_enters", "t_red_on",
           "t_enters_next", "t_vacates", "t_red_off", "TSAR", "T_onset",
           "T_clear", "T_offset", "T_travel", "T_coach") %>%
    arrange(.data$train_id, .data$t_enters) %>%
    mutate(across(TSAR:last_col(), lubridate::as.duration),
           across(TSAR:last_col(), as.double))

  return(berth_events)
}
