env <- new.env(parent = emptyenv())
env$map <- data.frame(
  signal = character(),
  berth = character(),
  track = character(),
  event = character()
)

#' Report network map
#'
#' @export
#'
get_map <- function() {
  return(env$map)
}

#' Set network map
#'
#' @param map New network map to set.
#'
#' @export
#'
set_map <- function(map) {
  template <- data.frame(
    signal = character(),
    berth = character(),
    track = character(),
    event = character()
  )
  stopifnot(vetr::alike(template, map))

  old <- env$map
  env$map <- map
  invisible(old)
}

#' Split raw events into separate signal and track events
#'
#' @param raw_events Data frame containing raw data.
#' @param is_track Expression determining how to split tracks and signals.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate group_by group_split
#'
split_signal_track_events <- function(raw_events,
                                      is_track =
                                        quote(stringr::str_starts(asset, "T"))
) {
  tpl_raw_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  stopifnot(vetr::alike(tpl_raw_events, raw_events))

  events <- raw_events %>%
    mutate(is_track = eval(is_track)) %>%
    group_by(is_track) %>%
    group_split(.keep = F)

  return(events)
}

#' Preprocess raw signal events
#'
#' @param raw_signal_events Raw signal data.
#' @param state_mapping Data frame defining how to convert signal states to
#'        aspects.
#'
#' @export
#'
#' @import dplyr
#'
preprocess_signal_events <- function(raw_signal_events, state_mapping) {
  # Define the expected data structure
  tpl_signal_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  # Check whether the raw data matches the expected structure
  stopifnot(vetr::alike(tpl_signal_events, raw_signal_events))

  # Define the expected structure for state_mapping
  tpl_state_mapping <- data.frame(
    state = character(),
    aspect = factor()
  )
  # Check the structure
  stopifnot(vetr::alike(tpl_state_mapping, state_mapping))

  # Identify signal IDs and states
  signal_events <- raw_signal_events %>%
    mutate(
      signal = stringr::str_split_i(asset, " ", i = 1),
      state = stringr::str_split_i(asset, " ", i = 2)
    ) %>%
    filter(transition == "DN to UP") %>%
    select(signal, dt, state, period)

  signals <- get_map() %>%
    select(signal)

  # Convert states to aspects
  aspect_events <- signal_events %>%
    semi_join(signals, by = "signal") %>%
    inner_join(
      state_mapping,
      by = "state"
    ) %>%
    arrange(signal, dt) %>%
    select(period, signal, dt, aspect) %>%
    group_by(signal) %>%
    mutate(past_aspect = lag(aspect)) %>%
    ungroup()

  return(aspect_events)
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
  tpl_track_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  stopifnot(vetr::alike(tpl_track_events, raw_track_events))

  tracks <- get_map() %>%
    select(track)

  track_activations <- raw_track_events %>%
    mutate(occupied = if_else(transition == "UP to DN", T, F)) %>%
    select(-transition) %>%
    rename(track = asset) %>%
    mutate(track = stringr::str_extract(track, "^([A-z])+(-[0-9])?")) %>%
    select(period, track, dt, occupied) %>%
    arrange(track, dt) %>%
    mutate(event = if_else(occupied, "enters", "vacates")) %>%
    mutate(date = lubridate::as_date(dt)) %>%
    semi_join(tracks, by = "track")

  return(track_activations)
}
