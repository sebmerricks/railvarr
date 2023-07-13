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
#' @param map New network map to set. Should be in order: the first entry is the
#'            start of the track, while the last entry is the end.
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

  vetr::vet(template, map, stop = TRUE)

  old <- env$map
  env$map <- map
  invisible(old)
}

env$state_mapping <- data.frame(
  state = c("RGE", "HGE", "HHGE", "DGE"),
  aspect = factor(c("R", "Y", "YY", "G"),
                  levels = c("R", "Y", "YY", "G"))
)

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
  vetr::vet(tpl_raw_events, raw_events, stop = TRUE)

  events <- raw_events %>%
    mutate(is_track = eval(is_track)) %>%
    group_by(is_track) %>%
    group_split(.keep = F)

  return(events)
}

#' Preprocess raw signal events
#'
#' @param raw_signal_events Raw signal data.
#'
#' @export
#'
#' @import dplyr
#'
preprocess_signal_events <- function(raw_signal_events) {
  # Define the expected data structure
  tpl_signal_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  # Check whether the raw data matches the expected structure
  vetr::vet(tpl_signal_events, raw_signal_events, stop = TRUE)

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
      env$state_mapping,
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

  vetr::vet(tpl_track_events, raw_track_events, stop = TRUE)

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
