env <- new.env(parent = emptyenv())
env$map <- data.frame(
  signal = character(),
  berth = character(),
  track = character(),
  event = character()
)

#' Retrieve network map
#'
#' `get_map()` retrieves the network map from the internal environment. By
#' default, this will equal `NULL`. Use [set_map()] to store a map.
#'
#' @export
get_map <- function() {
  return(env$map)
}

#' Set network map
#'
#' `set_map()` stores a network map in the internal environment for use by other
#' data processing functions. It is important to set the map before attempting
#' to process the data. Use [get_map()] to access the map once it has been
#' stored.
#'
#' @param map Network map to store. The following columns must be present:
#'  `signal, berth, track, event`\cr
#'  All columns must be of type [character()]. An error will be thrown if the
#'  map does not conform to this specification.\cr
#'  The map is assumed to be ordered, so the first row defines the start of the
#'  track section while the last row defines the end of the section.
#'
#' @examples
#' map <- dplyr::tribble(
#'   ~signal, ~berth, ~track, ~event,
#'   "S1", "B1", "T1", "enters",
#'   "S1", "B1", "T2", "vacates",
#'   "S2", "B2", "T3", "enters",
#'   "S2", "B2", "T4", "vacates"
#' )
#'
#' set_map(map)
#'
#' @export
set_map <- function(map) {
  names <- c("signal", "berth", "track", "event")
  types <- list(character(), character(), character(), character())

  check_df(map, names, types)

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
split_signal_track_events <- function(raw_events,
                                      is_track =
                                        quote(stringr::str_starts(asset, "T"))
) {
  names <- c("asset", "dt", "transition", "period")
  types <- list(character(), lubridate::POSIXct(), character(), numeric())
  check_df(raw_events, names, types)

  events <- raw_events %>%
    select(all_of(names)) %>%
    mutate(is_track = eval(is_track)) %>%
    group_by(.data$is_track) %>%
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
  names <- c("asset", "dt", "transition", "period")
  types <- list(character(), lubridate::POSIXct(), character(), numeric())
  check_df(raw_signal_events, names, types)

  signal_events <- raw_signal_events %>%
    mutate(
      signal = stringr::str_split_i(.data$asset, " ", i = 1),
      state = stringr::str_split_i(.data$asset, " ", i = 2)
    ) %>%
    filter(.data$transition == "DN to UP") %>%
    select("signal", "dt", "state", "period")

  signals <- get_map() %>%
    select("signal")

  aspect_events <- signal_events %>%
    semi_join(signals, by = "signal") %>%
    inner_join(
      env$state_mapping,
      by = "state"
    ) %>%
    arrange(.data$signal, .data$dt) %>%
    select("period", "signal", "dt", "aspect") %>%
    group_by(.data$signal) %>%
    mutate(past_aspect = lag(.data$aspect)) %>%
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
    semi_join(tracks, by = "track")

  return(track_activations)
}
