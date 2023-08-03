#' Wrangle Raw Centrix Data
#'
#' This function takes in raw Centrix data as an input and returns a fully
#' processed data frame containing berth-level signal and track observations,
#' along with calculations of TSAR and all its subcomponents.
#'
#' @param centrix A data frame containing raw Centrix data. Strict input
#'   validation is #' applied to ensure the data adhere to the expected
#'   structure. The data should be in a data frame with the following columns:
#'   \itemize{
#'     \item{\code{asset}}: A character vector containing the asset ID. Tracks
#'        are expected to follow the regex pattern `"T[A-Z]+(-[0-9])?"`. E.g.,
#'        'TABC' 'TABC-1' 'TABC-2'.
#'       Signals are expected to follow the regex pattern `"S[0-9]+\s[A-Z]+"`.
#'        E.g., 'S123 HGE' 'S123 I'.
#'       The second part of the signal ID should be a state code referring to
#'        the signal aspect, e.g. 'RGE' for red or 'DGE' for green. For other
#'        codes see:
#'    [https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals].
#'     \item{\code{dt}}: A [lubridate::POSIXct] object representing the date
#'       and time at which the observation was made.
#'     \item{\code{transition}}: A character vector representing the state
#'       transition that caused the observation. A transition can be one of
#'       "UP to DN" or "DN to UP", anything else will be ignored. For tracks,
#'       "UP to DN" signifies a train entering the track, while "DN to UP" is a
#'       train vacating the track. For signals, "UP to DN" represents an aspect
#'       change. Therefore, any signals with a "DN to UP" transition will be
#'       ignored.
#'   }
#' @param asset_map A data frame containing a 1-1 mapping from signals to berths
#'   and a 1-many mapping from berths to tracks. The map also contains a 1-many
#'   mapping from tracks to events, reflecting the fact that there is an 'enter'
#'   and a 'vacate' event for each track. Therefore, the data frame should
#'   adhere to the following structure:
#'   \itemize{
#'     \item{\code{signal}}: A character vector containing the signal ID. It
#'       should follow the regex pattern `"S[0-9]+"`.
#'     \item{\code{berth}}: A character vector containing the berth ID. It
#'       should follow the regex pattern `"[A-Z]+"`.
#'     \item{\code{track}}: A character vector containing the track ID. It
#'       should follow the regex pattern `"T[A-Z]+(-[0-9])?"`.
#'     \item{\code{event}}: A character vector containing the event type. It
#'       must be one of either "enters" or "vacates".
#'    }
#' @param state_mapping A data frame containing a 1-1 mapping from signal state
#'   to signal aspect. See [state_mapping] (the default) for the expected
#'   structure.
#'
#' @returns A data frame containing the fully processed Centrix data, containing
#'   berth-level information about signal and track events. This includes TSARs
#'   and all sub-components, e.g. 'T_travel' and 'T_offset'.
#'
#' @seealso All lower-level functions that this function wraps.
#'
#' @examples
#' # This will require some example data
#'
#' @export
wrangle_centrix <- function(raw_centrix, asset_map, state_mapping = NULL) {
  validate_centrix(raw_centrix)
  validate_asset_map(asset_map)
  validate_state_mapping(state_mapping)

  separated_events <- split_signal_track_events(raw_centrix)
  raw_aspect_events <- separated_events[[1]]
  raw_track_events <- separated_events[[2]]

  aspect_events <- preprocess_signal_events(raw_aspect_events)
  track_events <- preprocess_track_events(raw_track_events)

  time_windows <- calculate_time_windows(aspect_events, track_events)

  good_windows <- find_good_windows(time_windows)

  valid_track_events <- validate_track_events(track_events, good_windows)
  valid_red_events <- validate_aspect_events(aspect_events, good_windows)

  berth_events <- calculate_tsars(valid_track_events, valid_red_events)

  return(berth_events)
}

validate_centrix <- function(centrix) {
  stop_if_not(all(c("asset", "dt", "transition") %in% names(centrix)),
              msg = "Centrix data requires the columns 'asset', 'dt', and 'transition'.")
  assets <- centrix$asset
  stop_if_not(is.character(assets),
              msg = "Column 'asset' must be of type character().")
  stop_if_not(all(!is.na(
    stringr::str_extract(assets, "(S[0-9]+\\s[A-Z]+)|(T[A-Z]+(-[0-9])?)")
    )), msg = "Column 'asset' must match the regular expression
    '(S[0-9]+\\s[A-Z]+)|(T[A-Z]+(-[0-9])?)'
    e.g., 'S123 RGE', 'S123 HHGE', 'TABC-1', 'TXYZ'")
  dts <- centrix$dt
  stop_if_not(lubridate::is.POSIXct(dts),
              msg = "Column 'dt' must be of type lubridate::POSIXct().")
  transitions <- centrix$transition
  stop_if_not(is.character(transitions),
              msg = "Column 'transition' must be of type character().")
  stop_if_not(all(transitions %in% c("DN to UP", "UP to DN")),
              msg = "Every element in column 'transition' must be equal to either 'DN to UP' or 'UP to DN'.")
}

validate_asset_map <- function(asset_map) {
  stop_if_not(all(c("signal", "berth", "track", "event") %in% names(asset_map)),
              msg = "Asset map requires the columns 'signal', 'berth', 'track', and 'event'.")
  signals <- asset_map$signal
  stop_if_not(is.character(signals),
              msg = "Column 'signal' must be of type character().")
  stop_if_not(all(!is.na(stringr::str_extract(signals, "S[0-9]+"))),
              msg = "Column 'signal' must match the regular expression 'S[0-9]+'
              e.g., 'S1', 'S123'.")
  berths <- asset_map$berth
  stop_if_not(is.character(berths),
              msg = "Column 'berth' must be of type character().")
  stop_if_not(all(!is.na(stringr::str_extract(berths, "[A-Z]+"))),
              msg = "Column 'berth' must match the regular expression '[A-Z]+'
              e.g., 'A', 'ABC'.")
  tracks <- asset_map$track
  stop_if_not(is.character(tracks),
              msg = "Column 'track' must be of type character().")
  stop_if_not(all(!is.na(stringr::str_extract(tracks, "T[A-Z]+(-[0-9])?"))),
              msg = "Column 'track' must match the regular expression 'T[A-Z]+(-[0-9])?'
              e.g., 'TA', 'TABC', 'TXYZ-1'.")
  events <- asset_map$event
  stop_if_not(is.character(events),
              msg = "Column 'event' must be of type character().")
  stop_if_not(all(events %in% c("enters", "vacates")),
              msg = "Every element in column 'event' must be equal to either 'enters' or 'vacates'.")
}

validate_state_mapping <- function(state_mapping) {
  stop_if_not(all(c("state", "aspect") %in% names(state_mapping)),
              msg = "State mapping requires the columns 'state' and 'aspect'.")
  states <- state_mapping$state
  stop_if_not(is.character(states),
              msg = "Column 'state' must be of type character().")
  aspects <- state_mapping$aspect
  stop_if_not(is.factor(aspects),
              msg = "Column 'aspect' must be of type factor().")
}
