#' Wrangle Raw Centrix Data
#'
#' This function takes in raw Centrix data as an input and returns a fully
#' processed data frame containing berth-level signal and track observations,
#' along with calculations of TSAR and all its subcomponents.
#'
#' This function wraps:
#' \itemize{
#'   \item{[preprocess_signal_events()] [preprocess_track_events()]}
#'   \item{[calculate_time_windows()]}
#'   \item{[filter_aspect_events()] [filter_track_events()]}
#'   \item{[calculate_tsars()]}
#' }
#'
#' @param raw_centrix A data frame containing raw Centrix data. Strict input
#'   validation is applied to ensure the data adhere to the expected structure.
#'   The data should be in a data frame with the following columns:
#'   \itemize{
#'     \item{\code{asset}}: A [character()] vector containing the asset ID.
#'        Tracks are expected to follow the regex pattern `"T[A-Z]+(-[0-9])?"`.
#'        E.g., 'TABC' 'TABC-1' 'TABC-2'.
#'       Signals are expected to follow the regex pattern `"S[0-9]+\s[A-Z]+"`.
#'        E.g., 'S123 HGE' 'S123 I'.
#'       The second part of the signal ID should be a state code referring to
#'        the signal aspect, e.g. 'RGE' for red or 'DGE' for green. For other
#'        codes see:
#'    \url{https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals}.
#'     \item{\code{dt}}: A [lubridate::POSIXct()] object representing the date
#'       and time at which the observation was made.
#'     \item{\code{transition}}: A [character()] vector representing the state
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
#'     \item{\code{signal}}: A [character()] vector containing the signal ID. It
#'       should follow the regex pattern `"S[0-9]+"`.
#'     \item{\code{berth}}: A [character()] vector containing the berth ID. It
#'       should follow the regex pattern `"[A-Z]+"`.
#'     \item{\code{track}}: A [character()] vector containing the track ID. It
#'       should follow the regex pattern `"T[A-Z]+(-[0-9])?"`.
#'     \item{\code{event}}: A [character()] vector containing the event type. It
#'       must be one of either "enters" or "vacates".
#'    }
#' @param state_map A data frame containing a 1-1 mapping from signal state to
#'   signal aspect. See [state_mapping] (the default) for the expected
#'   structure.
#'
#' @returns A data frame containing the fully processed Centrix data, containing
#'   berth-level information about signal and track events. This includes TSARs
#'   and all sub-components, e.g. 'T_travel' and 'T_offset'.
#'
#' @examples
#' data(raw_centrix, asset_map)
#' raw_centrix
#' berth_events <- wrangle_centrix(raw_centrix, asset_map)
#' berth_events
#'
#' @export
wrangle_centrix <- function(raw_centrix,
                            asset_map,
                            state_map = state_mapping) {
  validate_centrix(raw_centrix)
  validate_asset_map(asset_map)
  validate_state_mapping(state_map)

  aspect_events <- preprocess_signal_events(raw_centrix, asset_map,
                                            state_map)
  track_events <- preprocess_track_events(raw_centrix, asset_map)

  time_windows <- calculate_time_windows(aspect_events, track_events, asset_map)

  # I need time_windows, track_events, aspect_events, asset_map

  valid_track_events <- filter_track_events(track_events, time_windows,
                                            asset_map)
  valid_aspect_events <- filter_aspect_events(aspect_events, time_windows,
                                              asset_map)

  berth_events <- calculate_tsars(valid_track_events, valid_aspect_events,
                                  asset_map)

  return(berth_events)
}
