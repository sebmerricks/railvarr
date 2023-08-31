#' Wrangle Raw Centrix Data
#'
#' This function takes in raw Centrix data as an input and returns a fully
#' processed data frame containing berth-level signal and track observations,
#' along with calculations of TSAR and all its subcomponents. For more
#' information, see the [Get Started
#' page](https://sebmerricks.github.io/railvarr/articles/railvarr.html#centrix)
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
#' @param asset_map The asset map is a representation of the track section.
#'   Currently, the track is represented by a linear data frame. However, this
#'   does not support more complex track layouts, such as junctions. The asset
#'   map should be a data frame containing a 1-1 mapping from signal ID to berth
#'   name, a 1-many mapping from berth name to track ID, and an extra `event`
#'   column. The `event` column represents the fact that there are separate
#'   Centrix observations for trains entering and exiting a track. Therefore,
#'   each track can have both an `'enters'` and a `'vacates'` events associated
#'   with it. Note that in order to calculate berth travel times, it is
#'   necessary to know when the train entered to next berth. Therefore, it may
#'   be impossible to calculate travel times for the final berth. The asset map
#'   data frame should adhere to the following structure:
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
#' @param state_map The state mapping provides a 1-1 mapping from signal state
#'   codes to signal aspects. This is standardised, so `railvarr` provides a
#'   default mapping (see [state_mapping]). Simply leave this parameter blank to
#'   make use of this default mapping. See [this
#'   website](https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals)
#'   for other codes.
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
#' @seealso [preprocess_signal_events()] [calculate_time_windows()]
#'   [filter_aspect_events()] [calculate_tsars()] [cluster_journeys()]
#'   [raw_centrix] [asset_map] [state_mapping] [berth_events]
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
