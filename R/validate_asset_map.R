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
