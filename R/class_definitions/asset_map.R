# Asset Mapping ----------------------------------------------------------------
new_asset_map <- function(map = data.frame(), class = character()) {
  stopifnot(is.data.frame(map))
  structure(map,
            class = c("asset_map", class))
}

validate_asset_map <- function(asset_map) {
  stopifnot(is_asset_map(asset_map))
  stopifnot(c("signal", "berth", "track", "event") %in% names(asset_map))

  stopifnot(is.character(asset_map$signal))
  stopifnot(stringr::str_like(asset_map$signal, "S[0-9]+"))

  stopifnot(is.character(asset_map$berth))
  stopifnot(stringr::str_like(asset_map$berth, "[A-Z]+"))

  stopifnot(is.character(asset_map$track))
  stopifnot(stringr::str_like(asset_map$track, "T[A-Z]+(-[0-9])?"))

  stopifnot(is.character(asset_map$event))
  stopifnot(all(asset_map$event %in% c("enters", "vacates")))

  return(asset_map)
}
