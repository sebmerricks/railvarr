# Asset Mapping ----------------------------------------------------------------
new_asset_map <- function(map = data.frame(), class = class(data.frame)) {
  stopifnot(is.data.frame(map))
  structure(map,
            class = c("asset_map", class))
}

validate_asset_map <- function(asset_map) {
  stopifnot(is_asset_map(asset_map))
  stopifnot(c("signal", "berth", "track", "event", "geo") %in% names(asset_map))

  stopifnot(is_signal(asset_map$signal))

  stopifnot(is.character(asset_map$berth))
  stopifnot(stringr::str_like(asset_map$berth, "[A-Z]+"))

  stopifnot(is_track(asset_map$track))

  stopifnot(is.character(asset_map$event))
  stopifnot(all(asset_map$event %in% c("enters", "vacates")))

  stopifnot(is.character(asset_map$geo))
  stopifnot(stringr::str_like(asset_map$geo, "([A-z]+( |-)?)+"))

  return(asset_map)
}

#' @export
asset_map <- function(map = data.frame("signal" = signal(),
                                       "berth" = character(),
                                       "track" = track(),
                                       "event" = character(),
                                       "geo" = character())) {
  class <- class(map)
  return(validate_asset_map(new_asset_map(map, class)))
}

#' @export
is_asset_map <- function(obj) {
  inherits(obj, "asset_map")
}

as_asset_map <- function(x) {
  stopifnot(c("signal", "berth", "track", "event", "geo") %in% names(x))
  x <- x %>%
    mutate(signal = as_signal(signal),
           berth = as.character(berth),
           track = as_track(track))
  return(asset_map(x))
}
