# Asset Mapping ----------------------------------------------------------------
source(get_class("asset_map"))

#' @export
asset_map <- function(map = data.frame()) {
  class <- class(map)
  return(validate_asset_map(new_asset_map(map, class)))
}

#' @export
is_asset_map <- function(obj) {
  inherits(obj, "asset_map")
}

# Centrix ----------------------------------------------------------------------
source(get_class("centrix"))

# Centrix ------------------------------------------

#' @export
centrix <- function(data = data.frame()) {
  class <- class(data)
  return(validate_centrix(new_centrix(data, class)))
}

#' @export
is_centrix <- function(obj) {
  inherits(obj, "centrix")
}

# Aspect Event -----------------------------------------------

#' @export
aspect_event <- function(data = data.frame()) {
  class <- class(data)
  return(validate_aspect_event(new_aspect_event(data, class)))
}

#' @export
is_aspect_event <- function(obj) {
  inherits(obj, "aspect_event")
}

# Track Event ----------------------------------------------

#' @export
track_event <- function(data = data.frame()) {
  class <- class(data)
  return(validate_track_event(new_track_event(data, class)))
}

#' @export
is_track_event <- function(obj) {
  inherits(obj, "track_event")
}
