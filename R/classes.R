source(system.file("R/class_definitions/asset_map.R", package = "railvarr"))

#' @export
asset_map <- function(map = data.frame()) {
  class <- class(map)
  return(validate_asset_map(new_asset_map(map, class)))
}

#' @export
is_asset_map <- function(obj) {
  inherits(obj, "asset_map")
}
