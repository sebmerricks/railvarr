# The point of this class is to manage any and all internal environments such as
# the one containing the Asset Mapping.

internal <- new.env(parent = emptyenv())
internal$asset_mapping <- asset_map()

#' Retrieve network map
#'
#' `get_map()` retrieves the network map from the internal environment. By
#' default, this will equal `NULL`. Use [set_map()] to store a map.
#'
#' @export
get_map <- function() {
  return(internal$asset_mapping)
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
#' @export
set_map <- function(map) {
  stopifnot(is_asset_map(map))

  old <- internal$asset_mapping
  internal$asset_mapping <- map
  invisible(old)
}

internal$state_mapping <- data.frame(
  state = c("RGE", "HGE", "HHGE", "DGE"),
  aspect = factor(c("R", "Y", "YY", "G"),
                  levels = c("R", "Y", "YY", "G"))
)
