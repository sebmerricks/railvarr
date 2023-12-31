#' Default signal state to aspect mapping
#'
#' The default 1-1 mapping from signal state to aspect. See
#' \url{https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals}.
#' This is used as the default state mapping in Centrix wrangling.
#'
#' @seealso [wrangle_centrix()]
#'
#' A data frame with 4 rows and 2 columns.
#'
#' @format
#' \describe{
#'   \item{state}{Signal state}
#'   \item{aspect}{Signal aspect as factor}
#' }
#'
#' @examples
#' state_mapping
#'
"state_mapping"

#' Example asset map
#'
#' An example of an asset map showing the relationship between different track
#' assets. There is a 1-1 mapping from signal to berth, a 1-many mapping from
#' berth to track, and a 1-1 mapping from track to event.
#'
#' @seealso [wrangle_centrix()] [calculate_time_windows()]
#'
#' A data frame containing 17 rows and 4 columns.
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID}
#'   \item{berth}{Berth ID}
#'   \item{track}{Track ID}
#'   \item{event}{Track event}
#' }
#'
#' @examples
#' asset_map
#'
"asset_map"
