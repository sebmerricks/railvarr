#' Default signal state to aspect mapping
#'
#' The default 1-1 mapping from signal state to aspect. See
#' \url{https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals}.
#' This is used as the default state mapping in Centrix wrangling.
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
#' @seealso [wrangle_centrix()]
"state_mapping"

#' Example Centrix data
#'
#' An example of a data frame containing raw Centrix data. This is real data
#' containing 15 days of Centrix data. The data have been anonymised.
#'
#' A data frame containing 110,339 rows and 3 columns.
#'
#' @format
#' \describe{
#'   \item{asset}{Centrix asset ID, signal assets include state data.}
#'   \item{dt}{Date and time of event.}
#'   \item{transition}{Event transition data.}
#' }
#'
#' @examples
#' raw_centrix
"raw_centrix"

#' Example asset map
#'
#' An example of an asset map showing the relationship between different track
#' assets. There is a 1-1 mapping from signal to berth, a 1-many mapping from
#' berth to track, and a 1-1 mapping from track to event.
#'
#' A data frame containing 17 rows and 4 columns.
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID.}
#'   \item{berth}{Berth ID.}
#'   \item{track}{Track ID.}
#'   \item{event}{Track event.}
#' }
#'
#' @examples
#' asset_map
"asset_map"

#' Example aspect events
#'
#' An example of Centrix data which have been pre-processed into aspect events.
#'
#' A data frame containing 32,512 rows and 4 columns.
#'
#' @format
#' \describe{
#'   \item{signal}{Signal ID.}
#'   \item{dt}{Date and time of event.}
#'   \item{aspect}{Signal aspect caused by event.}
#'   \item{past_aspect}{Signal aspect preceding event.}
#' }
#'
#' @examples
#' aspect_events
"aspect_events"

#' Example track events
#'
#' An example of Centrix data which have been pre-processed into track events.
#'
#' A data frame containing 25,242 rows and 4 columns.
#'
#' @format
#' \describe{
#'   \item{track}{Track ID.}
#'   \item{dt}{Date and time of event.}
#'   \item{occupied}{TRUE if train enters track, else FALSE.}
#'   \item{event}{'enters' if train enters track, else 'vacates'.}
#' }
#'
#' @examples
#' track_events
"track_events"
