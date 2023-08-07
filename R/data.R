#' Default State Mapping
#'
#' The default 1-1 mapping from signal state to aspect. See
#' \url{https://wiki.openraildata.com/index.php/Signalling_Nomenclature#Signals}.
#' This is used as the default state mapping in Centrix wrangling.
#'
#' @format ## `state_mapping`
#' A data frame with 4 rows and 2 columns:
#' \describe{
#'   \item{state}{Signal state}
#'   \item{aspect}{Signal aspect as factor}
#' }
#'
#' @seealso [wrangle_centrix()]
"state_mapping"

#' Default Event Mapping
#'
#' @format ## `event_mapping`
#' A data frame with 5 rows and 2 columns:
#' \describe{
#'   \item{event}{Raw event code}
#'   \item{name}{Event name}
#' }
#'
#' @seealso [wrangle_timetable()]
"event_mapping"
