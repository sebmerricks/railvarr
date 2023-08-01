environment <- new.env(parent = emptyenv())

environment$asset_mapping <- dplyr::tibble(signal = character(),
                                           berth = character(),
                                           track = character(),
                                           event = character(),
                                           geo = character())

validate <- function(data, rules) {
  out <- validate::confront(data, rules)
  fails <- validate::as.data.frame(out) %>%
    dplyr::filter(!.data$value) %>%
    dplyr::select("expression")
  if (nrow(fails) > 0) stop(fails)
}

#' Get the Asset Mapping
#'
#' This function retrieves the asset mapping from the current environment. The
#' asset mapping is stored in the environment variable 'asset_mapping'.
#'
#' @return A list containing the asset mapping.
#'
#' @export
#'
#' @examples
#' mapping <- get_asset_mapping()
#' print(mapping)
#'
#' @seealso Use [set_asset_mapping] to set the asset mapping in the environment.
get_asset_mapping <- function() {
  environment$asset_mapping
}

#' Set Asset Mapping with Validation
#'
#' This function allows you to set the asset mapping in the current environment
#' after validating the format of the provided mapping. The asset mapping is
#' stored in the environment variable 'asset_mapping'.
#'
#' @param new_asset_mapping A list representing the asset mapping containing
#'   information about assets, signals, berths, tracks, events, and geographic
#'   information. The input mapping must adhere to the expected format and
#'   validation rules.
#'
#' @details This function performs strict validation on the input
#'   `new_asset_mapping` using predefined rules. The function checks if the
#'   provided mapping adheres to the expected format and throws an error if any
#'   of the validation rules are violated. The validation rules ensure that each
#'   component of the mapping is in the correct format and data type.
#'
#'   The input `new_asset_mapping` must be a data frame with the following
#'   elements:
#' \itemize{
#'   \item \code{signal}: A character vector representing the signal ID in the
#'     format "S[0-9]+" (e.g., "S123").
#'   \item \code{berth}: A character vector representing the berth information.
#'   \item \code{track}: A character vector representing the track ID in the
#'     format "T[A-Z]+(-[0-9]{1})?" (e.g., "TA-1").
#'   \item \code{event}: A character vector representing the event type, which
#'     must be either "enters" or "vacates".
#'   \item \code{geo}: A character vector representing the geographic
#'     information.
#' }
#'
#' @seealso The validation rules and expected format for each element of the
#'   asset mapping can be found in the function implementation.
#' @seealso [get_asset_mapping()]
#'
#' @export
#'
#' @examples
#'
#'
#' @importFrom validate validator field_format
set_asset_mapping <- function(new_asset_mapping) {
  rules <- validate::validator(
    is.character(signal),
    field_format(signal, "S[0-9]+", type = "regex"),
    is.character(berth),
    is.character(track),
    field_format(track, "T[A-Z]+(-[0-9]{1})?", type = "regex"),
    is.character(event),
    field_format(event, "(enters)|(vacates)", type = "regex"),
    is.character(geo)
  )

  validate(new_asset_mapping, rules)

  environment$asset_mapping <- new_asset_mapping
}


set_asset_mapping(dplyr::tibble(
  signal = c("S1"),
  berth = c("A", "B"),
  track = c("TA", "TB"),
  event = c("vacates"),
  geo = c("")
))
