environment <- new.env(parent = emptyenv())

validate <- function(data, rules) {
  out <- validate::confront(data, rules)
  fails <- validate::as.data.frame(out) %>%
    dplyr::filter(!.data$value) %>%
    dplyr::select("expression")
  if (nrow(fails) > 0) stop(fails)
}

# Asset Mapping ----------------------------------------------------------------

environment$asset_mapping <- NULL

#' Get the Asset Mapping
#'
#' This function retrieves the asset mapping from the current environment. The
#' asset mapping is stored in the environment variable 'asset_mapping'.
#'
#' @return A data frame containing the asset mapping.
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
  stopifnot(c("signal", "berth", "track", "event", "geo") %in%
              names(new_asset_mapping))

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

# Centrix ----------------------------------------------------------------------

environment$centrix <- NULL

#' Get Centrix Data from Environment
#'
#' This function retrieves the centrix data from the current environment. The
#' centrix data is stored in the environment variable 'centrix'.
#'
#' @return A data frame containing the centrix data.
#'
#' @export
#'
#' @examples
#' centrix <- get_centrix()
#' print(centrix)
#'
#' @seealso Use [set_centrix] to set centrix data in the environment.
get_centrix <- function() {
  environment$centrix
}

#' Set Centrix in Environment with Validation
#'
#' This function allows you to set the Centrix data in the current environment
#' after validating the format of the provided data. The data are stored in the
#' environment variable 'centrix'.
#'
#' @param new_centrix A data frame representing the Centrix data containing
#'   columns 'asset', 'dt', 'transition', and 'period'. The input Centrix must
#'   adhere to the expected format and validation rules.
#'
#' @details This function performs strict validation on the input `new_centrix`
#'   using predefined rules. The function checks if the provided data adhere
#'   to the expected format and throws an error if any of the validation rules
#'   are violated.
#'
#'   The input `new_centrix` must be a data frame with the following elements:
#' \itemize{
#'   \item \code{asset}: A character vector representing the asset ID in the
#'     format "(S|T).+" (e.g., "S123" or "TABC").
#'   \item \code{dt}: A POSIXct object representing the date and time of the
#'     observation.
#'   \item \code{transition}: A character vector representing the transition
#'     type, which must be either "DN to UP" or "UP to DN".
#'   \item \code{period}: A numeric value representing the time period of the
#'     Centrix.
#' }
#'
#' @importFrom validate validator field_format
#' @importFrom lubridate is.POSIXct
#'
#' @export
#'
#' @examples
set_centrix <- function(new_centrix) {
  stopifnot(c("asset", "dt", "transition", "period") %in% names(new_centrix))

  rules <- validate::validator(
    is.character(asset),
    field_format(asset, "(S|T).+", type = "regex"),
    is.character(transition),
    field_format(transition, "(DN to UP)|(UP to DN)", type = "regex"),
    is.numeric(period)
  )

  validate(new_centrix, rules)
  stopifnot(is.finite.POSIXlt(new_centrix$dt))

  environment$centrix <- new_centrix %>%
    mutate(dt = lubridate::as_datetime(.data$dt))
}

# State Mapping ----------------------------------------------------------------

environment$state_mapping <- NULL

#' Get State Mapping from Environment
#'
#' This function retrieves the signal state mapping from the current
#' environment. The mapping is stored in the environment variable
#' 'state_mapping'.
#'
#' @return A data frame containing the state mapping.
#'
#' @export
#'
#' @examples
#' state_mapping <- get_state_mapping()
#' print(state_mapping)
#'
#' @seealso Use [set_state_mapping] to set state mapping in the environment.
get_state_mapping <- function() {
  environment$state_mapping
}

#' Set Signal State Mapping in Environment
#'
#' This function allows you to set the state mapping in the current environment
#' after validating the format of the provided mapping. The mapping is stored in
#' the environment variable 'state_mapping'.
#'
#' @param new_state_mapping A data frame representing the state mapping
#'   containing columns 'signal' and 'aspect'. The input mapping must adhere to
#'   the expected format and validation rules.
#'
#' @details This function performs strict validation on the input
#'   `new_state_mapping` using predefined rules. The function checks if the
#'   provided data adhere to the expected format and throws an error if any of
#'   the validation rules are violated.
#'
#'   The input `new_state_mapping` must be a data frame with the following
#'   elements:
#' \itemize{
#'   \item \code{state}: A character vector representing the signal state as
#'     recorded in the raw Centrix data. e.g., 'RGE' or 'DGE'.
#'   \item \code{aspect}: A character vector representing the signal aspect,
#'     e.g., 'R' or 'G'.
#' }
#'   This mapping should be a 1-1 mapping from state to aspect.
#'
#' @importFrom validate validator field_format
#' @importFrom lubridate is.POSIXct
#'
#' @export
#'
#' @examples
set_state_mapping <- function(new_state_mapping) {
  stopifnot(c("state", "aspect") %in% names(new_state_mapping))

  rules <- validate::validator(
    is.character(state),
    is.factor(aspect)
  )

  validate(new_state_mapping, rules)

  environment$state_mapping <- new_state_mapping
}
