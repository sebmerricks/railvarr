new_asset <- function(asset = character()) {
  if (!is.character(asset)) rlang::abort("`asset` must be a character vector")
  if (any(!stringr::str_detect(
    asset,
    stringr::regex("^T[A-Z]+(-[0-9])? TR$|^S[0-9]+\\s[A-Z]+$")
  ))) {
    rlang::abort("`asset` must match the regular expression: '^T[A-Z]+(-[0-9])? TR$|^S[0-9]+\\s[A-Z]+$'")
  }

  vctrs::new_vctr(asset, class = "railvarr_asset")
}

asset <- function(asset = character()) {
  asset <- vctrs::vec_cast(asset, character())
  new_asset(asset)
}

is_asset <- function(asset) {
  inherits(asset, "railvarr_asset")
}

vec_ptype_abbr.railvarr_asset <- function(asset, ...) {
  "asset"
}


new_transition <- function(transition = character()) {
  if (!is.character(transition))
    rlang::abort("`transition` must be a character vector")
  if (any(!stringr::str_detect(
    transition,
    stringr::regex("^DN to UP$|^UP to DN$")
  )))
    rlang::abort("`transition` must be either 'DN to UP' or 'UP to DN'")

  vctrs::new_vctr(transition, class = "railvarr_transition")
}

transition <- function(transition = character()) {
  transition <- vctrs::vec_cast(transition, character())
  new_transition(transition)
}

is_transition <- function(transition) {
  inherits(transition, "railvarr_transition")
}

vec_ptype_abbr.railvarr_transition <- function(transition, ...) {
  "trnstn"
}


new_raw_centrix <- function(asset = asset(),
                            dt = lubridate::POSIXct(),
                            transition = transition()) {
  if(!is_asset(asset)) rlang::abort("`asset` must be an asset vector")
  if(!lubridate::is.POSIXct(dt)) rlang::abort("`dt` must be a POSIXct vector")
  if(!is_transition(transition)) rlang::abort("`transition` must be a transition vector")

  dplyr::tibble(asset = asset,
                dt = dt,
                transition = transition)
}

raw_centrix <- function(asset = character(),
                        dt = 0,
                        transition = character()) {
  asset <- asset(asset)
  dt <- lubridate::as_datetime(dt)
  transition <- transition(transition)

  new_raw_centrix(asset, dt, transition)
}

is_raw_centrix <- function(x, ...) {
  if(!all(names(x) %in% c("asset", "dt", "transition"))) return(FALSE)
  is_asset(x$asset) &
    lubridate::is.POSIXct(x$dt) &
    is_transition(x$transition)
}

raw_centrix(c("S1 DGE", "S1 RGE"),
            c(1, 1),
            c("UP to DN", "DN to UP"))
