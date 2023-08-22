new_berth <- function(id = character(),
                      signal = character(),
                      state = aspect(),
                      station = character(),
                      length = integer(),
                      L1 = integer(),
                      L2 = integer()) {
  if (!is.integer(length))
    rlang::abort("`length` must be an integer vector representing the berth length in metres")
  if (!is.integer(L1))
    rlang::abort("`L1` must be an integer vector representing the distance from the start of the berth to the station in metres")
  if (!is.integer(L2))
    rlang::abort("`L2` must be an integer vector representing the distance from the station to the end of the berth in metres")

  vctrs::new_rcrd(list(berth = id,
                       signal = signal,
                       aspect = state,
                       station = station,
                       length = length,
                       L1 = L1,
                       L2 = L2),
                  class = "railvarr_berth")
}

#' Generate a new berth object
#'
#' @param id Berth name
#' @param signal Signal name
#' @param state Signal aspect
#' @param station Station name
#' @param length Berth length in metres
#' @param L1 Distance from start of berth to station in metres
#' @param L2 Distance from station to end of berth in metres
#'
#' @importFrom zeallot %<-%
#' @export
berth <- function(id = character(),
                  signal = character(),
                  state = aspect(),
                  station = character(),
                  length = integer(),
                  L1 = integer(),
                  L2 = integer()) {
  length <- as.integer(length)
  L1 <- as.integer(L1)
  L2 <- as.integer(L2)
  c(id, signal, state, station, length, L1, L2) %<-%
    vctrs::vec_recycle_common(id, signal, state, station, length, L1, L2)
  new_state <- aspect(state)
  validate_berth(new_berth(id, signal, new_state, station, length, L1, L2))
}

#' @importFrom rlang abort
validate_berth <- function(x) {
  if (!all(stringr::str_detect(vctrs::field(x, "signal"), "^S[0-9]+$"))) {
    rlang::abort("signal ID must match '^S[0-9]+$'")
  }

  x
}

#' Check whether an object is a berth
#' @param x Object to check
#' @export
is.berth <- function(x) inherits(x, "railvarr_berth")

#' @export
format.railvarr_berth <- function(x, ..., formatter = berth.default) {
  formatter(x, ...)
}

#' @export
vec_ptype_abbr.railvarr_berth <- function(x, ...) "berth"

#' @export
vec_ptype_full.railvarr_berth <- function(x, ...) "berth"

#' @export
vec_ptype2.railvarr_berth.railvarr_berth <- function(x, y, ...) new_berth()

#' @export
vec_cast.railvarr_berth.railvarr_berth <- function(x, to, ...) x

berth.default <- function(x, ...) {
  id <- vctrs::field(x, "berth")
  signal <- vctrs::field(x, "signal")
  aspect <- vctrs::field(x, "aspect")
  levels <- attr(aspect, "levels")
  idx <- unclass(aspect)
  state <- levels[idx]
  paste0(id, " ", signal, ":", state)
}

#' @rawNamespace S3method(pillar_shaft, railvarr_berth)
pillar_shaft.railvarr_berth <- function(x, ...) {
  out <- format.railvarr_berth(x, ..., formatter = berth.pillar)
  pillar::new_pillar_shaft_simple(out)
}

berth.pillar <- function(x, ...) {
  ids <- vctrs::field(x, "berth")
  sids <- vctrs::field(x, "signal")
  aspects <- vctrs::field(x, "aspect")
  colours <- list(
    "R" = "31",
    "Y" = "33",
    "YY" = "35",
    "G" = "32"
  )
  stations <- vctrs::field(x, "station")
  out <- sprintf(
    "%s:\u001b[%sm%s\u001b[39m%s",
    ids,
    colours[aspects],
    aspects,
    pillar::style_subtle(
      paste0(" (", stations, ")")
    )
  )
}
