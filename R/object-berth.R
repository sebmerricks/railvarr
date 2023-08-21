new_berth <- function(id = character(),
                      signal = character(),
                      state = aspect(),
                      station = character(),
                      length = numeric()) {
  vctrs::new_rcrd(list(berth = id,
                       signal = signal,
                       aspect = state,
                       station = station,
                       length = length),
                  class = "railvarr_berth")
}

#' @importFrom zeallot %<-%
#' @export
berth <- function(id = character(),
                  signal = character(),
                  state = aspect(),
                  station = character(),
                  length = numeric()) {
  c(id, signal, state, station, length) %<-%
    vctrs::vec_recycle_common(id, signal, state, station, length)
  new_state <- aspect(state)
  validate_berth(new_berth(id, signal, new_state, station, length))
}

validate_berth <- function(x) {
  if (!all(stringr::str_detect(vctrs::field(x, "signal"), "^S[0-9]+$"))) {
    rlang::abort("signal ID must match '^S[0-9]+$'")
  }
  x
}

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
