new_aspect <- function(x = character(), levels = character()) {
  i <- match(x, levels)
  vctrs::new_factor(i, levels, class = "railvarr_aspect")
}

aspect <- function(x = character(), levels = c("R", "Y", "YY", "G")) {
  validate_aspect(new_aspect(x, levels))
}

validate_aspect <- function(x) {
  values <- unclass(x)
  if (length(values) == 0) return(x)

  levels <- attr(x, "levels")

  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }

  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }

  x
}

new_aspect("R", c("R", "Y", "YY", "G"))
aspect("R")


new_berth <- function(id = character(),
                      signal = character(),
                      state = aspect(),
                      station = character()) {
  vctrs::new_rcrd(list(berth = id,
                       signal = signal,
                       aspect = state,
                       station = station),
                  class = "railvarr_berth")
}

berth <- function(id = character(),
                  signal = character(),
                  state = aspect(),
                  station = character()) {
  new_state <- aspect(state)
  validate_berth(new_berth(id, signal, new_state, station))
}

validate_berth <- function(x) {
  if (!all(stringr::str_detect(vctrs::field(x, "signal"), "^S[0-9]+$"))) {
    rlang::abort("signal ID must match '^S[0-9]+$'")
  }
  x
}

is.berth <- function(x) inherits(x, "railvarr_berth")

format.railvarr_berth <- function(x, ..., formatter = berth.default) {
  formatter(x, ...)
}

vec_ptype_abbr.railvarr_berth <- function(x, ...) "brth"
vec_ptype_full.railvarr_berth <- function(x, ...) "berth"

vec_ptype2.railvarr_berth.railvarr_berth <- function(x, y, ...) new_berth()

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


# c(signal("S1", "R"), signal("S2", "G"))
# dplyr::tibble(signal = c(signal("S1", "R"),
#                          signal("S2", "Y"),
#                          signal("S3", "YY"),
#                          signal("S4", "G")))

station_names <- dplyr::tribble(
  ~berth, ~station,
  "A", "Station 1",
  "D", "Station 2",
  "F", "Station 3"
)

dplyr::select(
  dplyr::mutate(
    dplyr::left_join(
      berth_events,
      station_names,
      by = "berth"
    ),
    berth_events,
    berth = berth(berth, signal, aspect, station)
  ),
  -signal, -aspect, -station
)
