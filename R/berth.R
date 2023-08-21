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
                      station = character(),
                      length = numeric()) {
  vctrs::new_rcrd(list(berth = id,
                       signal = signal,
                       aspect = state,
                       station = station,
                       length = length),
                  class = "railvarr_berth")
}

berth <- function(id = character(),
                  signal = character(),
                  state = aspect(),
                  station = character(),
                  length = numeric()) {
  c(id, signal, state, station, length) %<-% vctrs::vec_recycle_common(id, signal, state, station, length)
  new_state <- aspect(state)
  validate_berth(new_berth(id, signal, new_state, station, length))
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

vec_ptype_abbr.railvarr_berth <- function(x, ...) "berth"
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
    berth = berth(berth, signal, aspect, station, NA)
  ),
  -signal, -aspect, -station
)


new_tsar_event <- function(t_enters = lubridate::POSIXct(),
                           t_red_on = lubridate::POSIXct(),
                           t_enters_next = lubridate::POSIXct(),
                           t_vacates = lubridate::POSIXct(),
                           t_red_off = lubridate::POSIXct()) {
  TSAR <- lubridate::as.duration(t_red_off - t_red_on)
  T_onset <- lubridate::as.duration(t_red_on - t_enters)
  T_clear <- lubridate::as.duration(t_vacates - t_enters)
  T_offset <- lubridate::as.duration(t_red_off - t_vacates)
  T_travel <- lubridate::as.duration(t_enters_next - t_enters)
  T_coach <- lubridate::as.duration(t_vacates - t_enters_next)
  vctrs::new_rcrd(list(t_enters = t_enters,
                       t_red_on = t_red_on,
                       t_enters_next = t_enters_next,
                       t_vacates = t_vacates,
                       t_red_off = t_red_off,
                       TSAR = TSAR,
                       T_onset = T_onset,
                       T_clear = T_clear,
                       T_offset = T_offset,
                       T_travel = T_travel,
                       T_coach = T_coach),
                  class = "railvarr_tsar")
}

tsar_event <- function(t_enters = lubridate::POSIXct(),
                       t_red_on = lubridate::POSIXct(),
                       t_enters_next = lubridate::POSIXct(),
                       t_vacates = lubridate::POSIXct(),
                       t_red_off = lubridate::POSIXct()) {
  t_enters <- lubridate::as_datetime(t_enters)
  t_red_on <- lubridate::as_datetime(t_red_on)
  t_enters_next <- lubridate::as_datetime(t_enters_next)
  t_vacates <- lubridate::as_datetime(t_vacates)
  t_red_off <- lubridate::as_datetime(t_red_off)
  new_tsar_event(t_enters, t_red_on, t_enters_next, t_vacates, t_red_off)
}

format.railvarr_tsar <- function(x, ..., formatter = tsar_event.default) {
  formatter(x, ...)
}

vec_ptype_abbr.railvarr_tsar <- function(x, ...) "tsar"
vec_ptype_full.railvarr_tsar <- function(x, ...) "tsar_event"

vec_ptype2.railvarr_tsar.railvarr_tsar <- function(x, y, ...) new_tsar_event()

vec_cast.railvarr_tsar.railvarr_tsar <- function(x, to, ...) x

tsar_event.default <- function(x, ...) {
  sprintf(
    "%s %s",
    format(vctrs::field(x, "t_enters")),
    sprintf(
      "(%ds: %ds %ds %ds %ds)",
      round(vctrs::field(x, "TSAR")),
      round(vctrs::field(x, "T_onset")),
      round(vctrs::field(x, "T_travel")),
      round(vctrs::field(x, "T_coach")),
      round(vctrs::field(x, "T_offset"))
    )
  )
}

pillar_shaft.railvarr_tsar <- function(x, ...) {
  out <- format.railvarr_tsar(x, ..., formatter = tsar_event.pillar)
  pillar::new_pillar_shaft_simple(out)
}

tsar_event.pillar <- function(x, ...) {
  sprintf(
    "%s %s",
    format(vctrs::field(x, "t_enters")),
    sprintf(
      "(\u001b[31m%ds: \u001b[32m%ds \u001b[34m%ds \u001b[36m%ds \u001b[33m%ds\u001b[0m)",
      round(vctrs::field(x, "TSAR")),
      round(vctrs::field(x, "T_onset")),
      round(vctrs::field(x, "T_travel")),
      round(vctrs::field(x, "T_coach")),
      round(vctrs::field(x, "T_offset"))
    )
  )
}


dplyr::select(
  dplyr::mutate(
    dplyr::left_join(
      berth_events,
      station_names,
      by = "berth"
    ),
    berth = berth(berth, signal, aspect, station, NA),
    tsar = tsar_event(t_enters, t_red_on, t_enters_next, t_vacates, t_red_off)
  ),
  -signal, -aspect, -station,
  -t_enters, -t_red_on, -t_enters_next, -t_vacates, -t_red_off,
  -TSAR, -T_onset, -T_clear, -T_offset, -T_travel, -T_coach
)
