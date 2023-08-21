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

#' Create a new tsar event
#' @param t_enters Time train enters berth
#' @param t_red_on Time signal changes to red
#' @param t_enters_next Time train enters next berth
#' @param t_vacates Time train vacates berth
#' @param t_red_off Time signal changes off red
#' @export
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

#' @export
format.railvarr_tsar <- function(x, ..., formatter = tsar_event.default) {
  formatter(x, ...)
}

#' @export
vec_ptype_abbr.railvarr_tsar <- function(x, ...) "tsar"

#' @export
vec_ptype_full.railvarr_tsar <- function(x, ...) "tsar_event"

#' @export
vec_ptype2.railvarr_tsar.railvarr_tsar <- function(x, y, ...) new_tsar_event()

#' @export
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

#' @rawNamespace S3method(pillar_shaft, railvarr_tsar)
pillar_shaft.railvarr_tsar <- function(x, ...) {
  out <- format.railvarr_tsar(x, ..., formatter = tsar_event.pillar)
  pillar::new_pillar_shaft_simple(out)
}

tsar_event.pillar <- function(x, ...) {
  sprintf(
    "%s (%s: %s %s %s)",
    format(vctrs::field(x, "t_enters")),
    sprintf(
      "\u001b[31m%ds\u001b[39m",
      round(vctrs::field(x, "TSAR"))
    ),
    pillar::style_subtle(sprintf(
      "%s",
      round(vctrs::field(x, "T_onset"))
    )),
    sprintf(
      "\u001b[34m%ds\u001b[39m",
      round(vctrs::field(x, "T_travel"))
    ),
    pillar::style_subtle(sprintf(
      "%s %s",
      round(vctrs::field(x, "T_coach")),
      round(vctrs::field(x, "T_offset"))
    ))
  )
}
