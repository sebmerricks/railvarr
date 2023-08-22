# track event: track ID, datetime, enters/vacates
# signal event: signal ID, datetime, previous aspect, new aspect
# berth event: berth ID, t_enters, t_red_on, t_vacates, t_red_off
# journey: data frame of valid berth events

# Currently, as of 9:43pm 22nd August
# track events and signal events can both be created from the outputs of the
# preprocess_track_events() and preprocess_signal_events() functions.
# comparison and ordering is implemented, but more quality-of-life measures
# could be added, e.g., signal_event == "G" would be useful for filtering by
# aspect, in the same way that signal_event < "yyyy-mm-dd HH:MM:SS" is useful
# for filtering by time.
# the same for track_events, something like track_event == "enters" would be
# useful for filtering.
# next step would potentially be to implement track and signal events in the
# pre-processing functions, but this would require integration with the next
# step in the pipeline.
# the next step is the berth event, which is created by bringing together track
# and signal events.
# once that has been implemented, the next step would probably be to add the
# journey object, which would perform the time-windows validation step and the
# train id labeling.
# once journeys have been implemented, it is possible to calculate TSARs.
# journeys are necessary for this, because T_travel requires t_enters_next,
# which in turn requires both valid data and knowing the order of berth events.
# however, given that journeys already performs the validation, labeling, and
# ordering, it is not unreasonable to then give it the task of calculating
# TSARs, as the calculations are very simple once the data are all in place.
# once journeys have been fully implemented, the basic Centrix wrangling
# pipeline will be in place.
# the user would first interact with this pipeline by calling the pre-processing
# functions on the raw Centrix data.
# these functions would return the object-oriented track and signal events.
# once in posession of these events, the user can explore themselves, or pass
# them on to the berth_event() constructor, which brings them together.
# alternatively, it could be possible to simply provide a single pre-processing
# function which returns berth events, moving the track and signal events to the
# background.
# then, once the user is in possession of the berth events, they pass it on to
# the journeys.

new_track_event <- function(track_id = character(),
                            datetime = lubridate::POSIXct(),
                            event_type = character()) {
  if (!is.character(track_id)) rlang::abort("`track_id` must be a character vector")
  if (!lubridate::is.POSIXct(datetime)) rlang::abort("`datetime` must be a POSIXct vector")
  if (!is.character(event_type)) rlang::abort("`event_type` must be a character vector")
  if (!all(event_type %in% c("enters", "vacates"))) rlang::abort("`event_type` must be one of either 'enters'/'vacates'")

  vctrs::new_rcrd(list(track = track_id,
                       dt = datetime,
                       event = event_type),
                  class = "track_event")
}

#' @export
track_event <- function(track_id = character(),
                        datetime = lubridate::POSIXct(),
                        event_type = character()) {
  datetime <- lubridate::as_datetime(datetime)

  if (is.logical(event_type)) {
    event_type <- ifelse(event_type, "enters", "vacates")
  }

  new_track_event(track_id, datetime, event_type)
}

#' @importFrom vctrs field
#' @export
format.track_event <- function(x, ...) {
  event <- field(x, "event")
  sprintf(
    "%s (%s)%s %s",
    field(x, "track"),
    event,
    ifelse(event == "enters", " ", ""),
    field(x, "dt")
  )
}

#' @rawNamespace S3method(pillar_shaft, track_event)
pillar_shaft.track_event <- function(x, ...) {
  dt <- vctrs::field(x, "dt")
  date <- lubridate::date(dt)
  time <- stringr::str_extract(dt, "[0-9]{2}:[0-9]{2}:[0-9]{2}")  # thanks to milo [https://stackoverflow.com/a/71947825]
  event <- sprintf(
    "\u001b[%s%s\u001b[0m",
    "36m",
    vctrs::field(x, "event")
  )

  out <- sprintf(
    "%s (%s) %s %s",
    vctrs::field(x, "track"),
    event,
    date,
    pillar::style_subtle(time)
  )

  pillar::new_pillar_shaft_simple(out)
}

#' @export
vec_ptype_abbr.track_event <- function(x, ...) "trck"

#' @export
vec_ptype_full.track_event <- function(x, ...) "trck_vnt"

#' @importFrom vctrs field
#' @export
vec_proxy_compare.track_event <- function(x, ...) {
  as.data.frame(list(field(x, "dt"),
                     field(x, "track"),
                     field(x, "event")))
}

new_aspect <- function(x = integer(),
                       levels = character()) {
  if (!is.integer(x)) rlang::abort("`x` must be an integer vector")
  if (!is.character(levels)) rlang::abort("`levels` must be a character vector")

  vctrs::new_factor(x, levels, class = "aspect")
}

#' @export
aspect <- function(x = integer(),
                   levels = c("G", "YY", "Y", "R")) {
  if (!is.integer(x)) x <- match(x, levels)
  if (any(is.na(x))) rlang::abort("every value of `x` must be present in `levels`")
  if (any(length(levels) < x)) rlang::abort("`levels` too small: every value of `x` must correspond to a value in `levels`")

  new_aspect(x, levels)
}

#' @export
is_aspect <- function(x) inherits(x, "aspect")

colour_aspect <- function(x, ...) {
  colours <- list("G" = "32m",
                  "YY" = "35m",
                  "Y" = "33m",
                  "R" = "31m")
  idx <- vctrs::vec_data(x)
  asp <- attr(x, "levels")[idx]
  sprintf(
    "\u001b[%s%s\u001b[0m",
    colours[asp],
    asp
  )
}

new_signal_event <- function(signal_id = character(),
                             datetime = lubridate::POSIXct(),
                             previous_aspect = aspect(),
                             new_aspect = aspect()) {
  if (!is.character(signal_id)) rlang::abort("`signal_id` must be a character vector")
  if (!lubridate::is.POSIXct(datetime)) rlang::abort("`datetime` must be a POSIXct vector")
  if (!is_aspect(previous_aspect)) rlang::abort("`previous_aspect` must be an aspect")
  if (!is_aspect(new_aspect)) rlang::abort("`aspect` must be an aspect")

  vctrs::new_rcrd(list(signal = signal_id,
                       dt = datetime,
                       previous_aspect = previous_aspect,
                       new_aspect = new_aspect),
                  class = "signal_event")
}

#' @export
signal_event <- function(signal_id = character(),
                         datetime = lubridate::POSIXct(),
                         previous_aspect = aspect(),
                         new_aspect = aspect()) {
  datetime <- lubridate::as_datetime(datetime)
  previous_aspect <- aspect(previous_aspect)
  new_aspect <- aspect(new_aspect)

  new_signal_event(signal_id, datetime, previous_aspect, new_aspect)
}

#' @importFrom vctrs field
#' @export
format.signal_event <- function(x, ...) {
  sprintf(
    "%s (%s->%s) %s",
    field(x, "signal"),
    field(x, "previous_aspect"),
    field(x, "new_aspect"),
    field(x, "dt")
  )
}

#' @rawNamespace S3method(pillar_shaft, signal_event)
pillar_shaft.signal_event <- function(x, ...) {
  dt <- vctrs::field(x, "dt")
  date <- lubridate::date(dt)
  time <- stringr::str_extract(dt, "[0-9]{2}:[0-9]{2}:[0-9]{2}")  # thanks to milo [https://stackoverflow.com/a/71947825]

  pasp <- colour_aspect(vctrs::field(x, "previous_aspect"))
  nasp <- colour_aspect(vctrs::field(x, "new_aspect"))

  out <- sprintf(
    "%s (%s->%s) %s %s",
    vctrs::field(x, "signal"),
    pasp,
    nasp,
    date,
    pillar::style_subtle(time)
  )

  pillar::new_pillar_shaft_simple(out)
}

#' @export
vec_ptype_abbr.signal_event <- function(x, ...) "sgnl"

#' @export
vec_ptype_full.signal_event <- function(x, ...) "sgnl_vnt"

#' @importFrom vctrs field
#' @export
vec_proxy_compare.signal_event <- function(x, ...) {
  as.data.frame(list(field(x, "dt"),
                     field(x, "signal"),
                     field(x, "previous_aspect"),
                     field(x, "new_aspect")))
}


# Testing the ordering (+comparison) of signal events
#
# dplyr::tribble(~signal, ~dt, ~paspect, ~naspect,
#                "S135", "2023-05-23 13:54:40", "R", "Y",
#                "S135", "2023-05-23 13:54:40", "R", "G") %>%
#   mutate(se = signal_event(signal, dt, paspect, naspect)) %>%
#   arrange(se)
#
# dplyr::tribble(~signal, ~dt, ~paspect, ~naspect,
#                "S135", "2023-05-23 13:54:40", "R", "Y",
#                "S135", "2023-05-23 13:54:40", "G", "R") %>%
#   mutate(se = signal_event(signal, dt, paspect, naspect)) %>%
#   arrange(se)
#
# dplyr::tribble(~signal, ~dt, ~paspect, ~naspect,
#                "S137", "2023-05-23 13:54:40", "R", "Y",
#                "S135", "2023-05-23 13:54:41", "G", "R") %>%
#   mutate(se = signal_event(signal, dt, paspect, naspect)) %>%
#   arrange(se)
#
# dplyr::tribble(~signal, ~dt, ~paspect, ~naspect,
#                "S137", "2023-05-23 13:54:40", "R", "Y",
#                "S135", "2023-05-23 13:54:40", "G", "R") %>%
#   mutate(se = signal_event(signal, dt, paspect, naspect)) %>%
#   arrange(se)
#
# dplyr::tribble(~signal, ~dt, ~paspect, ~naspect,
#                "S135", "2023-05-23 13:54:40", "R", "Y",
#                "S135", "2023-05-23 13:54:39", "G", "R") %>%
#   mutate(se = signal_event(signal, dt, paspect, naspect)) %>%
#   arrange(se)
