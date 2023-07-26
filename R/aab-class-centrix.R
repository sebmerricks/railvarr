# Centrix ----------------------------------------------------------------------
new_centrix <- function(data = data.frame(), class = class(data.frame)) {
  stopifnot(is.data.frame(data))
  structure(data,
            class = c("centrix", class))
}

validate_centrix <- function(centrix) {
  stopifnot(is_centrix(centrix))
  stopifnot(c("asset", "dt", "transition", "period") %in% names(centrix))

  stopifnot(is.character(centrix$asset))
  stopifnot(lubridate::is.POSIXct(centrix$dt))
  stopifnot(is.character(centrix$transition))
  stopifnot(is.integer(centrix$period))

  stopifnot(all(centrix$transition %in% c("DN to UP", "UP to DN")))

  return(centrix)
}

#' @export
centrix <- function(data = data.frame("asset" = character(),
                                      "dt" = lubridate::POSIXct(),
                                      "transition" = character(),
                                      "period" = integer())) {
  data <- dplyr::tibble(data)
  class <- class(data)
  return(validate_centrix(new_centrix(data, class)))
}

#' @export
is_centrix <- function(obj) {
  inherits(obj, "centrix")
}

#' @export
as_centrix <- function(x) {
  return(centrix(x))
}

# Aspect Events ----------------------------------------------------------------
new_aspect_event <- function(data = data.frame(), class = class(data.frame)) {
  stopifnot(is.data.frame(data))
  structure(data,
            class = c("aspect_event", class))
}

validate_aspect_event <- function(aspect_event) {
  stopifnot(is_aspect_event(aspect_event))
  stopifnot(c("signal", "aspect", "past_aspect", "dt", "period") %in%
              names(aspect_event))

  stopifnot(is_signal(aspect_event$signal))
  stopifnot(is_aspect4(aspect_event$aspect))
  stopifnot(is_aspect4(aspect_event$past_aspect))
  stopifnot(lubridate::is.POSIXct(aspect_event$dt))
  stopifnot(is.integer(aspect_event$period))

  return(aspect_event)
}

#' @export
aspect_event <- function(data = data.frame()) {
  data <- dplyr::tibble(data)
  class <- class(data)
  return(validate_aspect_event(new_aspect_event(data, class)))
}

#' @export
is_aspect_event <- function(obj) {
  inherits(obj, "aspect_event")
}

# Track Events -----------------------------------------------------------------
new_track_event <- function(data = data.frame(), class = class(data.frame)) {
  stopifnot(is.data.frame(data))
  structure(data,
            class = c("track_event", class))
}

validate_track_event <- function(track_event) {
  stopifnot(is_track_event(track_event))
  stopifnot(c("track", "occupied", "dt", "event", "period") %in%
              names(track_event))

  stopifnot(is_track(track_event$track))
  stopifnot(is.logical(track_event$occupied))
  stopifnot(lubridate::is.POSIXct(track_event$dt))
  stopifnot(is.character(track_event$event))
  stopifnot(all(track_event$event %in% c("enters", "vacates")))
  stopifnot(is.integer(track_event$period))

  return(track_event)
}

#' @export
track_event <- function(data = data.frame()) {
  data <- dplyr::tibble(data)
  class <- class(data)
  return(validate_track_event(new_track_event(data, class)))
}

#' @export
is_track_event <- function(obj) {
  inherits(obj, "track_event")
}
