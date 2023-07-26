new_aspect4 <- function(aspect = integer(), levels = character()) {
  stopifnot(is.integer(aspect))
  stopifnot(is.character(levels))

  structure(aspect,
            levels = levels,
            class = "factor")
}

validate_aspect4 <- function(aspect4) {
  stopifnot(inherits(aspect4, "factor"))

  levels <- attr(aspect4, "levels")
  stopifnot(is.character(levels))
  stopifnot(length(levels) == 4)

  return(aspect4)
}

#' @export
aspect4 <- function(aspect = character(), levels = c("R", "Y", "YY", "G")) {
  idx <- as.integer(match(aspect, levels))
  return(validate_aspect4(new_aspect4(idx, levels)))
}

#' @export
is_aspect4 <- function(obj) {
  inherits(obj, "factor")
}

#' @export
as_aspect4 <- function(x) {
  return(aspect4(x))
}

# Signal -----------------------------------------------------------------------

new_signal <- function(signal_id = character()) {
  stopifnot(is.character(signal_id))

  signal_id <- stringr::str_extract(signal_id, "S[0-9]+")
  return(
    structure(signal_id,
              class = "signal")
  )
}

validate_signal <- function(signal) {
  stopifnot(inherits(signal, "signal"))
  signal_id <- unclass(signal)
  stopifnot(stringr::str_like(signal_id, "S[0-9]+"))
  return(signal)
}

#' @export
signal <- function(signal_id = character()) {
  return(validate_signal(new_signal(signal_id)))
}

#' @export
is_signal <- function(obj) {
  inherits(obj, "signal")
}

#' @export
as_signal <- function(x) {
  return(signal(x))
}

# Track ------------------------------------------------------------------------

new_track <- function(track_id = character()) {
  stopifnot(is.character(track_id))

  track_id <- stringr::str_extract(track_id, "T[A-Z]+(-[0-9])?")
  return(
    structure(track_id,
              class = "track")
  )
}

validate_track <- function(track) {
  stopifnot(inherits(track, "track"))
  track_id <- unclass(track)
  stopifnot(stringr::str_like(track_id, "T[A-Z]+(-[0-9])?"))
  return(track)
}

#' @export
track <- function(track_id = character()) {
  return(validate_track(new_track(track_id)))
}

#' @export
is_track <- function(obj) {
  inherits(obj, "track")
}

#' @export
as_track <- function(x) {
  return(track(x))
}

