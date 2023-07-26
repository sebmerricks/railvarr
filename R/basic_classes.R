# 4-State Aspect ---------------------------------------------------------------

source(get_class("aspect4"))

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

source(get_class("signal"))

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

source(get_class("track"))

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
