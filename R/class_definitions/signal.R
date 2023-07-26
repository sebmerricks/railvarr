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
  stopifnot(!is.na(stringr::str_extract(signal_id, "S[0-9]+")))
  return(signal)
}
