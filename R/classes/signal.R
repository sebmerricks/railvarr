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

signal <- function(signal_id = character()) {
  return(validate_signal(new_signal(signal_id)))
}

is_signal <- function(obj) {
  inherits(obj, "signal")
}

print.signal <- function(signal) {
  cat(vec_data(signal), "\n")
}

# tests
s <- signal("S1")
s
s <- signal("S2")
s
s <- signal("S1234")
s
s <- signal("S1234-1")
s
s <- signal("S1A43")
s
s <- signal("S")
s
s <- signal("SABC")
s
s1 <- signal("S1")
s2 <- signal("S2")
s3 <- signal("S12")
s1 < s2
s1 < s3
