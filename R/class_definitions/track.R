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
