stop_if_not <- function(..., msg = NULL) {
  tryCatch({
    stopifnot(...)
  }, error = function(e) {
    if (is.null(msg)) {
      stopifnot(...)
    } else {
      stop(msg)
    }
  })
}
