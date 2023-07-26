# 4-State Aspect ---------------------------------------------------------------

source(get_class("aspect4"))

aspect4 <- function(aspect = character(), levels = c("R", "YY", "Y", "G")) {
  idx <- as.integer(match(aspect, levels))
  return(validate_aspect4(new_aspect4(idx, levels)))
}

is_aspect4 <- function(obj) {
  inherits(obj, "factor")
}

# Signal -----------------------------------------------------------------------

source(get_class("signal"))

signal <- function(signal_id = character()) {
  return(validate_signal(new_signal(signal_id)))
}

is_signal <- function(obj) {
  inherits(obj, "signal")
}
