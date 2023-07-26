new_aspect4 <- function(aspect = integer(), levels = character()) {
  stopifnot(is.integer(aspect))
  stopifnot(is.character(levels))

  structure(aspect,
            levels = levels,
            class = "factor")
}

validate_aspect4 <- function(aspect4) {
  stopifnot(inherits(aspect4, "factor"))
  aspect <- unclass(aspect4)
  levels <- attr(aspect4, "levels")
  return(aspect4)
}

aspect4 <- function(aspect = character(), levels = c("R", "YY", "Y", "G")) {
  idx <- as.integer(match(aspect, levels))
  return(validate_aspect4(new_aspect4(idx, levels)))
}

is_aspect4 <- function(obj) {
  inherits(obj, "factor")
}

aspect <- aspect4("YY")
aspect
