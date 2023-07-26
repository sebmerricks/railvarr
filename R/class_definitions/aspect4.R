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

  aspect <- vctrs::vec_data(aspect4)
  stopifnot(!is.na(aspect))

  return(aspect4)
}
