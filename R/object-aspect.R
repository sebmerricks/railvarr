new_aspect <- function(x = character(), levels = character()) {
  i <- match(x, levels)
  vctrs::new_factor(i, levels, class = "railvarr_aspect")
}

#' Create new aspect
#' @param x Aspect
#' @param levels Possible aspects
#' @export
aspect <- function(x = character(), levels = c("R", "Y", "YY", "G")) {
  validate_aspect(new_aspect(x, levels))
}

validate_aspect <- function(x) {
  values <- unclass(x)
  if (length(values) == 0) return(x)

  levels <- attr(x, "levels")

  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }

  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }

  x
}
