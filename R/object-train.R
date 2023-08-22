new_train <- function(id = integer(),
                      group = character(),
                      stops = character()) {
  if (!is.integer(id)) rlang::abort("`id` must be an integer vector")
  if (!is.character(group)) rlang::abort("`group` must be a character vector")

  vctrs::new_rcrd(list(id = id,
                       group = group,
                       stops = stops),
                  class = "railvarr_train")
}

#' @importFrom zeallot %<-%
#' @export
train <- function(id = integer(),
                  group = character(),
                  stops = character()) {
  id <- vctrs::vec_cast(id, integer())
  c(id, group, stops) %<-% vctrs::vec_recycle_common(id, group, stops)
  new_train(id, group, stops)
}

#' @export
format.railvarr_train <- function(x, ...) {
  paste0(vctrs::field(x, "id"), ":", vctrs::field(x, "group"))
}

#' @export
vec_ptype_abbr.railvarr_train <- function(x, ...) "train"
#' @export
vec_ptype_full.railvarr_train <- function(x, ...) "train"

#' @rawNamespace S3method(pillar_shaft, railvarr_train)
pillar_shaft.railvarr_train <- function(x, ...) {
  out <- sprintf(
    "%s:%s",
    vctrs::field(x, "id"),
    pillar::style_subtle(vctrs::field(x, "group"))
  )
  pillar::new_pillar_shaft_simple(out)
}
