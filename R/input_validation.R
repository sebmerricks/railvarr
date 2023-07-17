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

#' @export
check_df <- function(df, names, types, allow_extra = TRUE) {
  ncols = length(names)
  ntypes = length(types)

  if (ncols != ntypes) {
    warning("`names` and `types` have different lengths, check documentation
            for expected behaviour.")
  }

  cols_to_check <- df
  ndata <- length(cols_to_check)

  if (ncols < ndata) {
    if (allow_extra) {
      cols_to_check <- df[1:ncols]
      ndata <- length(cols_to_check)
    } else {
      stop("More columns in data than specified in `names`. Did you intend
           `allow_extra = FALSE`?")
    }
  } else if (ncols > ndata) {
    names <- names[1:ndata]
    ncols = length(names)
  }

  if (ncols < ntypes) {
    types <- types[1:ncols]
    ntypes = length(types)
  } else if (ncols > ntypes) {
    for (i in (ntypes+1):ncols) {
      types <- append(types, list(NULL))
      ntypes = length(types)
    }
  }

  if (!(length(names) == length(types) &
        length(names) == length(cols_to_check))) {
    stop("Error in function definition.")
  }

  vetr::vet(names, names(cols_to_check))
  vetr::vet(types, cols_to_check)
}
