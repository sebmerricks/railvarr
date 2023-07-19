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

#' Check Data Frame Columns
#'
#' This function validates and checks the columns of a given data frame against
#' specified column names and types. It ensures that the data frame columns
#' match the expected names and data types.
#'
#' @param data The data frame to be checked.
#' @param names A character vector specifying the expected column names.
#' @param types A list of data types corresponding to the expected types of the
#'   data frame columns.
#' @param allow_extra Logical; if TRUE (default), allows additional columns in
#'   the data frame beyond the specified names. If FALSE, any extra columns will
#'   trigger an error.
#'
#' @details The function performs column validation for the given data frame
#'   `data` by comparing its columns against the expected `names` and `types`.
#'   The goal is to ensure that the data frame's structure matches the provided
#'   specifications. The following scenarios are considered during the
#'   validation process:
#' \itemize{
#'  \item{If the number of columns in `data` is greater than the number of
#'  specified `names` and `types`, the function will allow extra columns if
#'  `allow_extra = TRUE`. Otherwise, an error will be thrown, indicating that
#'  there are too many columns.}
#'  \item{If the number of columns in `data` is less than the number of
#'  specified `names` and `types`, the function will truncate both `names` and
#'  `types` to the length of `data`.}
#'  \item{If the length of `names` is greater than the length of `types`,
#'  `types` will be elongated with a generic type that will match any column.}
#'  \item{If the length of `names` is less than the length of `types`, `types`
#'  will be truncated to match `names`}
#' }
#'   The function uses the [vetr] package for validation.
#'
#' @seealso [vetr::vet()]
#'
#' @importFrom vetr vet
#'
#' @examples
#' # Set up the data frame to be checked
#' data <- dplyr::tribble(
#'   ~a, ~b, ~c,
#'   1, "a", 4
#' )
#'
#' # Define the expected names and types
#' names <- c("a", "b", "c")
#' types <- list(integer(), character(), numeric())
#'
#' # Check data
#' check_df(data, names, types)
#'
#' @export
check_df <- function(data, names, types, allow_extra = TRUE) {
  ncols = length(names)
  ntypes = length(types)

  if (ncols != ntypes) {
    warning("`names` and `types` have different lengths, check documentation
            for expected behaviour.")
  }

  if (allow_extra) {
    names_present <- which((names %in% colnames(data)) %in% TRUE)
    names <- names[names_present]
    cols_to_check <- data[,names]
  } else {
    cols_to_check <- data
  }

  ndata <- length(cols_to_check)

  if (ncols < ndata) {
    if (allow_extra) {
      cols_to_check <- data[1:ncols]
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

  names_equal = names == names(cols_to_check)
  idx <- match(FALSE, names_equal)
  if (!all(names_equal)) {
    stop(glue::glue("Column {names(cols_to_check[idx])} should be named
                    {names[idx]}"))
  }
  vetr::vet(types, cols_to_check, stop = TRUE)
}
