#' Read Centrix Data From Multiple Files
#'
#' @param path A string pointing to the directory pointing to the relevant files.
#' @param names A character vector containing the column names.
#' @param types A column specification created with \code{\link[readr]{cols}}.
#'
#' @export
read_centrix <- function(path, names, types) {
  # find all the files inside the directory at `path`
  filenames <- glue::glue("{path}/{list.files(path)}")
  # throw an error if the directory is empty
  stopifnot("`path` must not be an empty directory" = length(filenames) > 0)

  raw_centrix <- purrr::imap_dfr(filenames, ~ {
    readr::read_csv(
      file = .x,
      col_names = names,
      col_types = types,
      skip = 1L
    ) %>%
      dplyr::mutate(batch = .y)
  })

  return(raw_centrix)
}

# takes the raw data as input and returns the fully processed data

#' @export
wrangle_centrix <- function() {

}
