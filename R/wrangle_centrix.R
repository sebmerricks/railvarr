#' Read raw data from multiple files
#'
#' @param path A string pointing to the directory pointing to the relevant files.
#' @param names A character vector containing the column names.
#' @param types A column specification created with \code{\link[readr]{cols}}.
#'
#' @export
#'
#' @examples
#' # Create a temporary directory for the example
#' tempdir <- tempdir()
#' tempdir <- file.path(tempdir, "example")
#' dir.create(tempdir)
#'
#' # Generate a sample data frame
#' df <- data.frame(
#' x = c("a", "b", "c"),
#' y = c(1, 2, 3)
#' )
#'
#' # Define the file path
#' file_path <- file.path(tempdir, "example.csv")
#'
#' # Save the data frame as a CSV file
#' write.csv(df, file = file_path, row.names = FALSE)
#'
#' # Define column names and types
#' names <- c("x", "y")
#' types <- readr::cols(
#' "x" = readr::col_character(),
#' "y" = readr::col_integer()
#' )
#'
#' # Read the data using read_centrix()
#' read_files(tempdir, names, types)
#'
#' # Delete the temporary directory
#' unlink(tempdir, recursive = TRUE)
#'
read_files <- function(path, names = NULL, types = NULL) {
  # find all the files inside the directory at `path`
  filenames <- glue::glue("{path}/{list.files(path)}")
  # throw an error if the directory is empty
  stopifnot("`path` must not be an empty directory" = length(filenames) > 0)

  raw_data <- readr::read_csv(
    filenames,
    col_names = names,
    col_types = types,
    skip = 1L
  )

  return(raw_data)
}

#' Wrangle raw Centrix data
#'
#' @export
#'
wrangle_centrix <- function(raw_signal_events, raw_track_events) {
  # Define the expected data structure
  template <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  # Check whether the raw data matches the expected structure
  vet(template, raw_signal_events)
  vet(template, raw_track_events)
}


