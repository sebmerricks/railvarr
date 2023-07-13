#' Read raw CSV data from multiple files
#'
#' Reads all the CSV files contained in the directory specified  by `path` and
#' combines them into a single data frame.
#'
#' @param path A character string specifying the path to the directory
#'  containing the CSV files
#' @param ... Additional arguments to be passed to \link[readr]{read_csv}
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
#' read_csv_files(tempdir, names, types)
#'
#' # Delete the temporary directory
#' unlink(tempdir, recursive = TRUE)
#'
read_csv_files <- function(path, ...) {
  # Validate the directory
  stop_if_not(dir.exists(path),
              msg = glue::glue("Directory does not exist: {path}"))
  # find all the files inside the directory at `path`
  filenames <- glue::glue("{path}/{list.files(path)}")
  # throw an error if the directory is empty
  stop_if_not(length(filenames) > 0,
              msg = glue::glue("No files found in the directory: {path}"))

  raw_data <- readr::read_csv(filenames, ...)

  return(raw_data)
}

#' Read raw excel data from multiple files
#'
#' @param path A string pointing to the directory containing excel files
#' @param progress Whether to show a progress bar, see \code{\link{map}}
#' @param ... Additional arguments to be passed to \code{\link{read_excel}}
#'
#' @export
#'
read_excel_files <- function(path, progress = TRUE, ...) {
  stop_if_not(dir.exists(path),
              msg = glue::glue("Directory does not exist: {path}"))

  filenames <- glue::glue("{path}/{list.files(path)}")

  stop_if_not(length(filenames) > 0,
              msg = glue::glue("No files found in the directory: {path}"))

  raw_data <- purrr::map(filenames,
                  readxl::read_excel,
                  .progress = progress,
                  ...) %>%
    list_rbind()

  return(raw_data)
}
