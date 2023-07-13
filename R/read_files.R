#' Read raw CSV data from multiple files
#'
#' Reads all the CSV files contained in the directory specified  by `path` and
#' combines them into a single data frame. Errors will be thrown if either
#' the directory does not exist or is empty.
#'
#' @param path A character string specifying the path to the directory
#'  containing the CSV files.
#' @param ... Additional arguments to be passed to [readr::read_csv].
#' @return A single data frame.
#' @seealso [readr::read_csv()] which this function wraps.
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
#' # Read the data
#' read_csv_files(tempdir, names, types, skip = 1L)
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
#' Reads all the excel files contained in the directory specified by `path` and
#' combines them into a single data frame. Errors will be thrown if either
#' the directory does not exist or is empty.
#'
#' @param path A character string specifying the path to the directory
#'  containing the excel files.
#' @param progress Whether to show a progress bar, see [purrr::map()].
#' @param ... Additional arguments to be passed to [readxl::read_excel()].
#' @returns A single data frame.
#' @seealso [readxl::read_excel()] which this function wraps.
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
