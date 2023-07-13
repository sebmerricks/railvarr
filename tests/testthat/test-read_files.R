setup_test_read_csv_files <- function(name, n) {
  tempdir <- file.path(tempdir(), name)
  dir.create(tempdir)

  for (i in seq_len(n)) {
    df <- data.frame(
      x = c("a", "b", "c"),
      y = c(1*i, 2*i, 3*i)
    )

    file_path <- file.path(tempdir, paste0(name, i))
    write.csv(df, file = file_path, row.names = FALSE)
  }

  return(tempdir)
}

test_that("read_csv_files() errors if directory is empty", {
  tempdir <- setup_test_read_csv_files("error_test", 0)
  expect_error(read_csv_files(tempdir))
  unlink(tempdir, recursive = TRUE)
})

test_that("read_csv_files() reads a single file", {
  tempdir <- setup_test_read_csv_files("single_file_test", 1)
  names <- c("x", "y")
  types <- readr::cols(
    "x" = readr::col_character(),
    "y" = readr::col_integer()
  )

  expect_equal(nrow(read_csv_files(tempdir, names, types, skip = 1L)), 3)
  unlink(tempdir, recursive = TRUE)
})

test_that("read_csv_files() reads multiple files", {
  tempdir <- setup_test_read_csv_files("multiple_file_test", 5)
  names <- c("x", "y")
  types <- readr::cols(
    "x" = readr::col_character(),
    "y" = readr::col_integer()
  )
  expect_equal(nrow(read_csv_files(tempdir, names, types, skip = 1L)), 15)
})
