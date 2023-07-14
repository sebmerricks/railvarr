test_that("read_csv_files() errors if directory is empty", {
  tempdir <- setup_csv_test("error_test", 0)
  expect_error(read_csv_files(tempdir))
  unlink(tempdir, recursive = TRUE)
})

test_that("read_csv_files() reads a single file", {
  tempdir <- setup_csv_test("single_file_test", 1)
  names <- c("x", "y")
  types <- readr::cols(
    "x" = readr::col_character(),
    "y" = readr::col_integer()
  )

  expect_equal(nrow(read_csv_files(tempdir, names, types, skip = 1L)), 3)
  unlink(tempdir, recursive = TRUE)
})

test_that("read_csv_files() reads multiple files", {
  tempdir <- setup_csv_test("multiple_file_test", 5)
  names <- c("x", "y")
  types <- readr::cols(
    "x" = readr::col_character(),
    "y" = readr::col_integer()
  )
  expect_equal(nrow(read_csv_files(tempdir, names, types, skip = 1L)), 15)
})
