setup_csv_test <- function(name, n) {
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

read_rds_test <- function(filename) {
  dplyr::tibble(read_rds_test_raw(filename))
}

read_rds_test_raw <- function(filename) {
  readr::read_rds(testthat::test_path("fixtures", filename))
}
