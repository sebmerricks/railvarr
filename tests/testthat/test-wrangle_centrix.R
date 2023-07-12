setup_test_read_files <- function(name, n) {
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

test_that("read_files() errors if directory is empty", {
  tempdir <- setup_test_read_files("error_test", 0)
  expect_error(read_files(tempdir))
  unlink(tempdir, recursive = TRUE)
})

test_that("read_files() reads a single file", {
  tempdir <- setup_test_read_files("single_file_test", 1)
  names <- c("x", "y")
  types <- readr::cols(
    "x" = readr::col_character(),
    "y" = readr::col_integer()
  )
  expect_equal(nrow(read_files(tempdir, names, types)), 3)
  unlink(tempdir, recursive = TRUE)
})

test_that("read_files() reads multiple files", {
  tempdir <- setup_test_read_files("multiple_file_test", 5)
  names <- c("x", "y")
  types <- readr::cols(
    "x" = readr::col_character(),
    "y" = readr::col_integer()
  )
  expect_equal(nrow(read_files(tempdir, names, types)), 15)
})


test_that("split_signal_track_events() errors with incorrect raw_events
          structure", {
  test1 <- data.frame(
    asset = character(),
    dt = double(),
    transition = character(),
    period = numeric()
    )
  test2 <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character()
    )
  test3 <- data.frame(
    asset = character(),
    datetime = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
    )

  expect_error(split_signal_track_events(test1))
  expect_error(split_signal_track_events(test2))
  expect_error(split_signal_track_events(test3))
})

test_that("split_signal_track_events() splits signal and track events", {
  raw_events <- tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1", lubridate::as_datetime(100), "DN to UP", 1,
    "T1", lubridate::as_datetime(200), "DN to UP", 1
  )

  out <- raw_events %>%
    dplyr::mutate(is_track = stringr::str_starts(asset, "T")) %>%
    dplyr::group_by(is_track) %>%
    dplyr::group_split(.keep = F)

  expect_equal(split_signal_track_events(raw_events), out)
})

test_that("split_signal_track_events() works with custom is_track definition", {
  raw_events <- tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1", lubridate::as_datetime(100), "DN to UP", 1,
    "D1", lubridate::as_datetime(200), "DN to UP", 1
  )

  is_track = quote(stringr::str_starts(asset, "D"))

  out <- raw_events %>%
    dplyr::mutate(is_track = eval(is_track)) %>%
    dplyr::group_by(is_track) %>%
    dplyr::group_split(.keep = F)

  expect_equal(split_signal_track_events(raw_events, is_track), out)
})


test_that("preprocess_signal_events() errors if raw_signal_events has incorrect
          structure", {
  test1 <- data.frame(
    asset = character(),
    dt = double(),
    transition = character(),
    period = numeric()
    )
  test2 <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character()
  )
  test3 <- data.frame(
    asset = character(),
    datetime = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )

  dummy <- data.frame(
    state = character(),
    aspect = factor()
  )

  expect_error(preprocess_signal_events(test1, dummy))
  expect_error(preprocess_signal_events(test2, dummy))
  expect_error(preprocess_signal_events(test3, dummy))
})

test_that("preprocess_signal_events() errors if state_mapping has incorrect
          structure", {
  dummy <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )

  test1 <- data.frame(
    state = character(),
    aspect = character()
  )
  test2 <- data.frame(
    state = character()
  )
  test3 <- data.frame(
    status = character(),
    aspect = factor()
  )

  expect_error(preprocess_signal_events(dummy, test1))
  expect_error(preprocess_signal_events(dummy, test2))
  expect_error(preprocess_signal_events(dummy, test3))
})

test_that("preprocess_signal_events() completes if data structures are correct",
          {
  rse <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  sm <- data.frame(
    state = character(),
    aspect = factor()
  )

  expect_no_error(preprocess_signal_events(rse, sm))
})

test_that("preprocess_signal_events() successfully converts to signal/aspect", {
  rse <- dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP", 1,
    "S2 HGE", lubridate::as_datetime(200), "DN to UP", 1,
    "S3 DGE", lubridate::as_datetime(300), "DN to UP", 1,
    "S4 HHGE", lubridate::as_datetime(400), "DN to UP", 1
  )
  sm <- data.frame(
    state = c("RGE", "HGE", "HHGE", "DGE"),
    aspect = factor(
      c("R", "Y", "YY", "G"),
      levels = c("R", "Y", "YY", "G")
    )
  )

  out <- dplyr::tibble(data.frame(
    period = c(1, 1, 1, 1),
    signal = c("S1", "S2", "S3", "S4"),
    dt = c(lubridate::as_datetime(100), lubridate::as_datetime(200),
           lubridate::as_datetime(300), lubridate::as_datetime(400)),
    aspect = factor(
      c("R", "Y", "G", "YY"),
      levels = c("R", "Y", "YY", "G")
    ),
    past_aspect = factor(
      c(NA_character_, NA_character_, NA_character_, NA_character_),
      levels = c("R", "Y", "YY", "G")
    )
  ))

  expect_equal(preprocess_signal_events(rse, sm), out)
})
