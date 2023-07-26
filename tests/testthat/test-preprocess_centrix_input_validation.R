test_that("set_map() errors with incorrect map structure", {
  test1 <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    1, "A", "TA", "hi"
  )
  test2 <- dplyr::tribble(
    ~signal, ~berth, ~tracks,
    "1", "A", "TA",
  )
  test3 <- dplyr::tribble(
    ~signal, ~berth, ~tracks, ~event,
    "1", "A", "TA", "hi"
  )

  expect_error(set_map(test1))
  expect_error(set_map(test2))
  expect_error(set_map(test3))
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

            expect_error(preprocess_signal_events(test1))
            expect_error(preprocess_signal_events(test2))
            expect_error(preprocess_signal_events(test3))
          })


test_that("preprocess_track_events() errors if track_events has incorrect
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

            tracks <- data.frame(
              track = character()
            )

            expect_error(preprocess_track_events(test1, tracks))
            expect_error(preprocess_track_events(test2, tracks))
            expect_error(preprocess_track_events(test3, tracks))
          })

test_that("preprocess_track_events() errors if tracks has incorrect structure",
          {
            rse <- data.frame(
              asset = character(),
              dt = lubridate::POSIXct(),
              transition = character(),
              period = numeric()
            )

            test1 <- data.frame(
              track = character(),
              extra = numeric()
            )
            test2 <- data.frame(
              track = double()
            )
            test3 <- data.frame(
              tracks = character()
            )

            expect_error(preprocess_track_events(rse, test1))
            expect_error(preprocess_track_events(rse, test2))
            expect_error(preprocess_track_events(rse, test3))
          })
