test_that("find_time_windows correctly finds time windows", {
  track_events <- dplyr::tribble(
    ~track, ~dt, ~occupied, ~event,
    "TA", lubridate::as_datetime(100), T, "enters",
    "TA", lubridate::as_datetime(210), T, "vacates",
    "TB", lubridate::as_datetime(200), T, "enters",
    "TB", lubridate::as_datetime(310), T, "vacates",
    "TC", lubridate::as_datetime(300), T, "enters",
    "TC", lubridate::as_datetime(410), T, "vacates",
    "TA", lubridate::as_datetime(1100), T, "enters",
    "TA", lubridate::as_datetime(1210), T, "vacates",
    "TB", lubridate::as_datetime(1200), T, "enters",
    "TB", lubridate::as_datetime(1310), T, "vacates",
    "TC", lubridate::as_datetime(1300), T, "enters",
    "TC", lubridate::as_datetime(1410), T, "vacates",
    "TA", lubridate::as_datetime(2100), T, "enters",
    "TA", lubridate::as_datetime(2210), T, "vacates",
    "TB", lubridate::as_datetime(2200), T, "enters",
    "TB", lubridate::as_datetime(2310), T, "vacates",
    "TC", lubridate::as_datetime(2300), T, "enters",
    "TC", lubridate::as_datetime(2410), T, "vacates",
    "TA", lubridate::as_datetime(2300), T, "enters",
    "TA", lubridate::as_datetime(2410), T, "vacates",
    "TB", lubridate::as_datetime(2400), T, "enters",
    "TB", lubridate::as_datetime(2510), T, "vacates",
    "TC", lubridate::as_datetime(2500), T, "enters",
    "TC", lubridate::as_datetime(2610), T, "vacates"
  )

  asset_map <- dplyr::tribble(
    ~track,
    "TA",
    "TB",
    "TC"
  )

  out <- dplyr::tribble(
    ~window, ~interval,
    1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    3, lubridate::interval(start = 2100, end = 88500, tzone = "UTC")
  )

  expect_equal(find_time_windows(track_events, asset_map), out)
})
