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

test_that("validate_track_windows correctly filters out invalid windows", {
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
    "TC", lubridate::as_datetime(2610), T, "vacates",
    "TA", lubridate::as_datetime(3100), T, "enters",
    "TA", lubridate::as_datetime(3210), T, "vacates",
    "TB", lubridate::as_datetime(3200), T, "enters",
    "TB", lubridate::as_datetime(3310), T, "vacates",
    "TA", lubridate::as_datetime(4100), T, "enters",
    "TA", lubridate::as_datetime(4210), T, "vacates",
    "TB", lubridate::as_datetime(4200), T, "enters",
    "TB", lubridate::as_datetime(4310), T, "vacates",
    "TB", lubridate::as_datetime(4320), T, "enters",
    "TB", lubridate::as_datetime(4450), T, "vacates",
    "TC", lubridate::as_datetime(4300), T, "enters",
    "TC", lubridate::as_datetime(4410), T, "vacates",
    "TA", lubridate::as_datetime(5100), T, "enters",
    "TA", lubridate::as_datetime(5210), T, "vacates",
    "TA", lubridate::as_datetime(5220), T, "vacates",
    "TB", lubridate::as_datetime(5200), T, "enters",
    "TB", lubridate::as_datetime(5310), T, "vacates",
    "TC", lubridate::as_datetime(5300), T, "enters",
    "TC", lubridate::as_datetime(5410), T, "vacates",
    "TA", lubridate::as_datetime(6100), T, "enters",
    "TA", lubridate::as_datetime(6210), T, "vacates",
    "TB", lubridate::as_datetime(6200), T, "enters",
    "TB", lubridate::as_datetime(6310), T, "vacates",
    "TC", lubridate::as_datetime(6300), T, "enters",
    "TC", lubridate::as_datetime(6350), T, "enters",
    "TC", lubridate::as_datetime(6410), T, "vacates"
  )

  time_windows <- dplyr::tribble(
    ~window, ~interval,
    1, lubridate::interval(start = 100, end = 410, tzone = "UTC"),
    2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    4, lubridate::interval(start = 3100, end = 3310, tzone = "UTC"),
    5, lubridate::interval(start = 4100, end = 4410, tzone = "UTC"),
    6, lubridate::interval(start = 5100, end = 5410, tzone = "UTC"),
    7, lubridate::interval(start = 6100, end = 6410, tzone = "UTC"),
  )

  asset_map <- dplyr::tribble(
    ~track, "TA", "TB", "TC"
  )

  out <- dplyr::tribble(
    ~window, ~ntrains,
    1, 1,
    2, 1,
    3, 2
  )

  expect_equal(validate_track_windows(track_events, time_windows, asset_map),
               out)
})

test_that("validate_aspect_windows correctly filters out invalid windows", {
  aspect_events <- dplyr::tribble(
    ~signal, ~dt, ~aspect, ~past_aspect,
    "S1", lubridate::as_datetime(101), "R", "G",
    "S1", lubridate::as_datetime(220), "Y", "R",
    "S2", lubridate::as_datetime(201), "R", "G",
    "S2", lubridate::as_datetime(320), "Y", "R",
    "S3", lubridate::as_datetime(301), "R", "G",
    "S3", lubridate::as_datetime(420), "Y", "R",
    "S4", lubridate::as_datetime(401), "R", "G",
    "S1", lubridate::as_datetime(1101), "R", "G",
    "S1", lubridate::as_datetime(1220), "Y", "R",
    "S2", lubridate::as_datetime(1201), "R", "G",
    "S2", lubridate::as_datetime(1320), "Y", "R",
    "S3", lubridate::as_datetime(1301), "R", "G",
    "S3", lubridate::as_datetime(1420), "Y", "R",
    "S4", lubridate::as_datetime(1401), "R", "G",
    "S1", lubridate::as_datetime(2101), "R", "G",
    "S1", lubridate::as_datetime(2220), "Y", "R",
    "S2", lubridate::as_datetime(2201), "R", "G",
    "S2", lubridate::as_datetime(2320), "Y", "R",
    "S3", lubridate::as_datetime(2301), "R", "G",
    "S3", lubridate::as_datetime(2420), "Y", "R",
    "S4", lubridate::as_datetime(2401), "R", "G",
    "S1", lubridate::as_datetime(2301), "R", "G",
    "S1", lubridate::as_datetime(2420), "Y", "R",
    "S2", lubridate::as_datetime(2401), "R", "G",
    "S2", lubridate::as_datetime(2520), "Y", "R",
    "S3", lubridate::as_datetime(2501), "R", "G",
    "S3", lubridate::as_datetime(2620), "Y", "R",
    "S4", lubridate::as_datetime(2601), "R", "G",
    "S1", lubridate::as_datetime(3101), "R", "G",
    "S1", lubridate::as_datetime(3220), "Y", "R",
    "S2", lubridate::as_datetime(3201), "R", "G",
    "S3", lubridate::as_datetime(3301), "R", "G",
    "S3", lubridate::as_datetime(3420), "Y", "R",
    "S4", lubridate::as_datetime(3401), "R", "G",
    "S1", lubridate::as_datetime(4101), "R", "G",
    "S1", lubridate::as_datetime(4220), "Y", "R",
    "S1", lubridate::as_datetime(4151), "R", "G",
    "S1", lubridate::as_datetime(4270), "Y", "R",
    "S2", lubridate::as_datetime(4201), "R", "G",
    "S2", lubridate::as_datetime(4320), "Y", "R",
    "S3", lubridate::as_datetime(4301), "R", "G",
    "S3", lubridate::as_datetime(4420), "Y", "R",
    "S4", lubridate::as_datetime(4401), "R", "G"
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")),
           past_aspect = factor(past_aspect, levels = c("R", "Y", "YY", "G")))

  time_windows <- dplyr::tribble(
    ~window, ~interval,
    1, lubridate::interval(start = 100, end = 410, tzone = "UTC"),
    2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    4, lubridate::interval(start = 3100, end = 3310, tzone = "UTC"),
    5, lubridate::interval(start = 4100, end = 4410, tzone = "UTC")
  )

  asset_map <- dplyr::tribble(
    ~signal, ~event,
    "S1", "enters",
    "S1", "vacates",
    "S2", "enters",
    "S2", "vacates",
    "S3", "enters",
    "S3", "vacates",
    "S4", "enters"
  )

  out <- dplyr::tribble(
    ~window, ~ntrains,
    1, 1,
    2, 1,
    3, 2
  )

  expect_equal(validate_aspect_windows(aspect_events, time_windows, asset_map),
               out)
})

test_that("find_good_windows_in_common filters out bad windows", {
  valid_track_windows <- dplyr::tribble(
    ~window, ~ntrains,
    1, 1,
    2, 1,
    3, 2,
    4, 1,
    5, 3
  )

  valid_aspect_windows <- dplyr::tribble(
    ~window, ~ntrains,
    1, 1,
    2, 1,
    3, 2,
    5, 2,
    6, 1
  )

  out <- dplyr::tribble(
    ~window, 1, 2, 3
  )

  expect_equal(find_good_windows_in_common(valid_track_windows,
                                           valid_aspect_windows), out)
})

test_that("calculate_time_windows correctly wraps other functions", {
  aspect_events <- dplyr::tribble(
    ~signal, ~dt, ~aspect, ~past_aspect,
    "S1", lubridate::as_datetime(101), "R", "G",
    "S1", lubridate::as_datetime(220), "Y", "R",
    "S2", lubridate::as_datetime(201), "R", "G",
    "S2", lubridate::as_datetime(320), "Y", "R",
    "S3", lubridate::as_datetime(301), "R", "G",
    "S3", lubridate::as_datetime(420), "Y", "R",
    "S4", lubridate::as_datetime(401), "R", "G",
    "S1", lubridate::as_datetime(1101), "R", "G",
    "S1", lubridate::as_datetime(1220), "Y", "R",
    "S2", lubridate::as_datetime(1201), "R", "G",
    "S2", lubridate::as_datetime(1320), "Y", "R",
    "S3", lubridate::as_datetime(1301), "R", "G",
    "S3", lubridate::as_datetime(1420), "Y", "R",
    "S4", lubridate::as_datetime(1401), "R", "G",
    "S1", lubridate::as_datetime(2101), "R", "G",
    "S1", lubridate::as_datetime(2220), "Y", "R",
    "S2", lubridate::as_datetime(2201), "R", "G",
    "S2", lubridate::as_datetime(2320), "Y", "R",
    "S3", lubridate::as_datetime(2301), "R", "G",
    "S3", lubridate::as_datetime(2420), "Y", "R",
    "S4", lubridate::as_datetime(2401), "R", "G",
    "S1", lubridate::as_datetime(2301), "R", "G",
    "S1", lubridate::as_datetime(2420), "Y", "R",
    "S2", lubridate::as_datetime(2401), "R", "G",
    "S2", lubridate::as_datetime(2520), "Y", "R",
    "S3", lubridate::as_datetime(2501), "R", "G",
    "S3", lubridate::as_datetime(2620), "Y", "R",
    "S4", lubridate::as_datetime(2601), "R", "G",
    "S1", lubridate::as_datetime(3101), "R", "G",
    "S1", lubridate::as_datetime(3220), "Y", "R",
    "S2", lubridate::as_datetime(3201), "R", "G",
    "S3", lubridate::as_datetime(3301), "R", "G",
    "S3", lubridate::as_datetime(3420), "Y", "R",
    "S4", lubridate::as_datetime(3401), "R", "G",
    "S1", lubridate::as_datetime(4101), "R", "G",
    "S1", lubridate::as_datetime(4220), "Y", "R",
    "S1", lubridate::as_datetime(4151), "R", "G",
    "S1", lubridate::as_datetime(4270), "Y", "R",
    "S2", lubridate::as_datetime(4201), "R", "G",
    "S2", lubridate::as_datetime(4320), "Y", "R",
    "S3", lubridate::as_datetime(4301), "R", "G",
    "S3", lubridate::as_datetime(4420), "Y", "R",
    "S4", lubridate::as_datetime(4401), "R", "G"
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")),
           past_aspect = factor(past_aspect, levels = c("R", "Y", "YY", "G")))

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
    "TC", lubridate::as_datetime(2610), T, "vacates",
    "TA", lubridate::as_datetime(3100), T, "enters",
    "TA", lubridate::as_datetime(3210), T, "vacates",
    "TB", lubridate::as_datetime(3200), T, "enters",
    "TB", lubridate::as_datetime(3310), T, "vacates",
    "TA", lubridate::as_datetime(4100), T, "enters",
    "TA", lubridate::as_datetime(4210), T, "vacates",
    "TB", lubridate::as_datetime(4200), T, "enters",
    "TB", lubridate::as_datetime(4310), T, "vacates",
    "TB", lubridate::as_datetime(4320), T, "enters",
    "TB", lubridate::as_datetime(4450), T, "vacates",
    "TC", lubridate::as_datetime(4300), T, "enters",
    "TC", lubridate::as_datetime(4410), T, "vacates",
    "TA", lubridate::as_datetime(5100), T, "enters",
    "TA", lubridate::as_datetime(5210), T, "vacates",
    "TA", lubridate::as_datetime(5220), T, "vacates",
    "TB", lubridate::as_datetime(5200), T, "enters",
    "TB", lubridate::as_datetime(5310), T, "vacates",
    "TC", lubridate::as_datetime(5300), T, "enters",
    "TC", lubridate::as_datetime(5410), T, "vacates",
    "TA", lubridate::as_datetime(6100), T, "enters",
    "TA", lubridate::as_datetime(6210), T, "vacates",
    "TB", lubridate::as_datetime(6200), T, "enters",
    "TB", lubridate::as_datetime(6310), T, "vacates",
    "TC", lubridate::as_datetime(6300), T, "enters",
    "TC", lubridate::as_datetime(6350), T, "enters",
    "TC", lubridate::as_datetime(6410), T, "vacates"
  )

  asset_map <- dplyr::tribble(
    ~signal, ~track, ~event,
    "S1", "TA", "enters",
    "S1", "TA", "vacates",
    "S2", "TB", "enters",
    "S2", "TB", "vacates",
    "S3", "TC", "enters",
    "S3", "TC", "vacates"
  )

  out <- dplyr::tribble(
    ~window, ~interval,
    1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC")
  )

  expect_equal(calculate_time_windows(aspect_events, track_events, asset_map),
               out)
})

test_that("filter_track_events correctly filters track events", {
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
    "TC", lubridate::as_datetime(2610), T, "vacates",
    "TA", lubridate::as_datetime(3100), T, "enters",
    "TA", lubridate::as_datetime(3210), T, "vacates",
    "TB", lubridate::as_datetime(3200), T, "enters",
    "TB", lubridate::as_datetime(3310), T, "vacates",
    "TA", lubridate::as_datetime(4100), T, "enters",
    "TA", lubridate::as_datetime(4210), T, "vacates",
    "TB", lubridate::as_datetime(4200), T, "enters",
    "TB", lubridate::as_datetime(4310), T, "vacates",
    "TB", lubridate::as_datetime(4320), T, "enters",
    "TB", lubridate::as_datetime(4450), T, "vacates",
    "TC", lubridate::as_datetime(4300), T, "enters",
    "TC", lubridate::as_datetime(4410), T, "vacates",
    "TA", lubridate::as_datetime(5100), T, "enters",
    "TA", lubridate::as_datetime(5210), T, "vacates",
    "TA", lubridate::as_datetime(5220), T, "vacates",
    "TB", lubridate::as_datetime(5200), T, "enters",
    "TB", lubridate::as_datetime(5310), T, "vacates",
    "TC", lubridate::as_datetime(5300), T, "enters",
    "TC", lubridate::as_datetime(5410), T, "vacates",
    "TA", lubridate::as_datetime(6100), T, "enters",
    "TA", lubridate::as_datetime(6210), T, "vacates",
    "TB", lubridate::as_datetime(6200), T, "enters",
    "TB", lubridate::as_datetime(6310), T, "vacates",
    "TC", lubridate::as_datetime(6300), T, "enters",
    "TC", lubridate::as_datetime(6350), T, "enters",
    "TC", lubridate::as_datetime(6410), T, "vacates"
  )

  time_windows <- dplyr::tribble(
    ~window, ~interval,
    1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC")
  )

  asset_map <- dplyr::tribble(
    ~signal, ~track, ~event,
    "S1", "TA", "enters",
    "S1", "TA", "vacates",
    "S2", "TB", "enters",
    "S2", "TB", "vacates",
    "S3", "TC", "enters",
    "S3", "TC", "vacates"
  )

  out <- dplyr::tribble(
    ~track, ~dt, ~occupied, ~event, ~window, ~interval,
    "TA", lubridate::as_datetime(100), T, "enters", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "TA", lubridate::as_datetime(210), T, "vacates", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "TB", lubridate::as_datetime(200), T, "enters", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "TB", lubridate::as_datetime(310), T, "vacates", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "TC", lubridate::as_datetime(300), T, "enters", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "TC", lubridate::as_datetime(410), T, "vacates", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "TA", lubridate::as_datetime(1100), T, "enters", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "TA", lubridate::as_datetime(1210), T, "vacates", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "TB", lubridate::as_datetime(1200), T, "enters", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "TB", lubridate::as_datetime(1310), T, "vacates", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "TC", lubridate::as_datetime(1300), T, "enters", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "TC", lubridate::as_datetime(1410), T, "vacates", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "TA", lubridate::as_datetime(2100), T, "enters", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TA", lubridate::as_datetime(2210), T, "vacates", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TB", lubridate::as_datetime(2200), T, "enters", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TB", lubridate::as_datetime(2310), T, "vacates", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TC", lubridate::as_datetime(2300), T, "enters", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TC", lubridate::as_datetime(2410), T, "vacates", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TA", lubridate::as_datetime(2300), T, "enters", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TA", lubridate::as_datetime(2410), T, "vacates", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TB", lubridate::as_datetime(2400), T, "enters", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TB", lubridate::as_datetime(2510), T, "vacates", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TC", lubridate::as_datetime(2500), T, "enters", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "TC", lubridate::as_datetime(2610), T, "vacates", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC")
  )

  expect_equal(filter_track_events(track_events, time_windows, asset_map), out)
})

test_that("filter_aspect_events correctly filters aspect events", {
  aspect_events <- dplyr::tribble(
    ~signal, ~dt, ~aspect, ~past_aspect,
    "S1", lubridate::as_datetime(101), "R", "G",
    "S1", lubridate::as_datetime(220), "Y", "R",
    "S2", lubridate::as_datetime(201), "R", "G",
    "S2", lubridate::as_datetime(320), "Y", "R",
    "S3", lubridate::as_datetime(301), "R", "G",
    "S3", lubridate::as_datetime(420), "Y", "R",
    "S4", lubridate::as_datetime(401), "R", "G",
    "S1", lubridate::as_datetime(1101), "R", "G",
    "S1", lubridate::as_datetime(1220), "Y", "R",
    "S2", lubridate::as_datetime(1201), "R", "G",
    "S2", lubridate::as_datetime(1320), "Y", "R",
    "S3", lubridate::as_datetime(1301), "R", "G",
    "S3", lubridate::as_datetime(1420), "Y", "R",
    "S4", lubridate::as_datetime(1401), "R", "G",
    "S1", lubridate::as_datetime(2101), "R", "G",
    "S1", lubridate::as_datetime(2220), "Y", "R",
    "S2", lubridate::as_datetime(2201), "R", "G",
    "S2", lubridate::as_datetime(2320), "Y", "R",
    "S3", lubridate::as_datetime(2301), "R", "G",
    "S3", lubridate::as_datetime(2420), "Y", "R",
    "S4", lubridate::as_datetime(2401), "R", "G",
    "S1", lubridate::as_datetime(2301), "R", "G",
    "S1", lubridate::as_datetime(2420), "Y", "R",
    "S2", lubridate::as_datetime(2401), "R", "G",
    "S2", lubridate::as_datetime(2520), "Y", "R",
    "S3", lubridate::as_datetime(2501), "R", "G",
    "S3", lubridate::as_datetime(2620), "Y", "R",
    "S4", lubridate::as_datetime(2601), "R", "G",
    "S1", lubridate::as_datetime(3101), "R", "G",
    "S1", lubridate::as_datetime(3220), "Y", "R",
    "S2", lubridate::as_datetime(3201), "R", "G",
    "S3", lubridate::as_datetime(3301), "R", "G",
    "S3", lubridate::as_datetime(3420), "Y", "R",
    "S4", lubridate::as_datetime(3401), "R", "G",
    "S1", lubridate::as_datetime(4101), "R", "G",
    "S1", lubridate::as_datetime(4220), "Y", "R",
    "S1", lubridate::as_datetime(4151), "R", "G",
    "S1", lubridate::as_datetime(4270), "Y", "R",
    "S2", lubridate::as_datetime(4201), "R", "G",
    "S2", lubridate::as_datetime(4320), "Y", "R",
    "S3", lubridate::as_datetime(4301), "R", "G",
    "S3", lubridate::as_datetime(4420), "Y", "R",
    "S4", lubridate::as_datetime(4401), "R", "G"
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")),
           past_aspect = factor(past_aspect, levels = c("R", "Y", "YY", "G")))

  time_windows <- dplyr::tribble(
    ~window, ~interval,
    1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC")
  )

  asset_map <- dplyr::tribble(
    ~signal, ~track, ~event,
    "S1", "TA", "enters",
    "S1", "TA", "vacates",
    "S2", "TB", "enters",
    "S2", "TB", "vacates",
    "S3", "TC", "enters",
    "S3", "TC", "vacates"
  )

  out <- dplyr::tribble(
    ~signal, ~dt, ~aspect, ~past_aspect, ~window, ~interval,
    "S1", lubridate::as_datetime(101), "R", "G", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "S1", lubridate::as_datetime(220), "Y", "R", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "S2", lubridate::as_datetime(201), "R", "G", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "S2", lubridate::as_datetime(320), "Y", "R", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "S3", lubridate::as_datetime(301), "R", "G", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "S3", lubridate::as_datetime(420), "Y", "R", 1, lubridate::interval(start = -85990, end = 410, tzone = "UTC"),
    "S1", lubridate::as_datetime(1101), "R", "G", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "S1", lubridate::as_datetime(1220), "Y", "R", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "S2", lubridate::as_datetime(1201), "R", "G", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "S2", lubridate::as_datetime(1320), "Y", "R", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "S3", lubridate::as_datetime(1301), "R", "G", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "S3", lubridate::as_datetime(1420), "Y", "R", 2, lubridate::interval(start = 1100, end = 1410, tzone = "UTC"),
    "S1", lubridate::as_datetime(2101), "R", "G", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S1", lubridate::as_datetime(2220), "Y", "R", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S2", lubridate::as_datetime(2201), "R", "G", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S2", lubridate::as_datetime(2320), "Y", "R", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S3", lubridate::as_datetime(2301), "R", "G", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S3", lubridate::as_datetime(2420), "Y", "R", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S1", lubridate::as_datetime(2301), "R", "G", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S1", lubridate::as_datetime(2420), "Y", "R", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S2", lubridate::as_datetime(2401), "R", "G", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S2", lubridate::as_datetime(2520), "Y", "R", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S3", lubridate::as_datetime(2501), "R", "G", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
    "S3", lubridate::as_datetime(2620), "Y", "R", 3, lubridate::interval(start = 2100, end = 2610, tzone = "UTC"),
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")),
           past_aspect = factor(past_aspect, levels = c("R", "Y", "YY", "G")))

  expect_equal(filter_aspect_events(aspect_events, time_windows, asset_map), out)
})

