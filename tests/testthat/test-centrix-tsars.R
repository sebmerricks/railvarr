test_that("calculate_tsars successfully calculates TSAR and friends", {
  track_events <- dplyr::tribble(
    ~track, ~dt, ~occupied, ~event, ~window, ~interval,
    "TA", lubridate::as_datetime(100), T, "enters", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(210), T, "vacates", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(200), T, "enters", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(310), T, "vacates", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(300), T, "enters", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(410), T, "vacates", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(1100), T, "enters", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(1210), T, "vacates", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(1200), T, "enters", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(1310), T, "vacates", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(1300), T, "enters", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(1410), T, "vacates", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(2100), T, "enters", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(2210), T, "vacates", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(2200), T, "enters", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(2310), T, "vacates", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(2300), T, "enters", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(2410), T, "vacates", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(2300), T, "enters", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TA", lubridate::as_datetime(2410), T, "vacates", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(2400), T, "enters", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TB", lubridate::as_datetime(2510), T, "vacates", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(2500), T, "enters", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "TC", lubridate::as_datetime(2610), T, "vacates", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC")
  )

  aspect_events <- dplyr::tribble(
    ~signal, ~dt, ~aspect, ~past_aspect, ~window, ~interval,
    "S1", lubridate::as_datetime(101), "R", "G", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(220), "Y", "R", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(201), "R", "G", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(320), "Y", "R", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(301), "R", "G", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(420), "Y", "R", 1, lubridate::interval(start = as.POSIXct(-85990, origin = "1970-01-01"), end = as.POSIXct(410, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(1101), "R", "G", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(1220), "Y", "R", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(1201), "R", "G", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(1320), "Y", "R", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(1301), "R", "G", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(1420), "Y", "R", 2, lubridate::interval(start = as.POSIXct(1100, origin = "1970-01-01"), end = as.POSIXct(1410, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(2101), "R", "G", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(2220), "Y", "R", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(2201), "R", "G", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(2320), "Y", "R", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(2301), "R", "G", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(2420), "Y", "R", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(2301), "R", "G", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S1", lubridate::as_datetime(2420), "Y", "R", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(2401), "R", "G", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S2", lubridate::as_datetime(2520), "Y", "R", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(2501), "R", "G", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC"),
    "S3", lubridate::as_datetime(2620), "Y", "R", 3, lubridate::interval(start = as.POSIXct(2100, origin = "1970-01-01"), end = as.POSIXct(2610, origin = "1970-01-01"), tzone = "UTC")
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")),
           past_aspect = factor(past_aspect, levels = c("R", "Y", "YY", "G")))

  asset_map <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", "TA", "enters",
    "S1", "A", "TA", "vacates",
    "S2", "B", "TB", "enters",
    "S2", "B", "TB", "vacates",
    "S3", "C", "TC", "enters",
    "S3", "C", "TC", "vacates"
  )

  berth_events <- dplyr::tribble(
    ~signal, ~berth, ~train_id, ~aspect, ~t_enters, ~t_red_on, ~t_enters_next, ~t_vacates, ~t_red_off, ~TSAR, ~T_onset, ~T_clear, ~T_offset, ~T_travel, ~T_coach,
    "S1", "A", 1, "G", 100, 101, 200, 210, 220, 119, 1, 110, 10, 100, 10,
    "S2", "B", 1, "G", 200, 201, 300, 310, 320, 119, 1, 110, 10, 100, 10,
    "S3", "C", 1, "G", 300, 301, NA, 410, 420, 119, 1, 110, 10, NA, NA,
    "S1", "A", 2, "G", 1100, 1101, 1200, 1210, 1220, 119, 1, 110, 10, 100, 10,
    "S2", "B", 2, "G", 1200, 1201, 1300, 1310, 1320, 119, 1, 110, 10, 100, 10,
    "S3", "C", 2, "G", 1300, 1301, NA, 1410, 1420, 119, 1, 110, 10, NA, NA,
    "S1", "A", 3, "G", 2100, 2101, 2200, 2210, 2220, 119, 1, 110, 10, 100, 10,
    "S2", "B", 3, "G", 2200, 2201, 2300, 2310, 2320, 119, 1, 110, 10, 100, 10,
    "S3", "C", 3, "G", 2300, 2301, NA, 2410, 2420, 119, 1, 110, 10, NA, NA,
    "S1", "A", 4, "G", 2300, 2301, 2400, 2410, 2420, 119, 1, 110, 10, 100, 10,
    "S2", "B", 4, "G", 2400, 2401, 2500, 2510, 2520, 119, 1, 110, 10, 100, 10,
    "S3", "C", 4, "G", 2500, 2501, NA, 2610, 2620, 119, 1, 110, 10, NA, NA
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G"))) %>%
    mutate(across(starts_with("t", ignore.case=FALSE), lubridate::as_datetime),
           across(starts_with("T", ignore.case=FALSE), lubridate::as.duration),
           across(starts_with("T", ignore.case=FALSE), as.double),
           train_id = as.numeric(train_id))

  expect_equal(calculate_tsars(track_events, aspect_events, asset_map),
               berth_events)
})
