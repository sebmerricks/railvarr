test_that("find_intervals correctly identifies intervals with no events", {
  track_events <- dplyr::tribble(
    ~period, ~track, ~dt, ~event,
    3, "TA", lubridate::as_datetime(100), "enters",
    3, "TA", lubridate::as_datetime(210), "vacates",
    3, "TB", lubridate::as_datetime(200), "enters",
    3, "TB", lubridate::as_datetime(310), "vacates",
    3, "TA", lubridate::as_datetime(400), "enters",
    3, "TA", lubridate::as_datetime(510), "vacates",
    3, "TB", lubridate::as_datetime(500), "enters",
    3, "TB", lubridate::as_datetime(610), "vacates"
  )

  map <- dplyr::tibble(
    signal = c("S1", "S2"),
    berth = c("A", "B"),
    track = c("TA", "TB"),
    event = c("enters", "enters"),
    geo = c("", "")
  )
  set_asset_mapping(map)

  intervals <- dplyr::tribble(
    ~period, ~interval,
    3, lubridate::interval(start = lubridate::as_datetime(311),
                           end = lubridate::as_datetime(399))
  )

  expect_equal(find_intervals(track_events), intervals)
})

test_that("find_time_windows correctly identifies time windows", {
  intervals <- dplyr::tribble(
    ~period, ~interval,
    3, lubridate::interval(start = lubridate::as_datetime(100311),
                           end = lubridate::as_datetime(100399))
  )

  track_events <- dplyr::tribble(
    ~period, ~track, ~dt, ~event,
    3, "TA", lubridate::as_datetime(100100), "enters",
    3, "TA", lubridate::as_datetime(100210), "vacates",
    3, "TB", lubridate::as_datetime(100200), "enters",
    3, "TB", lubridate::as_datetime(100310), "vacates",
    3, "TA", lubridate::as_datetime(100400), "enters",
    3, "TA", lubridate::as_datetime(100510), "vacates",
    3, "TB", lubridate::as_datetime(100500), "enters",
    3, "TB", lubridate::as_datetime(100610), "vacates"
  )

  windows <- dplyr::tribble(
    ~period, ~window, ~interval,
    3, 1, lubridate::interval(start = lubridate::as_datetime(13910),
                              end = lubridate::as_datetime(100310)),
    3, 2, lubridate::interval(start = lubridate::as_datetime(100400),
                              end = lubridate::as_datetime(186800))
  )

  expect_equal(find_time_windows(intervals, track_events), windows)
})

test_that("window_track_events correctly windows track events", {
  track_events <- dplyr::tribble(
    ~period, ~track, ~dt, ~event,
    3, "TA", lubridate::as_datetime(100100), "enters",
    3, "TA", lubridate::as_datetime(100210), "vacates",
    3, "TB", lubridate::as_datetime(100200), "enters",
    3, "TB", lubridate::as_datetime(100310), "vacates",
    3, "TA", lubridate::as_datetime(100400), "enters",
    3, "TA", lubridate::as_datetime(100510), "vacates",
    3, "TB", lubridate::as_datetime(100500), "enters",
    3, "TB", lubridate::as_datetime(100610), "vacates"
  )

  windows <- dplyr::tribble(
    ~period, ~window, ~interval,
    3, 1, lubridate::interval(start = lubridate::as_datetime(13910),
                              end = lubridate::as_datetime(100310)),
    3, 2, lubridate::interval(start = lubridate::as_datetime(100400),
                              end = lubridate::as_datetime(186800))
  )

  track_events_windowed <- dplyr::tribble(
    ~period, ~track, ~dt, ~event, ~window,
    3, "TA", lubridate::as_datetime(100100), "enters", 1,
    3, "TA", lubridate::as_datetime(100210), "vacates", 1,
    3, "TB", lubridate::as_datetime(100200), "enters", 1,
    3, "TB", lubridate::as_datetime(100310), "vacates", 1,
    3, "TA", lubridate::as_datetime(100400), "enters", 2,
    3, "TA", lubridate::as_datetime(100510), "vacates", 2,
    3, "TB", lubridate::as_datetime(100500), "enters", 2,
    3, "TB", lubridate::as_datetime(100610), "vacates", 2
  ) %>%
    select(period, window, track, dt, event)

  expect_equal(window_track_events(track_events, windows),
               track_events_windowed)
})

test_that("window_aspect_events correctly windows aspect events", {
  aspect_events <- dplyr::tribble(
    ~period, ~signal, ~dt, ~aspect, ~past_aspect,
    3, "S1", lubridate::as_datetime(100101), "R", NA,
    3, "S1", lubridate::as_datetime(100220), "Y", "R",
    3, "S2", lubridate::as_datetime(100201), "R", NA,
    3, "S2", lubridate::as_datetime(100320), "Y", "R",
    3, "S1", lubridate::as_datetime(100401), "R", NA,
    3, "S1", lubridate::as_datetime(100520), "Y", "R",
    3, "S2", lubridate::as_datetime(100501), "R", NA,
    3, "S2", lubridate::as_datetime(100620), "Y", "R"
  )

  windows <- dplyr::tribble(
    ~period, ~window, ~interval,
    3, 1, lubridate::interval(start = lubridate::as_datetime(13910),
                              end = lubridate::as_datetime(100310)),
    3, 2, lubridate::interval(start = lubridate::as_datetime(100400),
                              end = lubridate::as_datetime(186800))
  )

  aspect_events_windowed <- dplyr::tribble(
    ~period, ~signal, ~dt, ~aspect, ~past_aspect, ~window,
    3, "S1", lubridate::as_datetime(100101), "R", NA, 1,
    3, "S1", lubridate::as_datetime(100220), "Y", "R", 1,
    3, "S2", lubridate::as_datetime(100201), "R", NA, 1,
    3, "S2", lubridate::as_datetime(100320), "Y", "R", 1,
    3, "S1", lubridate::as_datetime(100401), "R", NA, 2,
    3, "S1", lubridate::as_datetime(100520), "Y", "R", 2,
    3, "S2", lubridate::as_datetime(100501), "R", NA, 2,
    3, "S2", lubridate::as_datetime(100620), "Y", "R", 2
  ) %>%
    select(window, signal, dt, aspect, past_aspect)

  expect_equal(window_aspect_events(aspect_events, windows),
               aspect_events_windowed)
})

test_that("filter_red_events correctly identifies red events", {
  map <- dplyr::tibble(
    signal = c("S1", "S1", "S2", "S2"),
    berth = c("A", "A", "B", "B"),
    track = c("TA", "TA", "TB", "TB"),
    event = c("enters", "vacates", "enters", "vacates"),
    geo = c("", "", "", "")
  )
  set_asset_mapping(map)

  aspect_events_windowed <- dplyr::tribble(
    ~period, ~signal, ~dt, ~aspect, ~past_aspect, ~window,
    3, "S1", lubridate::as_datetime(100101), "R", NA, 1,
    3, "S1", lubridate::as_datetime(100220), "Y", "R", 1,
    3, "S2", lubridate::as_datetime(100201), "R", NA, 1,
    3, "S2", lubridate::as_datetime(100320), "Y", "R", 1,
    3, "S1", lubridate::as_datetime(100401), "R", NA, 2,
    3, "S1", lubridate::as_datetime(100520), "Y", "R", 2,
    3, "S2", lubridate::as_datetime(100501), "R", NA, 2,
    3, "S2", lubridate::as_datetime(100620), "Y", "R", 2
  ) %>%
    select(window, signal, dt, aspect, past_aspect)

  red_events_windowed <- dplyr::tribble(
    ~window, ~signal, ~dt, ~aspect, ~past_aspect, ~event,
    1, "S1", lubridate::as_datetime(100101), "R", NA, "red_on",
    1, "S1", lubridate::as_datetime(100220), "Y", "R", "red_off",
    1, "S2", lubridate::as_datetime(100201), "R", NA, "red_on",
    1, "S2", lubridate::as_datetime(100320), "Y", "R", "red_off",
    2, "S1", lubridate::as_datetime(100401), "R", NA, "red_on",
    2, "S1", lubridate::as_datetime(100520), "Y", "R", "red_off",
    2, "S2", lubridate::as_datetime(100501), "R", NA, "red_on",
    2, "S2", lubridate::as_datetime(100620), "Y", "R", "red_off"
  )

  expect_equal(filter_red_events(aspect_events_windowed),
               red_events_windowed)
})

test_that("find_good_windows", {

})
