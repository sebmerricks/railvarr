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
    3, 1, lubridate::interval(start = lubridate::as_datetime(100400),
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

test_that("find_valid_track_events filters out invalid track events", {
  track_events_windowed <- dplyr::tribble(
    ~period, ~track, ~dt, ~event, ~window,
    3, "TA", lubridate::as_datetime(100100), "enters", 1,
    3, "TA", lubridate::as_datetime(100210), "vacates", 1,
    3, "TB", lubridate::as_datetime(100200), "enters", 1,
    3, "TB", lubridate::as_datetime(100310), "vacates", 1,
    3, "TA", lubridate::as_datetime(100370), "vacates", 1,
    3, "TA", lubridate::as_datetime(100400), "enters", 2,
    3, "TA", lubridate::as_datetime(100510), "vacates", 2,
    3, "TB", lubridate::as_datetime(100500), "enters", 2,
    3, "TB", lubridate::as_datetime(100610), "vacates", 2
  ) %>%
    select(period, window, track, dt, event)

  valid_events <- dplyr::tribble(
    ~period, ~window, ~ntrains_track, ~ntracks, ~distinct_track_counts,
    ~any_different_enters_vacates, ~any_not_interlaced,
    3, 2, 1, 2, 1, F, F
  )

  expect_equal(find_valid_track_events(track_events_windowed), valid_events)
})

test_that("find_valid_aspect_events filters out invalid aspect events", {
  red_events_windowed <- dplyr::tribble(
    ~window, ~signal, ~dt, ~aspect, ~past_aspect, ~event,
    1, "S1", lubridate::as_datetime(100101), "R", NA, "red_on",
    1, "S1", lubridate::as_datetime(100220), "Y", "R", "red_off",
    1, "S2", lubridate::as_datetime(100201), "R", NA, "red_on",
    1, "S2", lubridate::as_datetime(100320), "Y", "R", "red_off",
    1, "S2", lubridate::as_datetime(100330), "R", "Y", "red_on",
    2, "S1", lubridate::as_datetime(100401), "R", NA, "red_on",
    2, "S1", lubridate::as_datetime(100520), "Y", "R", "red_off",
    2, "S2", lubridate::as_datetime(100501), "R", NA, "red_on",
    2, "S2", lubridate::as_datetime(100620), "Y", "R", "red_off"
  )

  valid_events <- dplyr::tribble(
    ~window, ~ntrains,
    2, 1
  )

  expect_equal(find_valid_aspect_events(red_events_windowed), valid_events)
})

test_that("find_good_windows correctly identifies good windows", {
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

  good_windows <- dplyr::tribble(
    ~period, ~window, ~ntrains_track, ~ntracks, ~distinct_track_counts,
    ~any_different_enters_vacates, ~any_not_interlaced, ~ntrains,
    3, 1, 1, 2, 1, F, F, 1,
    3, 2, 1, 2, 1, F, F, 1
  )

  expect_equal(find_good_windows(track_events_windowed, red_events_windowed),
               good_windows)
})

test_that("validate_track_events", {
  track_events_windowed <- dplyr::tribble(
    ~period, ~track, ~dt, ~event, ~window,
    3, "TA", lubridate::as_datetime(100100), "enters", 1,
    3, "TA", lubridate::as_datetime(100210), "vacates", 1,
    3, "TB", lubridate::as_datetime(100200), "enters", 1,
    3, "TB", lubridate::as_datetime(100310), "vacates", 1,
    3, "TA", lubridate::as_datetime(100400), "enters", 2,
    3, "TA", lubridate::as_datetime(100510), "vacates", 2,
    3, "TB", lubridate::as_datetime(100500), "enters", 2,
    3, "TB", lubridate::as_datetime(100610), "vacates", 2,
    3, "TA", lubridate::as_datetime(100610), "vacates", 3
  ) %>%
    select(period, window, track, dt, event)

  good_windows <- dplyr::tribble(
    ~period, ~window,
    3, 1,
    3, 2
  )

  expect_equal(validate_track_events(track_events_windowed, good_windows),
               track_events_windowed %>% filter(window != 3))
})

test_that("validate_red_events", {
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

  good_windows <- dplyr::tribble(
    ~period, ~window,
    3, 1,
    3, 2
  )

  valid_red_events <- dplyr::tribble(
  ~window, ~signal, ~window_train_id, ~t_red_on, ~t_red_off, ~aspect,
  1, "S1", 1, lubridate::as_datetime(100101), lubridate::as_datetime(100220), NA_character_,
  1, "S2", 1, lubridate::as_datetime(100201), lubridate::as_datetime(100320), NA_character_,
  2, "S1", 1, lubridate::as_datetime(100401), lubridate::as_datetime(100520), NA_character_,
  2, "S2", 1, lubridate::as_datetime(100501), lubridate::as_datetime(100620), NA_character_
  ) %>%
    mutate(TSAR = lubridate::as.duration(t_red_off - t_red_on))

  expect_equal(validate_red_events(red_events_windowed, good_windows),
               valid_red_events)
})

test_that("combine_track_aspect_events", {
  valid_track_events <- dplyr::tribble(
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

  valid_red_events <- dplyr::tribble(
    ~window, ~signal, ~window_train_id, ~t_red_on, ~t_red_off, ~aspect,
    1, "S1", 1, lubridate::as_datetime(100101), lubridate::as_datetime(100220), NA_character_,
    1, "S2", 1, lubridate::as_datetime(100201), lubridate::as_datetime(100320), NA_character_,
    2, "S1", 1, lubridate::as_datetime(100401), lubridate::as_datetime(100520), NA_character_,
    2, "S2", 1, lubridate::as_datetime(100501), lubridate::as_datetime(100620), NA_character_
  ) %>%
    mutate(TSAR = lubridate::as.duration(t_red_off - t_red_on))

  map <- dplyr::tibble(
    signal = c("S1", "S1", "S2", "S2"),
    berth = c("A", "A", "B", "B"),
    track = c("TA", "TA", "TB", "TB"),
    event = c("enters", "vacates", "enters", "vacates"),
    geo = c("1", "1", "2", "2")
  )
  set_asset_mapping(map)

  berth_events <- dplyr::tibble(
    signal = c("S1", "S2", "S1", "S2"),
    berth = c("A", "B", "A", "B"),
    train_id = c(1, 1, 2, 2),
    aspect = c(NA_character_, NA_character_, NA_character_, NA_character_),
    t_enters = c(lubridate::as_datetime(100100), lubridate::as_datetime(100200),
                 lubridate::as_datetime(100400), lubridate::as_datetime(100500)),
    t_red_on = c(lubridate::as_datetime(100101), lubridate::as_datetime(100201),
                 lubridate::as_datetime(100401), lubridate::as_datetime(100501)),
    t_enters_next = c(lubridate::as_datetime(100200), NA,
                      lubridate::as_datetime(100500), NA),
    t_vacates = c(lubridate::as_datetime(100210), lubridate::as_datetime(100310),
                  lubridate::as_datetime(100510), lubridate::as_datetime(100610)),
    t_red_off = c(lubridate::as_datetime(100220), lubridate::as_datetime(100320),
                  lubridate::as_datetime(100520), lubridate::as_datetime(100620))
  ) %>%
    mutate(TSAR = lubridate::as.duration(t_red_off - t_red_on),
           T_onset = lubridate::as.duration(t_red_on - t_enters),
           T_clear = lubridate::as.duration(t_vacates - t_enters),
           T_offset = lubridate::as.duration(t_red_off - t_vacates),
           T_travel = lubridate::as.duration(t_enters_next - t_enters),
           T_coach = lubridate::as.duration(t_vacates - t_enters_next)) %>%
    mutate(across(TSAR:last_col(), as.double))

  expect_equal(combine_track_aspect_events(valid_track_events, valid_red_events),
               berth_events)
})
