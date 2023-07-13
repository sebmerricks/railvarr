test_that("get_map() works", {
  expect_equal(get_map(), data.frame(
    signal = character(),
    berth = character(),
    track = character(),
    event = character()
  ))
})

test_that("set_map() works", {
  map <- data.frame(
    signal = character(),
    berth = character(),
    track = character(),
    event = character()
  )

  set_map(map)
  expect_equal(get_map(), map)
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


test_that("preprocess_signal_events() successfully converts to signal/aspect", {
  rse <- dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP", 1,
    "S2 HGE", lubridate::as_datetime(200), "DN to UP", 1,
    "S3 DGE", lubridate::as_datetime(300), "DN to UP", 1,
    "S4 HHGE", lubridate::as_datetime(400), "DN to UP", 1
  )

  map <- data.frame(
    signal = c("S1", "S2", "S3", "S4"),
    berth = c("", "", "", ""),
    track = c("", "", "", ""),
    event = c("", "", "", "")
  )
  set_map(map)

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

  expect_equal(preprocess_signal_events(rse), out)
})


test_that("preprocess_track_events() correctly processes track data", {
  dt <- lubridate::as_datetime(100)
  rte <- dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "TA-1", dt, "DN to UP", 1,
    "TA-2", dt, "UP to DN", 1,
    "TB-1", dt, "DN to UP", 1,
    "TB-2", dt, "UP to DN", 1,
    "TC", dt, "DN to UP", 1,
    "TD", dt, "UP to DN", 1
  )


  map <- data.frame(
    signal = c("", "", ""),
    berth = c("", "", ""),
    track = c("TA-1", "TB-1", "TC"),
    event = c("", "", "")
  )
  set_map(map)

  out <- dplyr::tribble(
    ~period, ~track, ~dt, ~occupied, ~event, ~date,
    1, "TA-1", dt, F, "vacates", lubridate::as_date(dt),
    1, "TB-1", dt, F, "vacates", lubridate::as_date(dt),
    1, "TC", dt, F, "vacates", lubridate::as_date(dt)
  )

  expect_equal(preprocess_track_events(rte), out)
})
