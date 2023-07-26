test_that("get_map() works", {
  set_map(asset_map())

  expect_equal(get_map(), asset_map())
})

test_that("set_map() works", {
  map <- asset_map()

  set_map(map)
  expect_equal(get_map(), map)
})


test_that("split_signal_track_events() splits signal and track events", {
  raw_events <- centrix(tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1", lubridate::as_datetime(100), "DN to UP", 1L,
    "T1", lubridate::as_datetime(200), "DN to UP", 1L
  ))

  out <- raw_events %>%
    dplyr::mutate(is_track = stringr::str_starts(asset, "T")) %>%
    dplyr::group_by(is_track) %>%
    dplyr::group_split(.keep = F)
  out <- list(as_centrix(out[[1]]), as_centrix(out[[2]]))

  expect_equal(split_signal_track_events(raw_events), out)
})

test_that("preprocess_signal_events() successfully converts to signal/aspect", {
  rse <- centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP", 1L,
    "S2 HGE", lubridate::as_datetime(200), "DN to UP", 1L,
    "S3 DGE", lubridate::as_datetime(300), "DN to UP", 1L,
    "S4 HHGE", lubridate::as_datetime(400), "DN to UP", 1L
  ))

  map <- asset_map(data.frame(
    signal = signal(c("S1", "S2", "S3", "S4")),
    berth = c("A", "A", "A", "A"),
    track = track(c("TA", "TA", "TA", "TA")),
    event = c("enters", "enters", "enters", "enters"),
    geo = c("a", "a", "a", "a")
  ))
  set_map(map)

  out <- aspect_event(dplyr::tibble(data.frame(
    signal = signal(c("S1", "S2", "S3", "S4")),
    aspect = aspect4(
      c("R", "Y", "G", "YY")
    ),
    past_aspect = aspect4(
      c(NA_character_, NA_character_, NA_character_, NA_character_)
    ),
    dt = c(lubridate::as_datetime(100), lubridate::as_datetime(200),
           lubridate::as_datetime(300), lubridate::as_datetime(400)),
    period = c(1L, 1L, 1L, 1L)
  )))

  expect_equal(preprocess_signal_events(rse), out)
})


test_that("preprocess_track_events() correctly processes track data", {
  dt <- lubridate::as_datetime(100)
  rte <- centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "TA-1", dt, "DN to UP", 1L,
    "TA-2", dt, "UP to DN", 1L,
    "TB-1", dt, "DN to UP", 1L,
    "TB-2", dt, "UP to DN", 1L,
    "TC", dt, "DN to UP", 1L,
    "TD", dt, "UP to DN", 1L
  ))


  map <- asset_map(data.frame(
    signal = signal(c("S1", "S1", "S1")),
    berth = c("A", "A", "A"),
    track = track(c("TA-1", "TB-1", "TC")),
    event = c("enters", "enters", "enters"),
    geo = c("a", "a", "a")
  ))
  set_map(map)

  out <- track_event(dplyr::tribble(
    ~period, ~track, ~dt, ~occupied, ~event,
    1, "TA-1", dt, F, "vacates",
    1, "TB-1", dt, F, "vacates",
    1, "TC", dt, F, "vacates",
  ) %>%
    mutate(track = as_track(track),
           period = as.integer(period)) %>%
    select(track, occupied, dt, event, period))

  expect_equal(preprocess_track_events(rte), out)
})
