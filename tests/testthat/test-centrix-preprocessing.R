# Split Signal Track Events ----------------------------------------------------

test_that("split_signal_track_events() splits signal and track events", {
  raw_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S123 RGE", lubridate::as_datetime(100), "DN to UP",
    "S123", lubridate::as_datetime(100), "DN to UP",
    "s123", lubridate::as_datetime(100), "DN to UP",
    "sabc", lubridate::as_datetime(100), "DN to UP",
    "TABC", lubridate::as_datetime(100), "DN to UP",
    "TABC-1", lubridate::as_datetime(100), "DN to UP",
    "tABC", lubridate::as_datetime(100), "DN to UP",
    "tabc", lubridate::as_datetime(100), "DN to UP",
    "x123", lubridate::as_datetime(100), "DN to UP"
  )

  out <- vctrs::list_of(
    dplyr::tribble(
      ~asset, ~dt, ~transition,
      "S123 RGE", lubridate::as_datetime(100), "DN to UP",
      "S123", lubridate::as_datetime(100), "DN to UP"
    ),
    dplyr::tribble(
      ~asset, ~dt, ~transition,
      "TABC", lubridate::as_datetime(100), "DN to UP",
      "TABC-1", lubridate::as_datetime(100), "DN to UP"
    )
  )

  expect_equal(split_signal_track_events(raw_events), out)
})

# Pre-process Signal Events ----------------------------------------------------

test_that("preprocess_signal_events() successfully converts to signal/aspect", {
  raw_signal_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 HGE", lubridate::as_datetime(50), "DN to UP", # output
    "S1 RGE", lubridate::as_datetime(100), "DN to UP", # output
    "S1 I", lubridate::as_datetime(100), "DN to UP", # not in state_mapping
    "S1 RGE",  lubridate::as_datetime(100), "UP to DN", # incorrect transition
    "S2 RGE", lubridate::as_datetime(100), "DN to UP", # output
    "S3 RGE", lubridate::as_datetime(100), "DN to UP", # output
    "S3 RGEK", lubridate::as_datetime(100), "DN to UP", # not in state_mapping
    "S4 RGE", lubridate::as_datetime(100), "DN to UP", # not in asset_map
  )

  asset_map <- dplyr::tribble(
    ~signal,
    "S1",
    "S2",
    "S3"
  )

  state_mapping <- dplyr::tribble(
    ~state, ~aspect,
    "RGE", factor("R", levels = c("R", "Y")),
    "HGE", factor("Y", levels = c("R", "Y"))
  )

  out <- dplyr::tribble(
    ~signal, ~dt, ~aspect, ~past_aspect,
    "S1", lubridate::as_datetime(50), factor("Y", levels = c("R", "Y")), factor(NA_character_, levels = c("R", "Y")),
    "S1", lubridate::as_datetime(100), factor("R", levels = c("R", "Y")), factor("Y", levels = c("R", "Y")),
    "S2", lubridate::as_datetime(100), factor("R", levels = c("R", "Y")), factor(NA_character_, levels = c("R", "Y")),
    "S3", lubridate::as_datetime(100), factor("R", levels = c("R", "Y")), factor(NA_character_, levels = c("R", "Y"))
  )

  expect_equal(
    preprocess_signal_events(raw_signal_events, asset_map, state_mapping), out)
})

# Pre-process Track Events -----------------------------------------------------

test_that("preprocess_track_events() correctly processes track data", {
  dt <- lubridate::as_datetime(100)
  raw_track_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "TA-1", dt, "DN to UP",
    "TA-2", dt, "UP to DN",
    "TB-1", dt, "DN to UP",
    "TB-2", dt, "UP to DN",
    "TC", dt, "DN to UP",
    "TD", dt, "UP to DN",
  )

  asset_map <- data.frame(
    signal = c("S1", "S2", "S3"),
    berth = c("", "", ""),
    track = c("TA-1", "TB-1", "TC"),
    event = c("vacates", "vacates", "vacates")
  )

  out <- dplyr::tribble(
    ~track, ~dt, ~occupied, ~event,
    "TA-1", dt, F, "vacates",
    "TB-1", dt, F, "vacates",
    "TC", dt, F, "vacates"
  )

  expect_equal(preprocess_track_events(raw_track_events, asset_map), out)
})
