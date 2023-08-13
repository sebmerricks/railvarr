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
    "TABC", lubridate::as_datetime(100), "DN to UP",
    "TABC-1", lubridate::as_datetime(100), "DN to UP",
  )

  asset_map <- dplyr::tribble(
    ~signal, ~track, ~berth, ~event,
    "S1", "TAAA", "A", "enters",
    "S2", "TAAB-1", "B", "enters",
    "S3", "TAAB-2", "C", "enters"
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
  raw_track_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 HGE", lubridate::as_datetime(50), "DN to UP", # output
    "S1 RGE", lubridate::as_datetime(100), "DN to UP", # output
    "S1 I", lubridate::as_datetime(100), "DN to UP", # not in state_mapping
    "S1 RGE",  lubridate::as_datetime(100), "UP to DN", # incorrect transition
    "S2 RGE", lubridate::as_datetime(100), "DN to UP", # output
    "S3 RGE", lubridate::as_datetime(100), "DN to UP", # output
    "S3 RGEK", lubridate::as_datetime(100), "DN to UP", # not in state_mapping
    "S4 RGE", lubridate::as_datetime(100), "DN to UP", # not in asset_map
    "TAAB-2 TR", lubridate::as_datetime(300), "UP to DN",
    "TAAB-2 1R" , lubridate::as_datetime(410), "DN to UP",
    "TAAA 453", lubridate::as_datetime(100), "UP to DN",
    "TAAA bf", lubridate::as_datetime(210), "DN to UP",
    "TAAB-1 RGE", lubridate::as_datetime(200), "UP to DN",
    "TAAB-1 gdf", lubridate::as_datetime(310), "DN to UP",
    "TAAC TR", lubridate::as_datetime(400), "UP to DN",
    "TAAC TR", lubridate::as_datetime(510), "DN to UP",
  )

  asset_map <- dplyr::tribble(
    ~signal, ~track, ~berth, ~event,
    "S1", "TAAA", "A", "enters",
    "S2", "TAAB-1", "B", "enters",
    "S3", "TAAB-2", "C", "enters"
  )

  out <- dplyr::tribble(
    ~track, ~dt, ~occupied, ~event,
    "TAAA", lubridate::as_datetime(100), T, "enters",
    "TAAA", lubridate::as_datetime(210), F, "vacates",
    "TAAB-1", lubridate::as_datetime(200), T, "enters",
    "TAAB-1", lubridate::as_datetime(310), F, "vacates",
    "TAAB-2", lubridate::as_datetime(300), T, "enters",
    "TAAB-2", lubridate::as_datetime(410), F, "vacates"
  )

  expect_equal(preprocess_track_events(raw_track_events, asset_map), out)
})
