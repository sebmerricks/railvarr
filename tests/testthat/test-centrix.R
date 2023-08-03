# Validation -------------------------------------------------------------------

# validate_centrix

test_that("validate_centrix errors with appropriate messages for incorrect data structure", {
  bad_names <- dplyr::tribble(
    ~signal, ~dt, ~transition,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP",
    "S2 HGE", lubridate::as_datetime(200), "DN to UP"
  )
  expect_snapshot(validate_centrix(bad_names), error = TRUE)

  bad_asset_type <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    1, lubridate::as_datetime(100), "DN to UP",
    2, lubridate::as_datetime(200), "DN to UP"
  )
  expect_snapshot(validate_centrix(bad_asset_type), error = TRUE)

  bad_asset_structure <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP",
    "TA-1", lubridate::as_datetime(200), "DN to UP",
    "T1-99", lubridate::as_datetime(200), "DN to UP"
  )
  expect_snapshot(validate_centrix(bad_asset_structure), error = TRUE)

  bad_dt_type <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 RGE", "2023-08-03 10:55:00", "DN to UP",
    "S2 HGE", "2023-08-03 10:55:30", "DN to UP"
  )
  expect_snapshot(validate_centrix(bad_dt_type), error = TRUE)

  bad_transition_type <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 RGE", lubridate::as_datetime(100), TRUE,
    "S2 HGE", lubridate::as_datetime(200), FALSE
  )
  expect_snapshot(validate_centrix(bad_transition_type), error = TRUE)

  bad_transition_structure <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 RGE", lubridate::as_datetime(100), "enters",
    "S2 HGE", lubridate::as_datetime(200), "vacates"
  )
  expect_snapshot(validate_centrix(bad_transition_structure), error = TRUE)
})

test_that("validate_centrix does nothing with correct data", {
  good_centrix <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP",
    "S2 HGE", lubridate::as_datetime(200), "DN to UP",
    "TA-1", lubridate::as_datetime(100), "DN to UP",
    "TA-2", lubridate::as_datetime(100), "UP to DN"
  )
  expect_silent(validate_centrix(good_centrix))
})

# validate_asset_map

test_that("validate_asset_map errors with appropriate messages for incorrect data structure", {
  bad_names <- dplyr::tribble(
    ~signal, ~berth, ~track,
    "S1", "A", "TA",
    "S1", "A", "TA"
  )
  expect_snapshot(validate_asset_map(bad_names), error = TRUE)

  bad_signal_type <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    1, "A", "TA", "enters",
    2, "A", "TA", "vacates",
  )
  expect_snapshot(validate_asset_map(bad_signal_type), error = TRUE)

  bad_signal_structure <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "SA", "A", "TA", "enters",
    "SB", "A", "TA", "vacates",
  )
  expect_snapshot(validate_asset_map(bad_signal_structure), error = TRUE)

  bad_berth_type <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", 1, "TA", "enters",
    "S1", 2, "TA", "vacates",
  )
  expect_snapshot(validate_asset_map(bad_berth_type), error = TRUE)

  bad_berth_structure <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "1", "TA", "enters",
    "S1", "2", "TA", "vacates",
  )
  expect_snapshot(validate_asset_map(bad_berth_structure), error = TRUE)

  bad_track_type <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", 1, "enters",
    "S1", "A", 2, "vacates",
  )
  expect_snapshot(validate_asset_map(bad_track_type), error = TRUE)

  bad_track_structure <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", "T1", "enters",
    "S1", "A", "T2", "vacates",
  )
  expect_snapshot(validate_asset_map(bad_track_structure), error = TRUE)

  bad_event_type <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", "TA", TRUE,
    "S1", "A", "TA", FALSE,
  )
  expect_snapshot(validate_asset_map(bad_event_type), error = TRUE)

  bad_event_structure <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", "TA", "UP to DN",
    "S1", "A", "TA", "DN to UP",
  )
  expect_snapshot(validate_asset_map(bad_event_structure), error = TRUE)
})

test_that("validate_asset_map does nothing with correct asset map", {
  good_asset_map <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", "TA", "enters",
    "S1", "A", "TA", "vacates"
  )
  expect_silent(validate_asset_map(good_asset_map))
})

# validate_state_mapping

test_that("validate_state_mapping errors with appropriate messages for incorrect data structure", {
  bad_names <- dplyr::tribble(
    ~signal, ~aspect,
    "RGE", factor("R", levels = c("R", "Y")),
    "HGE", factor("Y", levels = c("R", "Y"))
  )
  expect_snapshot(validate_state_mapping(bad_names), error = TRUE)

  bad_state_type <- dplyr::tribble(
    ~state, ~aspect,
    1, factor("R", levels = c("R", "Y")),
    2, factor("Y", levels = c("R", "Y"))
  )
  expect_snapshot(validate_state_mapping(bad_state_type), error = TRUE)

  bad_aspect_type <- dplyr::tribble(
    ~state, ~aspect,
    "RGE", "R",
    "HGE", "Y"
  )
  expect_snapshot(validate_state_mapping(bad_aspect_type), error = TRUE)
})

test_that("validate_state_mapping does nothing with correct state mapping", {
  good_state_mapping <- dplyr::tribble(
    ~state, ~aspect,
    "RGE", factor("R", levels = c("R", "Y")),
    "HGE", factor("Y", levels = c("R", "Y"))
  )
  expect_silent(validate_state_mapping(good_state_mapping))
})
