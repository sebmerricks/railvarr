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
