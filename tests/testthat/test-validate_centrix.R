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
