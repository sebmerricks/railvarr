test_that("centrix type checking works", {
  expect_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    character(), lubridate::POSIXct(), character(), double()
  )))

  expect_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    character(), character(), character(), integer()
  )))

  expect_no_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    character(), lubridate::POSIXct(), character(), integer()
  )))

  expect_no_error(centrix())
})

test_that("centrix$asset matching works", {
  expect_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "TAAQ", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3,
    "S133 RGE", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3
  )))

  expect_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "TAAQ TR", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3,
    "S133", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3
  )))

  expect_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "AAQ TR", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3,
    "S133 RGE", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3
  )))

  expect_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "TAAQ TR", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3,
    "SA12 RGE", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3
  )))

  expect_no_error(centrix(dplyr::tribble(
    ~asset, ~dt, ~transition, ~period,
    "TAAQ TR", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3,
    "S133 RGE", lubridate::as_datetime("2023-07-26 14:15:00"), "DN to UP", 3
  )))
})
