# Asset Mapping ----------------------------------------------------------------

environment$asset_mapping <- NULL

test_that("get_asset_mapping returns null when not set", {
  expect_equal(get_asset_mapping(), NULL)
})

test_that("get_asset_mapping returns the correct asset mapping", {
  mapping <- dplyr::tibble(signal = "S1",
                           berth = "A",
                           track = "TA",
                           event = "enters",
                           geo = "")
  environment$asset_mapping <- mapping
  expect_equal(get_asset_mapping(), mapping)
})

test_that("set_asset_mapping correctly sets the asset mapping", {
  environment$asset_mapping <- NULL
  mapping <- dplyr::tibble(signal = "S1",
                           berth = "A",
                           track = "TA",
                           event = "enters",
                           geo = "")
  set_asset_mapping(mapping)
  expect_equal(environment$asset_mapping, mapping)
})

test_that("set_asset_mapping throws an error for invalid structure", {
  mapping <- dplyr::tibble(berth = "A",
                           track = "TA",
                           event = "enters",
                           geo = "")
  expect_error(set_asset_mapping(mapping))
})

test_that("set_asset_mapping throws an error for invalid signal", {
  mapping <- dplyr::tibble(signal = "SA",
                           berth = "A",
                           track = "TA",
                           event = "enters",
                           geo = "")
  expect_error(set_asset_mapping(mapping))
})

test_that("set_asset_mapping throws an error for invalid track", {
  mapping <- dplyr::tibble(signal = "S1",
                           berth = "A",
                           track = "T123",
                           event = "enters",
                           geo = "")
  expect_error(set_asset_mapping(mapping))
})

test_that("set_asset_mapping throws an error for invalid berth", {
  mapping <- dplyr::tibble(signal = "S1",
                           berth = 1,
                           track = "TA",
                           event = "enters",
                           geo = "")
  expect_error(set_asset_mapping(mapping))
})

test_that("set_asset_mapping throws an error for invalid event", {
  mapping <- dplyr::tibble(signal = "S1",
                           berth = "A",
                           track = "TA",
                           event = "invalid",
                           geo = "")
  expect_error(set_asset_mapping(mapping))
})

test_that("set_asset_mapping throws an error for invalid geo", {
  mapping <- dplyr::tibble(signal = "S1",
                           berth = "A",
                           track = "TA",
                           event = "enters",
                           geo = 1)
  expect_error(set_asset_mapping(mapping))
})

# Centrix ----------------------------------------------------------------------

environment$centrix <- NULL

test_that("get_centrix returns null when not set", {
  expect_equal(get_centrix(), NULL)
})

test_that("get_centrix returns correct centrix data", {
  centrix <- dplyr::tibble(
    asset = "S1",
    dt = lubridate::as_datetime("2023-08-01 11:13:00"),
    transition = "UP to DN",
    period = 3
  )
  environment$centrix <- centrix
  expect_equal(get_centrix(), centrix)
})

test_that("set_centrix correctly sets centrix data", {
  environment$centrix <- NULL
  centrix <- dplyr::tibble(
    asset = "S1",
    dt = lubridate::as_datetime("2023-08-01 11:13:00"),
    transition = "UP to DN",
    period = 3
  )
  set_centrix(centrix)
  expect_equal(environment$centrix, centrix)
})

test_that("set_centrix throws an error for invalid structure", {
  centrix <- dplyr::tibble(
    dt = lubridate::as_datetime("2023-08-01 11:13:00"),
    transition = "UP to DN",
    period = 3
  )
  expect_error(set_centrix(centrix))
})

test_that("set_centrix throws an error for invalid asset", {
  centrix <- dplyr::tibble(
    asset = "XYZ",
    dt = lubridate::as_datetime("2023-08-01 11:13:00"),
    transition = "UP to DN",
    period = 3
  )
  expect_error(set_centrix(centrix))
})

test_that("set_centrix throws an error for invalid dt", {
  centrix <- dplyr::tibble(
    asset = "S1",
    dt = "invalid",
    transition = "UP to DN",
    period = 3
  )
  expect_error(set_centrix(centrix))
})

test_that("set_centrix throws an error for invalid transition", {
  centrix <- dplyr::tibble(
    asset = "S1",
    dt = lubridate::as_datetime("2023-08-01 11:13:00"),
    transition = "UP to UP",
    period = 3
  )
  expect_error(set_centrix(centrix))
})

test_that("set_centrix throws an error for invalid period", {
  centrix <- dplyr::tibble(
    asset = "S1",
    dt = lubridate::as_datetime("2023-08-01 11:13:00"),
    transition = "UP to DN",
    period = "3"
  )
  expect_error(set_centrix(centrix))
})

# State Mapping ----------------------------------------------------------------

environment$state_mapping <- NULL

test_that("get_state_mapping returns null when not set", {
  expect_equal(get_state_mapping(), NULL)
})

test_that("get_state_mapping returns the correct state mapping", {
  mapping <- dplyr::tibble(
    state = c("RGE", "HGE", "HHGE", "DGE"),
    aspect = factor(c("R", "Y", "YY", "G"), levels = c("R", "Y", "YY", "G"))
  )
  environment$state_mapping <- mapping
  expect_equal(get_state_mapping(), mapping)
})

test_that("set_state_mapping correctly sets the state mapping", {
  environment$state_mapping <- NULL
  mapping <- dplyr::tibble(
    state = c("RGE", "HGE", "HHGE", "DGE"),
    aspect = factor(c("R", "Y", "YY", "G"), levels = c("R", "Y", "YY", "G"))
  )
  set_state_mapping(mapping)
  expect_equal(environment$state_mapping, mapping)
})

test_that("set_state_mapping throws an error for invalid structure", {
  mapping <- dplyr::tibble(
    signal = c("RGE", "HGE", "HHGE", "DGE"),
    aspect = factor(c("R", "Y", "YY", "G"), levels = c("R", "Y", "YY", "G"))
  )
  expect_error(set_state_mapping(mapping))
})

test_that("set_state_mapping throws an error for invalid state", {
  mapping <- dplyr::tibble(
    state = c(1, 2, 3, 4),
    aspect = factor(c("R", "Y", "YY", "G"), levels = c("R", "Y", "YY", "G"))
  )
  expect_error(set_state_mapping(mapping))
})

test_that("set_state_mapping throws an error for invalid aspect", {
  mapping <- dplyr::tibble(
    signal = c("RGE", "HGE", "HHGE", "DGE"),
    aspect = c("R", "Y", "YY", "G")
  )
  expect_error(set_state_mapping(mapping))
})

# Timetable --------------------------------------------------------------------

environment$timetable <- NULL

test_that("get_timetable returns null when not set", {
  expect_equal(get_timetable(), NULL)
})

test_that("get_timetable returns the correct timetable", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", "2023-08-02 09:05:00", "GEO1", "P", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00", 0
  )
  environment$timetable <- timetable
  expect_equal(get_timetable(), timetable)
})

test_that("set_timetable correctly sets the timetable", {
  environment$timetable <- NULL
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", "2023-08-02 09:05:00", "GEO1", "P", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00", 0
  )
  set_timetable(timetable)
  expect_equal(environment$timetable, timetable %>%
                 mutate(dt_origin = lubridate::as_datetime(dt_origin),
                        wtt = lubridate::as_datetime(wtt),
                        t = lubridate::as_datetime(t)) %>%
                 mutate(event = paste0(event, "ass")))
})

test_that("set_timetable throws an error for invalid structure", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t,
    "A123", "2023-08-02 09:05:00", "GEO1", "P", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00"
  )
  expect_error(set_timetable(timetable))
})

test_that("set_timetable throws an error for invalid train_header", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    1234, "2023-08-02 09:05:00", "GEO1", "P", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00", 0
  )
  expect_error(set_timetable(timetable))
})

test_that("set_timetable throws an error for invalid dt_origin", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", "2023-08 09:05:00", "GEO1", "P", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00", 0
  )
  expect_error(set_timetable(timetable))
})

test_that("set_timetable throws an error for invalid event", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", "2023-08-02 09:05:00", "GEO1", "U", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00", 0
  )
  expect_error(set_timetable(timetable))
})

test_that("set_timetable throws an error for invalid delay", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", "2023-08-02 09:05:00", "GEO1", "P", "2023-08-02 09:07:00",
    "2023-08-02 09:07:00", "0"
  )
  expect_error(set_timetable(timetable))
})
