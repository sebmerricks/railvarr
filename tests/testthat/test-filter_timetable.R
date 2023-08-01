test_that("find_relevant_services filters to relevant services", {
  environment$start_station <- "START"
  environment$end_station <- "END"

  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo,
    "A123", lubridate::as_datetime(0), "START",
    "A123", lubridate::as_datetime(0), "MIDDLE",
    "A123", lubridate::as_datetime(0), "END",
    "BAD", lubridate::as_datetime(100), "NOTSTART",
    "BAD", lubridate::as_datetime(100), "NOTMIDDLE",
    "BAD", lubridate::as_datetime(100), "NOTEND"
  )

  relevant <- dplyr::tribble(
    ~train_header, ~dt_origin,
    "A123", lubridate::as_datetime(0)
  )

  expect_equal(find_relevant_services(timetable), relevant)
})

test_that("filter_forward_services filters out backwards services", {
  environment$start_station <- "START"
  environment$end_station <- "END"
  environment$forwards <- "START-END"
  environment$backwards <- "END-START"

  relevant <- dplyr::tribble(
    ~train_header, ~dt_origin,
    "A123", lubridate::as_datetime(0),
    "321A", lubridate::as_datetime(100)
  )

  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t,
    "A123", lubridate::as_datetime(0), "START", "Pass", lubridate::as_datetime(100), lubridate::as_datetime(100),
    "A123", lubridate::as_datetime(0), "MIDDLE", "Pass", lubridate::as_datetime(200), lubridate::as_datetime(200),
    "A123", lubridate::as_datetime(0), "END", "Pass", lubridate::as_datetime(300), lubridate::as_datetime(300),
    "321A", lubridate::as_datetime(100), "END", "Pass", lubridate::as_datetime(200), lubridate::as_datetime(200),
    "321A", lubridate::as_datetime(100), "MIDDLE", "Pass", lubridate::as_datetime(300), lubridate::as_datetime(300),
    "321A", lubridate::as_datetime(100), "START", "Pass", lubridate::as_datetime(400), lubridate::as_datetime(400)
  )

  forward <- dplyr::tribble(
    ~train_header, ~dt_origin, ~direction,
    "A123", lubridate::as_datetime(0), "START-END"
  )

  expect_equal(filter_forward_services(relevant, timetable), forward)
})

test_that("subset_timetable returns a formatted timetable subset", {
  environment$start_station <- "START"
  environment$end_station <- "END"
  environment$forwards <- "START-END"
  environment$backwards <- "END-START"
  set_stations(list("START", "MIDDLE", "END"))

  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", lubridate::as_datetime(0), "START", "Pass", lubridate::as_datetime(100), lubridate::as_datetime(100), 0,
    "A123", lubridate::as_datetime(0), "MIDDLE", "Pass", lubridate::as_datetime(200), lubridate::as_datetime(200), 0,
    "A123", lubridate::as_datetime(0), "END", "Pass", lubridate::as_datetime(300), lubridate::as_datetime(300), 0,
    "321A", lubridate::as_datetime(100), "END", "Pass", lubridate::as_datetime(200), lubridate::as_datetime(200), 0,
    "321A", lubridate::as_datetime(100), "MIDDLE", "Pass", lubridate::as_datetime(300), lubridate::as_datetime(300), 0,
    "321A", lubridate::as_datetime(100), "START", "Pass", lubridate::as_datetime(400), lubridate::as_datetime(400), 0
  )

  relevant <- dplyr::tribble(
    ~train_header, ~dt_origin, ~direction,
    "A123", lubridate::as_datetime(0), "START-END"
  )

  subset <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", lubridate::as_datetime(0), "START", "Pass", lubridate::as_datetime(100), lubridate::as_datetime(100), 0,
    "A123", lubridate::as_datetime(0), "MIDDLE", "Pass", lubridate::as_datetime(200), lubridate::as_datetime(200), 0,
    "A123", lubridate::as_datetime(0), "END", "Pass", lubridate::as_datetime(300), lubridate::as_datetime(300), 0
  ) %>%
    select(train_header, geo, dt_origin, event, wtt, t, delay) %>%
    mutate(across(c(dt_origin, wtt, t),
                  ~lubridate::with_tz(.x, tzone = "Europe/London")))

  expect_equal(subset_timetable(relevant, timetable), subset)
})

test_that("filter_timetable successfully filters raw timetable data", {
  timetable <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", lubridate::as_datetime(0), "START", "P", lubridate::as_datetime(100), lubridate::as_datetime(100), 0,
    "A123", lubridate::as_datetime(0), "MIDDLE", "P", lubridate::as_datetime(200), lubridate::as_datetime(200), 0,
    "A123", lubridate::as_datetime(0), "END", "P", lubridate::as_datetime(300), lubridate::as_datetime(300), 0,
    "321A", lubridate::as_datetime(100), "END", "P", lubridate::as_datetime(200), lubridate::as_datetime(200), 0,
    "321A", lubridate::as_datetime(100), "MIDDLE", "P", lubridate::as_datetime(300), lubridate::as_datetime(300), 0,
    "321A", lubridate::as_datetime(100), "START", "P", lubridate::as_datetime(400), lubridate::as_datetime(400), 0
  )

  set_timetable(timetable)
  set_stations(list("START", "MIDDLE", "END"))

  subset <- dplyr::tribble(
    ~train_header, ~dt_origin, ~geo, ~event, ~wtt, ~t, ~delay,
    "A123", lubridate::as_datetime(0), "START", "Pass", lubridate::as_datetime(100), lubridate::as_datetime(100), 0,
    "A123", lubridate::as_datetime(0), "MIDDLE", "Pass", lubridate::as_datetime(200), lubridate::as_datetime(200), 0,
    "A123", lubridate::as_datetime(0), "END", "Pass", lubridate::as_datetime(300), lubridate::as_datetime(300), 0
  ) %>%
    select(train_header, geo, dt_origin, event, wtt, t, delay) %>%
    mutate(across(c(dt_origin, wtt, t),
                  ~lubridate::with_tz(.x, tzone = "Europe/London")))

  expect_equal(filter_timetable(), subset)
})
