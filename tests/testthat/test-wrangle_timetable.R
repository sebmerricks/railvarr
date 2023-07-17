test_that("set_stations() errors if stations is not a list of characters", {
  test1 <- list()
  test2 <- c("a", "b")
  test3 <- list("a", 2)

  expect_error(set_stations(test1))
  expect_error(set_stations(test2))
  expect_error(set_stations(test3))
})

test_that("set_stations() sets stations correctly", {
  test1 <- list("a", "b", "c")
  test2 <- list("a", "b", c("c", "d"))

  set_stations(test1)
  expect_equal(get_stations(), test1)

  set_stations(test2)
  expect_equal(get_stations(), test2)
})

test_that("wrangle_timetable() wrangles timetables correctly", {
  timetable <- read_csv_test("data/timetable/timetable.csv") %>%
    mutate(across(c(dt_origin, wtt, t),
                  ~lubridate::with_tz(.x, tzone = "Europe/London")))
  set_stations(as.list(unique(timetable$geo)))

  tts <- wrangle_timetable(timetable)

  out <- timetable %>%
    mutate(allow = 0) %>%
    select(-allow_perf, -allow_path, -allow_eng) %>%
    rename(train_header = train_id) %>%
    mutate(dt_origin = lubridate::as_datetime(dt_origin),
           wtt = lubridate::as_datetime(wtt),
           t = lubridate::as_datetime(t))

  expect_equal(tts, out)
})
