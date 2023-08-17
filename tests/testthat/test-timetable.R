test_that("wrangle_timetable works", {
  timetable <- read_rds_test("timetable.rds")
  stations <- read_rds_test_raw("stations.rds")
  stopping_stations <- c("geo110", "geo111", "geo112")
  timetable_subset <- wrangle_timetable(timetable,
                                        stations,
                                        stopping_stations)
  out <- read_rds_test("timetable_subset.rds")
  expect_equal(timetable_subset, out)
})
