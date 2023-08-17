test_that("wrangle_timetable works", {
  timetable <- read_rds_test("timetable.rds")
  stations <- as.list(read_rds_test("stations.rds"))[[1]]
  stopping_stations <- c("geo110", "geo111", "geo112")
  timetable_subset <- wrangle_timetable(timetable,
                                        stations,
                                        stopping_stations)
  out <- read_rds_test("timetable_subset.rds")
  expect_equal(timetable_subset, out)
})
