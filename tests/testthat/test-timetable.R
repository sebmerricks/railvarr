test_that("wrangle_timetable works", {
  timetable <- read_rds_test("timetable.rds")
  stations <- as.list(read_rds_test("stations.rds"))
  timetable_subset <- wrangle_timetable(timetable, stations)
  out <- read_rds_test("timetable_subset.rds")
  expect_equal(timetable_subset, out)
})
