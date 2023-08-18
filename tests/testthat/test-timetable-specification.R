test_that("calculate_journey_specifications() works", {
  timetable_subset <- read_rds_test("timetable_subset.rds")
  spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")
  timetable_specification <- calculate_journey_specifications(timetable_subset,
                                                              spec_stations)
  out <- read_rds_test("timetable_specification.rds")
  expect_equal(timetable_specification, out)
})
