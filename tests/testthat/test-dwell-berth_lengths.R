test_that("estimate_berth_lengths() works", {
  timetable_specification <- read_rds_test("timetable_specification.rds")
  berth_events_groups <- read_rds_test("berth_events_groups.rds")
  id_matching <- read_rds_test("id_matching.rds")
  distance.miles = 5.97
  speed.miles = 79.6
  estimated_berth_lengths <- estimate_berth_lengths(timetable_specification,
                                                    berth_events_groups,
                                                    id_matching,
                                                    distance.miles = 5.97,
                                                    speed.miles = 79.6)
  out <- read_rds_test("estimated_berth_lengths.rds")
  expect_equal(estimated_berth_lengths, out)
})
