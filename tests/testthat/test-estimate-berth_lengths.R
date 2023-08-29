test_that("estimate_berth_lengths() works", {
  timetable_specification <- read_rds_test("timetable_specification.rds")
  berth_events_groups <- read_rds_test("berth_events_groups.rds")
  id_matching <- read_rds_test("id_matching.rds")
  estimated_berth_lengths <- estimate_berth_lengths(timetable_specification,
                                                    id_matching,
                                                    berth_events_groups,
                                                    expected_journey_time = 270,
                                                    track_length = 5.97)
  out <- read_rds_test("estimated_berth_lengths.rds")
  expect_equal(estimated_berth_lengths, out)
})
