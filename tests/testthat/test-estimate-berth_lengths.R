test_that("estimate_berth_lengths() works", {
  estimated_berth_lengths <-
    estimate_berth_lengths(railvarr::timetable_specification,
                           railvarr::id_matching,
                           railvarr::berth_events_groups,
                           expected_journey_time = 270,
                           track_length = 5.97)
  expect_equal(estimated_berth_lengths, railvarr::berth_lengths)
})
