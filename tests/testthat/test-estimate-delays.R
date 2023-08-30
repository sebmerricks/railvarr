test_that("estimate_delays() works", {
  expect_equal(
    estimate_delays(dwell_times, timetable_subset, id_matching),
    delays
  )
})
