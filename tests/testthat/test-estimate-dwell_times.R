test_that("estimate_dwell_times() works", {
  station_names <- dplyr::tribble(
    ~berth, ~station,
    "A", "geo110",
    "D", "geo111",
    "F", "geo112"
  )

  stopping_patterns <- dplyr::tribble(
    ~group, ~station,
    "stopping-all", "geo110",
    "stopping-all", "geo111",
    "stopping-all", "geo112",
    "stopping-geo112", "geo112"
  )

  a_brake = 0.4
  a_tract = 0.35

  calculated_dwell_times <- estimate_dwell_times(railvarr::berth_events_groups,
                                                 railvarr::berth_lengths,
                                                 station_names,
                                                 stopping_patterns,
                                                 a_brake,
                                                 a_tract)

  expect_equal(calculated_dwell_times, railvarr::dwell_times)
})
