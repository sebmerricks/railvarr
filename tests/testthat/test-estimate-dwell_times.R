test_that("estimate_dwell_times() works", {
  # There has to be a better way of doing it than this
  # These two parameters, station_names and stopping_patterns
  # They are janky and cumbersome, they don't work very well

  # What information are they giving to the function?
  #  Which berths line up with which stations?
  #  Which trains stop at which stations?

  # I think the first question can be answered a more subtle way
  # I think the second question is answered by the find_calling_patterns?

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

  a_brake = 0.407696
  a_tract = 0.358101

  dwell_times <- estimate_dwell_times(railvarr::berth_events_groups,
                                      railvarr::berth_lengths,
                                      station_names,
                                      stopping_patterns,
                                      a_brake,
                                      a_tract)
})
