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

dwell_times <- estimate_dwell_times(railvarr::berth_events_groups,
                                    railvarr::berth_lengths,
                                    station_names,
                                    stopping_patterns,
                                    a_brake,
                                    a_tract)

usethis::use_data(dwell_times, overwrite = TRUE)

delays <- estimate_delays(dwell_times, timetable_subset, id_matching)

usethis::use_data(delays, overwrite = TRUE)
