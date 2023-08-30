berth_lengths <-
  estimate_berth_lengths(railvarr::timetable_specification,
                         railvarr::id_matching,
                         railvarr::berth_events_groups,
                         expected_journey_time = 270,
                         track_length = 5.97)

usethis::use_data(berth_lengths, overwrite = TRUE)
