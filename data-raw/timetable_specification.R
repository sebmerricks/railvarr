raw_timetable <- read_rds_test("timetable.rds")

stations <- read_rds_test_raw("stations.rds")
stopping_stations <- read_rds_test_raw("stopping_stations.rds")

timetable_subset <- railvarr::wrangle_timetable(raw_timetable,
                                                stations,
                                                stopping_stations)

spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")

timetable_specification <-
  railvarr::calculate_journey_specifications(timetable_subset, spec_stations)

usethis::use_data(timetable_specification, overwrite = TRUE)
