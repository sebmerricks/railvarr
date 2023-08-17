timetable <- read_rds_test("timetable.rds")
stations <- read_rds_test_raw("stations.rds")

usethis::use_data(timetable, overwrite = TRUE)
usethis::use_data(stations, overwrite = TRUE)
