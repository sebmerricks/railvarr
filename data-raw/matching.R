timetable_groups <- read_rds_test("timetable_groups.rds")
berth_events_groups <- read_rds_test("berth_events_groups.rds")

usethis::use_data(timetable_groups, overwrite = TRUE)
usethis::use_data(berth_events_groups, overwrite = TRUE)
