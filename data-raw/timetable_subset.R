timetable_subset <- wrangle_timetable(railvarr::timetable,
                                      railvarr::stations,
                                      railvarr::stopping_stations)

usethis::use_data(timetable_subset, overwrite = TRUE)
