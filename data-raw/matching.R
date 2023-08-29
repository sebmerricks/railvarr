timetable_groups <- read_rds_test("timetable_groups.rds")
berth_events_groups <- read_rds_test("berth_events_groups.rds")

usethis::use_data(timetable_groups, overwrite = TRUE)
usethis::use_data(berth_events_groups, overwrite = TRUE)

match_mapping <- dplyr::tribble(
  ~group, ~berth, ~geo, ~lb, ~ub,
  "fast", "A", "geo6", -240, 60,
  "stopping-all", "A", "geo110", 0, 0,
  "stopping-all", "D", "geo111", 0, 0,
  "stopping-all", "F", "geo112", 0, 0
)

id_matching <- railvarr::match_ids(berth_events_classes,
                                   timetable_subset %>%
                                     select(train_header, dt_origin, group, geo, event, wtt, t),
                                   match_mapping)

usethis::use_data(id_matching, overwrite = TRUE)
