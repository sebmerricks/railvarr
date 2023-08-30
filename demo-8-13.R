# Centrix ----------------------------------------------------------------------

raw_centrix <- read_rds_test("raw_centrix.rds")
asset_map <- read_rds_test("asset_map.rds")

berth_events <- railvarr::wrangle_centrix(raw_centrix, asset_map)

berth_events_clusters <- railvarr::cluster_journeys(berth_events,
                                                    centers = 3L,
                                                    iter.max = 40L)
railvarr::plot_clusters(berth_events_clusters)

group_labels <- dplyr::tribble(
  ~cluster, ~group,
  1, "stopping-geo112",
  2, "stopping-all",
  3, "fast"
)

berth_events_classes <- berth_events %>%
  inner_join(berth_events_clusters %>%
               select(train_id, berth, cluster),
             by = c("train_id", "berth")) %>%
  inner_join(group_labels, by = "cluster") %>%
  select(-cluster)

# Timetable --------------------------------------------------------------------

raw_timetable <- read_rds_test("timetable.rds")

stations <- read_rds_test_raw("stations.rds")
stopping_stations <- read_rds_test_raw("stopping_stations.rds")

timetable_subset <- railvarr::wrangle_timetable(raw_timetable,
                                                stations,
                                                stopping_stations)

spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")

timetable_specification <-
  railvarr::calculate_journey_specifications(timetable_subset, spec_stations)

# ID Matching ------------------------------------------------------------------

match_mapping <- dplyr::tribble(
  ~group, ~berth, ~geo, ~lb, ~ub,
  "fast", "A", "geo6", -240, 60,
  "stopping-all", "A", "geo110", 0, 0,
  "stopping-all", "D", "geo111", 0, 0,
  "stopping-all", "F", "geo112", 0, 0
)

id_matching <- railvarr::match_ids(berth_events_classes,
                                   timetable_subset %>%
                                     select(train_header, dt_origin, group, geo,
                                            event, wtt, t) %>%
                                     filter(event %in%
                                              c("Arrive", "Depart", "Pass")),
                                   match_mapping)

berth_events_matched <- berth_events_classes %>%
  inner_join(id_matching, by = "train_id")

timetable_matched <- timetable_subset %>%
  inner_join(id_matching, by = c("train_header", "dt_origin"))

# Berth Lengths ----------------------------------------------------------------

estimated_berth_lengths <-
  railvarr::estimate_berth_lengths_new(timetable_specification,
                                       id_matching,
                                       berth_events_classes,
                                       expected_journey_time = 270,
                                       track_length = 5.97)

# Dwell Times ------------------------------------------------------------------

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

station_berth_lengths <- dplyr::tribble(
  ~station, ~L1, ~L2,
  "geo110", 1050, 124,
  "geo111", 594, 420,
  "geo112", 621, 366
)

a_brake = 0.4
a_tract = 0.35
dwell_times <- estimate_dwell_times(berth_events_classes,
                                    berth_lengths,
                                    station_names,
                                    stopping_patterns,
                                    a_brake,
                                    a_tract)

summary(dwell_times$T_dwell)


