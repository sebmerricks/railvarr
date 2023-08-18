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

raw_timetable <- read_rds_test("timetable.rds")

stations <- read_rds_test_raw("stations.rds")
stopping_stations <- read_rds_test_raw("stopping_stations.rds")

timetable_subset <- railvarr::wrangle_timetable(raw_timetable,
                                                stations,
                                                stopping_stations)

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

berth_events_matched <- berth_events_classes %>%
  inner_join(id_matching, by = "train_id")

timetable_matched <- timetable_subset %>%
  inner_join(id_matching, by = c("train_header", "dt_origin"))

spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")

timetable_specification <-
  railvarr::calculate_journey_specifications(timetable_subset, spec_stations)

estimated_berth_lengths <-
  railvarr::estimate_berth_lengths(timetable_specification,
                                   berth_events_classes,
                                   id_matching,
                                   distance.miles = 5.97,
                                   speed.miles = 79.6)

berth_events_groups <- berth_events_classes

berth_lengths <- estimated_berth_lengths %>%
  dplyr::select(berth, L.km) %>%
  dplyr::rename(L = L.km) %>%
  dplyr::mutate(L = L * 1000)

station_names <- dplyr::tribble(
  ~berth, ~station,
  "A", "geo110",
  "D", "geo111",
  "F", "geo112"
)

station_berth_lengths <- dplyr::tribble(
  ~station, ~L1, ~L2,
  "geo110", 1050, 124,
  "geo111", 594, 420,
  "geo112", 621, 366
)

a_brake = 0.407696
a_tract = 0.358101
a_brake = 0.5
a_tract = 0.5
dwell_times <- estimate_dwell_times(berth_events_groups,
                                    berth_lengths,
                                    station_names,
                                    a_brake,
                                    a_tract)
summary(dwell_times$T_dwell)


