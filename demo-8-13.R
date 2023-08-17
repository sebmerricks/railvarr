raw_centrix <- read_rds_test("raw_centrix.rds")
asset_map <- read_rds_test("asset_map.rds")

berth_events <- railvarr::wrangle_centrix(raw_centrix, asset_map)

berth_events_clusters <- railvarr::cluster_journeys(berth_events,
                                                    centers = 3L,
                                                    iter.max = 40L)
railvarr::plot_clusters(berth_events_clusters)

group_labels <- dplyr::tribble(
  ~cluster, ~group,
  1, "stopping-112",
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

stations <- list(
  "geo5",
  "geo6",
  "geo110",
  "geo111",
  "geo112",
  c("geo7", "geo8")
  )

timetable_subset <- railvarr::wrangle_timetable(raw_timetable, stations)

pattern_labels <- dplyr::tribble(
  ~pattern, ~group,
  "None", "fast",
  "geo5", "fast",
  "geo5,geo110,geo111,geo112,geo7", "stopping-all",
  "geo5,geo8", "fast",
  "geo5,geo8,geo7", "fast",
  "geo8", "fast",
  "geo8,geo7", "fast"
)

timetable_groups <- timetable_subset %>%
  inner_join(pattern_labels, by = "pattern") %>%
  select(-pattern)

match_mapping <- dplyr::tribble(
  ~group, ~berth, ~geo, ~lb, ~ub,
  "fast", "A", "geo6", -240, 60,
  "stopping-all", "A", "geo110", 0, 0,
  "stopping-all", "D", "geo111", 0, 0,
  "stopping-all", "F", "geo112", 0, 0
)

id_matching <- railvarr::match_ids(berth_events_classes,
                                   timetable_groups %>%
                                     select(train_header, dt_origin, group, geo, event, wtt, t),
                                   match_mapping)

berth_events_matched <- berth_events_classes %>%
  inner_join(id_matching, by = "train_id")

timetable_matched <- timetable_groups %>%
  inner_join(id_matching, by = c("train_header", "dt_origin"))