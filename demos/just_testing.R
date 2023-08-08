raw_centrix1 <- readr::read_csv(testthat::test_path("fixtures/just testing/raw_events_surbiton.csv")) %>%
  select(-Site, -'State/Transition') %>%
  rename(asset = Channel,
         dt = 'Date (UTC)',
         transition = 'Up/Dn')
raw_centrix2 <- readr::read_csv(testthat::test_path("fixtures/just testing/raw_events_walton.csv")) %>%
  select(-Site, -'State/Transition') %>%
  rename(asset = Channel,
         dt = 'Date (UTC)',
         transition = 'Up/Dn')

raw_centrix <- bind_rows(raw_centrix1, raw_centrix2)

asset_map <- read_rds_test("just testing/asset_map.rds")

raw_centrix %>%
  mutate(dt = lubridate::as_datetime(dt / 1000)) %>%
  filter(stringr::str_detect(asset, '(S[0-9]+\\s[A-Z]+)|(T[A-Z]+(-[0-9])?)'))

berth_events <- railvarr::wrangle_centrix(
  raw_centrix %>%
    mutate(dt = lubridate::as_datetime(dt / 1000)) %>%
    filter(stringr::str_detect(asset, '(S[0-9]+\\s[A-Z]+)|(T[A-Z]+(-[0-9])?)')),
  asset_map) %>%
  mutate(across(starts_with("t_", ignore.case = FALSE),
                ~lubridate::with_tz(.x, tzone = "Europe/London")))

berth_events_clusters <- railvarr::cluster_centrix(berth_events, k = 3L)
railvarr::plot_cluster_events(berth_events_clusters)

group_labels <- dplyr::tribble(
  ~cluster, ~group,
  1, "stopping-walton",
  2, "stopping-all",
  3, "fast"
)

berth_events_classes <- berth_events %>%
  inner_join(berth_events_clusters %>%
               select(train_id, berth, cluster),
             by = c("train_id", "berth")) %>%
  inner_join(group_labels, by = "cluster") %>%
  select(-cluster)

raw_timetable <- readxl::read_excel(testthat::test_path("fixtures/just testing/timetable.xlsx")) %>%
  rename(dt_origin = 'Origin Departure Date',
         train_header = 'Train Id',
         x1 = 'Planned Origin Location Full Name',
         x2 = 'Planned Dest Location Full Name',
         wtt = 'WTT Datetime',
         geo = 'Geography Description',
         event = 'Timing Event Description',
         t = 'Actual Datetime',
         delay = 'WTT Lateness',
         allow_perf = 'Performance Allowance',
         allow_path = 'Pathing Allowance',
         allow_eng = 'Engineering Allowance') %>%
  select(train_header, dt_origin, x1, x2, geo, event, wtt, t, delay, allow_perf, allow_path, allow_eng) %>%
  mutate(allow = allow_perf + allow_path + allow_eng)

stations <- list("Surbiton", "Hampton Court Jn.", "Esher", "Hersham", "Walton-On-Thames", c("Weybridge", "Woking"))

timetable_subset <- railvarr::wrangle_timetable(raw_timetable, stations) %>%
  arrange(dt_origin) %>%
  mutate(across(c("dt_origin", "wtt", "t"),
                ~lubridate::with_tz(.x, tzone = "Europe/London")))

pattern_labels <- dplyr::tribble(
  ~pattern, ~group,
  "None", "fast",
  "Surbiton", "fast",
  "Surbiton,Esher,Hersham,Walton-On-Thames,Weybridge", "stopping-all",
  "Surbiton,Woking", "fast",
  "Surbiton,Woking,Weybridge", "fast",
  "Woking", "fast",
  "Woking,Weybridge", "fast"
)

timetable_groups <- timetable_subset %>%
  inner_join(pattern_labels, by = "pattern") %>%
  select(-pattern)

match_mapping <- dplyr::tribble(
  ~group, ~berth, ~geo, ~lb, ~ub,
  "fast", "AAV", "Hampton Court Jn.", -240, 60,
  "stopping-all", "AAV", "Esher", 0, 0,
  "stopping-all", "AAY", "Hersham", 0, 0,
  "stopping-all", "ABA", "Walton-On-Thames", 0, 0
)

id_matching <- railvarr::match_ids(berth_events_classes, timetable_groups,
                                   match_mapping)
