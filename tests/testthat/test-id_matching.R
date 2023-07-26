# Step 1: read in train classes data
# Step 2: read in berth events data
# Step 3: preprocess berth data using preprocess_berths()
# Step 4: read in timetable calling patterns data
# Step 5: read in timetable data
# Step 6: preprocess timetable data using preprocess_timetable()
# Step 7: ensure that network map has been set with geos
# Step 8: create the match maps for each group
# Step 9: match the ids with match_ids()
# (Optional): check matches and accuracy using combine_ids() and accuracy()

test_that("preprocess_berths() produces expected output", {
  berth_events <- read_rds_test("id-matching/berth_events_sample.rds")
  train_classes <- read_rds_test("id-matching/train_classes.rds")

  berth_groups <- preprocess_berths(berth_events, train_classes)
  exp_names <- c("signal", "train_id", "t_enters", "t_vacates", "group", "day",
                 "start_hour", "end_hour")
  exp_types <- list(character(), integer(), lubridate::POSIXct(),
                    lubridate::POSIXct(), character(), lubridate::Date(),
                    integer(), integer())
  expect_no_error(check_df(berth_groups, exp_names, exp_types, allow_extra = F))
})

test_that("preprocess_timetable() produces expected output", {
  timetable <- read_rds_test("id-matching/timetable_sample.rds")
  calling_patterns <- read_rds_test("id-matching/calling_patterns_services.rds")

  timetable_groups <- preprocess_timetable(timetable, calling_patterns)
  exp_names <- c("train_header", "geo", "event", "wtt", "t", "group", "day",
                 "start_hour", "end_hour")
  exp_types <- list(character(), character(), character(), lubridate::POSIXct(),
                    lubridate::POSIXct(), character(), lubridate::Date(),
                    integer(), integer())
  expect_no_error(check_df(timetable_groups, exp_names, exp_types,
                           allow_extra = F))
})

test_that("match_ids() produces expected output", {
#  map <- read_rds_test("id-matching/network_map.rds")
#  set_map(map)
#
#  train_classes <- read_rds_test("id-matching/train_classes.rds")
#  berth_events <- read_rds_test("id-matching/berth_events_sample.rds") %>%
#    preprocess_berths(train_classes)
#  calling_patterns <- read_rds_test("id-matching/calling_patterns_services.rds")
#  timetable <- read_rds_test("id-matching/timetable_sample.rds") %>%
#    preprocess_timetable(calling_patterns)
#  group_map <- read_rds_test("id-matching/group_map.rds")
#
#  matched_ids <- match_ids(berth_events, timetable, group_map)
#
#  expected <- read_rds_test("id-matching/matched_ids_sample.rds")
#
#  expect_equal(matched_ids %>% select(train_id, train_header), expected)
})

test_that("match_ids() uses tolerance correctly", {
#  map <- read_rds_test("tolerance/network_map.rds")
#  set_map(map)
#
#  train_classes <- read_rds_test("tolerance/train_classes.rds")
#  berth_events <- read_rds_test("tolerance/berth_events.rds") %>%
#    preprocess_berths(train_classes)
#
#  calling_patterns <- read_rds_test("tolerance/calling_patterns.rds")
#  timetable <- read_rds_test("tolerance/timetable.rds") %>%
#    preprocess_timetable(calling_patterns)
#
#  group_map <- read_rds_test("tolerance/group_map.rds")
#  group_map
#
#  sample <- read_rds_test("tolerance/sample.rds")
#  berth_events <- berth_events %>% semi_join(sample, by = "train_id")
#  timetable <- timetable %>% semi_join(sample, by = "train_header")
#
#  matched_low <- match_ids(berth_events, timetable, group_map,
#                           operators = "fuzzy", fuzzy_tolerance = 0.25,
#                           tolerance = 0.25)
#
#  matched_mid <- match_ids(berth_events, timetable, group_map,
#                           operators = "fuzzy", fuzzy_tolerance = 1,
#                           tolerance = 1)
#
#  matched_high <- match_ids(berth_events, timetable, group_map,
#                            operators = "fuzzy", fuzzy_tolerance = 4,
#                            tolerance = 4)
#
#  sample_low <- sample %>% filter(tolerance == "low")
#  sample_mid <- sample %>% filter(tolerance != "high")
#
#  expect_equal(matched_low %>% distinct(train_id, train_header),
#               sample_low %>% distinct(train_id, train_header))
#  expect_equal(matched_mid %>% distinct(train_id, train_header),
#               sample_mid %>% distinct(train_id, train_header))
#  expect_equal(matched_high %>% distinct(train_id, train_header),
#               sample %>% distinct(train_id, train_header))
})
