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

test_that("", {
  berth_events <- read_rds_test("id-matching/berth_events_sample.rds")
  timetable <- read_rds_test("id-matching/timetable_sample.rds")
  train_classes <- read_rds_test("id-matching/train_classes.rds")
  calling_patterns <- read_rds_test("id-matching/calling_patterns_services.rds")
})
