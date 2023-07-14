test_that("wrangle_centrix() produces expected output", {
  map <- read_rds_test("sample_map.rds")
  set_map(map)

  se <- read_rds_test("sample_aspect_events.rds") %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")),
           past_aspect = factor(past_aspect, levels = c("R", "Y", "YY", "G")))
  te <- read_rds_test("sample_track_events.rds")

  out <- read_rds_test("sample_berth_events.rds") %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")))

  expect_equal(wrangle_centrix(se, te), out)
})

test_that("the whole pipeline works as expected", {
  map <- read_csv_test("gen-data/test_map.csv")
  set_map(map)

  path <- system.file("tests/testthat/fixtures/gen-data/centrix",
                      package = "railvarr")
  raw_data <- read_csv_files(path, show_col_types = FALSE) %>%
    dplyr::mutate(period = 1L,
                  dt = lubridate::as_datetime(dt))

  events <- split_signal_track_events(raw_data)
  aspect_events <- preprocess_signal_events(events[[1]])
  track_events <- preprocess_track_events(events[[2]])

  berth_events <- wrangle_centrix(aspect_events, track_events)

  out <- dplyr::tibble(read_csv_test("gen-data/gen_berth_events.csv")) %>%
    dplyr::mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G"))) %>%
    dplyr::mutate(dplyr::across(contains("T", ignore.case = FALSE),
                                lubridate::as.duration)) %>%
    dplyr::mutate(dplyr::across(contains("t_", ignore.case = FALSE),
                                lubridate::as_datetime))

  expect_equal(berth_events, out)
})
