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
