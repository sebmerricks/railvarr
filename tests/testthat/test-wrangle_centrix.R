read_rds_test <- function(path) {
  readr::read_rds(system.file("extdata", path, package = "railvarr", mustWork = TRUE))
}

test_that("wrangle_centrix() works", {
  map <- read_rds_test("sample_map.rds")
  set_map(map)

  se <- read_rds_test("sample_aspect_events.rds")
  te <- read_rds_test("sample_track_events.rds")

  out <- read_rds_test("sample_berth_events.rds")

  expect_equal(wrangle_centrix(se, te), out)
})
