test_that("wrangle_centrix() works", {
  map <- readr::read_rds("~/RStudio/railvarr/inst/extdata/sample_map.rds")
  set_map(map)

  se <- readr::read_rds("~/RStudio/railvarr/inst/extdata/sample_aspect_events.rds")
  te <- readr::read_rds("~/RStudio/railvarr/inst/extdata/sample_track_events.rds")

  os <- data.frame(
    signal = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8")
  )

  out <- readr::read_rds("~/RStudio/railvarr/inst/extdata/sample_berth_events.rds")

  expect_equal(wrangle_centrix(se, te, override_signals = os), out)
})
