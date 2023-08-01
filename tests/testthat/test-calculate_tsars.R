environment$asset_mapping <- NULL
environment$centrix <- NULL

make_asset_map <- function() {
  dplyr::tibble(
    signal = "S1",
    berth = "A",
    track = "TA",
    event = "enters",
    geo = ""
  )
}

test_that("calculate_tsars throws an error when asset_mapping is not set", {
  expect_error(calculate_tsars())
})

test_that("calculate_tsars throws an error when centrix is not set", {
  set_asset_mapping(make_asset_map())
  expect_error(calculate_tsars())
})
