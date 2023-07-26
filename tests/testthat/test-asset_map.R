test_that("asset_map type checking works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        "S1", "AAA", 1, "enters",
                                        "S2", "AAB", 2, "enters")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        "S1", 1, "TAAA-1", "enters",
                                        "S2", 2, "TAAB", "enters")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        1, "AAA", "TAAA-1", "enters",
                                        2, "AAB", "TAAB", "enters")))
})

test_that("asset_map signal matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                           "1", "AAA", "TAAA-1", "enters",
                           "S2", "AAB", "TAAB", "enters")))
})

test_that("asset_map berth matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        "S1", "B1", "TAAA-1", "enters",
                                        "S2", "B2", "TAAB", "enters")))
})

test_that("asset_map track matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        "S1", "AAA", "TAAA-1", "enters",
                                        "S2", "AAB", "TAA1", "enters")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        "S1", "AAA", "TAAA-12", "enters",
                                        "S2", "AAB", "TAAB", "enters")))
})

test_that("asset_map event matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                        "S1", "AAA", "TAAA-1", "e",
                                        "S2", "AAB", "TAAB", "e")))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                           "S1", "AAA", "TAAA-1", "enters",
                                           "S2", "AAB", "TAAB", "enters")))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event,
                                           "S1", "AAA", "TAAA-1", "enters",
                                           "S2", "AAB", "TAAB", "vacates")))
})
