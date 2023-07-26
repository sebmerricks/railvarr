test_that("asset_map type checking works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", 1, "enters", "Newc",
                                        "S2", "AAB", 2, "enters", "Newc")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", 1, "TAAA-1", "enters", "Newc",
                                        "S2", 2, "TAAB", "enters", "Newc")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        1, "AAA", "TAAA-1", "enters", "Newc",
                                        2, "AAB", "TAAB", "enters", "Newc")))
})

test_that("asset_map signal matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                           "1", "AAA", "TAAA-1", "enters", "Newc",
                           "S2", "AAB", "TAAB", "enters", "Newc")))
})

test_that("asset_map berth matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "B1", "TAAA-1", "enters", "Newc",
                                        "S2", "B2", "TAAB", "enters", "Newc")))
})

test_that("asset_map track matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", "TAAA-1", "enters", "Newc",
                                        "S2", "AAB", "TAA1", "enters", "Newc")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", "TAAA-12", "enters", "New",
                                        "S2", "AAB", "TAAB", "enters", "Newc")))
})

test_that("asset_map event matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", "TAAA-1", "e", "Newc",
                                        "S2", "AAB", "TAAB", "e", "Newc")))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                           "S1", "AAA", "TAAA-1", "enters", "Newc",
                                           "S2", "AAB", "TAAB", "enters", "Newc")))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                           "S1", "AAA", "TAAA-1", "enters", "Newc",
                                           "S2", "AAB", "TAAB", "vacates", "Newc")))
})

test_that("asset_map geo matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", "TAAA-1", "enters", "123",
                                        "S2", "AAB", "TAAB", "vacates", "Newc")))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                    "S1", "AAA", "TAAA-1", "enters", "Newc  a",
                                    "S2", "AAB", "TAAB", "vacates", "Newc")))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                    "S1", "AAA", "TAAA-1", "enters", "Newc-A",
                                    "S2", "AAB", "TAAB", "vacates", "Newc A")))
})
