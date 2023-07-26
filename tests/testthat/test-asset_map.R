test_that("asset_map type checking works", {
  expect_error(asset_map(data.frame(
    "signal" = signal(),
    "berth" = character(),
    "track" = integer(),
    "event" = character(),
    "geo" = character()
  )))

  expect_error(asset_map(data.frame(
    "signal" = signal(),
    "berth" = integer(),
    "track" = track(),
    "event" = character(),
    "geo" = character()
  )))

  expect_error(asset_map(data.frame(
    "signal" = integer(),
    "berth" = character(),
    "track" = track(),
    "event" = character(),
    "geo" = character()
  )))
})

test_that("asset_map$berth matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "B1", "TAAA-1", "enters", "Newc",
                                        "S2", "B2", "TAAB", "enters", "Newc") %>%
                           mutate(signal = as_signal(signal),
                                  track = as_track(track))))
})

test_that("asset_map$event matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", "TAAA-1", "e", "Newc",
                                        "S2", "AAB", "TAAB", "e", "Newc") %>%
                           mutate(signal = as_signal(signal),
                                  track = as_track(track))))

  expect_no_error(asset_map(dplyr::tribble(
    ~signal, ~berth, ~track, ~event, ~geo,
    "S1", "AAA", "TAAA-1", "enters", "Newc",
    "S2", "AAB", "TAAB", "enters", "Newc") %>%
      mutate(signal = as_signal(signal),
             track = as_track(track))))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                           "S1", "AAA", "TAAA-1", "enters", "Newc",
                                           "S2", "AAB", "TAAB", "vacates", "Newc") %>%
                              mutate(signal = as_signal(signal),
                                     track = as_track(track))))
})

test_that("asset_map$geo matching works", {
  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                        "S1", "AAA", "TAAA-1", "enters", "123",
                                        "S2", "AAB", "TAAB", "vacates", "Newc") %>%
                           mutate(signal = as_signal(signal),
                                  track = as_track(track))))

  expect_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                    "S1", "AAA", "TAAA-1", "enters", "Newc  a",
                                    "S2", "AAB", "TAAB", "vacates", "Newc") %>%
                           mutate(signal = as_signal(signal),
                                  track = as_track(track))))

  expect_no_error(asset_map(dplyr::tribble(~signal, ~berth, ~track, ~event, ~geo,
                                    "S1", "AAA", "TAAA-1", "enters", "Newc-A",
                                    "S2", "AAB", "TAAB", "vacates", "Newc A") %>%
                              mutate(signal = as_signal(signal),
                                     track = as_track(track))))
})
