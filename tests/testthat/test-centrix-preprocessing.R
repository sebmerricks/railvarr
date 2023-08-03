# Split Signal Track Events ----------------------------------------------------

test_that("split_signal_track_events() splits signal and track events", {
  raw_events <- tribble(
    ~asset, ~dt, ~transition, ~period,
    "S1", lubridate::as_datetime(100), "DN to UP", 1,
    "T1", lubridate::as_datetime(200), "DN to UP", 1
  )

  out <- raw_events %>%
    dplyr::mutate(is_track = stringr::str_starts(asset, "T")) %>%
    dplyr::group_by(is_track) %>%
    dplyr::group_split(.keep = F)

  expect_equal(split_signal_track_events(raw_events), out)
})

# Pre-process Signal Events ----------------------------------------------------

test_that("preprocess_signal_events() successfully converts to signal/aspect", {
  state_mapping <- dplyr::tribble(
    ~state, ~aspect,
    "RGE", factor("R", levels = c("R", "Y", "YY", "G")),
    "HGE", factor("Y", levels = c("R", "Y", "YY", "G")),
    "HHGE", factor("YY", levels = c("R", "Y", "YY", "G")),
    "DGE", factor("G", levels = c("R", "Y", "YY", "G"))
  )

  raw_signal_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 RGE", lubridate::as_datetime(100), "DN to UP",
    "S2 HGE", lubridate::as_datetime(200), "DN to UP",
    "S3 DGE", lubridate::as_datetime(300), "DN to UP",
    "S4 HHGE", lubridate::as_datetime(400), "DN to UP",
  )

  asset_map <- data.frame(
    signal = c("S1", "S2", "S3", "S4"),
    berth = c("", "", "", ""),
    track = c("TA", "TA", "TA", "TA"),
    event = c("enters", "enters", "enters", "enters")
  )

  out <- dplyr::tibble(data.frame(
    signal = c("S1", "S2", "S3", "S4"),
    dt = c(lubridate::as_datetime(100), lubridate::as_datetime(200),
           lubridate::as_datetime(300), lubridate::as_datetime(400)),
    aspect = factor(
      c("R", "Y", "G", "YY"),
      levels = c("R", "Y", "YY", "G")
    ),
    past_aspect = factor(
      c(NA_character_, NA_character_, NA_character_, NA_character_),
      levels = c("R", "Y", "YY", "G")
    )
  ))

  expect_equal(preprocess_signal_events(raw_signal_events, asset_map,
                                        state_mapping), out)
})

# Pre-process Track Events -----------------------------------------------------

test_that("preprocess_track_events() correctly processes track data", {
  dt <- lubridate::as_datetime(100)
  raw_track_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "TA-1", dt, "DN to UP",
    "TA-2", dt, "UP to DN",
    "TB-1", dt, "DN to UP",
    "TB-2", dt, "UP to DN",
    "TC", dt, "DN to UP",
    "TD", dt, "UP to DN",
  )

  asset_map <- data.frame(
    signal = c("S1", "S2", "S3"),
    berth = c("", "", ""),
    track = c("TA-1", "TB-1", "TC"),
    event = c("vacates", "vacates", "vacates")
  )

  out <- dplyr::tribble(
    ~track, ~dt, ~occupied, ~event,
    "TA-1", dt, F, "vacates",
    "TB-1", dt, F, "vacates",
    "TC", dt, F, "vacates"
  )

  expect_equal(preprocess_track_events(raw_track_events, asset_map), out)
})
