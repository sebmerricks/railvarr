test_that("wrangle_centrix successfully wraps other functions", {
  raw_track_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "TA TR", lubridate::as_datetime(100), "UP to DN",
    "TA TR", lubridate::as_datetime(210), "DN to UP",
    "TB TR", lubridate::as_datetime(200), "UP to DN",
    "TB TR", lubridate::as_datetime(310), "DN to UP",
    "TC TR", lubridate::as_datetime(300), "UP to DN",
    "TC TR", lubridate::as_datetime(410), "DN to UP",
    "TA TR", lubridate::as_datetime(1100), "UP to DN",
    "TA TR", lubridate::as_datetime(1210), "DN to UP",
    "TB TR", lubridate::as_datetime(1200), "UP to DN",
    "TB TR", lubridate::as_datetime(1310), "DN to UP",
    "TC TR", lubridate::as_datetime(1300), "UP to DN",
    "TC TR", lubridate::as_datetime(1410), "DN to UP",
    "TA TR", lubridate::as_datetime(2100), "UP to DN",
    "TA TR", lubridate::as_datetime(2210), "DN to UP",
    "TB TR", lubridate::as_datetime(2200), "UP to DN",
    "TB TR", lubridate::as_datetime(2310), "DN to UP",
    "TC TR", lubridate::as_datetime(2300), "UP to DN",
    "TC TR", lubridate::as_datetime(2410), "DN to UP",
    "TA TR", lubridate::as_datetime(2300), "UP to DN",
    "TA TR", lubridate::as_datetime(2410), "DN to UP",
    "TB TR", lubridate::as_datetime(2400), "UP to DN",
    "TB TR", lubridate::as_datetime(2510), "DN to UP",
    "TC TR", lubridate::as_datetime(2500), "UP to DN",
    "TC TR", lubridate::as_datetime(2610), "DN to UP",
    "TA TR", lubridate::as_datetime(3100), "UP to DN",
    "TA TR", lubridate::as_datetime(3210), "DN to UP",
    "TB TR", lubridate::as_datetime(3200), "UP to DN",
    "TB TR", lubridate::as_datetime(3310), "DN to UP",
    "TA TR", lubridate::as_datetime(4100), "UP to DN",
    "TA TR", lubridate::as_datetime(4210), "DN to UP",
    "TB TR", lubridate::as_datetime(4200), "UP to DN",
    "TB TR", lubridate::as_datetime(4310), "DN to UP",
    "TB TR", lubridate::as_datetime(4320), "UP to DN",
    "TB TR", lubridate::as_datetime(4450), "DN to UP",
    "TC TR", lubridate::as_datetime(4300), "UP to DN",
    "TC TR", lubridate::as_datetime(4410), "DN to UP",
    "TA TR", lubridate::as_datetime(5100), "UP to DN",
    "TA TR", lubridate::as_datetime(5210), "DN to UP",
    "TA TR", lubridate::as_datetime(5220), "DN to UP",
    "TB TR", lubridate::as_datetime(5200), "UP to DN",
    "TB TR", lubridate::as_datetime(5310), "DN to UP",
    "TC TR", lubridate::as_datetime(5300), "UP to DN",
    "TC TR", lubridate::as_datetime(5410), "DN to UP",
    "TA TR", lubridate::as_datetime(6100), "UP to DN",
    "TA TR", lubridate::as_datetime(6210), "DN to UP",
    "TB TR", lubridate::as_datetime(6200), "UP to DN",
    "TB TR", lubridate::as_datetime(6310), "DN to UP",
    "TC TR", lubridate::as_datetime(6300), "UP to DN",
    "TC TR", lubridate::as_datetime(6350), "UP to DN",
    "TC TR", lubridate::as_datetime(6410), "DN to UP",
  )

  raw_signal_events <- dplyr::tribble(
    ~asset, ~dt, ~transition,
    "S1 DGE", lubridate::as_datetime(101), "UP to DN",
    "S1 RGE", lubridate::as_datetime(101), "DN to UP",
    "S1 RGE", lubridate::as_datetime(220), "UP to DN",
    "S1 HGE", lubridate::as_datetime(220), "DN to UP",
    "S2 DGE", lubridate::as_datetime(201), "UP to DN",
    "S2 RGE", lubridate::as_datetime(201), "DN to UP",
    "S2 RGE", lubridate::as_datetime(320), "UP to DN",
    "S2 HGE", lubridate::as_datetime(320), "DN to UP",
    "S3 DGE", lubridate::as_datetime(301), "UP to DN",
    "S3 RGE", lubridate::as_datetime(301), "DN to UP",
    "S3 RGE", lubridate::as_datetime(420), "UP to DN",
    "S3 HGE", lubridate::as_datetime(420), "DN to UP",
    "S4 DGE", lubridate::as_datetime(401), "UP to DN",
    "S4 RGE", lubridate::as_datetime(401), "DN to UP",
    "S1 DGE", lubridate::as_datetime(1101), "UP to DN",
    "S1 RGE", lubridate::as_datetime(1101), "DN to UP",
    "S1 RGE", lubridate::as_datetime(1220), "UP to DN",
    "S1 HGE", lubridate::as_datetime(1220), "DN to UP",
    "S2 DGE", lubridate::as_datetime(1201), "UP to DN",
    "S2 RGE", lubridate::as_datetime(1201), "DN to UP",
    "S2 RGE", lubridate::as_datetime(1320), "UP to DN",
    "S2 HGE", lubridate::as_datetime(1320), "DN to UP",
    "S3 DGE", lubridate::as_datetime(1301), "UP to DN",
    "S3 RGE", lubridate::as_datetime(1301), "DN to UP",
    "S3 RGE", lubridate::as_datetime(1420), "UP to DN",
    "S3 HGE", lubridate::as_datetime(1420), "DN to UP",
    "S4 DGE", lubridate::as_datetime(1401), "UP to DN",
    "S4 RGE", lubridate::as_datetime(1401), "DN to UP",
    "S1 DGE", lubridate::as_datetime(2101), "UP to DN",
    "S1 RGE", lubridate::as_datetime(2101), "DN to UP",
    "S1 RGE", lubridate::as_datetime(2220), "UP to DN",
    "S1 HGE", lubridate::as_datetime(2220), "DN to UP",
    "S2 DGE", lubridate::as_datetime(2201), "UP to DN",
    "S2 RGE", lubridate::as_datetime(2201), "DN to UP",
    "S2 RGE", lubridate::as_datetime(2320), "UP to DN",
    "S2 HGE", lubridate::as_datetime(2320), "DN to UP",
    "S3 DGE", lubridate::as_datetime(2301), "UP to DN",
    "S3 RGE", lubridate::as_datetime(2301), "DN to UP",
    "S3 RGE", lubridate::as_datetime(2420), "UP to DN",
    "S3 HGE", lubridate::as_datetime(2420), "DN to UP",
    "S4 DGE", lubridate::as_datetime(2401), "UP to DN",
    "S4 RGE", lubridate::as_datetime(2401), "DN to UP",
    "S1 DGE", lubridate::as_datetime(2301), "UP to DN",
    "S1 RGE", lubridate::as_datetime(2301), "DN to UP",
    "S1 RGE", lubridate::as_datetime(2420), "UP to DN",
    "S1 HGE", lubridate::as_datetime(2420), "DN to UP",
    "S2 DGE", lubridate::as_datetime(2401), "UP to DN",
    "S2 RGE", lubridate::as_datetime(2401), "DN to UP",
    "S2 RGE", lubridate::as_datetime(2520), "UP to DN",
    "S2 HGE", lubridate::as_datetime(2520), "DN to UP",
    "S3 DGE", lubridate::as_datetime(2501), "UP to DN",
    "S3 RGE", lubridate::as_datetime(2501), "DN to UP",
    "S3 RGE", lubridate::as_datetime(2620), "UP to DN",
    "S3 HGE", lubridate::as_datetime(2620), "DN to UP",
    "S4 DGE", lubridate::as_datetime(2601), "UP to DN",
    "S4 RGE", lubridate::as_datetime(2601), "DN to UP",
    "S1 DGE", lubridate::as_datetime(3101), "UP to DN",
    "S1 RGE", lubridate::as_datetime(3101), "DN to UP",
    "S1 RGE", lubridate::as_datetime(3220), "UP to DN",
    "S1 HGE", lubridate::as_datetime(3220), "DN to UP",
    "S2 DGE", lubridate::as_datetime(3201), "UP to DN",
    "S2 RGE", lubridate::as_datetime(3201), "DN to UP",
    "S2 RGE", lubridate::as_datetime(3320), "UP to DN",
    "S2 HGE", lubridate::as_datetime(3320), "DN to UP",
    "S3 DGE", lubridate::as_datetime(3301), "UP to DN",
    "S3 RGE", lubridate::as_datetime(3301), "DN to UP",
    "S3 RGE", lubridate::as_datetime(3420), "UP to DN",
    "S3 HGE", lubridate::as_datetime(3420), "DN to UP",
    "S4 DGE", lubridate::as_datetime(3401), "UP to DN",
    "S4 RGE", lubridate::as_datetime(3401), "DN to UP",
    "S1 DGE", lubridate::as_datetime(4101), "UP to DN",
    "S1 RGE", lubridate::as_datetime(4101), "DN to UP",
    "S1 RGE", lubridate::as_datetime(4220), "UP to DN",
    "S1 HGE", lubridate::as_datetime(4220), "DN to UP",
    "S1 DGE", lubridate::as_datetime(4151), "UP to DN",
    "S1 RGE", lubridate::as_datetime(4151), "DN to UP",
    "S1 HHGE", lubridate::as_datetime(4270), "UP to DN",
    "S1 HGE", lubridate::as_datetime(4270), "DN to UP",
    "S2 DGE", lubridate::as_datetime(4201), "UP to DN",
    "S2 RGE", lubridate::as_datetime(4201), "DN to UP",
    "S2 RGE", lubridate::as_datetime(4320), "UP to DN",
    "S2 HGE", lubridate::as_datetime(4320), "DN to UP",
    "S3 DGE", lubridate::as_datetime(4301), "UP to DN",
    "S3 RGE", lubridate::as_datetime(4301), "DN to UP",
    "S3 RGE", lubridate::as_datetime(4420), "UP to DN",
    "S3 HGE", lubridate::as_datetime(4420), "DN to UP",
    "S4 DGE", lubridate::as_datetime(4401), "UP to DN",
    "S4 RGE", lubridate::as_datetime(4401), "DN to UP",
  )

  raw_centrix <- bind_rows(raw_track_events, raw_signal_events)

  asset_map <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event,
    "S1", "A", "TA", "enters",
    "S1", "A", "TA", "vacates",
    "S2", "B", "TB", "enters",
    "S2", "B", "TB", "vacates",
    "S3", "C", "TC", "enters",
    "S3", "C", "TC", "vacates"
  )

  berth_events <- dplyr::tribble(
    ~signal, ~berth, ~train_id, ~aspect, ~t_enters, ~t_red_on, ~t_enters_next, ~t_vacates, ~t_red_off, ~TSAR, ~T_onset, ~T_clear, ~T_offset, ~T_travel, ~T_coach,
    "S1", "A", 1, "Y", 1100, 1101, 1200, 1210, 1220, 119, 1, 110, 10, 100, 10,
    "S2", "B", 1, "Y", 1200, 1201, 1300, 1310, 1320, 119, 1, 110, 10, 100, 10,
    "S3", "C", 1, "Y", 1300, 1301, NA, 1410, 1420, 119, 1, 110, 10, NA, NA,
    "S1", "A", 2, "Y", 2100, 2101, 2200, 2210, 2220, 119, 1, 110, 10, 100, 10,
    "S2", "B", 2, "Y", 2200, 2201, 2300, 2310, 2320, 119, 1, 110, 10, 100, 10,
    "S3", "C", 2, "Y", 2300, 2301, NA, 2410, 2420, 119, 1, 110, 10, NA, NA,
    "S1", "A", 3, "Y", 2300, 2301, 2400, 2410, 2420, 119, 1, 110, 10, 100, 10,
    "S2", "B", 3, "Y", 2400, 2401, 2500, 2510, 2520, 119, 1, 110, 10, 100, 10,
    "S3", "C", 3, "Y", 2500, 2501, NA, 2610, 2620, 119, 1, 110, 10, NA, NA
  ) %>%
    mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G"))) %>%
    mutate(across(starts_with("t", ignore.case=FALSE), lubridate::as_datetime),
           across(starts_with("T", ignore.case=FALSE), lubridate::as.duration),
           across(starts_with("T", ignore.case=FALSE), as.double),
           train_id = as.numeric(train_id))

  expect_equal(wrangle_centrix(raw_centrix, asset_map),
               berth_events)
})
