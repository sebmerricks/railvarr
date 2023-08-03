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

test_that("calculate_tsars correctly processes raw centrix data", {
  # map <- dplyr::tibble(
  #   signal = c("S1", "S1", "S2", "S2"),
  #   berth = c("A", "A", "B", "B"),
  #   track = c("TA", "TA", "TB", "TB"),
  #   event = c("enters", "vacates", "enters", "vacates"),
  #   geo = c("1", "1", "2", "2")
  # )
  # set_asset_mapping(map)
  #
  # centrix <- dplyr::tribble(
  #   ~asset, ~dt, ~transition, ~period,
  #   "TA", lubridate::as_datetime(100100), "UP to DN", 3,
  #   "TB", lubridate::as_datetime(100200), "UP to DN", 3,
  #   "TA", lubridate::as_datetime(100210), "DN to UP", 3,
  #   "TB", lubridate::as_datetime(100310), "DN to UP", 3,
  #   "TA", lubridate::as_datetime(100400), "UP to DN", 3,
  #   "TB", lubridate::as_datetime(100500), "UP to DN", 3,
  #   "TA", lubridate::as_datetime(100510), "DN to UP", 3,
  #   "TB", lubridate::as_datetime(100610), "DN to UP", 3,
  #   "S1 RGE", lubridate::as_datetime(100101), "DN to UP", 3,
  #   "S2 RGE", lubridate::as_datetime(100201), "DN to UP", 3,
  #   "S1 HGE", lubridate::as_datetime(100220), "DN to UP", 3,
  #   "S2 HGE", lubridate::as_datetime(100320), "DN to UP", 3,
  #   "S1 RGE", lubridate::as_datetime(100401), "DN to UP", 3,
  #   "S2 RGE", lubridate::as_datetime(100501), "DN to UP", 3,
  #   "S1 HGE", lubridate::as_datetime(100520), "DN to UP", 3,
  #   "S2 HGE", lubridate::as_datetime(100620), "DN to UP", 3
  # )
  # set_centrix(centrix)
  #
  # state_mapping <- dplyr::tribble(
  #   ~state, ~aspect,
  #   "RGE", factor("R", levels = c("R", "Y", "YY", "G")),
  #   "HGE", factor("Y", levels = c("R", "Y", "YY", "G")),
  #   "HHGE", factor("YY", levels = c("R", "Y", "YY", "G")),
  #   "DGE", factor("G", levels = c("R", "Y", "YY", "G"))
  # )
  # set_state_mapping(state_mapping)
  #
  # berth_events <- dplyr::tibble(
  #   signal = c("S1", "S2"),
  #   berth = c("A", "B"),
  #   train_id = c(1, 1),
  #   aspect = c("Y", "Y"),
  #   t_enters = c(lubridate::as_datetime(100400), lubridate::as_datetime(100500)),
  #   t_red_on = c(lubridate::as_datetime(100401), lubridate::as_datetime(100501)),
  #   t_enters_next = c(lubridate::as_datetime(100500), NA),
  #   t_vacates = c(lubridate::as_datetime(100510), lubridate::as_datetime(100610)),
  #   t_red_off = c(lubridate::as_datetime(100520), lubridate::as_datetime(100620))
  # ) %>%
  #   mutate(TSAR = lubridate::as.duration(t_red_off - t_red_on),
  #          T_onset = lubridate::as.duration(t_red_on - t_enters),
  #          T_clear = lubridate::as.duration(t_vacates - t_enters),
  #          T_offset = lubridate::as.duration(t_red_off - t_vacates),
  #          T_travel = lubridate::as.duration(t_enters_next - t_enters),
  #          T_coach = lubridate::as.duration(t_vacates - t_enters_next)) %>%
  #   mutate(across(TSAR:last_col(), as.double)) %>%
  #   mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")))
  #
  # expect_equal(calculate_tsars(), berth_events)
})
