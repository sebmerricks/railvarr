library(dplyr)
source(testthat::test_path("fixtures/datagen/gen-centrix.R"))
source(testthat::test_path("fixtures/datagen/gen-timetable.R"))

gen_station_map <- function() {
  station_map <- dplyr::tribble(
    ~signal, ~station, ~T_journey, ~T_dwell,
    NA_character_, "Station 1", 0, 30,
    NA_character_, "Station 2", 90, 0,
    NA_character_, "Station 3", 330, 30
  )
}

gen_test_data <- function(n_trains = 10, t_between = 30,
                          n_tracks = 8, n_stations = 3,
                          start_dt = "2023-07-17 09:21:00",
                          station_map = gen_station_map()) {
  network_map <- gen_map(n_tracks)
  station_map <- station_map

  gen_centrix(n_trains, t_between, n_tracks, start_dt)
  gen_timetable(n_trains, t_between, start_dt,
                station_map$station, station_map$T_journey)
}
