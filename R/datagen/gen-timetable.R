gen_train <- function(stations, start_dt, stopping_pattern, travel_times,
                      dwell_times) {
  train_header = sprintf(stringi::stri_rand_strings(1, 5, "[A-Z0-9]"))

  start_dt = lubridate::as_datetime(start_dt)
  dt_origin = start_dt - lubridate::duration(21, units = "minutes")

  timetable <- dplyr::tribble(
    ~train_header, ~geo, ~dt_origin, ~event, ~wtt, ~t, ~delay, ~allow
  )

  delay = round(rnorm(1, mean = 0, sd = 4) * 2) /2

  for (i in seq_along(stations)) {
    station = stations[i]
    stopping = stopping_pattern[i]
    travel_time = travel_times[i]
    start_dt = start_dt + lubridate::duration(travel_time)
    wtt = start_dt
    t = wtt + lubridate::duration(delay, units = "minutes")

    if (stopping) {
      dwell = lubridate::duration(dwell_times[i])
      tt <- dplyr::tribble(
        ~train_header, ~geo, ~dt_origin, ~event, ~wtt, ~t, ~delay, ~allow,
        train_header, station, dt_origin, "Arrive", wtt, t, delay, 0,
        train_header, station, dt_origin, "Depart", wtt+dwell, t+dwell, delay, 0
      )
    } else {
      tt <- dplyr::tribble(
        ~train_header, ~geo, ~dt_origin, ~event, ~wtt, ~t, ~delay, ~allow,
        train_header, station, dt_origin, "Pass", wtt, t, delay, 0
      )
    }

    timetable <- dplyr::bind_rows(timetable, tt)
  }

  return(timetable)
}

gen_timetable <- function(n_trains, t_between, start_dt, stations, t_travels,
                          timetable_name = "timetable/timetable.csv") {
  # I will do only fast trains for now
  start_dt = lubridate::as_datetime(start_dt)
  t_between = lubridate::duration(t_between, units = "minutes")
  stopping_pattern = c(FALSE)
  for (i in seq_along(stations)) stopping_pattern = c(stopping_pattern, FALSE)

  timetable <- dplyr::tribble(
    ~train_header, ~geo, ~dt_origin, ~event, ~wtt, ~t, ~delay, ~allow
  )

  for (i in 1:n_trains) {
    tt <- gen_train(stations, start_dt, stopping_pattern,
                    t_travels, NA)
    timetable <- dplyr::bind_rows(timetable, tt)
    start_dt = start_dt + t_between
  }

  write_gen_data(timetable, timetable_name)
}
