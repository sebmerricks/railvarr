minute = 60
second = 1

gen_centrix_train <- function(n_tracks, start_dt, train_id) {
  start_dt <- as.double(lubridate::as_datetime(start_dt))

  events <- dplyr::tribble(~asset, ~dt, ~transition)
  berth_events <- dplyr::tribble(~signal, ~berth, ~train_id, ~aspect, ~t_enters,
                                 ~t_red_on, ~t_enters_next, ~t_vacates,
                                 ~t_red_off, ~TSAR, ~T_onset, ~T_clear,
                                 ~T_offset, ~T_travel, ~T_coach)

  for (i in 0:n_tracks) {
    signal = paste0("S", i+1)
    track = paste0("T", LETTERS[i+1], i+1)
    berth = LETTERS[i+1]

    t_enters = start_dt + i*minute
    t_red_on = t_enters + second
    t_enters_next = t_enters + minute
    t_vacates = t_enters + minute + 10*second
    t_red_off = t_vacates + 5*second

    signal_asset_on = paste0(signal, " RGE")
    signal_asset_off = paste0(signal, " HGE")
    track_asset = paste0(track, " TR")

    df <- dplyr::tribble(
      ~asset, ~dt, ~transition,
      track_asset, t_enters, "UP to DN",
      signal_asset_on, t_red_on, "DN to UP",
      track_asset, t_vacates, "DN to UP",
      signal_asset_off, t_red_off, "DN to UP"
    )

    events <- dplyr::bind_rows(events, df)

    if (i != n_tracks+1) {
      t_enters = lubridate::as_datetime(t_enters)
      t_red_on = lubridate::as_datetime(t_red_on)
      t_enters_next = lubridate::as_datetime(t_enters_next)
      if (i == n_tracks - 1+1) t_enters_next = NA
      t_vacates = lubridate::as_datetime(t_vacates)
      t_red_off = lubridate::as_datetime(t_red_off)

      TSAR = lubridate::as.duration(t_red_off - t_red_on)
      T_onset = lubridate::as.duration(t_red_on - t_enters)
      T_clear = lubridate::as.duration(t_vacates - t_enters)
      T_offset = lubridate::as.duration(t_red_off - t_vacates)
      if (i < n_tracks - 1+1) {
        T_travel = lubridate::as.duration(t_enters_next - t_enters)
        T_coach = lubridate::as.duration(t_vacates - t_enters_next)
      }

      berth_df <- dplyr::tribble(
        ~signal, ~berth, ~train_id, ~aspect, ~t_enters, ~t_red_on, ~t_enters_next,
        ~t_vacates, ~t_red_off, ~TSAR, ~T_onset, ~T_clear, ~T_offset, ~T_travel,
        ~T_coach,
        signal, berth, train_id, "Y", t_enters, t_red_on, t_enters_next,
        t_vacates, t_red_off, TSAR, T_onset, T_clear, T_offset, T_travel,
        T_coach
      )

      berth_events <- dplyr::bind_rows(berth_events, berth_df)
    }
  }

  berth_events <- berth_events %>%
    filter(signal != paste0("S", n_tracks + 1))

  return(list(events, berth_events))
}

write_gen_data <- function(data, filename) {
  path = testthat::test_path("fixtures/data", filename)
  write.csv(data, path, row.names = FALSE)
}

gen_map <- function(n_tracks) {
  map <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event
  )

  for (i in 1:(n_tracks+1)) {
    signal = paste0("S", i)
    berth = LETTERS[i]
    track = paste0("T", LETTERS[i])

    row <- dplyr::tribble(
      ~signal, ~berth, ~track, ~event,
      signal, berth, track, "enters",
      signal, berth, track, "vacates"
    )

    if (i == n_tracks + 1) {
      row <- row %>% filter(event == "enters")
    }

    map <- dplyr::bind_rows(map, row)
  }

  return(map)
}

gen_centrix <- function(n_trains, t_between, n_tracks, start_dt,
                        centrix_name = "centrix/gen_centrix.csv",
                        berth_name = "gen_berth_events.csv") {
  if (!grepl(centrix_name, ".csv")) centrix_name = paste0(centrix_name, ".csv")
  if (!grepl(berth_name, ".csv")) berth_name = paste0(berth_name, ".csv")

  centrix <- dplyr::tribble(~asset, ~dt, ~transition)
  berth_events <- dplyr::tribble(~signal, ~berth, ~train_id, ~aspect, ~t_enters,
                                 ~t_red_on, ~t_enters_next, ~t_vacates,
                                 ~t_red_off, ~TSAR, ~T_onset, ~T_clear,
                                 ~T_offset, ~T_travel, ~T_coach)

  start_dt <- as.double(lubridate::as_datetime(start_dt))

  for (i in 1:n_trains) {
    dt = start_dt + t_between*(i-1)*minute
    data <- gen_centrix_train(n_tracks, dt, i-1)
    events <- data[[1]]
    berths <- data[[2]]
    centrix <- dplyr::bind_rows(centrix, events)
    if (i > 1)
      berth_events <- dplyr::bind_rows(berth_events, berths)
  }

  map <- gen_map(n_tracks)

  write_gen_data(centrix, centrix_name)
  write_gen_data(berth_events, berth_name)
  write_gen_data(map, "test_map.csv")
}
