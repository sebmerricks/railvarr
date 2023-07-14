minute = 60
second = 1

gen_train <- function(n_tracks, start_dt) {
  start_dt <- as.double(lubridate::as_datetime(start_dt))

  events <- dplyr::tribble(~asset, ~dt, ~transition)

  for (i in 0:n_tracks) {
    signal = paste0("S", i+1)
    track = paste0("T", LETTERS[i+1], i+1)

    t_enters = start_dt + i*minute
    t_red_on = t_enters + second
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
  }

  return(events)
}

write_gen_data <- function(data, filename) {
  path = testthat::test_path("fixtures/gen-data", filename)
  write.csv(data, path, row.names = FALSE)
}

gen_map <- function(n_tracks) {
  map <- dplyr::tribble(
    ~signal, ~berth, ~track, ~event
  )

  for (i in 1:n_tracks) {
    signal = paste0("S", i)
    berth = as.character(i)
    track = paste0("T", LETTERS[i])

    row <- dplyr::tribble(
      ~signal, ~berth, ~track, ~event,
      signal, berth, track, "enters",
      signal, berth, track, "vacates"
    )

    map <- bind_rows(map, row)
  }

  return(map)
}

gen_centrix <- function() {
  n_trains = as.integer(readline(prompt = "How many trains: "))
  n_tracks = as.integer(readline(prompt = "How many tracks: "))
  start_dt = readline(prompt = "Starting time: ")
  filename = readline(prompt = "File name: ")

  if (!grepl(filename, ".csv")) {
    filename = paste0(filename, ".csv")
  }

  centrix <- dplyr::tribble(~asset, ~dt, ~transition)

  start_dt <- as.double(lubridate::as_datetime(start_dt))

  for (i in 1:n_trains) {
    dt = start_dt + 20*(i-1)*minute
    events <- gen_train(n_tracks, dt)
    centrix <- bind_rows(centrix, events)
  }

  map <- gen_map(n_tracks)

  write_gen_data(centrix, filename)
  write_gen_data(map, "test_map.csv")
}

gen_centrix()
