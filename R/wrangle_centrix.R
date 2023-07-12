#' Read raw data from multiple files
#'
#' @param path A string pointing to the directory pointing to the relevant files.
#' @param names A character vector containing the column names.
#' @param types A column specification created with \code{\link[readr]{cols}}.
#'
#' @export
#'
#' @examples
#' # Create a temporary directory for the example
#' tempdir <- tempdir()
#' tempdir <- file.path(tempdir, "example")
#' dir.create(tempdir)
#'
#' # Generate a sample data frame
#' df <- data.frame(
#' x = c("a", "b", "c"),
#' y = c(1, 2, 3)
#' )
#'
#' # Define the file path
#' file_path <- file.path(tempdir, "example.csv")
#'
#' # Save the data frame as a CSV file
#' write.csv(df, file = file_path, row.names = FALSE)
#'
#' # Define column names and types
#' names <- c("x", "y")
#' types <- readr::cols(
#' "x" = readr::col_character(),
#' "y" = readr::col_integer()
#' )
#'
#' # Read the data using read_centrix()
#' read_files(tempdir, names, types)
#'
#' # Delete the temporary directory
#' unlink(tempdir, recursive = TRUE)
#'
read_files <- function(path, names = NULL, types = NULL) {
  # find all the files inside the directory at `path`
  filenames <- glue::glue("{path}/{list.files(path)}")
  # throw an error if the directory is empty
  stopifnot("`path` must not be an empty directory" = length(filenames) > 0)

  raw_data <- readr::read_csv(
    filenames,
    col_names = names,
    col_types = types,
    skip = 1L
  )

  return(raw_data)
}

#' Split raw events into separate signal and track events
#'
#' @param raw_events Data frame containing raw data.
#' @param is_track Expression determining how to split tracks and signals.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate group_by group_split
#'
split_signal_track_events <- function(raw_events,
                                      is_track =
                                        quote(stringr::str_starts(asset, "T"))
                                      ) {
  tpl_raw_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  stopifnot(vetr::alike(tpl_raw_events, raw_events))

  events <- raw_events %>%
    mutate(is_track = eval(is_track)) %>%
    group_by(is_track) %>%
    group_split(.keep = F)

  return(events)
}

#' Preprocess raw signal events
#'
#' @param raw_signal_events Raw signal data.
#' @param state_mapping Data frame defining how to convert signal states to
#'        aspects.
#'
#' @export
#'
#' @import dplyr
#'
preprocess_signal_events <- function(raw_signal_events, state_mapping) {
  # Define the expected data structure
  tpl_signal_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  # Check whether the raw data matches the expected structure
  stopifnot(vetr::alike(tpl_signal_events, raw_signal_events))

  # Define the expected structure for state_mapping
  tpl_state_mapping <- data.frame(
    state = character(),
    aspect = factor()
  )
  # Check the structure
  stopifnot(vetr::alike(tpl_state_mapping, state_mapping))

  # Identify signal IDs and states
  signal_events <- raw_signal_events %>%
    mutate(
      signal = stringr::str_split_i(asset, " ", i = 1),
      state = stringr::str_split_i(asset, " ", i = 2)
    ) %>%
    filter(transition == "DN to UP") %>%
    select(signal, dt, state, period)

  # Convert states to aspects
  aspect_events <- signal_events %>%
    inner_join(
      state_mapping,
      by = "state"
    ) %>%
    arrange(signal, dt) %>%
    select(period, signal, dt, aspect) %>%
    group_by(signal) %>%
    mutate(past_aspect = lag(aspect)) %>%
    ungroup()

  return(aspect_events)
}

#' Preprocess raw track data
#'
#' @param raw_track_events Data frame containing raw track data
#' @param tracks Data frame containing list of tracks
#'
#' @export
#'
#' @importFrom dplyr %>% mutate if_else select rename arrange semi_join
#'
preprocess_track_events <- function(raw_track_events, tracks) {
  tpl_track_events <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  stopifnot(vetr::alike(tpl_track_events, raw_track_events))

  tpl_tracks <- data.frame(
    track = character()
  )
  stopifnot(vetr::alike(tpl_tracks, tracks))

  track_activations <- raw_track_events %>%
    mutate(occupied = if_else(transition == "UP to DN", T, F)) %>%
    select(-transition) %>%
    rename(track = asset) %>%
    mutate(track = stringr::str_extract(track, "^([A-z])+(-[0-9])?")) %>%
    select(period, track, dt, occupied) %>%
    arrange(track, dt) %>%
    mutate(event = if_else(occupied, "enters", "vacates")) %>%
    mutate(date = lubridate::as_date(dt)) %>%
    semi_join(tracks)

  return(track_activations)
}

#' Wrangle raw Centrix data
#'
#' @param raw_signal_events Data frame containing raw signal data.
#' @param raw_track_events Data frame containing raw track data.
#'
#' @export
#'
wrangle_centrix <- function(raw_signal_events, raw_track_events) {
  # Define the expected data structure
  template <- data.frame(
    asset = character(),
    dt = lubridate::POSIXct(),
    transition = character(),
    period = numeric()
  )
  # Check whether the raw data matches the expected structure
  vetr::vet(template, raw_signal_events)
  vetr::vet(template, raw_track_events)
}


