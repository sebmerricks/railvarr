env$event_mapping <- tribble(
  ~event, ~name,
  "O", "Originate",
  "P", "Pass",
  "A", "Arrive",
  "D", "Depart",
  "T", "Terminate"
)

#' Get Event Mapping
#'
#' This function retrieves the current event mapping table.
#'
#' @return The event mapping table as a tibble data frame.
#'
#' @details
#' The event mapping table is stored in the environment object named "env".
#'
#' @seealso [set_event_mapping]
#'
#' @export
get_event_mapping <- function() {
  return(env$event_mapping)
}

#' Set Event Mapping
#'
#' This function sets the event mapping table in the environment.
#'
#' @param mapping A tibble data frame representing the event mapping.
#'                The data frame should have columns "event" and "name".
#'                The default value is a predefined event mapping.
#'
#' @examples
#' # Set a custom event mapping
#' custom_mapping <- dplyr::tribble(
#'   ~event, ~name,
#'   "A", "Arrive",
#'   "D", "Depart"
#' )
#' set_event_mapping(custom_mapping)
#'
#' @export
set_event_mapping <- function(mapping) {
  stopifnot(is.data.frame(mapping))
  stopifnot(all(c("event", "name") %in% names(mapping)))

  env$event_mapping <- mapping
}

env$stations <- NULL

#' Get Stations
#'
#' This function retrieves the stations from the environment.
#'
#' @return A character vector containing the stations.
#'
#' @examples
#' # Create an environment containing the stations
#' env <- new.env()
#' env$stations <- c("Station A", "Station B", "Station C")
#'
#' # Call the function to get the stations
#' get_stations()
#'
#' @seealso
#' [set_stations]
#'
#' @export
get_stations <- function() {
  return(env$stations)
}

#' Set Stations
#'
#' This function sets the stations in the environment.
#'
#' @param stations A character vector containing the names of the stations.
#'
#' @details
#' The function validates the input stations to ensure they meet the
#' requirements: 1. The "stations" argument must be a non-empty character
#' vector. 2. Each element of the character vector must be a string (i.e., of
#' character type). If the input stations do not meet these conditions, the
#' function will raise an error.
#'
#' @examples
#' # Set stations using the function
#' set_stations(list('Station A', 'Station B', 'Station C'))
#'
#' @importFrom vetr vet
#'
#' @seealso
#' [get_stations]
#'
#' @export
set_stations <- function(stations) {
  vetr::vet(list(), stations, stop = TRUE)
  stopifnot("Please input stations using
            set_stations(c('station1', 'station2', 'station3'))" =
              length(stations) > 0)
  for (x in seq_along(stations)) {
    stopifnot("Every station must be a string! Please input stations using
              set_stations(c('station1', 'station2', 'station3'))" =
                is.character(stations[[x]]))
  }

  env$stations <- stations
}

#' Preprocess Timetable
#'
#' This function reads timetable data from Excel files, performs data
#' preprocessing, and returns a processed timetable.
#'
#' @param path The path to the directory containing the Excel files.
#' @param names A named character vector representing column name mappings for
#'   the timetable. The names should correspond to the column names in the Excel
#'   files. The names of the vector will be used as the target column names, and
#'   the values as the corresponding column names in the Excel files. The
#'   columns not listed in `names` will be ignored in the final timetable.
#' @param ... Additional arguments passed to the [read_excel_files] function.
#'
#' @return A preprocessed timetable with selected and renamed columns.
#'
#' @details The function reads Excel files from the specified directory using
#'   [read_excel_files]. It then performs the following preprocessing steps:
#' \itemize{
#'  \item{1. Selects columns based on the names provided in the `names`
#'  parameter. Columns with names that match the provided names (partially or
#'  fully) will be selected.}
#'  \item{2. Renames the selected columns using the corresponding names provided
#'  in `names`.} \item{3. Selects specific columns from the processed data to
#'  include in the final timetable.} \item{4. Performs a left join with the
#'  event mapping table retrieved using [get_event_mapping]. The event mapping
#'  table is stored in the environment object named "env". The event mapping
#'  should have two columns: "event" and "name".} \item{5. Renames the "name"
#'  column from the event mapping table to "event" in the timetable.} }
#'
#' @importFrom dplyr select rename_with starts_with left_join
#' @importFrom stringr str_subset
#'
#' @seealso [read_excel_files], [get_event_mapping]
#'
#' @export
preprocess_timetable <- function(path, names, ...) {
  timetable <- read_excel_files(path, ...) %>%
    select(
      names(names) %>% stringr::str_subset(".+")
    ) %>%
    rename_with(~names[.x]) %>%
    select("train_id", "dt_origin", "x1", "x2", "geo", "event", "wtt", "t",
           "delay", starts_with("allow"))

  timetable <- timetable %>%
    left_join(get_event_mapping(), by = "event") %>%
    select(-"event") %>%
    rename(event = .data$name)

  return(timetable)
}

#' Wrangle Timetable
#'
#' This function performs data wrangling on the given timetable and returns a
#' subset containing relevant information for specific stations and directions.
#'
#' @param timetable A data frame representing the timetable data.
#'
#' @return A data frame containing the wrangled timetable subset.
#'
#' @details The function performs the following steps to wrangle the timetable
#' data: \itemize{\item{1. Checks if the stations have been set using
#' [set_stations] and stops with an error if not.} \item{2. Identifies the start
#' and end stations based on the currently set stations.} \item{3. Determines
#' forward and backward directions based on the start and end stations.}
#' \item{4. Filters and selects services that pass through both the start and
#' end stations.} \item{5. Filters the services' direction (forward) and
#' relevant event types.} \item{6. Orders the services based on time, defines
#' their direction, and removes any edge cases.} \item{7. Selects the subset of
#' the timetable with relevant columns for the output.} \item{8. Converts
#' datetime columns to the "Europe/London" timezone using [lubridate::with_tz]}
#' }
#'
#' @importFrom dplyr select mutate group_by arrange filter distinct inner_join
#'   first if_else
#' @importFrom tidyr chop
#' @importFrom lubridate with_tz
#'
#' @seealso
#' [set_stations]
#'
#' @export
wrangle_timetable <- function(timetable) {
  stopifnot("Please input stations using
            set_stations(c('station1', 'station2', 'station3'))" =
              !is.null(env$stations))

  start_station <- first(env$stations)
  end_station <- last(env$stations)

  forwards <- glue::glue("{start_station[1]}-{end_station[1]}")
  backwards <- glue::glue("{end_station[1]}-{start_station[1]}")

  services_either_direction <- timetable %>%
    select(train_id, dt_origin, geo) %>%
    group_by(train_id, dt_origin) %>%
    tidyr::chop(geo) %>%
    ungroup() %>%
    mutate(is_subset = purrr::map_lgl(geo, ~{
      first <- any(.x %in% start_station)
      last <- any(.x %in% end_station)
      return(first & last)
    })) %>%
    filter(is_subset) %>%
    select(-geo, -is_subset)

  services_direction <- inner_join(
    services_either_direction,
    timetable,
    by = c("train_id", "dt_origin")
  ) %>%
    filter(geo %in% c(start_station, end_station)) %>%
    filter(event == "Pass" | event == "Arrive" | event == "Originate") %>%
    mutate(t_order = case_when(
      is.na(wtt) ~ t,
      is.na(t) ~ wtt,
      T ~ t
    )) %>%
    group_by(train_id, dt_origin) %>%
    arrange(t_order) %>%
    mutate(direction = if_else(
      first(geo) %in% start_station,
      forwards,
      backwards
    )) %>%
    ungroup() %>%
    distinct(train_id, dt_origin, direction)

  edge_cases <- inner_join(
    timetable,
    services_direction,
    by = c("train_id", "dt_origin")
  ) %>%
    filter(geo %in% env$stations) %>%
    filter(direction == forwards) %>%
    group_by(train_id, dt_origin) %>%
    filter(!first(geo) %in% start_station)

  services_direction <- services_direction %>%
    filter(!train_id %in% edge_cases$train_id)

  timetable_subset <- inner_join(
    timetable,
    services_direction,
    by = c("train_id", "dt_origin")
  ) %>%
    filter(geo %in% env$stations) %>%
    filter(direction == forwards) %>%
    mutate(allow = allow_perf + allow_path + allow_eng) %>%
    select(train_id, geo, dt_origin, event, wtt, t, delay, allow) %>%
    rename(train_header = train_id) %>%
    mutate(
      across(c(dt_origin, wtt, t),
             ~lubridate::with_tz(.x, tzone = "Europe/London"))
    )

  return(timetable_subset)
}
