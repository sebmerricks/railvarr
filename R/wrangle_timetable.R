env$event_mapping <- tribble(
  ~event, ~name,
  "O", "Originate",
  "P", "Pass",
  "A", "Arrive",
  "D", "Depart",
  "T", "Terminate"
)

env$stations <- NULL

get_stations <- function() {
  return(env$stations)
}

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

#' Reads and pre-processes timetable data
#'
#' Reads excel data from a given path and renames the columns according to
#' `names`
#'
#' @param path String pointing to the directory containing raw timetable data
#' @param names Named list containing column names and what to rename them to
#' @param ... Additional arguments passed to \code{\link{read_excel_files}}
#'
#' @export
#'
#' @importFrom dplyr %>% select rename_with left_join rename
#'
preprocess_timetable <- function(path, names, ...) {
  timetable <- read_excel_files(path, ...) %>%
    select(
      names(names) %>% stringr::str_subset(".+")
    ) %>%
    rename_with(~names[.x]) %>%
    select("train_id", "dt_origin", "x1", "x2", "geo", "event", "wtt", "t",
           "delay", starts_with("allow"))

  timetable <- timetable %>%
    left_join(event_mapping, by = "event") %>%
    select(-"event") %>%
    rename(event = .data$name)

  return(timetable)
}

#' Identifies the services that travel through the stations from start to end
#'
#' @param timetable Pre-processed timetable data
#'
#' @export
#'
#' @import dplyr
#'
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
