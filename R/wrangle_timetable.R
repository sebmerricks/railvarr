env$event_mapping <- tribble(
  ~event, ~name,
  "O", "Originate",
  "P", "Pass",
  "A", "Arrive",
  "D", "Depart",
  "T", "Terminate"
)

env$stations <- NULL

#' Read timetable data
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
read_timetable <- function(path, names, ...) {
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
    rename(.data$event = .data$name)

  return(timetable)
}

#' Identifies the services that go through the relevant stations in either
#' direction
#' @export
identify_services <- function(timetable) {
  stopifnot(!is.null(env$stations), "Please input stations using
            set_stations(c('station1', 'station2', 'station3'))")

  start_station <- first(env$stations)
  end_station <- last(env$stations)

  relevant_services <- timetable %>%
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
}
