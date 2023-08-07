wrangle_timetable <- function(timetable,
                              stations,
                              event_map = event_mapping) {
  # validate timetable
  # validate stations
  # validate event mapping

  timetable <- timetable %>%
    inner_join(event_map) %>%
    select(-"event") %>%
    rename(event = name)

  start <- first(stations)
  end <- last(stations)
  forwards <- paste0(start[1], "-", end[1])
  backwards <- paste0(end[1], "-", start[1])

  timetable_subset <- timetable %>%
    find_relevant_services() %>%
    filter_forward_services() %>%
    remove_edge_cases() %>%
    subset_timetable() %>%
    find_calling_patterns()

  return(timetable_subset)
}
