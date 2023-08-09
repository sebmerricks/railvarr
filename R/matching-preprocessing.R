#' @importFrom dplyr select inner_join group_by mutate first last ungroup
preprocess_berth_matching <- function(berth_events_classes, match_mapping) {
  berth_events_mapping <- berth_events_classes %>%
    select("train_id", "group", "berth", "t_enters", "t_vacates") %>%
    inner_join(match_mapping,
               by = c("group", "berth"))

  berth_events_times <- berth_events_mapping %>%
    group_by(.data$train_id) %>%
    mutate(first = first(.data$t_enters),
           last = last(.data$t_vacates)) %>%
    ungroup() %>%
    mutate(date = lubridate::date(.data$first),
           start_hour = lubridate::hour(.data$first),
           end_hour = lubridate::hour(.data$last)) %>%
    select(-"first", -"last")

  return(berth_events_times)
}

#' @importFrom dplyr select inner_join group_by mutate first last ungroup filter
#'   arrange
preprocess_timetable_matching <- function(timetable_groups, match_mapping) {
  timetable_subset_mapping <- timetable_groups %>%
    select("train_header", "dt_origin", "group", "geo", "event", "wtt", "t") %>%
    inner_join(match_mapping,
               by = c("group", "geo"))

  timetable_subset_times <- timetable_subset_mapping %>%
    filter(!is.na(.data$t)) %>%
    group_by(.data$train_header, .data$dt_origin) %>%
    mutate(first = first(.data$t),
           last = last(.data$t)) %>%
    ungroup() %>%
    mutate(date = lubridate::date(.data$first),
           start_hour = lubridate::hour(.data$first),
           end_hour = lubridate::hour(.data$last)) %>%
    arrange(.data$date, .data$t) %>%
    select(-"first", -"last")

  timetable_wider <- timetable_matching %>%
    filter(.data$event %in% c("Pass", "Arrive", "Depart")) %>%
    tidyr::pivot_wider(
      id_cols = c("train_header", "dt_origin", "group", "geo", "date",
                  "start_hour", "end_hour", "berth", "lb", "ub"),
      values_from = c("wtt", "t"),
      names_from = "event"
    )

  return(timetable_wider)
}
