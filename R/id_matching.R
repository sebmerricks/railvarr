#' Match Centrix and Timetable IDs
#'
#' Calculates matching IDs between Centrix and Timetable data.
#'
#' @param berth_events_classes Data frame containing berth-level Centrix data.
#' @param timetable_groups Data frame containing Timetable data.
#' @param match_mapping Data frame containing instructions for the matching.
#'
#' @returns Data frame containing a 1-1 mapping between Centrix IDs and
#'   Timetable IDs.
#'
#' @export
match_ids <- function(berth_events_classes, timetable_groups, match_mapping) {
  validate_berth_matching(berth_events_classes)
  validate_timetable_matching(timetable_subset)
  validate_match_mapping(match_mapping)

  berth_matching <- preprocess_berth_matching(berth_events_classes,
                                              match_mapping)

  timetable_matching <- preprocess_timetable_matching(timetable_groups,
                                                      match_mapping)

  return(match_observations(berth_matching, timetable_matching))
}

#' @importFrom dplyr filter select inner_join anti_join mutate if_else arrange
#'   group_by distinct
match_observations <- function(berth_matching, timetable_matching) {
  pair_start <- berth_matching %>%
    select(-"end_hour") %>%
    inner_join(timetable_matching %>%
                 select(-"end_hour"),
               by = c("group", "berth", "lb", "ub", "date", "start_hour", "geo"),
               relationship = "many-to-many")

  pair_end <- berth_matching %>%
    select(-"start_hour") %>%
    inner_join(timetable_matching %>%
                 select(-"start_hour"),
               by = c("group", "berth", "lb", "ub", "date", "end_hour", "geo"),
               relationship = "many-to-many")

  paired <- bind_rows(
    pair_start,
    pair_end %>% anti_join(pair_start,
                           by = c("train_id", "train_header", "dt_origin"))
  )

  matched <- paired %>%
    mutate(is_match = if_else(
      is.na(.data$t_Pass),
      (.data$t_enters + .data$lb < .data$t_Arrive &
         .data$t_vacates + .data$ub > .data$t_Depart),
      (.data$t_enters + .data$lb < .data$t_Pass &
         .data$t_enters + .data$ub > .data$t_Pass)
    )) %>%
    select("train_id", "train_header", "dt_origin", "berth", "geo", "t_enters",
           "t_Pass", "t_Arrive", "t_Depart", "t_vacates", "lb", "ub",
           "is_match") %>%
    arrange(.data$train_id, .data$dt_origin, .data$train_header) %>%
    group_by(.data$train_id, .data$train_header, .data$dt_origin) %>%
    filter(any(.data$is_match)) %>%
    distinct(.data$train_id, .data$train_header, .data$dt_origin) %>%
    ungroup()

  return(matched)
}
