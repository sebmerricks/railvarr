#' Estimate berth lengths
#' @param timetable_specification Timetable specification
#' @param id_matching Id matching
#' @param berth_events_groups Berth events groups
#' @param expected_journey_time Expected journey time in seconds from start of
#'   Centrix track to end
#' @param track_length Total length of Centrix track in miles
#' @export
estimate_berth_lengths <- function(timetable_specification,
                                   id_matching,
                                   berth_events_groups,
                                   expected_journey_time,
                                   track_length) {
  fast_trains <- timetable_specification %>%
    filter(group == "fast" &
             T_journey == expected_journey_time) %>%
    inner_join(id_matching, by = c("train_header", "dt_origin")) %>%
    inner_join(berth_events_groups, by = "train_id")

  expected_speed = track_length / (expected_journey_time / 3600)

  estimated_berth_lengths <- fast_trains %>%
    select(signal, berth, train_id, aspect, T_travel) %>%
    group_by(berth) %>%
    summarise(median_travel = median(T_travel)) %>%
    mutate(
      v.mph = expected_speed,
      L.miles = expected_speed * (median_travel / 3600),
      L.km = L.miles / 0.6213712,
      L = L.km * 1000
    )

  return(estimated_berth_lengths)
}
