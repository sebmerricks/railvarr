#' Estimate berth lengths
#' @export
estimate_berth_lengths <- function(timetable_specification,
                                   berth_events_groups,
                                   id_matching,
                                   distance.miles,
                                   speed.miles) {
  expected_journey_time = 3600 * distance.miles / speed.miles

  fast_trains_spec <- timetable_specification %>%
    filter(group == "fast") %>%
    filter(T_journey == expected_journey_time) %>%
    select(train_header, dt_origin) %>%
    inner_join(
      id_matching,
      by = c("train_header", "dt_origin")
    )

  estimated_berth_lengths <- berth_events_groups %>%
    semi_join(
      fast_trains_spec,
      by = "train_id"
      ) %>%
    select(signal, berth, train_id, aspect, T_travel) %>%
    group_by(berth) %>%
    summarise(
      median_travel.sec = median(T_travel)
    ) %>%
    mutate(
      v.mph = speed.miles,
      L.miles = speed.miles * (median_travel.sec / 3600),
      L.km = L.miles / 0.6213712
      # L.meters = ((80/km_to_miles)*1000)*(median_travel/3600)
    )

  return(estimated_berth_lengths)
}
