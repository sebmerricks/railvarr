validate_berth_matching <- function(berth_events_classes) {
  stop_if_not(c("berth", "train_id", "t_enters", "t_vacates", "group") %in%
                names(berth_events_classes))
  stop_if_not(is.character(berth_events_classes$berth))
  stop_if_not(is.numeric(berth_events_classes$train_id))
  stop_if_not(lubridate::is.POSIXct(berth_events_classes$t_enters))
  stop_if_not(lubridate::is.POSIXct(berth_events_classes$t_vacates))
  stop_if_not(is.character(berth_events_classes$group))
}
