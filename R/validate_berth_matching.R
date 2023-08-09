validate_berth_matching <- function(berth_events_classes) {
  stop_if_not(c("berth", "train_id", "t_enters", "t_vacates", "group") %in%
                names(berth_events_classes),
              msg = "berth_events must contain the columns 'berth', 'train_id', 't_enters', 't_vacates', and 'group'")
  stop_if_not(is.character(berth_events_classes$berth),
              msg = "column 'berth' must of type character()")
  stop_if_not(is.numeric(berth_events_classes$train_id),
              msg = "column 'train_id' must of type numeric()")
  stop_if_not(lubridate::is.POSIXct(berth_events_classes$t_enters),
              msg = "column 't_enters' must of type POSIXct()")
  stop_if_not(lubridate::is.POSIXct(berth_events_classes$t_vacates),
              msg = "column 't_vacates' must of type POSIXct()")
  stop_if_not(is.character(berth_events_classes$group),
              msg = "column 'group' must of type character()")
}
