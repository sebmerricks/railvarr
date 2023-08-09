validate_timetable_matching <- function(timetable_groups) {
  stop_if_not(c("train_header", "dt_origin", "group", "geo", "event", "wtt",
                "t") %in% names(timetable_groups))
  stop_if_not(is.character(timetable_groups$train_header))
  stop_if_not(lubridate::is.POSIXct(timetable_groups$dt_origin))
  stop_if_not(is.character(timetable_groups$group))
  stop_if_not(is.character(timetable_groups$geo))
  stop_if_not(is.character(timetable_groups$event))
  stop_if_not(lubridate::is.POSIXct(timetable_groups$wtt))
  stop_if_not(lubridate::is.POSIXct(timetable_groups$t))
}
