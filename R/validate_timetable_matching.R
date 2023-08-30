validate_timetable_matching <- function(timetable) {
  stop_if_not(all(c("train_header", "dt_origin", "group", "geo", "event", "wtt", "t") %in%
                names(timetable)),
              msg = "timetable must contain the columns 'train_header', 'dt_origin', 'group', 'geo', 'event', 'wtt', and 't'")
  stop_if_not(is.character(timetable$train_header),
              msg = "column 'train_header' must be of type character()")
  stop_if_not(lubridate::is.POSIXct(timetable$dt_origin),
              msg = "column 'dt_origin' must be of type POSIXct()")
  stop_if_not(is.character(timetable$group),
              msg = "column 'group' must be of type character()")
  stop_if_not(is.character(timetable$geo),
              msg = "column 'geo' must be of type character()")
  stop_if_not(is.character(timetable$event),
              msg = "column 'event' must be of type character()")
  stop_if_not(all(timetable$event %in% c("Pass", "Arrive", "Depart")),
              msg = "every element in column 'event' must be one of 'Pass', 'Arrive', or 'Depart'")
  stop_if_not(lubridate::is.POSIXct(timetable$wtt),
              msg = "column 'wtt' must be of type POSIXct()")
  stop_if_not(lubridate::is.POSIXct(timetable$t),
              msg = "column 't' must be of type POSIXct()")
}
