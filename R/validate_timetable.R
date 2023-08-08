validate_timetable <- function(timetable) {
  stop_if_not(all(c("train_header", "dt_origin", "geo", "event", "wtt", "t",
                    "delay", "allow") %in% names(timetable)),
              msg = "Timetable data requires the columns 'train_header', 'dt_origin', 'geo', 'event', 'wtt', 't', 'delay', and 'allow'.")
  stop_if_not(is.character(timetable$train_header),
              msg = "train_header must be of type character()")
  stop_if_not(lubridate::is.POSIXct(timetable$dt_origin),
              msg = "dt_origin must be of type POSIXct()")
  stop_if_not(is.character(timetable$geo),
              msg = "geo must be of type character()")
  stop_if_not(is.character(timetable$event),
              msg = "event must be of type character()")
  stop_if_not(all(timetable$event %in% c("Pass", "Arrive", "Depart", "Originate",
                                         "Terminate")),
              msg = "event must only contain the following: 'Pass', 'Arrive', 'Depart', 'Originate', 'Terminate'")
  stop_if_not(lubridate::is.POSIXct(timetable$wtt),
              msg = "wtt must be of type POSIXct()")
  stop_if_not(lubridate::is.POSIXct(timetable$t),
              msg = "t must be of type POSIXct()")
  stop_if_not(is.numeric(timetable$delay),
              msg = "delay must be of type numeric()")
  stop_if_not(is.numeric(timetable$allow),
              msg = "allow must be of type numeric()")
}

validate_stations <- function(stations) {
  stop_if_not(is.list(stations),
              msg = "stations must be a list of character vectors.")
}
