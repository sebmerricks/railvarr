validate_timetable <- function(timetable) {
  stop_if_not(all(c("train_header", "dt_origin", "geo", "event", "wtt", "t",
                    "delay", "allow") %in% names(timetable)),
              msg = "Timetable data requires the columns 'train_header', 'dt_origin', 'geo', 'event', 'wtt', 't', 'delay', and 'allow'.")
  stop_if_not(is.character(timetable$train_header))
  stop_if_not(lubridate::is.POSIXct(timetable$dt_origin))
  stop_if_not(is.character(timetable$geo))
  stop_if_not(is.character(timetable$event))
  stop_if_not(all(timetable$event %in% c("Pass", "Arrive", "Depart", "Originate",
                                         "Terminate")))
  stop_if_not(lubridate::is.POSIXct(timetable$wtt))
  stop_if_not(lubridate::is.POSIXct(timetable$t))
  stop_if_not(is.numeric(timetable$delay))
  stop_if_not(is.numeric(timetable$allow))
}

validate_stations <- function(stations) {
  stop_if_not(is.list(stations),
              msg = "stations must be a list of character vectors.")
}
