validate_match_mapping <- function(mapping) {
  stop_if_not(c("group", "berth", "geo", "lb", "ub") %in% names(mapping),
              msg = "mapping must contain the columns 'group', 'berth', 'geo', 'lb', and 'ub'")
  stop_if_not(is.character(mapping$group),
              msg = "column 'group' must be of type character()")
  stop_if_not(is.character(mapping$berth),
              msg = "column 'berth' must be of type character()")
  stop_if_not(is.character(mapping$geo),
              msg = "column 'geo' must be of type character()")
  stop_if_not(is.numeric(mapping$lb),
              msg = "column 'lb' must be of type numeric()")
  stop_if_not(is.numeric(mapping$ub),
              msg = "column 'ub' must be of type numeric()")
}
