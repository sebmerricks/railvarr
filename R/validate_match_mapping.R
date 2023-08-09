validate_match_mapping <- function(match_mapping) {
  stop_if_not(c("group", "berth", "geo", "lb", "ub") %in% names(match_mapping),
              msg = "match_mapping must contain the columns 'group', 'berth', 'geo', 'lb', and 'ub'")
  stop_if_not(is.character(match_mapping$group),
              msg = "column 'group' must be of type character()")
  stop_if_not(is.character(match_mapping$berth),
              msg = "column 'berth' must be of type character()")
  stop_if_not(is.character(match_mapping$geo),
              msg = "column 'geo' must be of type character()")
  stop_if_not(is.numeric(match_mapping$lb),
              msg = "column 'lb' must be of type numeric()")
  stop_if_not(is.numeric(match_mapping$ub),
              msg = "column 'ub' must be of type numeric()")
}
