validate_match_mapping <- function(match_mapping) {
  stop_if_not(c("group", "berth", "geo", "lb", "ub") %in% names(match_mapping))
  stop_if_not(is.character(match_mapping$group))
  stop_if_not(is.character(match_mapping$berth))
  stop_if_not(is.character(match_mapping$geo))
  stop_if_not(is.numeric(match_mapping$lb))
  stop_if_not(is.numeric(match_mapping$ub))
}
