validate_state_mapping <- function(state_mapping) {
  stop_if_not(all(c("state", "aspect") %in% names(state_mapping)),
              msg = "State mapping requires the columns 'state' and 'aspect'.")
  states <- state_mapping$state
  stop_if_not(is.character(states),
              msg = "Column 'state' must be of type character().")
  aspects <- state_mapping$aspect
  stop_if_not(is.factor(aspects),
              msg = "Column 'aspect' must be of type factor().")
}
