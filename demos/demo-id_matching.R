load_all()
library(tidyverse)
source("helper.R")

map <- read_rds_demo("network_map")
set_map(map)

train_classes <- read_rds_demo("train_classes")
berth_events <- read_rds_demo("berth_events") %>%
  preprocess_berths(train_classes)

calling_patterns <- read_rds_demo("calling_patterns")
timetable <- read_rds_demo("timetable") %>%
  preprocess_timetable(calling_patterns)

group_map <- read_rds_demo("group_map")
group_map

sample <- read_rds_demo("sample")
berth_events <- berth_events %>% semi_join(sample)
timetable <- timetable %>% semi_join(sample)

matched_low <- match_ids(berth_events, timetable, group_map,
                         operators = "fuzzy", fuzzy_tolerance = 0.25,
                         tolerance = 0.25)

matched_mid <- match_ids(berth_events, timetable, group_map,
                         operators = "fuzzy", fuzzy_tolerance = 1,
                         tolerance = 1)

matched_high <- match_ids(berth_events, timetable, group_map,
                          operators = "fuzzy", fuzzy_tolerance = 4,
                          tolerance = 4)


matched_ids_default <- match_ids(berth_events, timetable, group_map,
                                 operators = "fuzzy", fuzzy_tolerance = 1,
                                 tolerance = 1) %>%
  combine_data(berth_events, timetable)

matched_ids_exact <- match_ids(berth_events, timetable, group_map,
                               operators = "exact", fuzzy_tolerance = 1,
                               tolerance = 1) %>%
  combine_data(berth_events, timetable)

matched_ids_loose <- match_ids(berth_events, timetable, group_map,
                               operators = "fuzzy", fuzzy_tolerance = 3,
                               tolerance = 3) %>%
  combine_data(berth_events, timetable)

matched_ids_tight <- match_ids(berth_events, timetable, group_map,
                               operators = "exact", fuzzy_tolerance = 0.25,
                               tolerance = 0.25) %>%
  combine_data(berth_events, timetable)

details <- function(ids, map) {
  print(glue::glue("Trains matched: {length(unique(ids$train_id))}"))
  return(accuracy(ids, map))
}

# Accuracy measures the average difference between the berth data (t_enters
# adjusted by the lower bound set in group_map) and the timetable data (t_Pass/
# t_Arrive) for all matches.
# Squared accuracy provides an overview of how variable the accuracy is.
accuracy_default <- details(matched_ids_default, group_map)
hist(accuracy_default$acc)

accuracy_exact <- details(matched_ids_exact, group_map)
hist(accuracy_exact$acc)

accuracy_loose <- details(matched_ids_loose, group_map)
hist(accuracy_loose$acc)

accuracy_tight <- details(matched_ids_tight, group_map)
hist(accuracy_tight$acc)
