load_all()
library(tidyverse)

library(data.table)
library(digest)

anonymize <- function(x, algo="crc32") {
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo),
                       FUN.VALUE = "", USE.NAMES = TRUE)
  unname(unq_hashes[x])
}

anonymise <- function(data, columns) {
  return(as.data.frame(as.data.table(data)[,(columns) := lapply(.SD, anonymize), .SDcols = columns]))
}

berth_events <- read_rds_test("tolerance/berth_events.rds")
calling_patterns <- read_rds_test("tolerance/calling_patterns.rds")
group_map <- read_rds_test("tolerance/group_map.rds")
network_map <- read_rds_test("tolerance/network_map.rds")
sample <- read_rds_test("tolerance/sample.rds")
timetable <- read_rds_test("tolerance/timetable.rds")
train_classes <- read_rds_test("tolerance/train_classes.rds")

becols <- c("signal", "berth")
cpcols <- c("train_header", "pattern", "group")
gmcols <- c("group", "geo")
nmcols <- c("signal", "berth", "track", "geo")
sacols <- c("train_header", "group")
ttcols <- c("train_header", "geo")
tccols <- c("class_label", "group")

berth_events <- berth_events %>% anonymise(becols)
calling_patterns <- calling_patterns %>% anonymise(cpcols)
group_map <- group_map %>% anonymise(gmcols)
network_map <- network_map %>% anonymise(nmcols)
sample <- sample %>% anonymise(sacols)
timetable <- timetable %>% anonymise(ttcols)
train_classes <- train_classes %>% anonymise(tccols)

write_rds(berth_events, "berth_events.rds")
write_rds(calling_patterns, "calling_patterns.rds")
write_rds(group_map, "group_map.rds")
write_rds(network_map, "network_map.rds")
write_rds(sample, "sample.rds")
write_rds(timetable, "timetable.rds")
write_rds(train_classes, "train_classes.rds")
