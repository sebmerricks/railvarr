event_mapping <- dplyr::tribble(
  ~event, ~name,
  "O", "Originate",
  "P", "Pass",
  "A", "Arrive",
  "D", "Depart",
  "T", "Terminate"
)

usethis::use_data(event_mapping, overwrite = TRUE)
