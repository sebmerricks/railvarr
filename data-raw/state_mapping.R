state_mapping <- dplyr::tribble(
  ~state, ~aspect,
  "RGE", "R",
  "HGE", "Y",
  "HHGE", "YY",
  "DGE", "G"
) %>%
  dplyr::mutate(aspect = factor(aspect, levels = c("R", "Y", "YY", "G")))

usethis::use_data(state_mapping, overwrite = TRUE)
