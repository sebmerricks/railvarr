data(aspect_events, track_events, asset_map)
time_windows <- calculate_time_windows(aspect_events, track_events, asset_map)
usethis::use_data(time_windows, overwrite = TRUE)
