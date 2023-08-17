data(track_events, aspect_events, time_windows, asset_map)
valid_track_events <- filter_track_events(track_events,
                                         time_windows,
                                         asset_map)
valid_aspect_events <- filter_aspect_events(aspect_events,
                                            time_windows,
                                            asset_map)

usethis::use_data(valid_track_events, overwrite = TRUE)
usethis::use_data(valid_aspect_events, overwrite = TRUE)
