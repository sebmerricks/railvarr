data(raw_centrix, asset_map, state_mapping)
aspect_events <- preprocess_signal_events(raw_centrix,
                                          asset_map,
                                          state_mapping)
track_events <- preprocess_track_events(raw_centrix,
                                        asset_map)

usethis::use_data(aspect_events, overwrite = TRUE)
usethis::use_data(track_events, overwrite = TRUE)
