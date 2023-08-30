test_that("validate_timetable_matching errors for incorrect input", {
  test_tt <- railvarr::timetable_subset %>%
    filter(event %in% c("Arrive", "Depart", "Pass"))

  test_names_lack <- test_tt %>% select(-group)

  expect_error(validate_timetable_matching(test_names_lack))

  test_header <- test_tt %>% mutate(train_header = dplyr::row_number())
  test_origin <- test_tt %>% mutate(dt_origin = as.numeric(dt_origin))
  test_group <- test_tt %>% mutate(group = 1)
  test_geo <- test_tt %>% mutate(geo = 100)
  test_event_type <- test_tt %>% mutate(event = 100)
  test_event_content <- test_tt %>% mutate(event = "None")
  test_wtt <- test_tt %>% mutate(wtt = as.numeric(wtt))
  test_t <- test_tt %>% mutate(t = as.numeric(t))

  expect_error(validate_timetable_matching(test_header))
  expect_error(validate_timetable_matching(test_origin))
  expect_error(validate_timetable_matching(test_group))
  expect_error(validate_timetable_matching(test_geo))
  expect_error(validate_timetable_matching(test_event_type))
  expect_error(validate_timetable_matching(test_event_content))
  expect_error(validate_timetable_matching(test_wtt))
  expect_error(validate_timetable_matching(test_t))
})

test_that("validate_timetable_matching does not error for correct input", {
  expect_no_error(validate_timetable_matching(railvarr::timetable_subset %>%
                                                filter(event %in%
                                                         c("Arrive", "Depart", "Pass"))))
})
