test_that("validate_berth_matching errors for incorrect inputs", {
  test_be <- railvarr::berth_events_groups

  test_names_lack <- test_be %>%
    select(-"group")

  test_names_more <- test_be %>%
    mutate(whoops = 10L)

  expect_error(validate_berth_matching(test_names_lack))
  expect_no_error(validate_berth_matching(test_names_more))

  test_berth <- test_be %>%
    mutate(berth = match(berth, LETTERS))

  expect_error(validate_berth_matching(test_berth))

  test_train_id <- test_be %>%
    mutate(train_id = as.character(train_id))

  expect_error(validate_berth_matching(test_train_id))

  test_t_enters <- test_be %>%
    mutate(t_enters = as.numeric(t_enters))

  expect_error(validate_berth_matching(test_t_enters))

  test_t_vacates <- test_be %>%
    mutate(t_vacates = as.numeric(t_vacates))

  expect_error(validate_berth_matching(test_t_vacates))

  test_group <- test_be %>%
    mutate(group = 100)

  expect_error(validate_berth_matching(test_group))
})
