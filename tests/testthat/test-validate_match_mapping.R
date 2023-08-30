test_that("validate_match_mapping errors on incorrect input", {
  mapping <- dplyr::tribble(
    ~group, ~berth, ~geo, ~lb, ~ub,
    "fast", "A", "geo6", -240, 60,
    "stopping-all", "A", "geo110", 0, 0,
    "stopping-all", "D", "geo111", 0, 0,
    "stopping-all", "F", "geo112", 0, 0
  )

  test_names_lack <- mapping %>% select(-geo)
  test_names_more <- mapping %>% mutate(whoops = 10L)

  expect_error(validate_match_mapping(test_names_lack))
  expect_no_error(validate_match_mapping(test_names_more))

  test_group <- mapping %>% mutate(group = 1)

  expect_error(validate_match_mapping(test_group))

  test_berth <- mapping %>% mutate(berth = match(berth, LETTERS))

  expect_error(validate_match_mapping(test_berth))

  test_geo <- mapping %>% mutate(geo = 1)

  expect_error(validate_match_mapping(test_geo))

  test_lb <- mapping %>% mutate(lb = as.character(lb))
  test_ub <- mapping %>% mutate(ub = as.character(ub))

  expect_error(validate_match_mapping(test_lb))
  expect_error(validate_match_mapping(test_ub))
})

test_that("validate_match_mapping does not error for correct input", {
  match_mapping <- dplyr::tribble(
    ~group, ~berth, ~geo, ~lb, ~ub,
    "fast", "A", "geo6", -240, 60,
    "stopping-all", "A", "geo110", 0, 0,
    "stopping-all", "D", "geo111", 0, 0,
    "stopping-all", "F", "geo112", 0, 0
  )

  expect_no_error(validate_match_mapping(match_mapping))
})
