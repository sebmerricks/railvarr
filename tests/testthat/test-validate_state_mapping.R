test_that("validate_state_mapping errors with appropriate messages for incorrect data structure", {
  bad_names <- dplyr::tribble(
    ~signal, ~aspect,
    "RGE", factor("R", levels = c("R", "Y")),
    "HGE", factor("Y", levels = c("R", "Y"))
  )
  expect_snapshot(validate_state_mapping(bad_names), error = TRUE)

  bad_state_type <- dplyr::tribble(
    ~state, ~aspect,
    1, factor("R", levels = c("R", "Y")),
    2, factor("Y", levels = c("R", "Y"))
  )
  expect_snapshot(validate_state_mapping(bad_state_type), error = TRUE)

  bad_aspect_type <- dplyr::tribble(
    ~state, ~aspect,
    "RGE", "R",
    "HGE", "Y"
  )
  expect_snapshot(validate_state_mapping(bad_aspect_type), error = TRUE)
})

test_that("validate_state_mapping does nothing with correct state mapping", {
  good_state_mapping <- dplyr::tribble(
    ~state, ~aspect,
    "RGE", factor("R", levels = c("R", "Y")),
    "HGE", factor("Y", levels = c("R", "Y"))
  )
  expect_silent(validate_state_mapping(good_state_mapping))
})
