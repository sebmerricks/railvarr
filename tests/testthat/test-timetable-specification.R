test_that("calculate_journey_specifications() works", {
  spec_stations <- list("geo6", "geo110", "geo111", "geo112", "geo7")
  timetable_specification <-
    calculate_journey_specifications(railvarr::timetable_subset,
                                     spec_stations)

  expect_equal(timetable_specification, railvarr::timetable_specification)
})
