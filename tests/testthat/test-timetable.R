test_that("wrangle_timetable works", {
  timetable_subset <- wrangle_timetable(railvarr::timetable,
                                        railvarr::stations,
                                        railvarr::stopping_stations)

  expect_equal(timetable_subset, railvarr::timetable_subset)
})
