test_that("match_ids works", {
  match_mapping <- dplyr::tribble(
    ~group, ~berth, ~geo, ~lb, ~ub,
    "fast", "A", "geo6", -240, 60,
    "stopping-all", "A", "geo110", 0, 0,
    "stopping-all", "D", "geo111", 0, 0,
    "stopping-all", "F", "geo112", 0, 0
  )

  id_matching <- match_ids(railvarr::berth_events_groups,
                           railvarr::timetable_groups,
                           match_mapping)

  expect_equal(id_matching, railvarr::id_matching)

  match_mapping <- dplyr::tribble(
    ~group, ~berth, ~geo, ~lb, ~ub,
    "fast", "A", "geo6", -240, 60,
    "stopping-all", "A", "geo110", -60, 60,
    "stopping-all", "D", "geo111", -60, 60,
    "stopping-all", "F", "geo112", -60, 60
  )

  looser_matching <- match_ids(railvarr::berth_events_groups,
                               railvarr::timetable_groups,
                               match_mapping)

  expect_gt(nrow(looser_matching), nrow(id_matching))
})
