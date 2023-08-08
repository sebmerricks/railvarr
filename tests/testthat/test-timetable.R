test_that("wrangle_timetable works", {
  timetable <- read_rds_test("timetable/timetable.rds")
  stations <- list("Surbiton", "Hampton Court Jn.", "Esher", "Hersham",
                "Walton-On-Thames", c("Weybridge", "Woking"))
  timetable_subset <- wrangle_timetable(timetable, stations)
  out <- read_rds_test("timetable/wrangle_timetable_out.rds")
  expect_equal(timetable_subset, out)
})
