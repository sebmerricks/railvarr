test_that("a new berth can be created", {
  test <- berth("A", "S1", "R", "Sttn1", 1000)
  expect_equal(class(test), c("railvarr_berth", "vctrs_rcrd", "vctrs_vctr"))

  testy <- berth("B", "S2", "G", "Sttn2", 1000)

  cd <- c(test, testy)
  con <- berth(c("A", "B"),
               c("S1", "S2"),
               c("R", "G"),
               c("Sttn1", "Sttn2"),
               1000)
  expect_equal(cd, con)
})

test_that("malformed berths throw errors", {
  expect_error(berth("A", "SA", "R", "Sttn1", 1000))
  expect_error(berth(c("A", "B"),
                     c("S1", "S2"), ,
                     c("R", "Y"),
                     c("Sttn1", "Sttn2")))
})
