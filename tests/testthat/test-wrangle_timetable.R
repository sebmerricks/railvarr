test_that("set_stations() errors if stations is not a list of characters", {
  test1 <- list()
  test2 <- c("a", "b")
  test3 <- list("a", 2)

  expect_error(set_stations(test1))
  expect_error(set_stations(test2))
  expect_error(set_stations(test3))
})

test_that("set_stations() sets stations correctly", {
  test1 <- list("a", "b", "c")
  test2 <- list("a", "b", c("c", "d"))

  set_stations(test1)
  expect_equal(get_stations(), test1)

  set_stations(test2)
  expect_equal(get_stations(), test2)
})
