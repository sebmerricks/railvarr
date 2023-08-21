test_that("a new aspect can be created", {
  testr <- aspect("R")
  expect_equal(class(testr), c("railvarr_aspect", "factor"))
  expect_equal(vctrs::vec_data(testr), 1)

  testu <- aspect("U", levels = c("R", "U"))
  expect_equal(vctrs::vec_data(testu), 2)
})

test_that("malformed aspects throw errors", {
  expect_error(aspect("U"))
  expect_error(aspect("R", levels = c("Y", "G")))
  expect_error(aspect(1))
})
