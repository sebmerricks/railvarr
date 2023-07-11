test_that("internal state works", {
  path <- "asdf"
  read_centrix(path)
  expect_equal(wrangle_centrix(), path)
})
