# Centrix ----------------------------------------------------------------------

test_that("centrix column checking works", {

})

test_that("centrix type checking works", {
  expect_error(centrix(data.frame("asset" = character(),
                                  "dt" = lubridate::POSIXct(),
                                  "transition" = character(),
                                  "period" = double())))

  expect_error(centrix(data.frame("asset" = character(),
                                  "dt" = character(),
                                  "transition" = character(),
                                  "period" = integer())))

  expect_no_error(centrix(data.frame("asset" = character(),
                                     "dt" = lubridate::POSIXct(),
                                     "transition" = character(),
                                     "period" = integer())))

  expect_no_error(centrix())
})

test_that("centrix$transition checking works", {

})

# Aspect Events ----------------------------------------------------------------

test_that("aspect_event column checking works", {

})

test_that("aspect_event type checking works", {

})

# Track Events -----------------------------------------------------------------

test_that("track_event column checking works", {

})

test_that("track_event type checking works", {

})
