df <- function() {
  return(dplyr::tribble(
    ~a, ~b, ~c,
    1, "a", 4
  ))
}

test_that("check_df() warns if `names` and `types` have different lengths", {
  names <- c("a", "b", "c", "d")
  types <- list(integer(), character(), numeric())
  expect_warning(check_df(df(), names, types))

  names <- c("a", "b", "c")
  types <- list(integer(), character())
  expect_warning(check_df(df(), names, types))
})

test_that("check_df() works if `names` and `types` have same length &
          `allow_extra = TRUE`", {
            names = c("a", "b", "c", "d")
            types = list(integer(), character(), numeric(), numeric())
            expect_no_error(check_df(df(), names, types))

            names = c("a", "b")
            types = list(integer(), character())
            expect_no_error(check_df(df(), names, types))

            names = c("a", "b", "c")
            types = list(integer(), character(), numeric())
            df <- dplyr::tribble(
              ~a, ~b,
              1, "a"
            )
            expect_no_error(check_df(df, names, types))

            df <- dplyr::tribble(
              ~a, ~b, ~c, ~d,
              1, "a", 2, 3
            )
            expect_no_error(check_df(df, names, types))
          })

test_that("check_df() errors if `df` larger than `names` &
          `allow_extra = FALSE`", {
            names <- c("a", "b")
            types <- list(integer(), character())
            expect_error(check_df(df(), names, types, allow_extra = FALSE))
          })

test_that("check_df() works with correct inputs", {
  names <- c("a", "b", "c")
  types <- list(integer(), character(), numeric())
  expect_no_error(check_df(df(), names, types))
})
