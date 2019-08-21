context("util")

test_that("or", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_null(NULL %||% NULL)
})


test_that("error handling", {
  expect_equal(
    with_success(1 + 1),
    list(success = TRUE, value = 2, error = NULL))
  expect_equal(
    with_success(stop("an error")),
    list(success = FALSE, value = NULL, error = "an error"))
})
