context("util")

test_that("or", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_null(NULL %||% NULL)
})
