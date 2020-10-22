context("shiny")

test_that("remotesave save status - empty", {
  expect_equal(
    remotesave_save_status(NULL),
    simple_panel("danger", "Not yet saved", NULL))
})


test_that("remotesave save status - no label", {
  expect_equal(
    remotesave_save_status(list(time = "time")),
    simple_panel("success", "Last saved at time with no label", NULL))
  expect_equal(
    remotesave_save_status(list(time = "time", label = "")),
    simple_panel("success", "Last saved at time with no label", NULL))
})


test_that("remotesave save status - with label", {
  expect_equal(
    remotesave_save_status(list(time = "time", label = "label")),
    simple_panel("success", "Last saved at time with label 'label'", NULL))
})


test_that("remotesave remote", {
  expect_equal(
    remotesave_remote(NULL, NULL),
    unsuccessful("Auto-save root not ready"))
  expect_equal(
    remotesave_remote(simpleError("root error"), NULL),
    unsuccessful("root error"))
  expect_equal(
    remotesave_remote(TEST_ROOT, NULL),
    unsuccessful("Auto-save user not ready"))
  expect_equal(
    remotesave_remote(TEST_ROOT, simpleError("user error")),
    unsuccessful("user error"))
  res <- remotesave_remote(TEST_ROOT, "user", "---")
  expect_false(res$success)

  res <- remotesave_remote(TEST_ROOT, "user")
  expect_true(res$success)
  expect_is(res$value, "remote_save")
  expect_null(res$error)
})


test_that("remotesave status - enabled", {
  remote <- list(success = TRUE,
                 value = list(info = function() list(a = 1)))
  expect_equal(
    remotesave_status(remote, 10, "id"),
    collapseable_panel("success", "Autosave enabled",
                       list_to_html(list(a = 1, period = 10)),
                       collapsed = TRUE, id = "id"))
})


test_that("remotesave status - disabled", {
  remote <- list(success = FALSE, error = "reason")
  expect_equal(
    remotesave_status(remote, 10, "id"),
    collapseable_panel("danger", "Autosave disabled", "reason",
                       collapsed = TRUE, id = "id"))
})


test_that("remotesave_load - no row selected", {
  res <- remotesave_load(NULL, NULL, NULL, NULL)
  expect_false(res$success)
  expect_equal(res$error, "No row is selected")
})


test_that("remotesave_load - more than one row", {
  res <- remotesave_load(c(1, 2, 3), NULL, NULL, NULL)
  expect_false(res$success)
  expect_equal(res$error, "More than one row selected")
})
