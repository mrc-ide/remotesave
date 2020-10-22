context("shiny - support")


test_that("list_to_html", {
  expect_null(list_to_html(NULL))

  expect_equal(
    list_to_html(list(a = 1)),
    shiny::tags$ul(list(
      shiny::tags$li(shiny::tagList(shiny::tags$b("a"), ": 1")))))

  expect_equal(
    list_to_html(list(a = 1, b = 2)),
    shiny::tags$ul(list(
      shiny::tags$li(shiny::tagList(shiny::tags$b("a"), ": 1")),
      shiny::tags$li(shiny::tagList(shiny::tags$b("b"), ": 2")))))
})


test_that("panel icon", {
  expect_equal(panel_icon("name", "danger"),
               shiny::icon("name fa-lg"))
  expect_equal(panel_icon(NULL, "danger"),
               shiny::icon("exclamation-circle fa-lg"))
  expect_equal(panel_icon(NULL, "success"),
               shiny::icon("check-circle fa-lg"))
  expect_equal(panel_icon(NULL, "other"),
               shiny::icon("info-circle fa-lg"))
})


test_that("simple panel - no body", {
  a <- simple_panel("success", "title", NULL)
  b <- shiny::div(
    class = "panel-group",
    shiny::div(
      class = "panel panel-success",
      shiny::div(
        class = "panel-heading",
        shiny::tags$i(class = "fa fa-check-circle fa-lg"),
        "title")))
  expect_equal(as.character(a), as.character(b))
})


test_that("simple panel - with body", {
  a <- simple_panel("success", "title", "body")
  b <- shiny::div(
    class = "panel-group",
    shiny::div(
      class = "panel panel-success",
      shiny::div(
        class = "panel-heading",
        shiny::tags$i(class = "fa fa-check-circle fa-lg"),
        "title"),
      shiny::div(class = "panel-body", "body")))
  expect_equal(as.character(a), as.character(b))
})


test_that("collapseable_panel - open", {
  a <- collapseable_panel("success", "title", "body", id = "id")
  b <- shiny::div(
    class = "panel-group",
    shiny::div(
      class = "panel panel-success",
      shiny::div(
        class = "panel-heading",
        "data-toggle" = "collapse",
        "data-target" = "#id",
        shiny::tags$i(class = "fa fa-check-circle fa-lg"),
        "title"),
      shiny::div(class = "panel-body", id = "id", "body")))
  expect_equal(as.character(a), as.character(b))
})


test_that("collapseable_panel - collapsed", {
  a <- collapseable_panel("success", "title", "body", id = "id",
                          collapsed = TRUE)
  b <- shiny::div(
    class = "panel-group",
    shiny::div(
      class = "panel panel-success",
      shiny::div(
        class = "panel-heading",
        "data-toggle" = "collapse",
        "data-target" = "#id",
        shiny::tags$i(class = "fa fa-check-circle fa-lg"),
        "title"),
      shiny::div(class = "panel-body collapse", id = "id", "body")))
  expect_equal(as.character(a), as.character(b))
})
