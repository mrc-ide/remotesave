mod_cookies_ui <- function(id) {
  shiny::singleton(shiny::tags$script(shiny::HTML(cookies_script())))
}


mod_cookies_server <- function(input, output, session, name, valid = 30) {
  session$sendCustomMessage(
    type = "updateCookie",
    message = cookies_message(name, valid, session$ns))

  rv <- shiny::reactiveValues(cookies = NULL)
  shiny::observeEvent(input$value, {
    rv$value <- input$value
  })

  list(value = shiny::reactive(cookies_get(rv$value, name)))
}


cookies_script <- function() {
  path <- system.file("cookie.js", package = "remotesave", mustWork = TRUE)
  paste(readLines(path), collapse = "\n")
}


cookies_get <- function(value, name) {
  value %||% simpleError(sprintf("Cookie '%s' not found", name))
}


cookies_message <- function(name, valid, ns) {
  list(name = name,
       valid = valid,
       input = ns("value"),
       defaultValue = ids::adjective_animal())
}
