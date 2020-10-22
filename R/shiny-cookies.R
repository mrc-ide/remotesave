##' A shiny module for working with cookies.
##'
##' @title Cookies shiny module
##'
##' @param id An id used in the shiny module, required for namespacing
##'   even though no UI elements are created.
##'
##' @export
##' @rdname mod_cookies
mod_cookies_ui <- function(id) {
  shiny::singleton(shiny::tags$script(shiny::HTML(cookies_script())))
}


##' @rdname mod_cookies
##'
##' @param input,output,session Arguments to shiny
##'
##' @param name The name of the cookie to save/retrieve the username
##'   from.  This should be globally unique and we will set this
##'   cookie if not pesent.
##'
##' @param valid The period of validity of the cookie, in days.  We
##'   will refresh the cookie if it is present.
##' @export
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
