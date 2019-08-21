mod_cookies_ui <- function(id) {
  ns <- shiny::NS(id)
  cookies_script(ns("cookies_string"))
}


mod_cookies_server <- function(input, output, session) {
  session$sendCustomMessage(type = "readCookies", message = list())

  rv <- shiny::reactiveValues(cookies = NULL)
  shiny::observeEvent(input$cookies_string, {
    rv$cookies <- cookies_process(input$cookies_string)
  })

  list(all = shiny::reactive(rv$cookies),
       get = function(name) cookies_get(rv$cookies, name))
}


cookies_process <- function(str) {
  if (length(str) == 0L) {
    str <- ""
  }
  dat <- strsplit(strsplit(as.character(str), "; ")[[1]], "=")
  set_names(lapply(dat, "[[", 2L), vapply(dat, "[[", "", 1L))
}


cookies_script <- function(id) {
  script <- sprintf('
      Shiny.addCustomMessageHandler("readCookies", function(message) {
        Shiny.onInputChange("%s", document.cookie);
      })', id)
  shiny::tags$script(shiny::HTML(script))
}


cookies_get <- function(list, name) {
  list[[name]] %||% simpleError(sprintf("Cookie '%s' not found", name))
}
