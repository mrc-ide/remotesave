mod_remotesave_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}


mod_remotesave_server <- function(input, output, session,
                                  root, user, get_state, set_state, ...,
                                  period = 10) {
  rv <- shiny::reactiveValues()

  shiny::observe({
    rv$remote <- remotesave_remote(root(), user(), ...)
    if (isTRUE(rv$remote$success)) {
      rv$list <- rv$remote$value$list()
    }
  })

  output$ui <- shiny::renderUI(
    remotesave_ui(rv$remote$success, session$ns))

  output$status <- shiny::renderUI(
    remotesave_status(rv$remote, period))

  output$list <- DT::renderDataTable(
    DT::datatable(
      data = rv$list[c("time", "label")],
      options = list(paging = FALSE, dom = "t", searching = FALSE)))

  proxy <- DT::dataTableProxy("list")
  shiny::observeEvent(
    input$refresh,
    rv$list <- rv$remote$value$list())

  shiny::observe(DT::replaceData(proxy, rv$list[c("time", "label")]))

  ## On load we start a new session so that it's not possible to wipe
  ## a previous session accidentally
  shiny::observeEvent(
    input$load, {
      row <- input$list_rows_selected
      rv$remote$value$new_session()
      rv$result <- remotesave_load(row, rv$list, rv$remote$value, set_state)
      rv$remote$value$list()
    })

  output$result_status <- shiny::renderUI(
    remotesave_result_status(rv$result))

  output$save_status <- shiny::renderUI(
    remotesave_save_status(rv$last_save))

  shiny::observeEvent(
    input$delete, {
      remote$delete_sessions(rv$list$session[input$list_rows_selected])
      rv$list <- rv$remote$value$list()
    })

  shiny::observe({
    if (isTRUE(rv$remote$success)) {
      label <- shiny::debounce(shiny::reactive(input$label), 1000)()
      rv$remote$value$save(get_state(), label)
      shiny::invalidateLater(period * 1000)
      rv$last_save <- list(time = Sys.time(), label = label)
    }
  })
}


remotesave_load_error <- function(error) {
  remotesave_load_result(FALSE, error)
}


remotesave_load_success <- function(data) {
  remotesave_load_result(TRUE, data)
}


remotesave_load_result <- function(success, data) {
  ret <- list(success = success, time = Sys.time())
  if (success) {
    ret$value <- data
  } else {
    ret$error <- data
  }
  ret
}


remotesave_result_status <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  if (result$success) {
    class <- "success"
    title <- "State loaded"
    body <- sprintf("Saved from session %s (%s) loaded at %s",
                    result$value$session,
                    result$value$time,
                    result$time)
  } else {
    class <- "danger"
    title <- "Error loading previous state"
    body <- result$error
  }
  simple_panel(class, title, body)
}


remotesave_load <- function(row, list, remote, set_state) {
  if (is.null(row) || is.na(row)) {
    remotesave_load_error("No row is selected")
  } else if (length(row) > 1) {
    remotesave_load_error("More than one row selected")
  } else {
    set_state(remote$fetch(list$session[[row]]))
    remotesave_load_success(as.list(list[row, ]))
  }
}


remotesave_status <- function(remote, period, id = NULL) {
  if (isTRUE(remote$success)) {
    class <- "success"
    title <- "Autosave enabled"
    body <- list_to_html(c(remote$value$info(), list(period = period)))
  } else {
    class <- "danger"
    title <- "Autosave disabled"
    body <- remote$error %||% "Initialising"
  }
  collapseable_panel(class, title, body, collapsed = TRUE, id = id)
}


remotesave_save_status <- function(data) {
  if (is.null(data)) {
    simple_panel("danger", "Not yet saved", NULL)
  } else {
    if (is.null(data$label) || is.na(data$label) || data$label == "") {
      label_str <- "with no label"
    } else {
      label_str <- sprintf("with label '%s'", data$label)
    }
    msg <- sprintf("Last saved at %s %s", data$time, label_str)
    simple_panel("success", msg, NULL)
  }
}


remotesave_ui <- function(render, ns) {
  if (isTRUE(render)) {
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::textInput(ns("label"), "Session label"),
          shiny::actionButton(
            ns("load"), "Load",
            icon = shiny::icon("check"), class = "btn-primary"),
          shiny::actionButton(
            ns("refresh"), "Refresh",
            icon = shiny::icon("refresh"), class = "btn-info"),
          shiny::actionButton(
            ns("delete"), "Delete",
            icon = shiny::icon("times"), class = "btn-danger"),
          shiny::hr(),
          shiny::uiOutput(ns("result_status")),
          shiny::uiOutput(ns("save_status")),
          shiny::uiOutput(ns("status"))),
        shiny::column(
          6,
          DT::dataTableOutput(ns("list")))))
  } else {
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(6, shiny::uiOutput(ns("status"))),
        shiny::column(6)))
  }
}


remotesave_remote <- function(root, user, ...) {
  ## This needs some work...
  if (inherits(root, "error")) {
    return(unsuccessful(root))
  }
  if (is.null(root)) {
    return(unsuccessful("Auto-save root not ready"))
  }
  if (inherits(user, "error")) {
    return(unsuccessful(user))
  }
  if (is.null(user)) {
    return(unsuccessful("Auto-save user not ready"))
  }
  with_success(remote_save(root, user, ...))
}


simple_panel <- function(class, title, body, icon_name = NULL) {
  icon <- panel_icon(icon_name, class)
  head <- shiny::div(class = "panel-heading", icon, title)
  if (!is.null(body) && !identical(body, "")) {
    body <- shiny::div(class = "panel-body", body)
  } else {
    body <- NULL
  }
  shiny::div(
    class = "panel-group",
    shiny::div(
      class = sprintf("panel panel-%s", class), head, body))
}


collapseable_panel <- function(class, title, body, icon_name = NULL,
                               collapsed = FALSE, id = NULL) {
  id <- id %||% ids::random_id()
  icon <- panel_icon(icon_name, class)

  head <- shiny::div(class = "panel-heading", icon, title,
                     "data-toggle" = "collapse",
                     "data-target" = paste0("#", id))
  body_class <- paste0("panel-body", if (collapsed) " collapse" else "")
  body <- shiny::div(class = body_class, id = id, body)

  shiny::div(
    class = "panel-group",
    shiny::div(
      class = sprintf("panel panel-%s", class), head, body))
}


panel_icon <- function(icon_name, class) {
  if (is.null(icon_name)) {
    icon_name <- switch(
      class,
      danger = "exclamation-circle",
      success = "check-circle",
      "info-circle")
  }
  shiny::icon(sprintf("%s fa-lg", icon_name))
}


list_to_html <- function(x) {
  f <- function(name, value) {
    shiny::tags$li(
      shiny::tagList(
        shiny::tags$b(name), paste(":", value)))
  }

  if (length(x) > 0L) {
    shiny::tags$ul(unname(Map(f, names(x), vcapply(x, as.character))))
  }
}
