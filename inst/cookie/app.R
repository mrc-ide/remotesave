devtools::load_all()

ui <- shiny::fluidPage(
  shiny::titlePanel("Example app"),
  remotesave::mod_cookies_ui("cookies"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput("a", "a", NULL),
      shiny::numericInput("b", "b", NULL),
      shiny::numericInput("c", "c", NULL),
      shiny::actionButton("random", "Randomise",
                          class = "btn-success",
                          icon = shiny::icon("random"))),
    shiny::mainPanel(
      shiny::plotOutput("plot"))),
  shiny::div(
    shiny::hr(),
    remotesave::mod_remotesave_ui("save")))


server <- function(input, output, session) {
  rv <- shiny::reactiveValues()
  name <- "myapp"
  valid <- 0.1
  cookies <- shiny::callModule(
    remotesave::mod_cookies_server, "cookies", name, valid)

  root <- shiny::reactive(TEST_ROOT)
  user <- shiny::reactive(cookies$value())

  get_state <- function() {
    list(a = input$a, b = input$b, c = input$c)
  }
  set_state <- function(state) {
    shiny::updateNumericInput(session, "a", value = state$a)
    shiny::updateNumericInput(session, "b", value = state$b)
    shiny::updateNumericInput(session, "c", value = state$c)
  }

  save <- shiny::callModule(
    remotesave::mod_remotesave_server, "save",
    root, user, get_state, set_state)

  shiny::observeEvent(
    input$random, {
      shiny::updateNumericInput(session, "a", value = runif(1))
      shiny::updateNumericInput(session, "b", value = runif(1))
      shiny::updateNumericInput(session, "c", value = runif(1))
    })

  output$plot <- shiny::renderPlot({
    x <- c(input$a, input$b, input$c)
    if (any(is.finite(x))) {
      tt <- seq(0, 4 * pi, length.out = 501)
      yy <- outer(tt, x, function(t, x) sin(t / x))
      matplot(tt, yy, type = "l", lty = 1, xlab = "time", ylab = "value")
    }
  })

  list(get_state = get_state,
       set_state = set_state)
}

## document.cookie="username=John Doe"; document.cookie="SRVID=foo";
shiny::shinyApp(ui, server)
