#' track UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_track_ui <- function(id) {
  ns <- NS(id)
  tagList(tabPanel(
    title = "Track",
    value = "track",

    # to get the browser dimension
    # only update on page refresh
    shinybrowser::detect(),

    hr(),
    fillRow(
      fillCol(checkboxGroupInput(
        ns("checkboxstage"),
        label = h3("Stages in a student life."),
        choices = list(
          "1.Specialization in Higher Secondary Education" = "hsc_s",
          "2.Field of degree education" = "degree_t",
          "3.Specialisation of degree education" = "specialisation",
          "4.Employment status" = "status"
        )
      )),

      fillCol(plotly::plotlyOutput(ns("sankey"))),
      height = "calc(100vh - 125px)",
      flex = c(1, 3)
    )
  ))
}

#' track Server Functions
#'
#' @noRd
mod_track_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stages <- reactive({
      return(input$checkboxstage)
    })

    output$nb_steps <- renderText(length(stages()))
    output$sankey <-
      plotly::renderPlotly(plot_sankey_recursive(
        dataset,
        stages(),
        plot_height = as.numeric(shinybrowser::get_height()) - 140
      ))

  })
}

## To be copied in the UI
# mod_track_ui("track_1")

## To be copied in the server
# mod_track_server("track_1")
