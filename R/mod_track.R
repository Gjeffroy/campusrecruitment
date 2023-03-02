#' track UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_track_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel(title = "Track",
             value = "track",
             hr(),
             br(), br(),
             fluidRow(
               column(width = 3,
                      checkboxGroupInput(ns("checkboxstage"),
                                         label = h3("Select at least two stages in the student track to visualize the flow"),
                                         choices = list("1.Specialization in Higher Secondary Education" = "hsc_s",
                                                        "2.Field of degree education" = "degree_t",
                                                        "3.Specialisation of degree education" = "specialisation",
                                                        "4.Employed" = "status")
                      )
               ),
               column(width = 9, align = "center",
                      plotly::plotlyOutput(ns("sankey")))
             )

    )

  )
}

#' track Server Functions
#'
#' @noRd
mod_track_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    stages <- reactive({
      return(input$checkboxstage)
    })

    output$sankey <- plotly::renderPlotly(plot_sankey_recursive(dataset, stages()))

  })
}

## To be copied in the UI
# mod_track_ui("track_1")

## To be copied in the server
# mod_track_server("track_1")
