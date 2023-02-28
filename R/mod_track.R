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
             checkboxGroupInput(ns("checkboxstage"),
                                label = h3("Select student at least two stages to add to the Sankey plot"),
                                choices = list("Specialization in Higher Secondary Education" = "hsc_s",
                                               "Field of degree education" = "degree_t",
                                               "Specialisation of degree education" = "specialisation",
                                               "Employed" = "status")
                                ),
             plotly::plotlyOutput(ns("sankey"))
    )

  )
}

#' track Server Functions
#'
#' @noRd
mod_track_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$sankey <- plotly::renderPlotly(plot_sankey_recursive(dataset))

  })
}

## To be copied in the UI
# mod_track_ui("track_1")

## To be copied in the server
# mod_track_server("track_1")
