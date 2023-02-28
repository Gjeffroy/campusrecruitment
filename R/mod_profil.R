#' profil UI Function
#'
#' @description This module give a profil of three group of student using radar plots and stats
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profil_ui <- function(id) {
  ns <- NS(id)
  tagList(tabPanel(
    title = "Radar",
    value = "Radar",
    hr(),
    br(),
    fluidRow(
      column(
        width = 4,
        align = "center",
        h3('Science'),
        h5(max_salary(dataset, "Science")),
        h5(mean_salary(dataset, "Science")),
        h5(min_salary(dataset, "Science")),
        h5(employ_rate(dataset, "Science")),
        br(),
        shiny::tags$b("Student profil"),
        plotly::plotlyOutput(ns("radarS")),

      ),



      column(
        width = 4,
        align = "center",
        h3('Commerce'),
        h5(max_salary(dataset, "Commerce")),
        h5(mean_salary(dataset, "Commerce")),
        h5(min_salary(dataset, "Commerce")),
        h5(employ_rate(dataset, "Commerce")),
        br(),
        shiny::tags$b("Student profil"),
        plotly::plotlyOutput(ns("radarC"))
      ),
      column(
        width = 4,
        align = "center",
        h3('Arts'),
        h5(max_salary(dataset, "Arts")),
        h5(mean_salary(dataset, "Arts")),
        h5(min_salary(dataset, "Arts")),
        h5(employ_rate(dataset, "Arts")),
        br(),
        shiny::tags$b("Student profil"),
        plotly::plotlyOutput(ns("radarA"))
      )

    ),
    fluidRow(
      column(width = 5),
      column(
        width = 1,
        HTML('<hr class= "hr_legend_b"></hr><h5>Employed</h5>')
      ),
      column(
        width = 1,
        HTML('<hr class = "hr_legend_o"></hr><h5>Not employed</h5>')
      ),
      column(width = 5)
    )
  ))
}

#' profil Server Functions
#'
#' @noRd
mod_profil_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$radarS <-
      plotly::renderPlotly(plot_radar(student_profil, 'Science'))
    output$radarA <-
      plotly::renderPlotly(plot_radar(student_profil, 'Arts'))
    output$radarC <-
      plotly::renderPlotly(plot_radar(student_profil, 'Commerce'))
    output$plottest <- renderPlot(plot(iris))

  })
}

## To be copied in the UI
# mod_profil_ui("profil_1")

## To be copied in the server
# mod_profil_server("profil_1")
