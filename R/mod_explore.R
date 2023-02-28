#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(tabPanel(
    title = "Explore",
    value = "Explore",
    hr(),


    fluidRow(
      column(width = 3),
      column(
        width = 2,
        align = "center",
        selectInput(
          inputId = ns("gender_sl"),
          label = "Gender:",
          choices = c("All" = "c('M','F')",
                      "M" =
                        "c('M')",
                      "F" = "c('F')"),
          selected = "All"
        ),
      ),
      column(
        width = 2,
        align = "center",
        selectInput(
          inputId = ns("speciality_sl"),
          label = "Speciality:",
          choices = c(
            "All" = "c('Arts','Commerce', 'Science')",
            "Arts" =
              "c('Arts')",
            "Commerce" = "c('Commerce')",
            "Science" = "c('Science')"
          ),
          selected = "All"
        )
      ),
      column(
        width = 2,
        align = "center",
        selectInput(
          inputId = ns("workex_sl"),
          label = "Work experience:",
          choices = c(
            "All" = "c('Yes','No')",
            "Yes" =
              "c('Yes')",
            "No" = "c('No')"
          ),
          selected = "All"
        )
      ),
      column(width = 3),

    ),
    br(),
    fluidRow(
      column(
        width = 4,
        align = "center",
        plotly::plotlyOutput(ns("salary_plot"))
      ),
      column(
        width = 4,
        align = "center",
        plotly::plotlyOutput(ns("workex_plot"))
      ),
      column(
        width = 4,
        align = "center",
        plotly::plotlyOutput(ns("etest_plot"))
      )
    )
  ))
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    subset_df <- reactive({
      dataset %>% dplyr::filter(gender %in% eval(parse(text = input$gender_sl)),
                                hsc_s %in% eval(parse(text = input$speciality_sl)),
                                workex %in% eval(parse(text = input$workex_sl)))
    })


    output$salary_plot <-
      plotly::renderPlotly(salary_distribution_plot(dataset, subset_df()))
    output$workex_plot <-
      plotly::renderPlotly(workex_plot(subset_df()))
    output$etest_plot <-
      plotly::renderPlotly(etest_distribution_plot(subset_df()))

  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
