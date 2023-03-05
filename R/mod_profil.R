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
    fluidRow(column(
      width = 12,
      align = "center",
      selectInput(
        ns("categories_sl"),
        label = "Choose a category to split on",
        choices = c("No split", cat_col)
      ),
      htmlOutput(ns("desc_tt"))
    )),

    fluidRow(lapply(1:3, function(i) {
      uiOutput(ns(paste0('stats', i)))
    })),

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

    # add description of the selected variable
    output$desc_tt <- renderText(
      if(input$categories_sl!= "No split"){
        sprintf("<b>%s</b>: %s", input$categories_sl, col_description[[input$categories_sl]])
      }
    )

    # prepare data for the plots
    data <- reactive({
      data <- prepare_radar_data(dataset, input$categories_sl)
      return(data)
    })

    # get the categories based on select input
    categories <- reactive({
      if (input$categories_sl != "No split") {
        return(data() %>% select(input$categories_sl) %>% unique())
      } else {
        return(data.frame(c("")))
      }
    })

    # count the number of categories to limit the dynamic generation of UI and plots
    nb_categories <- reactive({
      if (input$categories_sl != "No split") {
        return(nrow(categories()))
      } else{
        return(1)
      }
    })

    # dynamically generate the plots in loop
    lapply(1:3, function(i) {
      output[[paste0('plot', i)]] <-

        plotly::renderPlotly(if (i <= nb_categories()) {
          plot_radar2(data(), input$categories_sl, categories()[i,])
        })
    })

    # dynamically generate the UI in loop
    lapply(1:3, function(i) {
      output[[paste0('stats', i)]] <- renderUI({
        if (i <= nb_categories()) {
          column(
            width = 12 / nb_categories(),
            align = "center",
            h3(categories()[i,]),
            h5(max_salary(dataset, input$categories_sl, categories()[i,])),
            h5(mean_salary(dataset, input$categories_sl, categories()[i,])),
            h5(min_salary(dataset, input$categories_sl, categories()[i,])),
            h5(employ_rate(dataset, input$categories_sl, categories()[i,])),
            br(),
            shiny::tags$b("Student profil"),
            plotly::plotlyOutput(ns(paste0(
              'plot', i
            )))
          )
        }
      })
    })
  })
}

## To be copied in the UI
# mod_profil_ui("profil_1")

## To be copied in the server
# mod_profil_server("profil_1")
