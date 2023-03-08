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
  tagList(
    tabPanel(
      title = "Radar",
      value = "Radar",
      shinybrowser::detect(),

      # to get the browser dimension
      # only update on page refresh
      hr(),
      fluidRow(
        shinyWidgets::radioGroupButtons(ns("categories_sl"),
                                        choices = c("No split", cat_col[!cat_col %in% c('status')])),
        align = "center"
      ),
      fluidRow(htmlOutput(ns("desc_tt")),
               align = "center"),
      fillRow(lapply(1:3, function(i) {
          uiOutput(ns(paste0('stats', i)))
        }),
        height = "calc(100vh - 195px)",)
    )
  )
}

#' profil Server Functions
#'
#' @noRd
mod_profil_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # add description of the selected variable
    output$desc_tt <-
      renderText(if (input$categories_sl != "No split") {
        sprintf("<h3>Average student profil split by %s</h3>",
                col_description[[input$categories_sl]])
      } else {
        "<h3>Average student profil</h3>"
      })

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
          plot_radar(
            data(),
            input$categories_sl,
            categories()[i,],
            plot_height = as.numeric(shinybrowser::get_height()) - 320 - (if(input$categories_sl != "No split") 30 else 0)
          )
        })
    })

    # dynamically generate the UI in loop
    lapply(1:3, function(i) {
      output[[paste0('stats', i)]] <- renderUI({
        if (i <= nb_categories()) {
          column(
            align = "center",
            width = 12/nb_categories(),
            h3(categories()[i,])
            ,
            h5(
              mean_salary(dataset, input$categories_sl, categories()[i,])
            ),
            h5(
              employ_rate(dataset, input$categories_sl, categories()[i,])
            ),
            plotly::plotlyOutput(ns(paste0('plot', i)))
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
