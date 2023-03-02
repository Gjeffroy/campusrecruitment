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
      column(
        width = 3,
        radioButtons(
          ns("plottype_sl"),
          label = "Plot Type",
          choices = c('distribution', 'scatter')
        ),
        conditionalPanel(
          condition = "input.plottype_sl == 'distribution'",
          ns = ns ,
          selectInput(
            ns("variable_sl"),
            label = "Variable",
            choices = c("", colnames(dataset)),
            selected = ""
          ),
          selectInput(
            ns("type_sl"),
            label = "Type",
            choices = c('count', 'density'),
          )
        ),
        conditionalPanel(
          condition = "input.plottype_sl == 'scatter'",
          ns = ns ,
          selectInput(
            ns("variable_x_sl"),
            label = "X variable",
            choices = c("", numeric_col),
            selected = ""
          ),
          selectInput(
            ns("variable_y_sl"),
            label = "Y variable",
            choices = c("", numeric_col),
            selected = ""
          )
        ),
        selectInput(
          ns("colsplitby_sl"),
          label = "Column split by",
          choices = c("No split", cat_col),
          selected = "No split"
        ),
        selectInput(
          ns("rowsplitby_sl"),
          label = "Row split by",
          choices = c("No split", cat_col),
          selected = "No split"
        ),
        selectInput(
          ns("colorsplitby_sl"),
          label = "Color by",
          choices = c("No split", cat_col),
          selected = "No split"
        )
      ),

      column(width = 9,
             textOutput(ns("descriptions")),
             conditionalPanel(
               condition = "(input.plottype_sl == 'distribution') & (input.variable_sl!='')",
               ns = ns,
               plotlyOutput(ns("dist_plot"))
             ),
             conditionalPanel(
               condition = "(input.plottype_sl == 'scatter') & (input.variable_y_sl!='') & (input.variable_x_sl!='')",
               ns = ns,
               plotlyOutput(ns("scatter_plot"))
             ),

             conditionalPanel(
               condition = "(input.plottype_sl == 'distribution') & (input.variable_sl=='')",
               ns = ns ,
               h3("Select a variable to plot", style="text-align: center;")

             ),
             conditionalPanel(
               condition = "(input.plottype_sl == 'scatter') & !((input.variable_y_sl!='') & (input.variable_x_sl!=''))",
               ns = ns ,
               h3("Select variable x and y to plot", style="text-align: center;")

             ),
             align = "center"
      )

  )
  )
  )
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    variable <- reactive({
      return(input$variable_sl)
    })


    dist_plot <- reactive({
      if (input$plottype_sl == "distribution") {
        if (input$variable_sl %in% cat_col) {
          bar_plot(
            dataset,
            input$variable_sl,
            colsplit_by = input$colsplitby_sl,
            rowsplit_by = input$rowsplitby_sl,
            colorsplit_by = input$colorsplitby_sl
          )
        } else {
          distribution_plot(
            dataset,
            input$variable_sl,
            type = input$type_sl,
            colsplit_by = input$colsplitby_sl,
            rowsplit_by = input$rowsplitby_sl,
            colorsplit_by = input$colorsplitby_sl
          )

          }
        }
      })

    scat_plot <- reactive({
      if(input$plottype_sl == "scatter"){

        scatter_plot(
          dataset,
          input$variable_x_sl,
          input$variable_y_sl,
          colsplit_by = input$colsplitby_sl,
          rowsplit_by = input$rowsplitby_sl,
          colorsplit_by = input$colorsplitby_sl)

      }
    })

    descriptions <- reactive({
      var_list = list()
      if(input$plottype_sl == "scatter"){
        if(input$variable_x_sl!=""){
          var_list <- append(var_list, input$variable_x_sl)
        }

        if(input$variable_y_sl!=""){
          var_list <- append(var_list, input$variable_y_sl)
        }
      }

      if(input$plottype_sl == "distribution"){
        if(input$variable_sl!=""){
          var_list <- append(var_list, input$variable_sl)
        }
      }

      if(input$colsplitby_sl!="No split"){
        var_list <- append(var_list, input$colsplitby_sl)
      }

      if(input$rowsplitby_sl!="No split"){
        var_list <- append(var_list, input$rowsplitby_sl)
      }

      if(input$colorsplitby_sl!="No split"){
        var_list <- append(var_list, input$colorsplitby_sl)
      }


      str = ""
      for (e in var_list){
        str = paste(str, sprintf("%s: %s", e, col_description[[e]]), " / ")

      }
      return(stringr::str_sub(str,0, -3))
    })

    output$descriptions <- renderText(descriptions())


    output$dist_plot <-
      plotly::renderPlotly(dist_plot())

    output$scatter_plot <-
      plotly::renderPlotly(scat_plot())



  })
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
