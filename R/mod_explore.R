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
  tagList(
    tabPanel(
      title = "Explore",
      value = "Explore",

      # add shinyjs to get the disable input select option
      shinyjs::useShinyjs(),
      # to get the browser dimension
      # only update on page refresh
      shinybrowser::detect(),

      hr(),
      textOutput(ns("activated_rd")),
      fillRow(
        fillCol(
          fillRow(
            radioButtons(
              ns("plottype_sl"),
              label = "Plot Type",
              choices = c('distribution', 'scatter'),
              selected = 'distribution'
            ),
            height = "125px"
          ),
          uiOutput(ns("ui_input")),
          flex = c(1, 5),
          height = "calc(100vh - 125px)"
        ),
        uiOutput(ns("ui_plot")),
        flex = c(1, 3),
        height = "calc(100vh - 125px)"
      ),
    )
  )
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # disable the type dropdown in distribution mode if the variable id categorical
    # In that case the density does not make sens, so the plot will be on count
    observeEvent(input$variable_sl, {
      if (input$variable_sl %in% cat_col) {
        shinyjs::disable(id = "type_sl")
      } else{
        shinyjs::enable(id = "type_sl")
      }

    })

    # create plot
    plot <- reactive({
      # create distribution or bar plot base on input if scatter mode is on
      # and if the selected variable is categorical or numeric
      if (input$plottype_sl == "distribution") {
        if (input$variable_sl %in% cat_col) {
          bar_plot(
            dataset,
            input$variable_sl,
            colsplit_by = input$colsplitby_sl,
            rowsplit_by = input$rowsplitby_sl,
            colorsplit_by = input$colorsplitby_sl,
            plot_height = as.numeric(shinybrowser::get_height()) - 180
          )
        } else {
          distribution_plot(
            dataset,
            input$variable_sl,
            type = input$type_sl,
            colsplit_by = input$colsplitby_sl,
            rowsplit_by = input$rowsplitby_sl,
            colorsplit_by = input$colorsplitby_sl,
            plot_height = as.numeric(shinybrowser::get_height()) - 180
          )

        }
      }
      # create scatter plot base on input if scatter mode is on
      else if (input$plottype_sl == "scatter") {
        scatter_plot(
          dataset,
          input$variable_x_sl,
          input$variable_y_sl,
          colsplit_by = input$colsplitby_sl,
          rowsplit_by = input$rowsplitby_sl,
          colorsplit_by = input$colorsplitby_sl,
          plot_height = as.numeric(shinybrowser::get_height()) - 180
        )

      }
    })

    # add the legend / meaning of the variable on top of the chart
    descriptions <- reactive({
      # create an emplty variable list / dict
      var_list = list()

      # add variable name and description for the scatter plot x and y
      if (input$plottype_sl == "scatter") {
        if (input$variable_x_sl != "") {
          var_list <- append(var_list, input$variable_x_sl)
        }

        if (input$variable_y_sl != "") {
          var_list <- append(var_list, input$variable_y_sl)
        }
      }

      # add variable name and description for the distribution plot
      if (input$plottype_sl == "distribution") {
        if (input$variable_sl != "") {
          var_list <- append(var_list, input$variable_sl)
        }
      }

      #  add variables name and description chosen for splitting the data (row, col, and color)
      if (input$colsplitby_sl != "No split") {
        var_list <- append(var_list, input$colsplitby_sl)
      }
      if (input$rowsplitby_sl != "No split") {
        var_list <- append(var_list, input$rowsplitby_sl)
      }
      if (input$colorsplitby_sl != "No split") {
        var_list <- append(var_list, input$colorsplitby_sl)
      }


      # concat all variable in var list into a string to display as HTML
      str = ""
      for (e in var_list) {
        str = paste(str, sprintf("<b>%s</b>: %s", e, col_description[[e]]), " / ")

      }
      return(stringr::str_sub(str, 0, -3))
    })

    # create the text object and store it as output
    output$descriptions <- renderText(descriptions())

    # create the plotly object and store them as output
    output$plot <-
      plotly::renderPlotly(plot())


    # generate the inputs on the left side panel
    output$ui_input <- renderUI({
      fillRow(
        fillCol(
          if (input$plottype_sl == 'distribution') {
            selectInput(
              ns("variable_sl"),
              label = "Variable",
              choices = c("", colnames(dataset)),
              selected = ""
            )
          } else {
            selectInput(
              ns("variable_x_sl"),
              label = "X variable",
              choices = c("", numeric_col),
              selected = ""
            )
          },
          if (input$plottype_sl == 'distribution') {
            selectInput(ns("type_sl"),
                        label = "Type",
                        choices = c('count', 'density'),
            )
          } else {
            selectInput(
              ns("variable_y_sl"),
              label = "X variable",
              choices = c("", numeric_col),
              selected = ""
            )
          },

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
          ),
          flex = 1,
          height = "calc(100vh - 250px)"

        ),
        height = "calc(100vh - 250px)"
      )

    })


    # generate the output on the main panel
    output$ui_plot <- renderUI({
      if (input$plottype_sl == 'distribution') {
        validate(need(input$variable_sl, 'Select a variable to plot'))

        fillCol(
          htmlOutput(ns("descriptions")),
          plotly::plotlyOutput(ns("plot")),
          flex = c(1, 15),
          height = "calc(100vh - 160px)"
        )
      } else if (input$plottype_sl == 'scatter') {
        validate(
          need(input$variable_x_sl, 'Select a variable for the x axis'),
          need(input$variable_y_sl, 'Select a variable for the y axis')
        )
        fillCol(
          fillRow(htmlOutput(ns(
            "descriptions"
          )),
          height = "20px"),
          fillRow(plotly::plotlyOutput(ns("plot")),
                  height = "calc(100vh - 160px)")
          ,
          flex = c(1, 15),
          height = "calc(100vh - 160px)"
        )
      }


    })

  })



}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
