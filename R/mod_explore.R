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

    # add shinyjs to get the disable input select option
    shinyjs::useShinyjs(),
    hr(),
    fluidRow(

      # The left side panel with all the input selects
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

      # The main panel with all the plots
      column(
        width = 9,
        htmlOutput(ns("descriptions")),
        conditionalPanel(condition = "(input.plottype_sl == 'distribution') & (input.variable_sl!='')",
                         ns = ns,
                         plotlyOutput(ns("dist_plot"))),
        conditionalPanel(condition = "(input.plottype_sl == 'scatter') & (input.variable_y_sl!='') & (input.variable_x_sl!='')",
                         ns = ns,
                         plotlyOutput(ns("scatter_plot"))),

        conditionalPanel(
          condition = "(input.plottype_sl == 'distribution') & (input.variable_sl=='')",
          ns = ns ,
          h3("Select a variable to plot", style = "text-align: center;")

        ),
        conditionalPanel(
          condition = "(input.plottype_sl == 'scatter') & !((input.variable_y_sl!='') & (input.variable_x_sl!=''))",
          ns = ns ,
          h3("Select variable x and y to plot", style = "text-align: center;")
        ),
        align = "center"
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


    # disable the type dropdown in distribution mode if the variable id categorical
    # In that case the density does not make sens, so the plot will be on count
    observeEvent(input$variable_sl, {
      if (input$variable_sl %in% cat_col) {
        shinyjs::disable(id = "type_sl")
      } else{
        shinyjs::enable(id = "type_sl")
      }

    })

    # create distribution or bar plot base on input if scatter mode is on
    # and if the selected variable is categorical or numeric
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

    # create scatter plot base on input if scatter mode is on
    scat_plot <- reactive({
      if (input$plottype_sl == "scatter") {
        scatter_plot(
          dataset,
          input$variable_x_sl,
          input$variable_y_sl,
          colsplit_by = input$colsplitby_sl,
          rowsplit_by = input$rowsplitby_sl,
          colorsplit_by = input$colorsplitby_sl
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
