#' home UI Function
#'
#' @description The landing page of the application with a description and navigation boxes
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel(title = "Home",
             value = "home",
             hr(),
             br(), br(),
             HTML("<h1><center>WELCOME TO <b>Campus Recruitment</b> .... </center></h1>"),
             br(), br(), br(), br(),
             column(width = 3, align = "center",
                    pretty_tabs(texto = "Explore",
                                text_desc = "Explore freely the data through distribution and scatter plot, and split them in different categories",
                                cor = box_colors[1], icon = "fa-solid fa-map", id = ns("explore_bt"))
             ),
             column(width = 3, align = "center",
                    pretty_tabs(texto = "Profil",
                                text_desc = "Check the average profiles of student split by different categories",
                                cor = box_colors[2], icon = "fa fa-user", id = ns("profil_bt"))
             ),
             column(width = 3, align = "center",
                    pretty_tabs(texto = "Track",
                                text_desc = "Look at how this group of student flows from high school to their first job.",
                                cor = box_colors[3], icon ="fa fa-shuffle", id = ns("bias_bt"))
             ),
             column(width = 3, align = "center",
                    pretty_tabs(texto = "Credit",
                                text_desc = "Acknowledge other people's work",
                                cor = box_colors[4], icon = "fa fa-person-praying", id = ns("credit_bt"))
             ),
             column(width = 12,
                    br(), br(), br(), br(),
                    wellPanel(
                      HTML("<h1><b>CAMPUS Recruitment</b></h1>"),
                      HTML("<h4><b>Campus Recruitment</b> looks at the employability of a group of Indian students.
                           The university has set up an employability score based on a test which measures the chance of
                           getting employed. This app explores the world of young adults newly on the market by comparing their student
                           profiles and looking at the educational choices they made, and the score obtained at the different stages
                           (high school, university, specialization, etc.).</h4>")
                    )
             ),

    )

  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    tab_react <- reactiveValues(count=0)
    observeEvent(input$explore_bt,{
      tab_react$id <-"Explore"
      tab_react$count <- tab_react$count +1
    })
    observeEvent(input$profil_bt,{
      tab_react$id <-"Profil"
      tab_react$count <- tab_react$count +1
    })
    observeEvent(input$credit_bt,{
      tab_react$id <-"Credit"
      tab_react$count <- tab_react$count +1
    })

    observeEvent(input$bias_bt,{
      tab_react$id <-"Track"
      tab_react$count <- tab_react$count +1
    })

    return(tab_react)

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
