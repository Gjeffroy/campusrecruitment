#' credit UI Function
#'
#' @description In this module, we give credit to the work of other that inspired this app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_credit_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel(title = "Credit",
             value = "credit",
             hr(),
             br(), br(),
             h5("Developing this app was made possible by the work of other people.
         Giving credit to :"),
             HTML("<ul>
           <li>Ben Roshan D, MBA student and the organiser of the Kaggle competition, and his teacher, Dr. Dhimant Ganatara, Professor Jain University in Bangalore, for providing the <a href= 'https://www.kaggle.com/datasets/benroshan/factors-affecting-campus-placement'>data.</a></li>
           <li>The team of developpers of the <a href= 'https://github.com/voronoys/voronoys_sc'>Voronoy app</a> for the template that inspired this UI of this app.</li>
           <li> The thinkr developper team for the <a href= 'https://thinkr-open.github.io/golem/'> golem package </a> that facilitate the developpment of this production grade app.</li>
           <li> The R studio team for the awesome open source R Shiny package and the tidyverse</li>
           </ul>")
    )

  )
}

#' credit Server Functions
#'
#' @noRd
mod_credit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_credit_ui("credit_1")

## To be copied in the server
# mod_credit_server("credit_1")
