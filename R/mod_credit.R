#' credit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_credit_ui <- function(id){
  ns <- NS(id)
  tagList(
 
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
