#' profil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_profil_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' profil Server Functions
#'
#' @noRd 
mod_profil_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_profil_ui("profil_1")
    
## To be copied in the server
# mod_profil_server("profil_1")
