#' bias UI Function
#'
#' @description This module epxl
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bias_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' bias Server Functions
#'
#' @noRd
mod_bias_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}

## To be copied in the UI
# mod_bias_ui("bias_1")

## To be copied in the server
# mod_bias_server("bias_1")
