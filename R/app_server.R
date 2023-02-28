#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  tab_react  <-mod_home_server("home_1")
  observe({updateTabsetPanel(session = session, inputId = "navbar", selected = tab_react$id)
    tab_react$count})
  mod_explore_server("explore_1")
  mod_profil_server("profil_1")
  mod_track_server("track_1")
  mod_credit_server("credit_1")
}
