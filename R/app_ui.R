#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tags$head(
        tags$link(rel = "shortcut icon", href = "img/logo.png"),
        #-- biblio js ----
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.3.0/css/all.min.css"),
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
      ),
      list(tags$head(
        HTML('<link rel="icon", href="img/logo.png",
                        type="image/png" />')
      )),
      div(
        style = "padding: 1px 0px; width: '100%'; height:5px",
        titlePanel(title = "", windowTitle = "Campus Explorer"),

      ),

      ##-- Header ----
      navbarPage(
        title = div(img(src = "www/logo.png",
                        height = "60px"), style = "padding-left:100px;"),
        id = "navbar",
        selected = "Home",
        theme = "www/custom.css",
        fluid = T,
        ##-- TABS ----
        tabPanel("Home",
                 mod_home_ui("home_1")),
        tabPanel("Explore",
                 mod_explore_ui("explore_1")),
        tabPanel("Profil",
                 mod_profil_ui("profil_1")),
        tabPanel("Track",
                 mod_track_ui("track_1")),
        tabPanel("Credit",
                 mod_credit_ui("credit_1")),



      ),

      ##-- Footer ----
      div(includeHTML("inst/app/www/footer.html"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(path = app_sys("app/www"),
                     app_title = "campusrecruitment")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
