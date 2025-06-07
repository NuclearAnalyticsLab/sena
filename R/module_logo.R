# R/module_logo.R

#' UI function for the logo module
#'
#' @param id The module ID
#' @return A UI element
#' @export
logoUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::uiOutput(ns("navbar_logo"))
}

#' Server function for the logo module
#'
#' @param id The module ID
#' @param session The current Shiny session
#' @param logo_path Path to the logo image
#' @param height Logo height in pixels
#' @param alt_text Alternative text for the logo
#' @return NULL
#' @export
logoServer <- function(id,
                       session,
                       logo_path = "nuclearff/nuclearff-navbar-icon-color.png",
                       height = 70,
                       alt_text = "Nuclear Analytics Lab") {
    shiny::moduleServer(id, function(input, output, session) {
        # Render the logo UI - no theme checking
        output$navbar_logo <- shiny::renderUI({
            # Return the image tag with fixed logo
            shiny::tags$div(
                style = "padding: 10px;",
                shiny::tags$img(
                    src = logo_path,
                    height = paste0(height, "px"),
                    alt = alt_text
                )
            )
        })
    })
}
