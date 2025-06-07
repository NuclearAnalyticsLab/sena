# home_module.R

# UI Function for Home module
homeUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("NUCLEARFF"),
    sidebarLayout(
      sidebarPanel(
        h3("About"),
        p("NuclearFF Dynasty"),
        p("Nuclear Fantasy Football Home Page.")
      ),
      mainPanel(
        h3("Main Content"),
        p("This is the main content area where data visualizations and tables will be displayed. In progress.")
      )
    )
  )
}

# Server function for Home module
homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Currently no server-side logic needed for home page
    # Add any reactive elements here if needed in the future
  })
}
