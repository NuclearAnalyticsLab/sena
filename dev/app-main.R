library(shiny)
library(bslib)
library(DT)
library(palmerpenguins)
library(dplyr)

ui <- page_navbar(
  title = "NUCLEARFF",
  id = "page",

  nav_panel("Home", dataTableOutput("table")),
  nav_panel("Rules", "Page B content"),
  nav_panel("Transactions", "Page C content"),

  nav_spacer(),

  # X icon
  nav_item(
    tags$a(
      href = "https://x.com/nuclearffnolan",
      target = "_blank",
      title = "X",
      tags$span(icon("x-twitter"), style = "font-size: 1.5rem; padding-top: 5px;")
    )
  ),

  # Discord icon
  nav_item(
    tags$a(
      href = "https://discord.gg/6BsAYh5S",
      target = "_blank",
      title = "Discord",
      tags$span(icon("discord"), style = "font-size: 1.5rem; padding-top: 5px;")
    )
  ),

  # GitHub icon
  nav_item(
    tags$a(
      href = "https://github.com/NuclearAnalyticsLab/otis",
      target = "_blank",
      title = "GitHub",
      tags$span(icon("github"), style = "font-size: 1.5rem; padding-top: 5px;")
    )
  ),

  # Sleeper icon
  nav_item(
    tags$a(
      href = "https://sleeper.com/leagues/1190192546172342272/league",
      target = "_blank",
      title = "NuclearFF Dynasty",
      tags$img(src = "nuclearff/nuclearff-navbar-icon.png", height = "30px", style = "padding-top: 0px;")
    )
  )

)

server <- function(input, output) {
  output$table <-
    renderDataTable({datatable(penguins)})
}


shinyApp(ui = ui, server = server)
