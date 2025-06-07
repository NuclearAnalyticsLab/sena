# Module UI function for social media navigation items
socialNavItemsUI <- function(id) {
  ns <- NS(id) # Create a namespace function

  # Return the list of nav items
  tagList(
    # X icon
    nav_item(
      tags$a(
        href = "https://x.com/nuclearffnolan",
        target = "_blank",
        title = "X",
        tags$span(icon("x-twitter"), style = "font-size: 1.3rem; padding-top: 0px;")
      )
    ),
    # Discord icon
    nav_item(
      tags$a(
        href = "https://discord.gg/6BsAYh5S",
        target = "_blank",
        title = "Discord",
        tags$span(icon("discord"), style = "font-size: 1.3rem; padding-top: 0px;")
      )
    ),
    # GitHub icon
    nav_item(
      tags$a(
        href = "https://github.com/NuclearAnalyticsLab/otis",
        target = "_blank",
        title = "GitHub",
        tags$span(icon("github"), style = "font-size: 1.3rem; padding-top: 0px;")
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
}
