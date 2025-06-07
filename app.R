# app.R
# Load required packages using pacman
pacman::p_load(
  shiny, # For Shiny app functionality
  bslib, # For Bootstrap 5 and dark mode support
  dplyr, # For data manipulation
  tibble, # For tibble dataframes
  purrr, # For functional programming
  ggplot2, # For plots
  RSQLite # For SQLite database support
)

# Source module files
source("R/module_home.R")
source("R/module_explorer.R")
source("R/module_viz.R")
source("R/module_logo.R")
source("R/league_users.R")

league_id <- "1190192546172342272" # 2025 NuclearFF Dynasty
users <- parse_league_users(league_id)

# Define UI
ui <- bslib::page_navbar(
  # Use the logoUI module for the navbar title
  title = logoUI("app_logo"),
  theme = bslib::bs_theme(
    version = 5,
    bg = "#101010",
    fg = "#FFFFFF",
    base_font = font_google("Roboto Mono"),
    code_font = font_google("Roboto Mono"),
    heading_font = font_google("Roboto Mono")
  ) |> bslib::bs_add_rules(
    # Custom CSS to reduce navbar height
    ".navbar { padding: 0rem !important;}
    .navbar .container-fluid {
      padding-bottom: 0px !important;
    }"
    # Remove the underline from active tabs
    # ".nav-link.active {
    #   border-bottom: none !important;
    #   box-shadow: none !important;
    # }",
  ),
  navbar_options = bslib::navbar_options(
    position = "static-top",
    underline = FALSE
  ),
  # First tab: Home
  bslib::nav_panel(
    title = "HOME",
    homeUI("home")
  ),

  # Second tab: Data Explorer
  # bslib::nav_panel(
  #   title = "DATA",
  #   explorerUI("explorer")
  # ),

  # Third tab: Visualizations
  # bslib::nav_panel(
  #   title = "VIZ",
  #   vizUI("viz")
  # ),
  bslib::nav_panel(
    title = "USERS",
    DT::dataTableOutput("users")
  ),

  # Spacer push dark mode toggle right
  bslib::nav_spacer(),

  # X icon
  nav_item(
    tags$a(
      href = "https://x.com/nuclearffnolan",
      target = "_blank",
      title = "X",
      tags$span(icon("x-twitter"),
        style = "font-size: 1.3rem; padding: 0px;"
      )
    )
  ),

  # Discord icon
  nav_item(
    tags$a(
      href = "https://discord.gg/6BsAYh5S",
      target = "_blank",
      title = "Discord",
      tags$span(icon("discord"), style = "font-size: 1.3rem; padding: 0px;")
    )
  ),

  # GitHub icon
  nav_item(
    tags$a(
      href = "https://github.com/NuclearAnalyticsLab/otis",
      target = "_blank",
      title = "GitHub",
      tags$span(icon("github"), style = "font-size: 1.3rem; padding: 0px;")
    )
  ),
  # Dark mode toggle in the navbar
  bslib::nav_item(bslib::input_dark_mode())
)

# Define server
server <- function(input, output, session) {
  # Call module servers
  homeServer("home")
  explorerServer("explorer")
  vizServer("viz")

  # Initialize the logo module with single logo
  logoServer(
    id = "app_logo",
    session = session,
    logo_path = "nuclearff/nuclearff-navbar-icon-color.png",
    height = 30,
    alt_text = "Nuclear Analytics Lab"
  )

  output$users <- DT::renderDataTable(
    DT::datatable(users, options = list(paging = FALSE, searching = FALSE))
  )
}

# Run the app
shiny::shinyApp(ui = ui, server = server)
