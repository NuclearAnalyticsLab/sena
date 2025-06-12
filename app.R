# app.R
# Load required packages using pacman
pacman::p_load(
  shiny, # For Shiny app functionality
  bslib, # For Bootstrap 5 and dark mode support
  dplyr, # For data manipulation
  tibble, # For tibble dataframes
  purrr, # For functional programming
  ggplot2, # For plots
  RSQLite, # For SQLite database support
  auth0 # For authentication
)

# Source module files
source("R/module_home.R")
source("R/module_explorer.R")
source("R/module_viz.R")
source("R/module_logo.R")
source("R/league_users.R")

league_id <- "1190192546172342272" # 2025 NuclearFF Dynasty
users <- parse_league_users(league_id)

# USER INTERFACE (UI) -----------------------------------------------------------------
ui <- fluidPage(
  # Apply dark theme
  theme = bslib::bs_theme(
    version = 5,
    bg = "#101010",
    fg = "#FFFFFF",
    base_font = font_google("Roboto Mono"),
    code_font = font_google("Roboto Mono"),
    heading_font = font_google("Roboto Mono")
  ),

  # Custom CSS for the navbar and general styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #101010;
        color: #FFFFFF;
        font-family: 'Roboto Mono', monospace;
      }

      .custom-navbar {
        background-color: #101010;
        padding: 10px 15px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 1000;
        border-bottom: 1px solid #333;
        box-shadow: 0 2px 4px rgba(0,0,0,0.3);
        margin-left: -15px;
        padding-left: 30px;
        padding-right: 30px;
      }

      .navbar-left {
        display: flex;
        align-items: center;
        gap: 20px;
      }

      .navbar-right {
        display: flex;
        align-items: center;
        gap: 15px;
      }

      .nav-link {
        color: #FFFFFF;
        text-decoration: none;
        padding: 8px 12px;
        border-radius: 4px;
        transition: background-color 0.3s;
      }

      .nav-link:hover {
        background-color: rgba(255, 255, 255, 0.1);
        color: #FFFFFF;
        text-decoration: none;
      }

      .nav-link.active {
        background-color: rgba(255, 255, 255, 0.2);
        font-weight: bold;
      }

      .social-icon {
        font-size: 1.3rem;
        padding: 8px;
        color: #FFFFFF;
        transition: color 0.3s;
      }

      .social-icon:hover {
        color: #007bff;
        text-decoration: none;
      }

      .main-content {
        margin-top: 80px;
        padding: 20px;
      }

      .logout-btn {
        background: transparent;
        border: 1px solid #FFFFFF;
        color: #FFFFFF;
        padding: 6px 12px;
        border-radius: 4px;
        cursor: pointer;
        transition: all 0.3s;
      }

      .logout-btn:hover {
        background-color: #FFFFFF;
        color: #101010;
      }

      /* Tab content styling */
      .tab-content {
        background-color: #101010;
        color: #FFFFFF;
        padding: 20px;
        border-radius: 8px;
        margin-top: 10px;
      }

      /* Override any Bootstrap styles that might interfere */
      .nav-tabs .nav-link {
        border: none;
        background: transparent;
        color: #FFFFFF;
      }

      .nav-tabs .nav-link.active {
        background-color: rgba(255, 255, 255, 0.2);
        color: #FFFFFF;
        border: none;
      }
    "))
  ),

  # Custom navbar header
  tags$header(
    class = "custom-navbar",

    # Left side - Logo and navigation
    tags$div(
      class = "navbar-left",
      # Logo
      logoUI("app_logo"),
      # Navigation links
      tags$a("HOME", href = "#", class = "nav-link", id = "home-tab"),
      tags$a("USERS", href = "#", class = "nav-link", id = "users-tab")
    ),

    # Right side - Social icons, dark mode, logout
    tags$div(
      class = "navbar-right",
      # X (Twitter) icon
      tags$a(
        href = "https://x.com/nuclearffnolan",
        target = "_blank",
        title = "X",
        class = "social-icon",
        icon("x-twitter")
      ),
      # Discord icon
      tags$a(
        href = "https://discord.gg/6BsAYh5S",
        target = "_blank",
        title = "Discord",
        class = "social-icon",
        icon("discord")
      ),
      # GitHub icon
      tags$a(
        href = "https://github.com/NuclearAnalyticsLab/otis",
        target = "_blank",
        title = "GitHub",
        class = "social-icon",
        icon("github")
      ),
      # Dark mode toggle
      bslib::input_dark_mode(),
      # Logout button
      auth0::logoutButton(label = "Logout", class = "logout-btn")
    )
  ),

  # Main content area
  tags$div(
    class = "main-content",

    # Tab content (initially show home)
    tags$div(
      id = "home-content",
      class = "tab-content",
      homeUI("home")
    ),
    tags$div(
      id = "users-content",
      class = "tab-content",
      style = "display: none;",
      h3("League Users"),
      DT::dataTableOutput("users")
    )
  ),

  # JavaScript for tab switching
  tags$script(HTML("
    $(document).ready(function() {
      // Set initial active tab
      $('#home-tab').addClass('active');

      // Tab switching functionality
      $('#home-tab').click(function(e) {
        e.preventDefault();
        $('.nav-link').removeClass('active');
        $(this).addClass('active');
        $('.tab-content').hide();
        $('#home-content').show();
      });

      $('#users-tab').click(function(e) {
        e.preventDefault();
        $('.nav-link').removeClass('active');
        $(this).addClass('active');
        $('.tab-content').hide();
        $('#users-content').show();
      });
    });
  "))
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

  # Users data table
  output$users <- DT::renderDataTable(
    DT::datatable(
      users,
      options = list(
        paging = FALSE,
        searching = FALSE,
        dom = "t", # Only show table, no other controls
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#333', 'color': '#fff'});",
          "}"
        )
      )
    ) %>%
      DT::formatStyle(columns = colnames(users), backgroundColor = "#101010", color = "#FFFFFF")
  )
}

# Run the app
auth0::shinyAppAuth0(ui, server)
