# app.R - Mobile-Optimized NUCLEARFF App
# Load required packages using pacman
pacman::p_load(
  shiny, # Core Shiny functionality
  shinyMobile, # Mobile-first Framework7 components
  dplyr, # Data manipulation
  DT, # Data tables
  tibble, # Modern data frames
  purrr, # Functional programming
  ggplot2, # Data visualization
  RSQLite, # Database support
  auth0, # Authentication
  openxlsx, # Excel export
  htmltools # HTML utilities
)

# Source mobile-optimized module files
source("R/mobile_module_home.R")
source("R/mobile_module_data.R")
source("R/mobile_module_viz.R")

# Try to source league_users.R at startup (optional)
if (file.exists("R/league_users.R")) {
  tryCatch(
    {
      source("R/league_users.R")
    },
    error = function(e) {
      message("Could not source R/league_users.R: ", e$message)
    }
  )
}

# MOBILE USER INTERFACE (UI) --------------------------------------------------------
ui <- shinyMobile::f7Page(
  # Framework7 app configuration for iOS/Android compatibility
  title = "NUCLEARFF",
  theme = "auto", # Automatically adapts to device theme (iOS/Android/dark/light)

  # Enhanced mobile options
  options = list(
    theme = "auto", # Auto-detect device theme
    dark = TRUE, # Enable dark mode support
    filled = FALSE, # Framework7 styling option
    color = "#007AFF", # Primary color (iOS blue)
    touch = list(
      tapHold = TRUE, # Enable tap-and-hold gestures
      tapHoldDelay = 750 # Delay for tap-and-hold (ms)
    ),
    navbar = list(
      hideOnPageScroll = TRUE, # Hide navbar when scrolling down
      iosCenterTitle = TRUE # Center title on iOS
    ),
    toolbar = list(
      hideOnPageScroll = FALSE # Keep bottom toolbar visible
    )
  ),

  # Include custom CSS for mobile enhancements
  tags$head(
    # Mobile viewport meta tag for proper scaling
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),

    # PWA manifest for app-like experience
    tags$link(rel = "manifest", href = "manifest.json"),

    # Custom mobile CSS
    includeCSS("www/mobile_styles.css"),

    # Font Awesome for icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),

    # Apple touch icon for iOS home screen
    tags$link(rel = "apple-touch-icon", href = "nuclearff/nuclearff-navbar-icon-color.png"),

    # Custom JavaScript for mobile interactions
    tags$script(HTML("
      // Enhanced mobile interactions
      $(document).ready(function() {
        // Add haptic feedback for supported devices
        function addHapticFeedback(element) {
          if ('vibrate' in navigator) {
            navigator.vibrate(10); // Short vibration
          }
        }

        // Add haptic feedback to buttons and links
        $('.button, .link, .tab-link').on('touchstart', function() {
          addHapticFeedback(this);
        });

        // Prevent zoom on double tap for iOS
        let lastTouchEnd = 0;
        document.addEventListener('touchend', function(event) {
          let now = (new Date()).getTime();
          if (now - lastTouchEnd <= 300) {
            event.preventDefault();
          }
          lastTouchEnd = now;
        }, false);

        // Handle orientation changes
        window.addEventListener('orientationchange', function() {
          setTimeout(function() {
            // Refresh any charts or responsive elements
            window.dispatchEvent(new Event('resize'));
          }, 100);
        });
      });
    "))
  ),

  # Main Framework7 app structure
  shinyMobile::f7TabLayout(
    # Navigation bar with mobile-optimized layout
    navbar = f7Navbar(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$img(
          src = "nuclearff/nuclearff-navbar-icon-color.png",
          height = "30px",
          alt = "Nuclear Analytics Lab"
        ),
        "NUCLEARFF"
      ),
      # Right side navigation elements
      rightPanel = tags$div(
        # Social media links optimized for mobile
        shinyMobile::f7Button(
          href = "https://x.com/nuclearffnolan",
          label = icon("x-twitter"),
          external = TRUE,
          fill = FALSE,
          color = "gray",
          size = "small"
        ),
        shinyMobile::f7Button(
          href = "https://discord.gg/6BsAYh5S",
          label = icon("discord"),
          external = TRUE,
          fill = FALSE,
          color = "gray",
          size = "small"
        ),
        shinyMobile::f7Button(
          href = "https://github.com/NuclearAnalyticsLab/otis",
          label = icon("github"),
          external = TRUE,
          fill = FALSE,
          color = "gray",
          size = "small"
        )
      )
    ),

    # Tab-based navigation optimized for mobile
    shinyMobile::f7Tabs(
      animated = TRUE, # Smooth transitions between tabs
      swipeable = TRUE, # Allow swiping between tabs on mobile

      # Home tab
      shinyMobile::f7Tab(
        tabName = "home",
        icon = f7Icon("house_fill"),
        text = "Home",
        active = TRUE,
        mobileHomeUI("home")
      ),

      # Data tab
      shinyMobile::f7Tab(
        tabName = "data",
        icon = f7Icon("table_fill"),
        text = "Users",
        mobileDataUI("data")
      ),

      # Visualizations tab
      shinyMobile::f7Tab(
        tabName = "viz",
        icon = f7Icon("chart_bar_fill"),
        text = "Charts",
        mobileVizUI("viz")
      )
    ),

    # Optional bottom toolbar for additional actions
    toolbar = shinyMobile::f7Toolbar(
      position = "bottom",
      # Theme toggle button
      shinyMobile::f7Button(
        inputId = "theme_toggle",
        label = f7Icon("circle_lefthalf_fill"),
        fill = FALSE,
        color = "gray",
      ),
      # Spacer
      tags$div(style = "flex: 1;"),
      # Logout button optimized for mobile
      shinyMobile::f7Button(
        inputId = "logout_btn",
        label = "Logout",
        fill = TRUE,
        color = "red",
        size = "small",
      ),
    )
  )
)

# MOBILE-OPTIMIZED SERVER ------------------------------------------------------------
server <- function(input, output, session) {
  # Initialize mobile modules with enhanced functionality
  mobileHomeServer("home")
  mobileDataServer("data")
  mobileVizServer("viz")

  # Theme toggle functionality for mobile
  observeEvent(input$theme_toggle, {
    # Toggle between light and dark themes
    shinyMobile::updateF7App(
      options = list(
        theme = if (input$theme_toggle %% 2 == 0) "light" else "dark"
      )
    )

    # Show toast notification for theme change
    shinyMobile::f7Toast(
      text = paste("Switched to", if (input$theme_toggle %% 2 == 0) "light" else "dark", "theme"),
      position = "center",
      closeTimeout = 2000
    )
  })

  # Mobile-optimized logout functionality
  observeEvent(input$logout_btn, {
    # Show confirmation dialog optimized for mobile
    shinyMobile::f7Dialog(
      title = "Logout",
      text = "Are you sure you want to logout?",
      type = "confirm"
    )
  })

  # Handle logout confirmation
  observeEvent(input$logout_confirm, {
    if (input$logout_confirm) {
      # Use auth0 logout or custom logout logic
      auth0::logout()
    }
  })

  # Handle connection quality changes (for mobile networks)
  observe({
    # Add logic to handle poor network conditions
    # This could include reducing data requests, caching, etc.
  })

  # Device orientation handling
  observeEvent(input$device_orientation, {
    # Adjust layouts based on orientation changes
    # Useful for responsive chart sizing
  })
}

# Enhanced app launch with mobile-specific options
auth0::shinyAppAuth0(
  ui = ui,
  server = server,
  config_file = "_auth0.yml" # Ensure auth0 config is mobile-friendly
)
