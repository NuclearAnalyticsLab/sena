# app.R
# Load required packages using pacman
pacman::p_load(
  shiny, # For Shiny app functionality
  bslib, # For Bootstrap 5 and dark mode support
  dplyr, # For data manipulation
  DT,
  tibble, # For tibble dataframes
  purrr, # For functional programming
  ggplot2, # For plots
  RSQLite, # For SQLite database support
  auth0, # For authentication
  openxlsx # For Excel export functionality
)

# Source module files
source("R/module_home.R")
source("R/module_viz.R")
source("R/module_logo.R")
source("R/module_data.R")

# Try to source league_users.R at startup (optional - module will handle this)
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

# USER INTERFACE (UI) -----------------------------------------------------------------
ui <- fluidPage(
  # Apply minimal theme - let styles.css handle most theming
  theme = bslib::bs_theme(
    version = 5,
    base_font = font_google("Roboto Mono")
  ),

  # Include custom CSS file and theme detection
  tags$head(
    includeCSS("www/styles.css"),
    # Add Font Awesome
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    # JavaScript for enhanced theme detection and custom handling
    tags$script(HTML("
      // Theme detection and management
      function initThemeDetection() {
        // Function to update theme-specific elements
        function updateThemeElements() {
          const isDark = document.documentElement.getAttribute('data-bs-theme') === 'dark' ||
                        document.body.classList.contains('dark-mode') ||
                        (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);

          // Dispatch custom event for theme change
          window.dispatchEvent(new CustomEvent('themechange', {
            detail: { isDark: isDark }
          }));

          // Update any JavaScript-dependent styling
          updateDataTableTheme(isDark);
        }

        // Function to update DataTable theme
        function updateDataTableTheme(isDark) {
          // This will be called when DataTables are rendered
          setTimeout(() => {
            if (isDark) {
              $('.dataTables_wrapper').addClass('dark-theme');
              $('.dataTables_wrapper table').css({
                'background-color': '#101010',
                'color': '#ffffff'
              });
            } else {
              $('.dataTables_wrapper').removeClass('dark-theme');
              $('.dataTables_wrapper table').css({
                'background-color': '#ffffff',
                'color': '#212529'
              });
            }
          }, 100);
        }

        // Observer for theme changes
        const observer = new MutationObserver((mutations) => {
          mutations.forEach((mutation) => {
            if (mutation.type === 'attributes' &&
                (mutation.attributeName === 'data-bs-theme' ||
                 mutation.attributeName === 'class')) {
              updateThemeElements();
            }
          });
        });

        // Custom dark mode toggle functionality
        $(document).ready(function() {
          $('#custom-dark-toggle').click(function() {
            // Toggle the dark theme
            const html = document.documentElement;
            const currentTheme = html.getAttribute('data-bs-theme');
            const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
            html.setAttribute('data-bs-theme', newTheme);

            // Store preference in sessionStorage
            sessionStorage.setItem('theme', newTheme);
          });

          // Initialize theme from sessionStorage or system preference
          function initializeTheme() {
            const savedTheme = sessionStorage.getItem('theme');
            const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
            const theme = savedTheme || (prefersDark ? 'dark' : 'light');
            document.documentElement.setAttribute('data-bs-theme', theme);
          }

          initializeTheme();
        });

        // Start observing
        observer.observe(document.documentElement, {
          attributes: true,
          attributeFilter: ['data-bs-theme', 'class']
        });

        observer.observe(document.body, {
          attributes: true,
          attributeFilter: ['class']
        });

        // Listen for system theme changes
        if (window.matchMedia) {
          window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', updateThemeElements);
        }

        // Initial theme setup
        updateThemeElements();
      }

      // Initialize when DOM is ready
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initThemeDetection);
      } else {
        initThemeDetection();
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
      tags$a("USERS", href = "#", class = "nav-link", id = "data-tab")
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
      # Enhanced dark mode toggle
      tags$button(
        id = "custom-dark-toggle",
        class = "custom-dark-mode-toggle",
        title = "Light/Dark Mode",
        tags$i(class = "fa-solid fa-circle-half-stroke")
      ),
      # Logout button
      auth0::logoutButton(label = "Logout", class = "logout-btn", title = "Logout")
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

    # Data tab content (initially hidden)
    tags$div(
      id = "data-content",
      class = "tab-content",
      style = "display: none;",
      dataUI("data")
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

      $('#data-tab').click(function(e) {
        e.preventDefault();
        $('.nav-link').removeClass('active');
        $(this).addClass('active');
        $('.tab-content').hide();
        $('#data-content').show();
      });

      // Listen for theme changes to update components
      window.addEventListener('themechange', function(event) {
        console.log('Theme changed to:', event.detail.isDark ? 'dark' : 'light');
        // Add any additional theme-specific JavaScript here
      });
    });
  "))
)

# Define server
server <- function(input, output, session) {
  # Call module servers
  homeServer("home")
  vizServer("viz")
  dataServer("data")

  # Initialize the logo module with single logo
  logoServer(
    id = "app_logo",
    session = session,
    logo_path = "nuclearff/nuclearff-navbar-icon-color.png",
    height = 30,
    alt_text = "Nuclear Analytics Lab"
  )

  # Reactive value to track theme state
  current_theme <- reactive({
    input$dark_mode
  })

  # Optional: React to theme changes
  observeEvent(current_theme(), {
    # Add any server-side logic that should respond to theme changes
    message("Theme changed to: ", if (current_theme()) "dark" else "light")
  })
}

# Run the app
auth0::shinyAppAuth0(ui, server)
