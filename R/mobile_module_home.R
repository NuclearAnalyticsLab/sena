# R/mobile_module_home.R
# Mobile-optimized home module using Framework7 components

#' Mobile Home UI Module
#' 
#' Creates a mobile-first home page layout using shinyMobile components
#' 
#' @param id Module namespace ID
#' @return Mobile-optimized UI elements
#' @export
mobileHomeUI <- function(id) {
  ns <- NS(id)
  
  # Framework7 page container for mobile layout
  f7Page(
    # Page content with mobile-optimized layout
    f7Block(
      # Hero section with mobile-friendly card
      f7Card(
        title = "Welcome to NUCLEARFF",
        # Mobile-optimized image and content
        tags$div(
          class = "hero-section",
          style = "text-align: center; padding: 20px;",
          
          # Responsive logo
          tags$img(
            src = "nuclearff/nuclearff-navbar-icon-color.png",
            style = "max-width: 120px; height: auto; margin-bottom: 20px;",
            alt = "Nuclear Analytics Lab"
          ),
          
          # Mobile-friendly typography
          tags$h2(
            "NuclearFF Dynasty",
            style = "font-size: 1.8rem; margin: 15px 0; font-weight: 600;"
          ),
          
          tags$p(
            "Your Fantasy Football Command Center",
            style = "font-size: 1.1rem; color: var(--f7-text-color); margin: 10px 0;"
          )
        )
      ),
      
      # Feature cards using mobile grid layout
      f7Row(
        # Data Management card
        f7Col(
          width = 100,  # Full width on mobile, will stack
          tablet = 50,  # Half width on tablet
          desktop = 33, # Third width on desktop
          
          f7Card(
            title = "League Data",
            footer = f7Button(
              inputId = ns("goto_data"),
              label = "View Data",
              fill = TRUE,
              color = "blue",
              size = "small"
            ),
            
            tags$div(
              style = "padding: 15px;",
              f7Icon("table_fill", style = "font-size: 2rem; color: #007AFF; margin-bottom: 10px;"),
              tags$p(
                "Access and analyze your Sleeper league data with powerful filtering and export options.",
                style = "font-size: 0.95rem; line-height: 1.4;"
              )
            )
          )
        ),
        
        # Analytics card
        f7Col(
          width = 100,
          tablet = 50,
          desktop = 33,
          
          f7Card(
            title = "Visualizations", 
            footer = f7Button(
              inputId = ns("goto_viz"),
              label = "View Charts", 
              fill = TRUE,
              color = "green",
              size = "small"
            ),
            
            tags$div(
              style = "padding: 15px;",
              f7Icon("chart_bar_fill", style = "font-size: 2rem; color: #34C759; margin-bottom: 10px;"),
              tags$p(
                "Interactive charts and graphs to visualize your league performance and trends.",
                style = "font-size: 0.95rem; line-height: 1.4;"
              )
            )
          )
        ),
        
        # Community card
        f7Col(
          width = 100,
          tablet = 50,
          desktop = 33,
          
          f7Card(
            title = "Community",
            footer = f7Button(
              inputId = ns("goto_community"),
              label = "Join Us",
              fill = TRUE, 
              color = "purple",
              size = "small"
            ),
            
            tags$div(
              style = "padding: 15px;",
              f7Icon("person_3_fill", style = "font-size: 2rem; color: #AF52DE; margin-bottom: 10px;"),
              tags$p(
                "Connect with fellow fantasy managers on Discord and follow us on social media.",
                style = "font-size: 0.95rem; line-height: 1.4;"
              )
            )
          )
        )
      ),
      
      # Quick stats section for mobile
      f7Card(
        title = "Quick Stats",
        
        f7List(
          inset = TRUE,
          
          # League status indicator
          f7ListItem(
            title = "League Status", 
            subtitle = "Connected",
            media = f7Icon("checkmark_circle_fill", color = "green"),
            right = textOutput(ns("league_status"))
          ),
          
          # Last update indicator  
          f7ListItem(
            title = "Last Updated",
            subtitle = "Data freshness",
            media = f7Icon("clock_fill", color = "blue"),
            right = textOutput(ns("last_update"))
          ),
          
          # Quick action items
          f7ListItem(
            title = "Quick Actions",
            subtitle = "Tap to expand",
            media = f7Icon("gear", color = "gray"),
            # Accordion for additional options
            f7Accordion(
              f7AccordionItem(
                title = "Settings",
                
                f7List(
                  f7ListItem(
                    title = "Dark Mode",
                    right = f7Toggle(
                      inputId = ns("dark_mode_toggle"),
                      checked = FALSE
                    )
                  ),
                  
                  f7ListItem( 
                    title = "Notifications",
                    right = f7Toggle(
                      inputId = ns("notifications_toggle"),
                      checked = TRUE
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # News/Updates section optimized for mobile scrolling
      f7Card(
        title = "Latest Updates",
        
        tags$div(
          style = "max-height: 200px; overflow-y: auto;",
          
          f7List(
            f7ListItem(
              title = "New Features Added",
              subtitle = "Enhanced mobile experience",
              text = "Mobile-optimized interface with Framework7 components for better iOS and Android compatibility.",
              media = f7Icon("star_fill", color = "orange")
            ),
            
            f7ListItem(
              title = "Data Export Available", 
              subtitle = "Export your league data",
              text = "Now you can export league data to Excel format for external analysis.",
              media = f7Icon("square_and_arrow_up", color = "blue")
            ),
            
            f7ListItem(
              title = "Performance Improvements",
              subtitle = "Faster loading times", 
              text = "Optimized data loading and caching for better performance on mobile networks.",
              media = f7Icon("speedometer", color = "green")
            )
          )
        )
      )
    ),
    
    # Floating action button for quick access (mobile pattern)
    f7Fab(
      inputId = ns("quick_action"),
      label = f7Icon("plus"),
      color = "blue",
      position = "right-bottom"
    )
  )
}

#' Mobile Home Server Module
#' 
#' Server logic for mobile-optimized home page
#' 
#' @param id Module namespace ID
#' @export
mobileHomeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Navigation handlers for mobile-friendly transitions
    observeEvent(input$goto_data, {
      # Switch to data tab with smooth animation
      updateF7Tabs(
        session = session,
        id = "main_tabs", 
        selected = "data"
      )
      
      # Mobile feedback
      f7Toast(
        text = "Loading league data...",
        position = "center",
        closeTimeout = 1500
      )
    })
    
    observeEvent(input$goto_viz, {
      # Switch to visualization tab
      updateF7Tabs(
        session = session,
        id = "main_tabs",
        selected = "viz" 
      )
      
      f7Toast(
        text = "Loading visualizations...",
        position = "center", 
        closeTimeout = 1500
      )
    })
    
    observeEvent(input$goto_community, {
      # Open external links in mobile-friendly way
      f7Dialog(
        title = "Join Our Community",
        text = "Choose your platform:",
        type = "custom",
        buttons = list(
          list(
            text = "Discord",
            onClick = "window.open('https://discord.gg/6BsAYh5S', '_blank');"
          ),
          list(
            text = "X (Twitter)", 
            onClick = "window.open('https://x.com/nuclearffnolan', '_blank');"
          ),
          list(
            text = "GitHub",
            onClick = "window.open('https://github.com/NuclearAnalyticsLab/otis', '_blank');"
          ),
          list(text = "Cancel")
        )
      )
    })
    
    # League status reactive output
    output$league_status <- renderText({
      # This would connect to your actual league data checking logic
      "Active"
    })
    
    # Last update timestamp
    output$last_update <- renderText({
      format(Sys.time(), "%H:%M")
    })
    
    # Dark mode toggle handler
    observeEvent(input$dark_mode_toggle, {
      # Update app theme based on toggle
      theme <- if (input$dark_mode_toggle) "dark" else "light"
      
      updateF7App(
        options = list(theme = theme)
      )
      
      # Mobile-friendly notification
      f7Toast(
        text = paste("Switched to", theme, "mode"),
        position = "bottom",
        closeTimeout = 2000
      )
    })
    
    # Notifications toggle
    observeEvent(input$notifications_toggle, {
      if (input$notifications_toggle) {
        f7Notification(
          title = "Notifications Enabled",
          text = "You'll receive updates about your leagues",
          icon = f7Icon("bell_fill"),
          closeTimeout = 3000
        )
      } else {
        f7Toast(
          text = "Notifications disabled",
          position = "center",
          closeTimeout = 2000
        )
      }
    })
    
    # Floating action button handler
    observeEvent(input$quick_action, {
      # Show action sheet with quick options (mobile pattern)
      f7ActionSheet(
        title = "Quick Actions",
        buttons = list(
          list(
            text = "Refresh Data",
            icon = f7Icon("arrow_clockwise"),
            color = "blue"
          ),
          list(
            text = "Export Data", 
            icon = f7Icon("square_and_arrow_up"),
            color = "green"
          ),
          list(
            text = "Settings",
            icon = f7Icon("gear"),
            color = "gray"
          ),
          list(
            text = "Cancel",
            color = "red"
          )
        )
      )
    })
    
    # Handle device-specific optimizations
    observe({
      # Detect mobile device and adjust accordingly
      session$onFlushed(function() {
        session$sendCustomMessage(
          type = "mobile-optimization",
          message = list(
            timestamp = Sys.time(),
            optimize_for_mobile = TRUE
          )
        )
      })
    })
  })
}