# R/mobile_module_viz.R
# Mobile-optimized visualization module with Framework7 components

#' Mobile Visualization UI Module
#' 
#' Creates mobile-first data visualization interface using Framework7
#' 
#' @param id Module namespace ID
#' @return Mobile-optimized visualization UI
#' @export
mobileVizUI <- function(id) {
  ns <- NS(id)
  
  f7Page(
    # Mobile chart controls in compact card format
    f7Block(
      f7Card(
        title = "Chart Controls",
        
        # Mobile-optimized input controls
        f7List(
          # Chart type selector
          f7ListItem(
            title = "Chart Type",
            subtitle = "Select visualization style",
            right = f7Select(
              inputId = ns("chart_type"),
              choices = list(
                "Performance Overview" = "performance",
                "Win/Loss Analysis" = "winloss", 
                "Points Distribution" = "points",
                "Team Comparison" = "comparison",
                "Trend Analysis" = "trends"
              ),
              selected = "performance"
            )
          ),
          
          # Data source selector
          f7ListItem(
            title = "Data Source",
            subtitle = "Choose dataset",
            right = f7Select(
              inputId = ns("data_source"),
              choices = list(
                "Current League" = "current",
                "Sample Data" = "sample",
                "Historical Data" = "historical"
              ),
              selected = "sample"
            )
          ),
          
          # Theme selector for charts
          f7ListItem(
            title = "Chart Theme",
            subtitle = "Visual style",
            right = f7Select(
              inputId = ns("chart_theme"),
              choices = list(
                "Auto (System)" = "auto",
                "Light Theme" = "light",
                "Dark Theme" = "dark",
                "High Contrast" = "contrast"
              ),
              selected = "auto"
            )
          )
        ),
        
        # Quick action buttons
        f7Row(
          f7Col(
            width = 50,
            f7Button(
              inputId = ns("refresh_chart"),
              label = "Refresh",
              fill = FALSE,
              color = "blue",
              size = "large"
            )
          ),
          f7Col(
            width = 50,
            f7Button(
              inputId = ns("export_chart"),
              label = "Export",
              fill = TRUE,
              color = "green", 
              size = "large"
            )
          )
        )
      )
    ),
    
    # Chart display area optimized for mobile
    f7Block(
      # Chart container with responsive sizing
      f7Card(
        title = tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          textOutput(ns("chart_title")),
          f7Button(
            inputId = ns("chart_options"),
            label = f7Icon("ellipsis_circle"),
            fill = FALSE,
            color = "gray",
            size = "small"
          )
        ),
        
        # Responsive chart output
        tags$div(
          style = "width: 100%; overflow-x: auto; -webkit-overflow-scrolling: touch;",
          plotOutput(
            ns("main_chart"),
            height = "400px", # Fixed height for mobile consistency
            # Touch interactions for mobile
            click = ns("chart_click"),
            dblclick = ns("chart_dblclick"),
            brush = brushOpts(
              id = ns("chart_brush"),
              resetOnNew = TRUE
            )
          )
        ),
        
        # Chart interaction feedback
        conditionalPanel(
          condition = paste0("input['", ns("chart_click"), "'] != null"),
          f7Card(
            title = "Chart Interaction",
            tags$div(
              style = "padding: 10px;",
              verbatimTextOutput(ns("click_info"))
            )
          )
        )
      )
    ),
    
    # Statistics summary optimized for mobile
    f7Block(
      f7Card(
        title = "Quick Stats",
        
        f7List(
          inset = TRUE,
          
          # Dynamic statistics based on current chart
          f7ListItem(
            title = "Total Teams",
            right = textOutput(ns("stat_teams"))
          ),
          
          f7ListItem(
            title = "Average Points",
            right = textOutput(ns("stat_avg_points"))
          ),
          
          f7ListItem(
            title = "Top Performer",
            right = textOutput(ns("stat_top_performer"))
          ),
          
          f7ListItem(
            title = "Data Updated", 
            right = textOutput(ns("stat_last_update"))
          )
        )
      )
    ),
    
    # Additional insights in expandable format
    f7Block(
      f7Card(
        title = "Insights & Analytics",
        
        f7Accordion(
          # Performance insights
          f7AccordionItem(
            title = "Performance Analysis",
            
            tags$div(
              style = "padding: 15px;",
              uiOutput(ns("performance_insights"))
            )
          ),
          
          # Trend analysis
          f7AccordionItem(
            title = "Trend Analysis",
            
            tags$div(
              style = "padding: 15px;",
              uiOutput(ns("trend_insights"))
            )
          ),
          
          # Recommendations
          f7AccordionItem(
            title = "Recommendations",
            
            tags$div(
              style = "padding: 15px;",
              uiOutput(ns("recommendations"))
            )
          )
        )
      )
    ),
    
    # Floating action button for chart actions
    f7Fab(
      inputId = ns("chart_fab"),
      label = f7Icon("chart_bar"),
      color = "green",
      position = "left-bottom"
    )
  )
}

#' Mobile Visualization Server Module
#' 
#' Server logic for mobile-optimized data visualizations
#' 
#' @param id Module namespace ID  
#' @export
mobileVizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for chart management
    current_data <- reactiveVal()
    chart_settings <- reactiveVal(list())
    
    # Sample data generator for visualization testing
    create_sample_viz_data <- function() {
      tibble::tibble(
        team_id = 1:10,
        team_name = paste("Team", 1:10),
        manager = paste("Manager", LETTERS[1:10]),
        wins = sample(0:13, 10, replace = TRUE),
        losses = 13 - wins,
        points_for = round(runif(10, 1200, 1800), 1),
        points_against = round(runif(10, 1150, 1750), 1),
        week = rep(1:13, length.out = 10),
        position = sample(c("QB", "RB", "WR", "TE", "K", "DST"), 10, replace = TRUE)
      )
    }
    
    # Initialize with sample data
    observe({
      current_data(create_sample_viz_data())
    })
    
    # Dynamic chart title based on selection
    output$chart_title <- renderText({
      switch(input$chart_type,
        "performance" = "Team Performance Overview",
        "winloss" = "Win/Loss Analysis", 
        "points" = "Points Distribution",
        "comparison" = "Team Comparison",
        "trends" = "Season Trends",
        "Fantasy Chart"
      )
    })
    
    # Main chart output with mobile optimizations
    output$main_chart <- renderPlot({
      data <- current_data()
      if (is.null(data)) return(NULL)
      
      # Base theme for mobile optimization
      base_theme <- theme_minimal() +
        theme(
          text = element_text(size = 12),           # Readable on mobile
          axis.text = element_text(size = 10),      # Appropriate axis text size
          axis.title = element_text(size = 11),     # Clear axis titles
          plot.title = element_text(size = 14, face = "bold"), # Prominent title
          legend.text = element_text(size = 10),    # Readable legend
          legend.position = "bottom",               # Better for mobile layout
          panel.grid.minor = element_blank(),       # Cleaner look on small screens
          plot.margin = margin(10, 10, 10, 10)     # Adequate margins
        )
      
      # Apply dark theme if selected or system is dark
      if (input$chart_theme == "dark" || 
          (input$chart_theme == "auto" && input$system_theme == "dark")) {
        base_theme <- base_theme +
          theme(
            panel.background = element_rect(fill = "#1a1a1a", color = NA),
            plot.background = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            panel.grid.major = element_line(color = "#333333", size = 0.2)
          )
      }
      
      # Generate chart based on type selection
      if (input$chart_type == "performance") {
        # Team performance overview
        ggplot(data, aes(x = reorder(team_name, points_for), y = points_for)) +
          geom_col(aes(fill = wins), alpha = 0.8) +
          coord_flip() + # Better for mobile viewing
          scale_fill_gradient(low = "#ff6b6b", high = "#4ecdc4", name = "Wins") +
          labs(
            title = "Team Performance by Points",
            x = "Team",
            y = "Points For"
          ) +
          base_theme
          
      } else if (input$chart_type == "winloss") {
        # Win/Loss analysis
        data |>
          pivot_longer(cols = c(wins, losses), names_to = "result", values_to = "count") |>
          ggplot(aes(x = reorder(team_name, wins), y = count, fill = result)) +
          geom_col(position = "stack") +
          coord_flip() +
          scale_fill_manual(
            values = c("wins" = "#4ecdc4", "losses" = "#ff6b6b"),
            labels = c("Losses", "Wins")
          ) +
          labs(
            title = "Win/Loss Record by Team",
            x = "Team", 
            y = "Games",
            fill = "Result"
          ) +
          base_theme
          
      } else if (input$chart_type == "points") {
        # Points distribution  
        ggplot(data, aes(x = points_for)) +
          geom_histogram(bins = 15, fill = "#4ecdc4", alpha = 0.7, color = "white") +
          geom_vline(aes(xintercept = mean(points_for)), 
                    color = "#ff6b6b", linetype = "dashed", size = 1) +
          labs(
            title = "Distribution of Points Scored",
            x = "Points For",
            y = "Frequency"
          ) +
          base_theme
          
      } else if (input$chart_type == "comparison") {
        # Team comparison scatter plot
        ggplot(data, aes(x = points_for, y = points_against)) +
          geom_point(aes(color = wins, size = wins), alpha = 0.7) +
          geom_smooth(method = "lm", se = FALSE, color = "#ff6b6b", linetype = "dashed") +
          scale_color_gradient(low = "#ff6b6b", high = "#4ecdc4", name = "Wins") +
          scale_size_continuous(range = c(3, 8), name = "Wins") +
          labs(
            title = "Points For vs Points Against",
            x = "Points For",
            y = "Points Against"
          ) +
          base_theme
          
      } else if (input$chart_type == "trends") {
        # Simple trend analysis (mock weekly data)
        trend_data <- data |>
          select(team_name, points_for) |>
          slice_head(n = 6) |>
          cross_join(tibble(week = 1:13)) |>
          mutate(
            weekly_points = points_for + rnorm(n(), 0, 50),
            weekly_points = pmax(weekly_points, 50) # Minimum realistic score
          )
        
        ggplot(trend_data, aes(x = week, y = weekly_points, color = team_name)) +
          geom_line(alpha = 0.7, size = 1) +
          geom_smooth(se = FALSE, size = 0.5) +
          scale_x_continuous(breaks = 1:13) +
          labs(
            title = "Season Scoring Trends", 
            x = "Week",
            y = "Points Scored",
            color = "Team"
          ) +
          base_theme +
          theme(legend.position = "none") + # Remove legend for cleaner mobile view
          facet_wrap(~team_name, scales = "free_y", ncol = 2) # Small multiples for mobile
      }
      
    }, height = 400, res = 96) # Optimized resolution for mobile
    
    # Chart click interaction handler
    output$click_info <- renderText({
      if (is.null(input$chart_click)) {
        "Tap on chart elements for details"
      } else {
        paste(
          "Clicked coordinates:",
          paste("X:", round(input$chart_click$x, 2)),
          paste("Y:", round(input$chart_click$y, 2)),
          sep = "\n"
        )
      }
    })
    
    # Quick statistics outputs
    output$stat_teams <- renderText({
      data <- current_data()
      if (is.null(data)) return("0")
      as.character(nrow(data))
    })
    
    output$stat_avg_points <- renderText({
      data <- current_data()
      if (is.null(data)) return("0")
      round(mean(data$points_for, na.rm = TRUE), 1)
    })
    
    output$stat_top_performer <- renderText({
      data <- current_data()
      if (is.null(data)) return("None")
      top_team <- data[which.max(data$points_for), ]
      top_team$team_name
    })
    
    output$stat_last_update <- renderText({
      format(Sys.time(), "%H:%M")
    })
    
    # Performance insights
    output$performance_insights <- renderUI({
      data <- current_data()
      if (is.null(data)) return(p("No data available"))
      
      # Calculate insights
      avg_points <- round(mean(data$points_for), 1)
      top_team <- data[which.max(data$points_for), ]
      bottom_team <- data[which.min(data$points_for), ]
      
      tagList(
        f7Icon("chart_line_uptrend_xyaxis", color = "green"),
        tags$h4("Key Insights", style = "margin: 10px 0;"),
        tags$p(paste("League average:", avg_points, "points")),
        tags$p(paste("Highest scorer:", top_team$team_name, "-", top_team$points_for, "points")),
        tags$p(paste("Lowest scorer:", bottom_team$team_name, "-", bottom_team$points_for, "points")),
        tags$p(paste("Point spread:", round(top_team$points_for - bottom_team$points_for, 1), "points"))
      )
    })
    
    # Trend insights  
    output$trend_insights <- renderUI({
      data <- current_data()
      if (is.null(data)) return(p("No data available"))
      
      tagList(
        f7Icon("arrow_up_right", color = "blue"),
        tags$h4("Trends", style = "margin: 10px 0;"),
        tags$p("• League is highly competitive with close scoring"),
        tags$p("• Points distribution shows balanced offensive strategies"), 
        tags$p("• Win correlation with points suggests consistent performers"),
        tags$p("• No significant outliers detected in team performance")
      )
    })
    
    # Recommendations
    output$recommendations <- renderUI({
      data <- current_data()
      if (is.null(data)) return(p("No data available"))
      
      tagList(
        f7Icon("lightbulb", color = "orange"),
        tags$h4("Recommendations", style = "margin: 10px 0;"),
        tags$p("• Monitor waiver wire for consistent point scorers"),
        tags$p("• Focus on matchup analysis for weekly optimization"),
        tags$p("• Consider trade opportunities with bottom performers"),
        tags$p("• Track injury reports for roster management")
      )
    })
    
    # Chart refresh handler
    observeEvent(input$refresh_chart, {
      f7Toast(
        text = "Refreshing chart data...",
        position = "center",
        closeTimeout = 1500
      )
      
      # Simulate data refresh
      current_data(create_sample_viz_data())
    })
    
    # Chart export handler
    observeEvent(input$export_chart, {
      f7ActionSheet(
        title = "Export Chart",
        buttons = list(
          list(
            text = "Save as PNG",
            icon = f7Icon("photo"),
            color = "blue"
          ),
          list(
            text = "Save as PDF", 
            icon = f7Icon("doc"),
            color = "green"
          ),
          list(
            text = "Share",
            icon = f7Icon("square_and_arrow_up"),
            color = "purple"
          ),
          list(text = "Cancel", color = "red")
        )
      )
    })
    
    # Chart options handler
    observeEvent(input$chart_options, {
      f7ActionSheet(
        title = "Chart Options",
        buttons = list(
          list(
            text = "Fullscreen View",
            icon = f7Icon("arrow_up_left_and_arrow_down_right"), 
            color = "blue"
          ),
          list(
            text = "Reset Zoom",
            icon = f7Icon("arrow_counterclockwise"),
            color = "orange"
          ),
          list(
            text = "Chart Settings",
            icon = f7Icon("gear"),
            color = "gray"
          ),
          list(text = "Cancel")
        )
      )
    })
    
    # Floating action button handler
    observeEvent(input$chart_fab, {
      f7ActionSheet(
        title = "Chart Actions",
        buttons = list(
          list(
            text = "Generate Report",
            icon = f7Icon("doc_text"),
            color = "blue"
          ),
          list(
            text = "Compare Teams",
            icon = f7Icon("arrow_left_arrow_right"),
            color = "green"
          ),
          list(
            text = "Prediction Model",
            icon = f7Icon("brain"),
            color = "purple"
          ),
          list(text = "Cancel")
        )
      )
    })
    
    # Handle data source changes
    observeEvent(input$data_source, {
      if (input$data_source == "sample") {
        current_data(create_sample_viz_data())
        f7Toast(
          text = "Loaded sample data",
          position = "center",
          closeTimeout = 1500
        )
      } else if (input$data_source == "current") {
        f7Toast(
          text = "Loading current league data...",
          position = "center", 
          closeTimeout = 2000
        )
        # Here you would load actual league data
      } else if (input$data_source == "historical") {
        f7Toast(
          text = "Loading historical data...",
          position = "center",
          closeTimeout = 2000
        )
        # Here you would load historical data
      }
    })
    
    # Theme change handler
    observeEvent(input$chart_theme, {
      f7Toast(
        text = paste("Chart theme:", input$chart_theme),
        position = "center",
        closeTimeout = 1500
      )
    })
  })
}