# R/mobile_module_data.R
# Mobile-optimized data module with Framework7 components

#' Mobile Data UI Module  
#' 
#' Creates mobile-first data management interface using Framework7
#' 
#' @param id Module namespace ID
#' @return Mobile-optimized data UI
#' @export
mobileDataUI <- function(id) {
  ns <- NS(id)
  
  f7Page(
    # Mobile-optimized page header
    f7Block(
      # Compact header for mobile
      f7Card(
        title = "League Data Manager",
        
        tags$div(
          style = "padding: 15px;",
          
          # Mobile-friendly league ID input
          f7Text(
            inputId = ns("league_id"),
            label = "Sleeper League ID",
            placeholder = "Enter your league ID",
            value = "",
            # Mobile keyboard optimization
            style = "font-size: 16px;" # Prevents zoom on iOS
          ),
          
          # Action buttons in mobile layout
          f7Row(
            f7Col(
              width = 50,
              f7Button(
                inputId = ns("load_data"),
                label = "Load Data",
                fill = TRUE,
                color = "blue",
                size = "large",
                # Add loading state for mobile feedback
                onClick = "this.classList.add('loading');"
              )
            ),
            f7Col(
              width = 50, 
              f7Button(
                inputId = ns("refresh_data"),
                label = "Refresh",
                fill = FALSE,
                color = "blue", 
                size = "large"
              )
            )
          )
        )
      ),
      
      # Data options accordion (space-efficient on mobile)
      f7Card(
        title = "Data Options",
        
        f7Accordion(
          # Export options
          f7AccordionItem(
            title = "Export Options",
            
            f7List(
              f7ListItem(
                title = "Export to Excel",
                subtitle = "Download current data",
                media = f7Icon("square_and_arrow_up", color = "green"),
                right = f7Button(
                  inputId = ns("export_excel"),
                  label = "Export",
                  fill = TRUE,
                  color = "green", 
                  size = "small"
                )
              ),
              
              f7ListItem(
                title = "Export to CSV",
                subtitle = "Comma-separated values",
                media = f7Icon("doc_text", color = "blue"),
                right = f7Button(
                  inputId = ns("export_csv"),
                  label = "Export", 
                  fill = TRUE,
                  color = "blue",
                  size = "small"
                )
              )
            )
          ),
          
          # Filter options
          f7AccordionItem(
            title = "Filter Data",
            
            f7List(
              f7ListItem(
                title = "Active Users Only",
                right = f7Toggle(
                  inputId = ns("filter_active"),
                  checked = TRUE
                )
              ),
              
              f7ListItem(
                title = "Include Historical Data",
                right = f7Toggle(
                  inputId = ns("include_historical"),
                  checked = FALSE
                )
              )
            )
          ),
          
          # Debug information (collapsible on mobile)
          f7AccordionItem(
            title = "Debug Info",
            verbatimTextOutput(ns("debug_info"))
          )
        )
      ),
      
      # Data status indicator for mobile
      f7Card(
        f7List(
          f7ListItem(
            title = "Data Status",
            subtitle = textOutput(ns("data_status")),
            media = uiOutput(ns("status_icon")),
            right = textOutput(ns("record_count"))
          )
        )
      )
    ),
    
    # Main data display area optimized for mobile
    f7Block(
      # Loading indicator for mobile 
      conditionalPanel(
        condition = paste0("$('html').hasClass('shiny-busy')"),
        f7Card(
          tags$div(
            class = "text-center",
            style = "padding: 40px;",
            f7Preloader(color = "blue", size = 42),
            tags$br(),
            tags$h4("Loading league data...", style = "margin-top: 15px;")
          )
        )
      ),
      
      # Mobile-optimized data table
      f7Card(
        title = tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          "League Users",
          f7Button(
            inputId = ns("table_settings"),
            label = f7Icon("gear"),
            fill = FALSE,
            color = "gray",
            size = "small"
          )
        ),
        
        # Data table with mobile optimizations
        tags$div(
          style = "overflow-x: auto; -webkit-overflow-scrolling: touch;", # Smooth scrolling on iOS
          DT::dataTableOutput(ns("users_table"))
        )
      )
    ),
    
    # Floating action menu for mobile actions
    f7Fab(
      inputId = ns("fab_menu"),
      label = f7Icon("ellipsis"),
      color = "blue",
      position = "right-bottom"
    )
  )
}

#' Mobile Data Server Module
#' 
#' Server logic for mobile-optimized data management  
#' 
#' @param id Module namespace ID
#' @export
mobileDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for data management
    data_store <- reactiveVal()
    loading_state <- reactiveVal(FALSE)
    
    # Mobile-optimized data loading function
    load_data <- function(league_id) {
      # Set loading state for mobile feedback
      loading_state(TRUE)
      
      tryCatch({
        # Check if otis package is available
        if (!requireNamespace("otis", quietly = TRUE)) {
          f7Notification(
            title = "Package Missing",
            text = "otis package not found. Using sample data.",
            icon = f7Icon("exclamationmark_triangle"),
            closeTimeout = 4000
          )
          data_store(create_sample_data())
          return()
        }
        
        # Load data with mobile-friendly progress indication
        data <- otis::league_users_details(league_id)
        
        if (is.null(data) || nrow(data) == 0) {
          f7Notification(
            title = "No Data Found", 
            text = "No users found for this league ID",
            icon = f7Icon("exclamationmark_circle"),
            closeTimeout = 3000
          )
          data_store(create_sample_data())
        } else {
          data_store(data)
          f7Toast(
            text = paste("Loaded", nrow(data), "users"),
            position = "center",
            closeTimeout = 2000
          )
        }
        
      }, error = function(e) {
        f7Notification(
          title = "Loading Error",
          text = paste("Error:", e$message),
          icon = f7Icon("xmark_circle"),
          closeTimeout = 4000
        )
        data_store(create_sample_data())
        
      }, finally = {
        loading_state(FALSE)
        # Remove loading class from button
        session$sendCustomMessage("remove-loading-class", "load_data")
      })
    }
    
    # Sample data generator for testing
    create_sample_data <- function() {
      tibble::tibble(
        user_id = c("123456789", "987654321", "456789123"),
        username = c("NuclearFF_User1", "Dynasty_Player2", "FF_Champion3"),
        display_name = c("Nuclear Mike", "Dynasty Dan", "Champion Charlie"),
        team_name = c("Nuclear Reactors", "Dynasty Squad", "Championship Team"),
        wins = c(8, 6, 10),
        losses = c(5, 7, 3),
        points_for = c(1450.5, 1320.8, 1580.2),
        points_against = c(1380.2, 1390.5, 1250.8),
        created = as.Date(c("2020-08-15", "2019-07-20", "2021-09-01"))
      )
    }
    
    # Load data button handler
    observeEvent(input$load_data, {
      req(input$league_id)
      
      if (nchar(trimws(input$league_id)) == 0) {
        f7Dialog(
          title = "League ID Required",
          text = "Please enter a valid Sleeper League ID",
          type = "alert"
        )
        return()
      }
      
      load_data(input$league_id)
    })
    
    # Refresh data handler
    observeEvent(input$refresh_data, {
      if (is.null(input$league_id) || nchar(trimws(input$league_id)) == 0) {
        f7Toast(
          text = "Enter League ID first",
          position = "center",
          closeTimeout = 2000
        )
        return()
      }
      
      load_data(input$league_id)
    })
    
    # Filtered data reactive with mobile-optimized filtering
    filtered_data <- reactive({
      data <- data_store()
      if (is.null(data)) return(NULL)
      
      # Apply mobile-friendly filters
      if (input$filter_active) {
        # Filter logic for active users
        data <- data |> dplyr::filter(!is.na(username))
      }
      
      if (!input$include_historical) {
        # Filter out historical data if needed
        if ("created" %in% names(data)) {
          data <- data |> dplyr::filter(created >= as.Date("2023-01-01"))
        }
      }
      
      data
    })
    
    # Mobile-optimized data table output
    output$users_table <- DT::renderDataTable({
      data <- filtered_data()
      if (is.null(data)) return(NULL)
      
      # Mobile-optimized DataTable options
      DT::datatable(
        data,
        options = list(
          responsive = TRUE,         # Responsive design for mobile
          scrollX = TRUE,           # Horizontal scroll for wide tables
          scrollCollapse = TRUE,    # Collapse scroll when not needed
          pageLength = 10,          # Reasonable page size for mobile
          lengthMenu = c(5, 10, 15, 25), # Mobile-friendly page sizes
          dom = 'ftip',            # Simplified layout for mobile
          columnDefs = list(
            list(
              targets = '_all',
              className = 'dt-center' # Center align for better mobile display
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "  // Add mobile-specific styling after table initialization",
            "  $(this.api().table().container()).addClass('mobile-table');",
            "}"
          )
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      ) |> 
      DT::formatRound(columns = c("points_for", "points_against"), digits = 1)
      
    }, server = TRUE) # Use server-side processing for better mobile performance
    
    # Data status outputs
    output$data_status <- renderText({
      if (loading_state()) {
        "Loading..."
      } else if (is.null(data_store())) {
        "No data loaded"
      } else {
        "Data loaded successfully"
      }
    })
    
    output$status_icon <- renderUI({
      if (loading_state()) {
        f7Icon("arrow_clockwise", color = "blue")
      } else if (is.null(data_store())) {
        f7Icon("exclamationmark_circle", color = "red")
      } else {
        f7Icon("checkmark_circle", color = "green")
      }
    })
    
    output$record_count <- renderText({
      data <- filtered_data()
      if (is.null(data)) {
        "0 records"
      } else {
        paste(nrow(data), "records")
      }
    })
    
    # Debug information
    output$debug_info <- renderText({
      data <- data_store()
      if (is.null(data)) {
        "No data available"
      } else {
        paste(
          "Columns:", paste(names(data), collapse = ", "),
          "\nRows:", nrow(data),
          "\nLast updated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      }
    })
    
    # Export functionality optimized for mobile
    observeEvent(input$export_excel, {
      data <- filtered_data()
      if (is.null(data)) {
        f7Toast(
          text = "No data to export",
          position = "center",
          closeTimeout = 2000
        )
        return()
      }
      
      # Mobile-friendly export with progress indication
      f7Toast(
        text = "Preparing Excel export...",
        position = "center",
        closeTimeout = 2000
      )
      
      # Export logic here
      tryCatch({
        filename <- paste0("league_data_", Sys.Date(), ".xlsx")
        openxlsx::write.xlsx(data, filename)
        
        f7Notification(
          title = "Export Complete",
          text = paste("Exported to", filename),
          icon = f7Icon("checkmark_circle"),
          closeTimeout = 3000
        )
        
      }, error = function(e) {
        f7Notification(
          title = "Export Failed",
          text = paste("Error:", e$message),
          icon = f7Icon("xmark_circle"),
          closeTimeout = 4000
        )
      })
    })
    
    observeEvent(input$export_csv, {
      data <- filtered_data()
      if (is.null(data)) {
        f7Toast(
          text = "No data to export",
          position = "center", 
          closeTimeout = 2000
        )
        return()
      }
      
      # CSV export with mobile feedback
      f7Toast(
        text = "Preparing CSV export...",
        position = "center",
        closeTimeout = 2000
      )
      
      filename <- paste0("league_data_", Sys.Date(), ".csv")
      write.csv(data, filename, row.names = FALSE)
      
      f7Notification(
        title = "Export Complete",
        text = paste("Exported to", filename),
        icon = f7Icon("checkmark_circle"),
        closeTimeout = 3000
      )
    })
    
    # Table settings handler
    observeEvent(input$table_settings, {
      f7ActionSheet(
        title = "Table Options",
        buttons = list(
          list(
            text = "Refresh Table",
            icon = f7Icon("arrow_clockwise"),
            color = "blue"
          ),
          list(
            text = "Export Visible Data", 
            icon = f7Icon("square_and_arrow_up"),
            color = "green"
          ),
          list(
            text = "Reset Filters",
            icon = f7Icon("arrow_counterclockwise"),
            color = "orange"
          ),
          list(text = "Cancel", color = "red")
        )
      )
    })
    
    # Floating action menu handler
    observeEvent(input$fab_menu, {
      f7ActionSheet(
        title = "Quick Actions",
        buttons = list(
          list(
            text = "Load Sample Data",
            icon = f7Icon("doc_text"),
            color = "blue",
            onClick = paste0("Shiny.setInputValue('", session$ns("load_sample"), "', Math.random());")
          ),
          list(
            text = "Clear All Data",
            icon = f7Icon("trash"),
            color = "red",
            onClick = paste0("Shiny.setInputValue('", session$ns("clear_data"), "', Math.random());")
          ),
          list(text = "Cancel")
        )
      )
    })
    
    # Load sample data handler
    observeEvent(input$load_sample, {
      data_store(create_sample_data())
      f7Toast(
        text = "Sample data loaded",
        position = "center",
        closeTimeout = 2000
      )
    })
    
    # Clear data handler
    observeEvent(input$clear_data, {
      f7Dialog(
        title = "Clear Data",
        text = "Are you sure you want to clear all data?",
        type = "confirm",
        inputId = session$ns("clear_confirm")
      )
    })
    
    observeEvent(input$clear_confirm, {
      if (input$clear_confirm) {
        data_store(NULL)
        f7Toast(
          text = "Data cleared",
          position = "center",
          closeTimeout = 2000
        )
      }
    })
    
    # Custom message handler for removing loading states
    session$onCustomMessage("remove-loading-class", function(button_id) {
      session$sendCustomMessage("js-eval", paste0(
        "document.getElementById('", session$ns(button_id), "').classList.remove('loading');"
      ))
    })
  })
}