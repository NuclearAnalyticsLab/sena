# explorer_module.R
#
# This module provides an interactive data explorer interface for league users data.
# It allows users to filter, search, and explore the users tibble with enhanced functionality.
#
# Dependencies:
#   - shiny
#   - dplyr
#   - tibble
#   - DT
#
# Author: Updated on June 11, 2025

#' UI Function for Users Data Explorer Module
#'
#' Creates the user interface for the users data explorer module.
#'
#' @param id A character string that defines the namespace for the module
#'
#' @return A Shiny UI definition
#'
#' @examples
#' # In app.R:
#' # ui <- fluidPage(
#' #   explorerUI("explorer")
#' # )
explorerUI <- function(id) {
  # Create a namespace function to avoid ID collisions
  ns <- NS(id)

  fluidPage(
    titlePanel("League Users Explorer"),
    sidebarLayout(
      # Left sidebar for user controls
      sidebarPanel(
        width = 3,
        h3("Filters & Controls"),

        # Summary statistics
        h4("Data Summary"),
        verbatimTextOutput(ns("data_summary")),

        # Column selection for display
        h4("Column Visibility"),
        checkboxGroupInput(
          inputId = ns("columns_to_show"),
          label = "Select columns to display:",
          choices = NULL, # Will be populated in server
          selected = NULL
        ),

        # Export options
        h4("Export Data"),
        downloadButton(
          outputId = ns("download_csv"),
          label = "Download CSV",
          class = "btn btn-primary",
          style = "margin-bottom: 10px;"
        ),
        br(),
        downloadButton(
          outputId = ns("download_excel"),
          label = "Download Excel",
          class = "btn btn-success"
        )
      ),

      # Main panel for data display
      mainPanel(
        width = 9,
        h3("League Users Data"),
        p("Interactive table with search, sort, and filter capabilities."),

        # Data info
        fluidRow(
          column(4, uiOutput(ns("total_users"))),
          column(4, uiOutput(ns("total_columns"))),
          column(4, uiOutput(ns("data_updated")))
        ),
        br(),

        # DT interactive data table output container
        DT::DTOutput(ns("users_table"))
      )
    )
  )
}

#' Server Function for Users Data Explorer Module
#'
#' Defines the server-side logic for the users data explorer module.
#' Handles data filtering, display, and export functionality.
#'
#' @param id A character string matching the ID used in the UI function
#' @param users_data A reactive or static data frame containing users data
#'
#' @return A Shiny module server function
#'
#' @examples
#' # In app.R:
#' # server <- function(input, output, session) {
#' #   explorerServer("explorer", users)
#' # }
explorerServer <- function(id, users_data) {
  moduleServer(id, function(input, output, session) {
    # Convert users_data to reactive if it's not already
    users_reactive <- reactive({
      if (is.reactive(users_data)) {
        users_data()
      } else {
        users_data
      }
    })

    # Update column choices when data loads
    observe({
      cat("Updating column choices...\n")
      if (is.null(users_reactive())) {
        cat("Users data is NULL in observe\n")
        return()
      }

      column_names <- names(users_reactive())
      cat("Available columns:", paste(column_names, collapse = ", "), "\n")

      updateCheckboxGroupInput(
        session,
        "columns_to_show",
        choices = column_names,
        selected = column_names # Select all by default
      )
      cat("Column choices updated\n")
    })

    # Filtered dataset based on selected columns
    filtered_data <- reactive({
      cat("Filtering data...\n")
      if (is.null(users_reactive())) {
        cat("Users data is NULL\n")
        return(NULL)
      }
      if (is.null(input$columns_to_show) || length(input$columns_to_show) == 0) {
        cat("No columns selected, returning all data\n")
        return(users_reactive())
      }

      result <- users_reactive() %>%
        select(any_of(input$columns_to_show))

      cat("Filtered result:", nrow(result), "rows,", ncol(result), "cols\n")
      return(result)
    })

    # Data summary
    output$data_summary <- renderText({
      if (is.null(users_reactive())) {
        return("No data loaded")
      }
      data <- users_reactive()
      paste(
        paste("Rows:", nrow(data)),
        paste("Columns:", ncol(data)),
        paste("Updated:", Sys.Date()),
        sep = "\n"
      )
    })

    # Value boxes (if using shinydashboard, otherwise use renderUI)
    output$total_users <- renderUI({
      user_count <- if (is.null(users_reactive())) 0 else nrow(users_reactive())
      div(
        class = "info-box",
        style = "background-color: var(--bg-secondary); padding: 15px; border-radius: 8px; text-align: center; border: 1px solid var(--border-color);",
        h4(user_count, style = "margin: 0; color: var(--social-hover);"),
        p("Total Users", style = "margin: 5px 0 0 0; color: var(--text-secondary);")
      )
    })

    output$total_columns <- renderUI({
      col_count <- if (is.null(filtered_data())) 0 else ncol(filtered_data())
      div(
        class = "info-box",
        style = "background-color: var(--bg-secondary); padding: 15px; border-radius: 8px; text-align: center; border: 1px solid var(--border-color);",
        h4(col_count, style = "margin: 0; color: var(--social-hover);"),
        p("Visible Columns", style = "margin: 5px 0 0 0; color: var(--text-secondary);")
      )
    })

    output$data_updated <- renderUI({
      div(
        class = "info-box",
        style = "background-color: var(--bg-secondary); padding: 15px; border-radius: 8px; text-align: center; border: 1px solid var(--border-color);",
        h4(format(Sys.Date(), "%m/%d"), style = "margin: 0; color: var(--social-hover);"),
        p("Last Updated", style = "margin: 5px 0 0 0; color: var(--text-secondary);")
      )
    })

    # Render Interactive Users Data Table
    output$users_table <- DT::renderDataTable({
      # Debug: Print to console what we have
      cat("Rendering table...\n")
      cat("Users data rows:", ifelse(is.null(users_reactive()), "NULL", nrow(users_reactive())), "\n")
      cat("Selected columns:", length(input$columns_to_show %||% c()), "\n")

      # Check if we have data and selected columns
      if (is.null(users_reactive()) || length(input$columns_to_show %||% c()) == 0) {
        cat("No data or no columns selected\n")
        return(NULL)
      }

      # Get the filtered data
      data_to_show <- users_reactive() %>%
        select(any_of(input$columns_to_show))

      cat("Filtered data rows:", nrow(data_to_show), "cols:", ncol(data_to_show), "\n")

      # Create a simple datatable first
      DT::datatable(
        data_to_show,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "frtip" # Simpler dom without buttons for now
        ),
        class = "table table-striped table-hover theme-aware-table",
        rownames = FALSE,
        caption = "League Users Data"
      )
    })

    # Download handlers
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("league_users_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data_to_export <- filtered_data()
        if (is.null(data_to_export)) {
          data_to_export <- data.frame(message = "No data available")
        }
        write.csv(data_to_export, file, row.names = FALSE)
      }
    )

    output$download_excel <- downloadHandler(
      filename = function() {
        paste("league_users_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        data_to_export <- filtered_data()
        if (is.null(data_to_export)) {
          data_to_export <- data.frame(message = "No data available")
        }

        # Requires openxlsx package
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(data_to_export, file, rowNames = FALSE)
        } else {
          # Fallback to CSV if openxlsx not available
          write.csv(data_to_export, file, row.names = FALSE)
        }
      }
    )
  })
}
