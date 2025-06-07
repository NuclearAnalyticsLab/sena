# explorer_module.R
#
# This module provides an interactive data explorer interface using the DT package.
# It allows users to select from predefined datasets and view them in an interactive table.
#
# Dependencies:
#   - shiny
#   - dplyr
#   - tibble
#   - DT
#   - ggplot2 (for diamonds dataset)
#
# Author: Updated on May 20, 2025

#' UI Function for Data Explorer Module
#'
#' Creates the user interface for the data explorer module.
#'
#' @param id A character string that defines the namespace for the module
#'
#' @return A Shiny UI definition
#'
#' @examples
#' # In app.R:
#' # ui <- fluidPage(
#' #   explorerUI("dataExplorer")
#' # )
explorerUI <- function(id) {
  # Create a namespace function to avoid ID collisions
  ns <- NS(id)

  fluidPage(
    titlePanel("Data Explorer"),
    sidebarLayout(
      # Left sidebar for user controls
      sidebarPanel(
        h3("Controls"),
        # Dataset selection dropdown
        selectInput(
          inputId = ns("dataset"),
          label = "Choose a dataset:",
          choices = c("iris", "mtcars", "diamonds")
        )
      ),
      # Main panel for data display
      mainPanel(
        h3("Data Preview"),
        # DT interactive data table output container
        DT::DTOutput(ns("data_table"))
      )
    )
  )
}

#' Server Function for Data Explorer Module
#'
#' Defines the server-side logic for the data explorer module.
#' Handles dataset selection and renders an interactive data table.
#'
#' @param id A character string matching the ID used in the UI function
#'
#' @return A Shiny module server function
#'
#' @examples
#' # In app.R:
#' # server <- function(input, output, session) {
#' #   explorerServer("dataExplorer")
#' # }
explorerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #' Reactive Dataset
    #'
    #' Creates a reactive dataset based on user selection.
    #' Changes to input$dataset will trigger this reactive to update.
    dataset <- reactive({
      # Switch statement to return the appropriate dataset
      switch(input$dataset,
        "iris" = iris, # Iris flower dataset
        "mtcars" = mtcars, # Motor Trend Car Road Tests
        "diamonds" = ggplot2::diamonds %>% slice_sample(n = 100) # Random sample of 100 diamonds
      )
    })

    #' Render Interactive Data Table
    #'
    #' Uses DT package to create an interactive data table with
    #' features like pagination, sorting, and searching.
    output$data_table <- DT::renderDataTable({
      # Process and display the dataset
      dataset() %>%
        # Convert to tibble for consistent data handling
        as_tibble() %>%
        # Create interactive datatable with customized options
        DT::datatable(
          options = list(
            pageLength = 10, # Default number of rows per page
            lengthMenu = c(10, 25, 50), # Options for rows per page
            searching = TRUE, # Enable search functionality
            ordering = TRUE, # Enable column sorting
            info = TRUE # Show table information
          ),
          rownames = FALSE, # Hide row numbers
          caption = paste("Preview of", input$dataset, "dataset"),
          fillContainer = FALSE # Make table responsive to container size
        )
    })
  })
}
