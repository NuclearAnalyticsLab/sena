# module_data.R

# UI Function for Data module
dataUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("League Data"),
    sidebarLayout(
      sidebarPanel(
        h3("League Information"),
        p("Enter your League ID to load data"),
        br(),
        # League ID input
        textInput(
          ns("league_id"),
          "League ID:",
          value = "",
          placeholder = "Enter your league ID"
        ),
        br(),
        h4("Data Options"),
        p("Enter league ID above and click refresh to load league users."),
        br(),
        # Add refresh button for debugging
        actionButton(ns("refresh"), "Load Data", class = "btn-primary"),
        br(), br(),
        # Add debug info
        verbatimTextOutput(ns("debug_info"))
      ),
      mainPanel(
        h3("League Users"),
        br(),
        # Add loading indicator
        conditionalPanel(
          condition = paste0("$('html').hasClass('shiny-busy')"),
          tags$div(
            class = "text-center",
            tags$i(class = "fa fa-spinner fa-spin fa-2x"),
            br(), br(),
            "Loading data..."
          )
        ),
        # DataTable output for users
        DT::dataTableOutput(ns("users"))
      )
    )
  )
}

# Server function for Data module
dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive value to store the data
    data_store <- reactiveVal()

    # Function to load data
    load_data <- function(league_id) {
      tryCatch(
        {
          # Try to source the file if it exists
          if (file.exists("R/league_users.R")) {
            source("R/league_users.R", local = TRUE)

            # Check if function exists
            if (exists("parse_league_users")) {
              data <- parse_league_users(league_id)

              # Validate data
              if (is.null(data) || nrow(data) == 0) {
                # Return sample data if no real data
                data_store(create_sample_data())
              } else {
                data_store(data)
              }
            } else {
              warning("parse_league_users function not found")
              data_store(create_sample_data())
            }
          } else {
            warning("R/league_users.R file not found")
            data_store(create_sample_data())
          }
        },
        error = function(e) {
          warning(paste("Error loading data:", e$message))
          data_store(create_sample_data())
        }
      )
    }

    # Reactive expression to get users data
    users_data <- reactive({
      data_store()
    })

    # Helper function to create sample data for testing
    create_sample_data <- function() {
      tibble::tibble(
        user_id = c("123456789", "987654321", "456789123"),
        username = c("NuclearFF_User1", "Dynasty_Player2", "Fantasy_Master3"),
        display_name = c("Nuclear User 1", "Dynasty Player 2", "Fantasy Master 3"),
        team_name = c("Nuclear Reactors", "Dynasty Dominators", "Fantasy Champions"),
        wins = c(8, 6, 10),
        losses = c(5, 7, 3),
        ties = c(0, 0, 0)
      )
    }

    # Debug output
    output$debug_info <- renderText({
      data <- users_data()
      if (is.null(data)) {
        "No data loaded yet - enter League ID and click 'Load Data'"
      } else {
        paste(
          "Data Status:",
          paste("League ID:", input$league_id),
          paste("Rows:", nrow(data)),
          paste("Columns:", ncol(data)),
          paste("File exists:", file.exists("R/league_users.R")),
          paste("Function exists:", exists("parse_league_users")),
          sep = "\n"
        )
      }
    })

    # Render the DataTable
    output$users <- DT::renderDataTable({
      data <- users_data()

      # Validate data before rendering
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }

      DT::datatable(
        data,
        options = list(
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          dom = "t" # Only show table (no other controls)
        ),
        class = "cell-border stripe hover",
        rownames = FALSE
      )
    })

    # Handle refresh button - only load data if league_id is provided
    observeEvent(input$refresh, {
      # Validate league_id input
      if (is.null(input$league_id) || input$league_id == "" || nchar(trimws(input$league_id)) == 0) {
        showNotification(
          "Please enter a League ID before loading data",
          type = "error",
          duration = 3
        )
        return()
      }

      # Show loading notification
      showNotification("Loading data...", type = "message", duration = 2)

      # Load data with the provided league_id
      load_data(trimws(input$league_id))
    })
  })
}
