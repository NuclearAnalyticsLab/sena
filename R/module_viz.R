# viz_module.R

# UI Function for Visualizations module
vizUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Data Visualizations"),
    sidebarLayout(
      sidebarPanel(
        h3("Plot Controls"),
        selectInput(ns("plot_type"), "Select plot type:",
                    choices = c("Scatter Plot", "Bar Chart", "Histogram")),
        selectInput(ns("dataset"), "Choose a dataset:",
                    choices = c("iris", "mtcars", "diamonds"))
      ),
      mainPanel(
        h3("Plot Output"),
        plotOutput(ns("plot"))
      )
    )
  )
}

# Server function for Visualizations module
vizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactive dataset based on user selection
    dataset <- reactive({
      switch(input$dataset,
             "iris" = iris,
             "mtcars" = mtcars,
             "diamonds" = ggplot2::diamonds %>% slice_sample(n = 100)
      )
    })
    
    # Render plot based on user selection
    output$plot <- renderPlot({
      data <- dataset()
      
      if (input$plot_type == "Scatter Plot") {
        # For iris
        if (input$dataset == "iris") {
          ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
            geom_point() +
            theme_minimal() +
            labs(title = "Iris Sepal Length vs Width")
        }
        # For mtcars
        else if (input$dataset == "mtcars") {
          ggplot(data, aes(x = mpg, y = hp, color = factor(cyl))) +
            geom_point() +
            theme_minimal() +
            labs(title = "MPG vs Horsepower by Cylinders")
        }
        # For diamonds
        else {
          ggplot(data, aes(x = carat, y = price, color = cut)) +
            geom_point() +
            theme_minimal() +
            labs(title = "Diamond Price vs Carat by Cut")
        }
      }
      else if (input$plot_type == "Bar Chart") {
        # For iris
        if (input$dataset == "iris") {
          ggplot(data, aes(x = Species, fill = Species)) +
            geom_bar() +
            theme_minimal() +
            labs(title = "Count of Iris Species")
        }
        # For mtcars
        else if (input$dataset == "mtcars") {
          ggplot(data, aes(x = factor(cyl), fill = factor(cyl))) +
            geom_bar() +
            theme_minimal() +
            labs(title = "Count of Cars by Cylinders", x = "Cylinders")
        }
        # For diamonds
        else {
          ggplot(data, aes(x = cut, fill = cut)) +
            geom_bar() +
            theme_minimal() +
            labs(title = "Count of Diamonds by Cut")
        }
      }
      else if (input$plot_type == "Histogram") {
        # For iris
        if (input$dataset == "iris") {
          ggplot(data, aes(x = Sepal.Length, fill = Species)) +
            geom_histogram(bins = 20, alpha = 0.7) +
            theme_minimal() +
            labs(title = "Distribution of Iris Sepal Length")
        }
        # For mtcars
        else if (input$dataset == "mtcars") {
          ggplot(data, aes(x = mpg, fill = factor(cyl))) +
            geom_histogram(bins = 20, alpha = 0.7) +
            theme_minimal() +
            labs(title = "Distribution of MPG")
        }
        # For diamonds
        else {
          ggplot(data, aes(x = price)) +
            geom_histogram(bins = 20, fill = "steelblue") +
            theme_minimal() +
            labs(title = "Distribution of Diamond Prices")
        }
      }
    })
  })
}