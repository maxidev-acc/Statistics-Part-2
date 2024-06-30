library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Switch Between Datasets and Make Plots"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("mtcars", "iris"))
    ),
    
    mainPanel(
      plotOutput("histogram"),
      plotOutput("scatterplot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to return the selected dataset
  selected_data <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris)
  })
  
  # Render histogram based on the selected dataset
  output$histogram <- renderPlot({
    hist(selected_data()$mpg, main = "Histogram of MPG", xlab = "MPG")
  })
  
  # Render scatterplot based on the selected dataset
  output$scatterplot <- renderPlot({
    plot(selected_data()$wt, selected_data()$mpg, 
         main = "Scatterplot of Weight vs MPG",
         xlab = "Weight", ylab = "MPG")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
