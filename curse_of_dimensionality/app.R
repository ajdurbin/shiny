library(shiny)


# Define UI ----
ui <- fluidPage(
  titlePanel("The Curse Of Dimensionality"),
  
  sidebarLayout(
    
    sidebarPanel(
                 
      sliderInput(inputId = "n",
                  label = "Number of data points:",
                  min = 1,
                  max = 1000,
                  value = 100),
      
      sliderInput(inputId = "p",
                  label = "Number of dimensions:",
                  min = 1,
                  max = 1000,
                  value = 1),
      
      sliderInput(inputId = "s",
                  label = "Number of simulations:",
                  min = 1,
                  max = 1000,
                  value = 100)
      
    ),
    
    mainPanel(
      
      helpText("This simulation is descirbed in Chapter 2 of Elements of
               Statistical Learning. We are simulating data from a uniform(0, 1)
               distribution in p dimensions. We then calculate the distance to
               the origin of all the points and take the minimum in each
               simulation. After the simulations are complete, we calculate the
               median distance of these minimum distances. What do you notice as
               the number of dimensions increase?"),
      
      verbatimTextOutput(outputId = "result"),
      
      actionButton(inputId = "button", label = "Answer"),
      textOutput(outputId = "text")
      
    )
    
  )

)

# Define server logic ----
server <- function(input, output) {
  
  tmp <- reactive({
    
    origin <- rep(0,input$p)
    d <- rep(0,input$s)
    
    for (i in 1:input$s) {
      
      raw <- matrix(data = runif(n = input$n * input$p, min = 0, max = 1), 
                    ncol = input$p, nrow = input$n)
      
      d[i] <- min(
        apply(raw, 1, function(row) sum((row - origin)^2))
      )
      
    }
    
    return(median(d))
    
  })
  
  output$result <- renderPrint(paste0("Median: ",tmp()))
  
  observeEvent(input$button, {
    output$text <- renderText({"The median distance from the origin increases
      without bound as the dimensions increase. This example illustrates that 
      the location-based statistical learning methods like k-nearest neighbors
      are not appropriate for high dimensional data. As the dimensions increase,
      we see that the majority of the data are located on the boundary."})
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
