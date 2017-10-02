library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Box-Mueller Algorithm"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "samples",
                  label = "Number of samples:",
                  min = 100,
                  max = 10000,
                  value = 100)
      
    ),
    
    mainPanel = (
      
      tabsetPanel(
        tabPanel("Background"),
        tabPanel("Summary"),
        tabPanel("Plots")
      )
      
    )
    
  )
  
)

# Define server logic ----
server <- function(input, output){}

# Run the app ----
shinyApp(ui = ui, server = server)