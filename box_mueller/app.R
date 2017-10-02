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
                  min = 1,
                  max = 1000,
                  value = 100)
      
    ),
    
    mainPanel = (
      
      tabsetPanel(
        tabPanel("Background", 
                 helpText("The Box-Mueller algorithm is a direct method for
                          generating independent standard normal random variables
                          given independent uniform(0,1) random variables. 
                          The algorithm is as follows:"),
                 verbatimTextOutput(outputId = "algo")),
        
        tabPanel("Summary", 
                 helpText("Below is a quick summary of the generated data. 
                          As the number of samples approaches infinitiy, 
                          we expect the mean to approach 0."),
                 verbatimTextOutput(outputId = "summary"),
                 helpText("Below is the variance-covariance matrix of our generated
                          samples. The variance of x, y should each approach 1. 
                          Additionally, the covariance between x, y should also 
                          approach 0, showing they are independent random 
                          variables."),
                 verbatimTextOutput(outputId = "var"),
                 helpText("We use the Shapiro-Wilk test for normality of our data.
                          So that if the p-value is large, we have generated two
                          independent, standard normal random variables."),
                 verbatimTextOutput(outputId = "shapiro")
        ),
        
        tabPanel("Plots", plotOutput(outputId = "grid"))
      )
      
    )
    
  )
  
)

# Define server logic ----
server <- function(input, output){
  
  set.seed(sample(1:10000, size = 1))
  
  sample_size <- reactive({
    
    return(input$samples)
    
  })
  
  the_data <- reactive({
    
    num <- sample_size()
    the_data <- matrix(runif(n = num, min = 0, max = 1), ncol = 2)
    r <- sqrt(-2 * log(the_data[, 1]))
    theta <- 2 * pi * the_data[, 2]
    x <- r * cos(theta)
    y <- r * sin(theta)
    pckg <- cbind(x,y)
    return(pckg)
    
  })
  
  plot_grid <- reactive({
    
    qx <- ggplot(data = as.data.frame(the_data())) +
      geom_qq(mapping = aes(sample = x), alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      ggtitle("X QQ Plot")
  
    qy <- ggplot(data = as.data.frame(the_data())) +
      geom_qq(mapping = aes(sample = y), alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      ggtitle("Y QQ Plot")
    
    hx <- ggplot(data = as.data.frame(the_data())) +
      geom_histogram(mapping = aes(x = x, y = ..density..), binwidth = 0.5) +
      geom_density(mapping = aes(x = x), alpha = 0.2, color = "red") +
      ggtitle("X Histogram With Density")
    
    hy <- ggplot(data = as.data.frame(the_data())) +
      geom_histogram(mapping = aes(x = y, y = ..density..), binwidth = 0.5) +
      geom_density(mapping = aes(x = y), alpha = 0.2, color = "red") +
      ggtitle("Y Histogram With Density")

    return(grid.arrange(hx, qx, hy, qy))
    
  })
  
  output$summary <- renderPrint(summary(the_data()))
  output$var <- renderPrint(var(the_data()))
  output$shapiro <- renderPrint(shapiro.test(the_data()))
  output$grid <- renderPlot(plot_grid())
  output$algo <- renderText(
    "Generate U, V ~ uniform(0, 1) independent random variables 
    \nSet R = sqrt(-2 * log(U)), S = 2 * pi * V 
    \nSet X = R * cos(S), Y = R * sin(S) 
    \nThen X, Y are independent standard normal random variables")

  
}

# Run the app ----
shinyApp(ui = ui, server = server)