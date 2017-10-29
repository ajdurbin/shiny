library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI ----
ui <- fluidPage(
  
  titlePanel("The Bootstrap"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Choose a distribution:",
                  choices = c("uniform",
                              "normal",
                              "lognormal",
                              "exponential",
                              "cauchy")),                           
      
      sliderInput(inputId = "sample_size",
                  label = "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 100),
      
      sliderInput(inputId = "bootstrap_samples",
                  label = "Number of bootstrap samples:",
                  min = 1,
                  max = 1000,
                  value = 100)
      
    ),
    mainPanel(
      plotOutput(outputId = "plots")
    )
  )
)

# Define server logic ----
server <- function(input, output){
  
  the_data <- reactive({
    
    df <- data.frame(
      uniform = runif(input$sample_size),
      normal = rnorm(input$sample_size),
      lognormal = rlnorm(input$sample_size),
      exponential = rexp(input$sample_size),
      cauchy = rcauchy(input$sample_size))
      
    return(df)
      
  })
  
  boot <- reactive({
    
    x <- rep(0, input$bootstrap_samples)
    for(i in 1:input$bootstrap_samples){
      
      x_boot <- sample(the_data()[, input$dataset], nrow(the_data()), replace = TRUE)
      x[i] <- mean(x_boot)
      
    }
    
    return(x)
    
  })
  
  output$plots <- renderPlot({
    
    s <- ggplot(the_data(), aes(x = the_data()[, input$dataset])) + 
      geom_histogram(aes(y = ..density..), colour = "black", 
                     fill = "white") +
      geom_density(alpha = 0.2, fill = "#FF6666") +
      geom_vline(aes(xintercept = mean(the_data()[, input$dataset]),
                     color = "mean"), linetype="dashed", size=1) +
      geom_vline(aes(xintercept = quantile(the_data()[, input$dataset], 
                                           probs = 0.025), 
                     color = '95% Confidence Interval'), linetype = "dashed", 
                 size = 1) +
      geom_vline(aes(xintercept = quantile(the_data()[, input$dataset],
                                           probs = 0.975)), 
                 color = "red", linetype = "dashed", size = 1) +
      scale_color_manual("statistics", values = c("red", "blue")) +
      ggtitle("Normal Approximation") +
      xlab("x")
    
    b <- ggplot() + 
      geom_histogram(aes(x = boot(), y = ..density..), colour = "black", 
                     fill = "white") +
      geom_density(aes(x = boot(), y = ..density..), alpha = 0.2, fill = "#FF6666") +
      geom_vline(aes(xintercept = mean(boot()),
                     color = "mean"), linetype="dashed", size=1) +
      geom_vline(aes(xintercept = quantile(boot(), 
                                           probs = 0.025), 
                     color = '95% Confidence Interval'), linetype = "dashed", 
                 size = 1) +
      geom_vline(aes(xintercept = quantile(boot(),
                                           probs = 0.975)), 
                 color = "red", linetype = "dashed", size = 1) +
      scale_color_manual("statistics", values = c("red", "blue")) +
      ggtitle("Bootstrap Approximation") +
      xlab("x")
    
    return(grid.arrange(s,b))
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)