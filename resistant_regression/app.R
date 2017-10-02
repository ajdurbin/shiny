library(shiny)
library(MASS)
library(broom)
library(ggplot2)

set.seed(sample(1:1000))

# generate data

x <- rnorm(n = 100, mean = 0, sd = 1)

e1 <- rnorm(n = 100, mean = 0, sd = 2)
e2 <- rlnorm(n = 100, mean = 0, sd = 2)
e3 <- rcauchy(n = 100, location = 0, scale = 2)

y1 <- 1 + 2 * x + e1
y2 <- 1 + 2 * x + e2
y3 <- 1 + 2 * x + e3

# Define UI ----
ui <- fluidPage(
  
  titlePanel("Resistant Regression Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Choose a distribution of residuals."),
      
      selectInput(inputId = "dist", 
                  label = "Distribution",
                  choices = c("Normal", 
                              "Lognormal",
                              "Cauchy"),
                  selected = "Normal")
      
    ),
    
    mainPanel = (
      
      tabsetPanel(
        tabPanel("Residual Plot", plotOutput(outputId = "residplot")), 
        tabPanel("Histogram", plotOutput(outputId = "histplot")),
        tabPanel("QQ Plot", plotOutput(outputId = "qqplot")),
        tabPanel("Regression", plotOutput(outputId = "regplot"))
      )
      
    )
    
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dist,
           "Normal" = y1,
           "Lognormal" = y2,
           "Cauchy" = y3)
  })
  
  output$residplot <- renderPlot({
    
    mod <- lm(datasetInput() ~ x)
    df <- augment(mod)
    ggplot(data = df) + 
      geom_jitter(mapping = aes(x = .fitted, y = .resid)) +
      geom_abline(intercept = 0, slope = 0) +
      ggtitle("Residuals Versus Fitted") +
      xlab("Fitted") +
      ylab("Residuals")

  })
  
  output$histplot <- renderPlot({
    
    mod <- lm(datasetInput() ~ x)
    df <- augment(mod)
    ggplot(data = df) + 
      geom_histogram(mapping = aes(x = .resid), bins = 50) +
      ggtitle("Histogram of Residuals") +
      xlab("Residuals")
    
  })
  
  output$qqplot <- renderPlot({
    
    mod <- lm(datasetInput() ~ x)
    df <- augment(mod)
    ggplot(data = df) +
      stat_qq(mapping = aes(sample = .resid)) +
      ggtitle("Normal QQ Plot") +
      ylab("Standardized Residuals") +
      xlab("Theoretical Quantiles")
    
  })
  
  output$regplot <- renderPlot({
    
    ggplot(mapping = aes(x = x, y = datasetInput())) +
      geom_jitter() +
      geom_smooth(mapping = aes(color = 'lm'), method = 'lm', se = FALSE) +
      geom_smooth(mapping = aes(color = 'loess'), method = 'loess', se = FALSE) +
      geom_smooth(mapping = aes(color = 'rlm'), method = 'rlm', se = FALSE) +
      ggtitle("Regression Fits") +
      ylab("y")
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)