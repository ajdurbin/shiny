library(ggplot2)
library(shiny)
library(gridExtra)
library(rmutil)

# Define UI ----
ui <- fluidPage(
  
  titlePanel("The Metropolis Algorithm"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "generate",
                  label = "Generate samples from a",
                  choices = c("laplace",
                              "exp")),
      
      conditionalPanel(condition = "input.generate == 'laplace'",
                       selectInput(inputId = "sampler",
                                   label = "given samples from a",
                                   choices = c("norm",
                                               "cauchy")
                       )
      ),
      
      conditionalPanel(condition = "input.generate == 'exp'",
                       selectInput(inputId = "sampler",
                                   label = "using samples from a",
                                   choices = c("lnorm",
                                               "chisq")
                       )
      ),
      
      sliderInput(inputId = "sample_size",
                  label = "Number of samples:",
                  min = 1,
                  max = 100000,
                  value = 1000),
      
      sliderInput(inputId = "burn_in",
                  label = "Burnin Period:",
                  min = 1,
                  max = 10000,
                  value = 100)
      
    ),
    mainPanel(
      plotOutput(outputId = "plots")
    )
  )
)

# Define server ----
server <- function(input, output){
  
  rgen <- function(n){
    
    pckg <- switch(input$generate,
                   laplace = rlaplace,
                   exp = rexp)
    return(pckg(n))
    
  }
  
  dgen <- function(x){
    
    pckg <- switch(input$generate,
                   laplace = dlaplace,
                   exp = dexp)
    return(pckg(x))
    
  }
  
  rsam <- function(n){
    
    pckg <- switch(input$sampler,
                   norm = rnorm,
                   cauchy = rcauchy,
                   lnorm = rlnorm,
                   chisq = rchisq)
    return(pckg(n)) 
    
  }
  
  dsam <- function(x){
    
    pckg <- switch(input$sampler,
                   norm = dnorm,
                   cauchy = dcauchy,
                   lnorm = dlnorm,
                   chisq = dchisq)
    return(pckg(x)) 
    
  }
  
  sam_gen <- reactive({
    
    u.vec = rep(0, input$sample_size)
    p.vec = rep(0, input$sample_size)
    z.vec = rep(0, input$sample_size)
    v.vec = rep(0, input$sample_size)
    z.vec[1] = rgen(n = 1)
    
    for(i in 2:input$sample_size){
      
      u.vec[i] = runif(n = 1, min = 0, max = 1)
      v.vec[i] = rsam(n = 1)
      
      p.vec[i] = min( 
        (dgen(x = v.vec[i]) / dsam(x = v.vec[i]) 
         * 
           (dsam(x = z.vec[i-1]) / dgen(x = z.vec[i-1]))),
        1)
      
      if(u.vec[i] <= p.vec[i]){
        
        z.vec[i] = v.vec[i]
        
      } else{
        
        z.vec[i] = z.vec[i-1]
        
      }
      
    }
    
    z.vec <- z.vec[input$burn_in:length(z.vec)]
    v.vec <- v.vec[input$burn_in:length(v.vec)]
    pckg <- list(gen = z.vec, sam = v.vec)
    return(pckg)
    # return(z.vec)
    
  })
  
  gen <- reactive({
    return(sam_gen()[["gen"]])
  })
  
  sam <- reactive({
    return(sam_gen()[["sam"]])
  })
  
  output$plots <- renderPlot({
    
    g <- ggplot() +
      # geom_histogram(mapping = aes(x = sam_gen()), bins = 30) +
      geom_histogram(mapping = aes(x = gen()), bins = 30) +
      xlab("samples") +
      ggtitle("Generated Distribution")
    
    s <- ggplot() +
      geom_histogram(mapping = aes(x = sam()), bins = 30) +
      xlab("samples") +
      ggtitle("Sampling Distribution")
    
    return(grid.arrange(g, s))
    # return(g)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)