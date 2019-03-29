#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ui <- shinyUI(fluidPage(
  
  titlePanel("Diagnostics testing and Bayes' Theorem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # User interaction for prevalence, specificity and sensitivity
      numericInput("prevalence",min = 0.01,max = 100,step = .01, label = "Prevalence(%)", value = 1),
      sliderInput("specificity",min = 1,max = 100,step = 1, label = "Specificity(%)", value = 95),
      sliderInput("sensitivity",min = 1,max = 100,step = 1, label = "Sensitivity(%)", value = 95)
    ),

    mainPanel(
      p("Calculate the posterior probility distribution of a diagnostic test using the bayesian Approach"),
      p("Based on sensitivity, specificity of the test and prevalence of the disease. The plot shows the posterior probabilities for true/false positives an true/false negatives."),
      p("Enter the values of prevalence, sensitivity and specificity with the sliders on the left panel. The plot will show the updated positive and negative predictive values"),
      plotOutput("bayesPlot")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  
  bayes <- reactiveValues()
  
  observeEvent(input$prevalence, {
    bayes$prevalence <- 0.01 * input$prevalence
    bayes$nonprevalence <- 1 - (0.01 * input$prevalence)
    })
  
  observeEvent(input$specificity, {
    bayes$specificity <- (0.01 * input$specificity)
    bayes$nonspecificity <- 1 - (0.01 * input$specificity)
  })
  
  observeEvent(input$sensitivity, {
    bayes$sensitivity <- (0.01 * input$sensitivity)
    bayes$nonsensitivity <- 1 - (0.01 * input$sensitivity)
  })
  
  # Plot bayes probability distribution
  output$bayesPlot <- renderPlot({
    # calculate p(Disease|Positive test)
    truePositive <- (bayes$prevalence * bayes$sensitivity)/((bayes$prevalence * bayes$sensitivity) + ((bayes$nonprevalence) * (bayes$nonspecificity)))
    # Calculate p(not Disease|Positive test)
    falsePositive <- 1 - truePositive
    # Calculate p(not Disease|Negative test)
    trueNegative <- ((bayes$nonprevalence) * bayes$specificity) / (((bayes$nonprevalence) * bayes$specificity) + (bayes$prevalence * (bayes$nonsensitivity)))
    # Calculate p(Disease|Negative test)
    falseNegative <- 1 - trueNegative
    # Plot the probaility distribution
    barplot(c(truePositive,falsePositive, trueNegative,falseNegative), names.arg = c('True positive', 'False positive', 'True Negative', 'False Negative'),ylim = c(0,1), ylab = "Probality")
  })
})

# Run the application   
shinyApp(ui = ui, server = server)