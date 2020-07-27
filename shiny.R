library(shiny)

library(tidyverse)

ui = fluidPage(
  titlePanel("Regression Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", label = h3("Outcome"),
                  choices = list("High School Points" = "hsc_p",
                                 "Degree Points" = "degree_p",
                                 "Etest Points" = "etest_p",
                                 "MBA Points" = "mba_p",
                                 "Salary" = "salary"), selected = 1),
      
      selectInput("indepvar", label = h3("Explanatory variable"),
                  choices = list("High School Points" = "hsc_p",
                                 "Degree Points" = "degree_p",
                                 "Etest Points" = "etest_p",
                                 "MBA Points" = "mba_p",
                                 "Salary" = "salary"), selected = 1),
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Distribution", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")))
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))



# SERVER
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(placement[,input$outcome] ~ placement[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(placement, options = list(lengthChange = FALSE))
  })
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(placement[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(placement[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
}

shinyApp(ui = ui, server = server)
