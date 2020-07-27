library(shiny)
library(plotly)
library(COVID19)

# Define UI for application
ui <- fluidPage(
  
  selectInput(
    "country", 
    label    = "Country", 
    multiple = TRUE, 
    choices  = unique(covid19()$administrative_area_level_1), 
    selected = "Italy"
  ),
  selectInput(
    "type", 
    label    = "type", 
    choices  = c("confirmed", "tests", "recovered", "deaths")
  ),
  selectInput(
    "level", 
    label    = "Granularity", 
    choices  = c("Country" = 1, "Redata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==gion" = 2, "City" = 3), 
    selected = 2
  ),
  dateRangeInput(
    "date", 
    label    = "Date", 
    start    = "2020-01-01"
  ),   
  
  plotlyOutput("covid19plot")
  
)


# Define server logic
server <- function(input, output) {
  output$covid19plot <- renderPlotly({
    if(!is.null(input$country)){
      
      x <- covid19(
        country = input$country, 
        level   = input$level, 
        start   = input$date[1], 
        end     = input$date[2]
      )
      
      color <- paste0("administrative_area_level_", input$level)
      plot_ly(x = x[["date"]], y = x[[input$type]], color = x[[color]])
    }
  })
  
}

shinyApp(ui = ui, server = server)
