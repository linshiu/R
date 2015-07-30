# Shiny ui 

library(shiny)

shinyUI(fluidPage(
  titlePanel("Cars"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Select input to choose the type of plot (points, line, step)
      selectInput("plotType", "Plot Type:",
                  c("Points"="p", "Line"="l","Step"="s")),
      
      # Select input to choose the type of car ("Sports vs Non-sports")
      selectInput("carType", "Car Type:",
                  c("Sports.Car"=1, "Non-sports.Car"=0)),
      
      # Slider to adjust the x axis (5,...65)
      sliderInput("xmax", "X axis:", value = 30, min = 5, max = 65),
      
      # Numeric input to select number of observations to show
      numericInput('obs', 'Number of observations:', 100, min = 0, max = 379)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("View", tableOutput("view")))
    )
  )
))