library(shiny)

# Define UI for miles per gallon application
shinyUI(
  fluidPage(
  #pageWithSidebar(
  
  # Application title
  headerPanel("Miles Per Gallon"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Cylinders" = "cyl", 
                     "Transmission" = "am", 
                     "Gears" = "gear")),
    
    checkboxInput("outliers", "Show outliers", FALSE) #if outline is not true, the outliers are not drawn
  ),
  
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),
    img(src="mileage_calculator.png", height = 100, width = 60,align = "right"),
    plotOutput("mpgPlot")
  )
))


