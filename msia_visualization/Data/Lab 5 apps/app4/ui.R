# ui.R

shinyUI(pageWithSidebar( 
  # Application title 
  headerPanel("Reactivity"),
  
  # Sidebar
  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"), 
    selectInput("dataset", "Choose a dataset:", choices = c("rock", "pressure", "cars")), 
    numericInput("obs", "Number of observations to view:", 10) ), 
  
  # Show the caption, a summary of the dataset and an HTML table with the requested number of observations 
  mainPanel( h3(textOutput("caption")), verbatimTextOutput("summary"), tableOutput("view") ) ))
