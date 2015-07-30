# Lab session 5 exercise 

##### Load data ##############################################################

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_411_Data_Visualization"
datafolder = "/Lab/Data"
setwd(file.path(main,course, datafolder))

carsdata =read.csv("04cars data.csv",header=TRUE,
                   na.strings=c("","*","NA"))

# Shiny server
library(shiny)

carsdata = read.csv("04cars data.csv",header=TRUE,na.strings=c(" ","*","NA"))
carsdata = carsdata[order(carsdata$City.MPG),] # this is for lines plot

# Define server logic required to plot various variables against mpg

shinyServer(function(input, output) {
  output$plot = renderPlot({
    plot(Retail.Price~City.MPG, 
         data=head(carsdata[carsdata$Sports.Car==input$carType,],input$obs),
         type=input$plotType,xlim=range(c(5,input$xmax)),
         main="Plot Retail.Price vs. City.MPG",pch=16,col="darkblue",lwd=1.0)
  })

    output$view = renderTable({
            head(carsdata[carsdata$Sports.Car==input$carType,],input$obs)
  })
        
})




