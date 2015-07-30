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


##### Ex1  #############################################################

course = "MSIA_411_Data_Visualization"
datafolder = "/Lab/Exercises/lab5_exercise_app"
setwd(file.path(main,course, datafolder))

library(shiny) 
runApp("app3)

