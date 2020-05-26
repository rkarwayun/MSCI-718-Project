#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

data <- read.csv("data.csv")
col = c("Samsung", "Apple", "Huawei")
x = seq(as.Date("2017/1/1"), by = "month", length.out = 33)

shinyUI(pageWithSidebar(

# Define UI for application that draws a histogram
headerPanel("Mobile Phone Market Share"),


sidebarPanel(
    
    helpText("Select Company"),
    
    selectInput("company", "Company:",
                col),
),


mainPanel(
    
    tabsetPanel(
        tabPanel(
            "Market Share History", 
            plotOutput("timeseries")
        ),
        
        tabPanel(
            "Prediction Plot",
            plotOutput("prediction")
        ),
        
        tabPanel(
            "Residuals' Plot",
            plotOutput("residuals")     
        ),
        tabPanel(
            "Decomposed Plots",
            plotOutput("decomposed")
        )
        
        
    )
    
)
))
