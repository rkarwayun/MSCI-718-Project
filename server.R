library(shiny)
library(quantmod)
library(ggplot2)
library(tseries)
library(forecast)

## Intialize DAX data
data <- read.csv("data.csv")
col = cbind("Samsung", "Apple", "Huawei")
x = seq(as.Date("2017/1/1"), by = "month", length.out = 33)


# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
    
    # Return the requested dataset
    datasetInput <- reactive({
        
        data <- read.csv("data.csv")
        
        return(data)
    })
    
    
    
    
    # generate a plot
    output$timeseries <- renderPlot({
        
        cmp <- reactive({input$company})
        date = seq(as.Date("2017/1/1"), by = "month", length.out = 33)
        dataset <- datasetInput()
        if(cmp() == "Samsung"){
            ggplot(dataset, aes(date, data$Samsung)) + geom_line() + ggtitle("Samsung's Mobile Phone Market Share") + xlab("Time") + ylab("Market Share(in %)")
        }
        else if(cmp() == "Apple"){
            ggplot(dataset, aes(date, data$Apple)) + geom_line() + ggtitle("Apple's Mobile Phone Market Share") + xlab("Time") + ylab("Market Share(in %)")
        }
        else{
            ggplot(dataset, aes(date, data$Huawei)) + geom_line() + ggtitle("Huawei's Mobile Phone Market Share") + xlab("Time") + ylab("Market Share(in %)")
        }
        
        
        
        
    })
    
    
    # generate a plot
    output$prediction <- renderPlot({
        
        cmp <- reactive({input$company})
        dataset <- datasetInput()
        if(cmp() == "Samsung"){
            fit_samsung <- auto.arima(log(ts(data$Samsung, start = c(2017,1), frequency = 12)), stepwise = FALSE, approximation = FALSE, d = 2)
            futurVal_samsung_fit_2020 <- forecast(fit_samsung, h=12, level=c(99.5))
            plot(futurVal_samsung_fit_2020, main = "Forecast for Samsung's Mobile Phone Market Share", xlab = "Time",  ylab = "Market Share (in log)")
        }
        else if(cmp() == "Apple"){
            fit_apple <- auto.arima(log(ts(data$Apple, start = c(2017,1), frequency = 12)), stepwise = FALSE, approximation = FALSE, d = 2)
            futurVal_apple_fit_2020 <- forecast(fit_apple, h=12, level=c(99.5))
            plot(futurVal_apple_fit_2020, main = "Forecast for Apple's Mobile Phone Market Share", xlab = "Time",  ylab = "Market Share (in log)")
        }
        else{
            fit_huawei <- auto.arima(log(ts(data$Huawei, start = c(2017,1), frequency = 12)), stepwise = FALSE, approximation = FALSE, d = 2)
            futurVal_huawei_fit_2020 <- forecast(fit_huawei, h=12, level=c(99.5))
            plot(futurVal_huawei_fit_2020, main = "Forecast for Huawei's Mobile Phone Market Share", xlab = "Time",  ylab = "Market Share (in log)")
        }
    })
    
    #generate a plot
    output$decomposed <- renderPlot({
        
        cmp <- reactive({input$company})
        dataset <- datasetInput()
        if(cmp() == "Samsung"){
            plot.new()
            ts_demo_components_samsung <- decompose(ts(data$Samsung, start = c(2017,1), frequency = 12))
            plot(ts_demo_components_samsung)
        }
        else if(cmp() == "Apple"){
            ts_demo_components_apple <- decompose(ts(data$Apple, start = c(2017,1), frequency = 12))
            plot(ts_demo_components_apple)
        }
        else{
            ts_demo_components_huawei <- decompose(ts(data$Huawei, start = c(2017,1), frequency = 12))
            plot(ts_demo_components_huawei)
        }
    })
    
    ##gernerate a plot
    output$residuals <- renderPlot({
        cmp <- reactive({input$company})
        dataset <- datasetInput()
        if(cmp() == "Samsung"){
            fit_samsung <- auto.arima(log(ts(data$Samsung, start = c(2017,1), frequency = 12)), stepwise = FALSE, approximation = FALSE, d = 2)
            tsdisplay(residuals(fit_samsung), lag.max=45, main = "Plots of Residuals for Samsung Model")
        }
        else if(cmp() == "Apple"){
            fit_apple <- auto.arima(log(ts(data$Apple, start = c(2017,1), frequency = 12)), stepwise = FALSE, approximation = FALSE, d = 2)
            tsdisplay(residuals(fit_apple), lag.max=45, main = "Plots of Residuals for Apple Model")
        }
        else{
            fit_huawei <- auto.arima(log(ts(data$Huawei, start = c(2017,1), frequency = 12)), stepwise = FALSE, approximation = FALSE, d = 2)
            tsdisplay(residuals(fit_huawei), lag.max=45, main = "Plots of Residuals for Huawei Model")
        }
        
    })
})
