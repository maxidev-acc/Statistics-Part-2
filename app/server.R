#this file contains the server logic
library(ggplot2)
library(dplyr)
source("functions.R")

#default number of bins used throughout the app:
default_bins = 27




server <- function(input, output) {
  
  
  datensatz <- reactive({
    switch(input$download)
  })
  
  
  data = data.frame(swiss)
  df <- swiss
  i= 1
  for (k in df){
    mycols <- colnames(df)
    i<-i+1
  }
  
  
  output$distPlot <- renderPlot({
    
    
    # x    <- data[,input$download]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # hist(data[,input$download],breaks = bins, main = paste(input$download),freq = F, xlab = "Values"); lines(density(data[,input$download]))
    #plottingHistogram(data, input_column = input$download, bins = input$bins)
    plottingHistogramGgplot(data, input_column = input$download, bins = input$bins)
  })
  
  output$distPlot3 <- renderPlot({
    qqnorm(data[,input$download], main = paste(input$download))
    qqline(data[,input$download],col=2,lwd=2)
    if( input$somevalue == TRUE){ abline(h=median(data[,input$download]))
      abline(h=mean(data[,input$download]),col=4)
    }
    
    
  })
  output$distPlot4 <- renderPlot({
    plottingBoxplot(data, input_column = input$download)
    #boxplot(data[,input$download], horizontal = T, main = paste(input$download))
  })
  
  
  
  
}