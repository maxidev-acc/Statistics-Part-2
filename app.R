
library(shiny)

library(vcd)
library(moments)
library(hrbrthemes)

##
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hausübung 2 - Visualisierung der Datensets in R.Shiny"),
    
  
    # Sidebar with a slider input for number of bins 
   

        # Show a plot of the generated distribution
        mainPanel(
          
         
          
          
          
          tabsetPanel(
            id = "dataset_tabs", 
            tabPanel("Aufgabe 1", value = 1, 
                     titlePanel("Aufgabe 1 - Swiss "),
                     
                     
                     sidebarPanel(
                      selectInput("download", "Select Parameter", choices = colnames(swiss)),
                      sliderInput("bins",
                                   "Number of bins for Histogramm:",
                                   min = 1,
                                   max = 50,
                                   value = 30)),
                     mainPanel( 
                     plotOutput("distPlot1"),
                     plotOutput("distPlot2"),
                     checkboxInput("somevalue", "Abline Mean (blue) and median (black)", FALSE),
                     plotOutput("distPlot3"),
                     plotOutput("distPlot4"),
                     
                     )),
            
            
            tabPanel("Aufgabe 2", value= 2,
                     titlePanel("Aufgabe 2 - State 77 "),
                     
                     
                     
                     sidebarPanel(
                       selectInput("downloadStates", "Select Parameter", choices = colnames(state.x77)),
                       sliderInput("bins2",
                                   "Number of bins for Histogramm:",
                                   min = 1,
                                   max = 50,
                                   value = 30)),
                     mainPanel( 
                       plotOutput("distPlot5"),
                       plotOutput("distPlot6"),
                       checkboxInput("somevalue2", "Abline Mean (blue) and median (black)", FALSE),
                       plotOutput("distPlot7"),
                       plotOutput("distPlot8"),
                       
                     )),
                     
                     
                     
                     
                             
                    tabPanel("Aufgabe 3",
                             titlePanel("Aufgabe 3 - State Lake Huron "),
                             
                             #selectInput("downloadStates", "Select Parameter", choices = colnames(LakeHuron)),
                             checkboxInput("modelforLake", "Abline Lienar Model for Lake Huron", FALSE),
                             plotOutput("distPlot9"),
                             sliderInput("bins3",
                                         "Number of bins for Histogramm:",
                                         min = 1,
                                         max = 50,
                                         value = 30),
                             plotOutput("distPlot10"),
                             #checkboxInput("somevalue2", "Abline Mean (blue) and median (black)", FALSE),
                             plotOutput("distPlot11"),
                             plotOutput("distPlot12"),     
                             
                             ),
            
                      
                    tabPanel("Aufgabe 4",
                             mainPanel(  
                             titlePanel("Aufgabe 4 - Titanic"),          
                             plotOutput("mosaicPlot"),
                             selectInput("titanic", "Select Parameter", choices = c("Sex", "Class", "Age")),
                             
                             plotOutput("mosaicPlot1"),
                             ),
                           ),
          
          ),
           
      
        ),)
        
    
    
    
    
    





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
 

 
    
   
    #Aufgabe 1
    output$distPlot1 <- renderPlot({
        data <- swiss
        x    <- data[,input$download]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(data[,input$download],breaks = bins, main = paste(input$download),freq = F, xlab = "Values"); lines(density(data[,input$download]))
       
   
    })
   
    output$distPlot2 <- renderPlot({
      data <- swiss
      qqnorm(data[,input$download], main = paste(input$download))
      qqline(data[,input$download],col=2,lwd=2)
      if( input$somevalue == TRUE){ abline(h=median(data[,input$download]))
        abline(h=mean(data[,input$download]),col=4)
        }
      
    })
    output$distPlot3 <- renderPlot({
      data <- swiss
      boxplot(data[,input$download], horizontal = T, main = paste(input$download))
      })
    
    output$distPlot4 <- renderPlot({
      data <- swiss
      library("PerformanceAnalytics")
      my_data <- swiss[, c(1,2,4,5,6)]
      chart.Correlation(my_data, histogram=TRUE, pch=19)
    })
    
    #Aufgabe2
    
    output$distPlot5 <- renderPlot({
      data <- state.x77
      x    <- data[,input$downloadStates]
      bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
      hist(data[,input$downloadStates],breaks = bins, main = paste(input$downloadStates),freq = F, xlab = "Values"); lines(density(data[,input$downloadStates]))
      
      
    })
    
    output$distPlot6 <- renderPlot({
      data <- state.x77
      qqnorm(data[,input$downloadStates], main = paste(input$downloadStates))
      qqline(data[,input$downloadStates],col=2,lwd=2)
      if( input$somevalue2 == TRUE){ abline(h=median(data[,input$downloadStates]))
        abline(h=mean(data[,input$downloadStates]),col=4)
      }
      
    })
    output$distPlot7 <- renderPlot({
      data <- state.x77
      boxplot(data[,input$downloadStates], horizontal = T, main = paste(input$downloadStates))
    })
    
    output$distPlot8 <- renderPlot({
      data <- swiss
      library("PerformanceAnalytics")
      my_data <- state.x77[, c(1,2,4,5,6)]
      chart.Correlation(my_data, histogram=TRUE, pch=19)
    })
    
    
    #Aufgabe 3
    
    output$distPlot9 <- renderPlot({
      require(utils)
      data(LakeHuron)
      
      huron <- data.frame(feet=as.matrix(LakeHuron), date=time(LakeHuron))
      huron["year"] <- 1875:1972
      huron <- subset(huron, select = -c(date))
      model <- lm(formula = feet ~ year, data=huron)
      
      plot(LakeHuron,  main="Lake Huron Wasserstand über den Zeitraum 1875-1972", xlab="Jahr", ylab="Wasserstand in feet")
      if (input$modelforLake == TRUE){
        abline(model, col="green")
        
      }
      
      abline(h=mean(LakeHuron),col=2,lwd=2)
      
      
    })
    
  
      
    output$distPlot10 <- renderPlot({
      hist(LakeHuron,freq=F, breaks = input$bins3, main="Lake Huron relativer Wasserstand im Zeitraum 1875-1972", ylab="Relative Verteilung", xlab="Wasserstand in feet")
      lines(density(LakeHuron),col=2,lwd=2)
      
      
      
    })
      
    
    output$distPlot11 <- renderPlot({
      boxplot(LakeHuron, horizontal = T, main="Lake Huron Wasserstand im Zeitraum 1875-1972", xlab="Wasserstand in feet")
      stripchart(LakeHuron,           
                 method = "jitter", # Random noise
                 pch = 1,          # Pch symbol
                 col = 1,           # Color of the symbol
                 add = TRUE)
      
      
    })
    
    
    
    
    output$distPlot12 <- renderPlot({
      qqnorm(LakeHuron)
      qqline(LakeHuron,col=2,lwd=3)
      
    })
    
    
    output$mosaicPlot <- renderPlot({
      # Load the Titanic dataset
      data("Titanic")
      
      # Create a mosaic plot
      mosaic(~ Class + Sex + Age + Survived, data = as.table(Titanic), shade = TRUE, legend = TRUE ,width  = 700, height = 700, unit ="px")
    })
    
    output$mosaicPlot1 <- renderPlot({
    #Überleben: Klasse
      j <-input$titanic
      formula <- as.formula(paste("~ Survived +", j))
      struct <- structable(formula, data=as.table(Titanic))
      mosaic(struct, direction=c("h", "v"), pop = FALSE, widthDetails(10), main=paste0("Überleben nach ", input$titanic), gp=gpar(fill=c("lightblue", "pink")))
      labeling_cells(text = as.table(struct), margin = 0)(as.table(struct))
    })
    
   
    
    
    
  
  
    
    
    
    
    
    

}



# Run the application 
shinyApp(ui = ui, server = server)
