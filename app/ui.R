#RUN THIS APP: Either in file UI.R or SERVER.R click on "Run App" in the top right corner,
#next to the green triangle.


#this file contains the user inputs and what the user sees.
library(shiny)

#default number of bins & minimum number used throughout the app:
default_bins = 23
min_bins = 3

ui <- fluidPage(

    # Application title
    titlePanel("Hausübung 2 - Visualisierung der Datensets in R.Shiny"),
    
    
   

        # Show a plot of the generated distribution
        mainPanel(
          
          #choose tabs for different exercises
          tabsetPanel(
            tabPanel("Aufgabe 1",
                     
                     #select data column
                     sidebarPanel(
                       selectInput("download", "Select Parameter", choices = colnames(swiss)),
                       ),
                     
                     mainPanel(
                      plotOutput("distPlot"),
                       #histogram bins input
                      sliderInput("bins",
                                   "Number of bins for Histogramm:",
                                   min = min_bins,
                                   max = 50,
                                   value = default_bins),
                      plotOutput("distPlot3"),
                      checkboxInput("somevalue", "Abline Mean (blue) and median (black)", FALSE),
                      plotOutput("distPlot4"),
                      )),
            
            
            tabPanel("Aufgabe 2",#  (dummy data)
                     
                     sidebarPanel(
                       selectInput("download2", "Select Parameter", choices = colnames(swiss)),
                       sliderInput("bins",
                                   "Number of bins for Histogramm:",
                                   min = 1,
                                   max = 50,
                                   value = 30)),
                     mainPanel( 
                       plotOutput("distPlot2"),
                       plotOutput("distPlot32"),
                       checkboxInput("somevalue2", "Abline Mean (blue) and median (black)", FALSE),
                       plotOutput("distPlot42"),
                     )),
            
            tabPanel("Aufgabe 3"),
            tabPanel("Aufgabe 4")
            
          ),
          
          
           
      
        )
        
)



