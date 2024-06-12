#this file is used to define functions used in server.R


min_bins = 3

#' @param: data to plot, nr. of bins, 
#' @returns: plot (histogram)
plottingHistogram <- function(data, input_column, bins)
{
  x    <- data[,input_column]
  
  bins <- seq(min(x), max(x), length.out = bins + 1)
  plot <- hist(data[,input_column],breaks = bins, main = paste(input_column),freq = F, xlab = "Values"); lines(density(data[,input_column]))
  
  return(plot)
}

#' @description: plot a histogram with ggplot2
#' @param: data to plot, nr. of bins, 
#' @returns: plot (histogram)
plottingHistogramGgplot <- function(data, input_column, bins)
{
  #get height of y axis by dividing nr. of rows by min. bin number
  # y_height <- data %>% 
  #   dplyr::select(input_column) %>% 
  #   nrow() / min_bins
  #get vector from string input (column_name == input$column, which is a string)
  input_column_vector <- data %>% 
    dplyr::select(input_column) %>% 
    dplyr::pull()
  print(input_column_vector)

  y_height <- length(input_column_vector) #round number up

  
  plot <- ggplot(
    data, 
    aes(x=.data[[input_column]])) +
    
    geom_histogram(
      colour = 4, 
      fill = "white", 
      bins = bins) +
    ylim(0, y_height)
  
  return(plot)
}


#' @description: plot a boxplot with ggplot2
#' @param: data to plot
#' @returns: plot (boxplot)
plottingBoxplot <- function(data, input_column)
{
  plot <- ggplot(
    data, 
    aes(x=.data[[input_column]])) +
    
    geom_boxplot()
  
  return(plot)
}