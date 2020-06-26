### SERVER SCRIPT

### LOAD PACKAGES
library(rsconnect)
library(shiny)
library(ggplot2)
library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(usmap)
###


function(input, output) {
  
  
  output$plot <- renderPlot( {
    
    plot(hist(rnorm(100)))
    
    
  }, height = 1200, width = 1200 )
  
}
