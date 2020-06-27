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

one <- rnorm(30)
two <- rnorm(30)
three <- rnorm(30)
four <- rnorm(30)




###  MAIN FUNCTION  ####
function(input, output) {
  
 # output$plot <- renderPlot( {
 #   ggplot(us.date) + 
 #     geom_col(aes(date,input$type))
 #   
 # } )
 
output$plot <- renderPlot( {
  
  if(input$type == "Daily Cases")  ggplot(us.date) + geom_col(aes(date,`Daily Cases`))
  
  
  
  
} )
  
output$table <- renderTable(us.date[ , .(date,`Daily Cases`)])

}
