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
  else ggplot(data = us.date) +
    geom_col(aes(x = date, y = `Daily Deaths`), fill = "darkorange3") +
    geom_line(aes(x = date, y = five_day_deaths), colour = 'blue', size = 1) +
    geom_hline(aes(yintercept = average), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Deaths Covid-19 in US by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("Total US ",input$type," :: ", total ),
                          paste0(input$type," per million Americans :: ", as.integer(total / 327) ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Five Day Moving Average :: Blue Line",
                          sep = "\n"),
         y = "Cumulative Deaths",
         x = "2020")
  
  
  
  
} )
  
output$table <- renderTable(us.date[ , .(date,`Daily Cases`)])

}
