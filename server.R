### SERVER SCRIPT

### LOAD PACKAGES
library(rsconnect)
library(fpp2)
library(scales)
library(shiny)
library(ggplot2)
library(ggrepel)
library(stringr)
library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(usmap)
library(maps)
library(shinythemes)
###
options(datatable.optimize=1)

###  SETUP :: IMPORT :: TRANSFORM  ####
source('setup_anlyss.R')




###  MAIN FUNCTION  ####
function(input, output) {
  
###  THE WORLD  ####
###  WHERE ARE WE
# world rank cumulative
output$plot_today_world <- renderPlot( {
  
  
  if(input$daily == 'cases') 
    ggplot(world.today, aes(fill = cum_cases)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
    
}, width = 850, height = 400 ) # output$plot_today_world
# world rank cumulative 100K  
output$plot_today_world_100k <- renderPlot( {
  
  
  if(input$daily == 'cases') 
    ggplot(world.today, aes(fill = ccper100k)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  
}, width = 850, height = 400 ) # output$plot_today_world
# world rank cumulative 100k % change
output$plot_today_world_100k_pctchg <- renderPlot( {
  
  
  if(input$daily == 'cases') 
    ggplot(world.today, aes(fill = cc_pctchg)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  
}, width = 850, height = 400 ) # output$plot_today_world




} # function























