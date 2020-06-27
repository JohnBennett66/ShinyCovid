### UI SCRIPT
### 

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




###  PAGE LAYOUT :: MAIN & SIDEBAR :: SINGLE PLOT  ####
bootstrapPage(

  titlePanel("COVID-19 DATA TRACKING APPLICATION", windowTitle = "CovidTracker"),
  
  
  sidebarLayout( 
  
    sidebarPanel(
     varSelectInput('type', "Select a data type", us.date[,5:6]),
     width = 3
    ), #sidebar
  
    mainPanel(
    
    textOutput('text'),
    
    plotOutput('plot')
    
    ), #main
 
  position = "right") #sidebarlayout

) #fluidpage

