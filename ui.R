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
fluidPage(
  
  sidebarPanel(
     varSelectInput('type', "Select a data type", us.date[,5:6])
  ),
  
  mainPanel(
    
    textOutput('text'),
    
    plotOutput('plot')
    
    
    
  )

)

