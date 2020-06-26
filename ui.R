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
  
  # titlePanel("US :: COVID-19 :: BASIC DATA"),
  
  sidebarLayout(
  
  sidebarPanel(
    
    selectInput('type', "Date to display…", c("Daily Cases", "Daily Deaths"))
    
    ,width = 3),
  
  
  
  mainPanel(
    
    plotOutput("plot")
    
  , width = 9),
  
  position = "right",
  fluid = TRUE)

)

