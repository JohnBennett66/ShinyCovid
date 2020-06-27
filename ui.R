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
library(shinythemes)
###




###  PAGE LAYOUT :: MAIN & SIDEBAR :: SINGLE PLOT  ####
fluidPage(

  navbarPage("COVID-19 DATA TRACKING", theme = shinytheme("spacelab"), 
             
             tabPanel("Daily US", fluid = TRUE, icon =icon("flag-usa"), 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          em("Which Data?", style="font-size:15px"),
                          shinythemes::themeSelector(),
                          selectInput('daily', p("Cases or Deaths?", style="font-size:13px"), 
                                      c("Daily Cases" = "cases", "Daily Deaths" = "deaths"))
                          #    width = 3
                          
                        ),
                        mainPanel(
                          
                          plotOutput('plot_dus')
                          
                        ),
                      ),
                     ), #tab-daily-us
    
             tabPanel("State Comparisons", fluid = TRUE, icon =icon("flag-usa"), 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('cumulative', p("Cases or Deaths?", style="font-size:13px"), 
                                      c("Cumulative Cases" = "cases", "Cumulative Deaths" = "deaths")),
                          selectInput("scope", p("Total this Week or Percent Change from Last Week"), 
                                      c("Total" = "total", "Percent Change" = "change"))
                          
                        ),
                        mainPanel(
                          
                          plotOutput('plot_st')
                          
                        ),
                      ),
             ) #tab-state-comp
    
    
    
  ), # navbarpage
  
  
  
) #fluidpage

