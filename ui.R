### UI SCRIPT
### 

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

# STATE LIST
states.lst <- us.state[,state]
states.lst <- unique(states.lst)


###  PAGE LAYOUT :: MAIN & SIDEBAR :: SINGLE PLOT  ####
fluidPage(

  navbarPage("COVID-19 DATA TRACKING", theme = shinytheme("spacelab"), 
             
             tabPanel("Daily US", fluid = TRUE, 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          # shinythemes::themeSelector(),
                          selectInput('daily', p("Which Data? Cases or Deaths", style="font-size:13px"), 
                                      c("Daily Cases" = "cases", "Daily Deaths" = "deaths")),
                          
                          selectInput('forecast', p("Include Forcast? Yes/No", style="font-size:13px"), 
                                      c("No" = "no", "Yes" = "yes")),
                          
                          width = 2
                          
                        ),
                        mainPanel(
                          
                          # textOutput('daily'),
                          # textOutput('forecast'),
                          plotOutput('plot_dus'),
                          tableOutput('table_states'),
                          HTML("<strong>NOTE:</strong> The Percent Change columns are this week compared to last week.")
                          
                        ),
                      ),
                     ), #tab-daily-us
    
             # tabPanel("Forecast", fluid = TRUE, 
             #          
             #          # sidebarLayout(
             #          #   sidebarPanel(
             #          #     
             #          #     # selectInput('daily', p("Which Data? Cases or Deaths", style="font-size:13px"), 
             #          #     #             c("Daily Cases" = "cases", "Daily Deaths" = "deaths")),
             #          #     width = 2
             #          #     
             #          #   ),
             #            mainPanel(
             #              
             # 
             #              # plotOutput('forecast.c'),
             #              # plotOutput('forecast.d')
             #              HTML("<h3>Coming Soon, Analysis of Predicted Values vs Actual</h3>")
             #              
             #            ),
             #          # ),
             # ), #tab-forecast
             
             tabPanel("State Comparisons", fluid = TRUE, 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('cumulative', p("Cases or Deaths?", style="font-size:13px"), 
                                      c("Cumulative Cases" = "cases", "Cumulative Deaths" = "deaths")),
                          width = 2
                        ),
                        mainPanel(
                          
                          plotOutput('plot_st'),
                          plotOutput('plot_chg')
                          
                        ),
                      ),
             ), #tab-state-comp
    
             tabPanel("Daily Single State", fluid = TRUE, 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('state', p("Which State", style="font-size:13px"), 
                                      us.state[,unique(state)], selected = "Alabama"),
                          
                          p(),
                          p("These states had higher than average change last week ::",
                             style="font-size:15px"),
                          p(),
                          textOutput('st.lst'),
                          p(),
                          em("Listed in descending order.",
                             style="font-size:11px"),
                          width = 2
                        ),
                       
                        mainPanel(
                         
                          plotOutput('plot_dss_cases'),
                          plotOutput('plot_dss_death')
                          
                        ),
                      ),
             ), #tab-single-state
             
             
             tabPanel("Counties — US", fluid = TRUE,
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('counties', p("Which State", style="font-size:13px"), 
                                      states.lst, selected = "Alabama"),
                          width = 2
                        ),
                        
                        mainPanel(
                          plotOutput('plot_county_cases'),
                          plotOutput('plot_county_deaths')
                        ),
                      ),
                      
                      
                      ), #tab-counties-us
             
             tabPanel("World — Rankings", fluid = TRUE,
                      
                      mainPanel(
                        plotOutput('world.c'),
                        plotOutput('world.d')
                      ),
               
             ), #tab-world-rankings
             
             tabPanel("World — Trends", fluid = TRUE,
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Surging",
                            plotOutput('world.surge') 
                          ), 
                          tabPanel("Recent Wave",
                              plotOutput('world.recent')
                          ),  
                          tabPanel("Wave Past",
                              plotOutput('world.past') 
                          )
                        ) # tabset
                      )
                        
             ), #tab-world-trend
             
             tabPanel("World — Stats", fluid = TRUE, 
                      
                      mainPanel(
                        h3("Table of Basis Stats for World Countries"),
                        h5("Sorted by 'Deaths per Million Pop' descending"),
                        h5("You can search for a country using the browser's search capability — ctrl + F"),
                        tableOutput('stats_world')
                      )
                      
                      
            ), #tab-world-stats
             
             tabPanel("Other Causes", fluid = TRUE, 
                     sidebarLayout(
                       sidebarPanel(
                         selectInput('state_oc',
                                     p('Which State?*', style="font-size:13px"),
                                     jurs.lst[,state], selected = "United States"),
                                     p("* Choose United States or a specific state; 
                                        also includes District of Columbia, Puerto Rico, 
                                       and New York City.", style="font-size:11px"),
                         width = 2
                       ),
                     mainPanel(
                       plotOutput('other_causes')
                     ),
                   ),
                 ), # tab-"Other Causes
                     
                     
                     
                     
             
             tabPanel("About", fluid = TRUE,
                      mainPanel(
                        h3("About the Data"),
                        p("This data is provided from John Hopkins."),
                        p("The data is available from dataworld.com, 
                          which is where I refresh the data for this app."),
                        p("The visualizations are mine, not that they are so fancy."),
                        p("Regarding the app, I had inspiration/assistance from the 
                          NCAA Swim Team Finder app in the Shiny gallery."),
                        p("You can see it here, ",
                        a(href="https://shiny.rstudio.com/gallery/ncaa-swim-team-finder.html",
                          "NCAA Swim Team Finder Shiny App Example")),

                        
                        h3("About the Author"),
                        p("I am John Bennett. I am a data scientest looking to
                          improve my skills so I built this Shiny app for fun."),
                        p("If you have suggestions for this app or want to reach
                          me for any reason, you can find me here."),
                        p("Github :: JohnBennett66"),
                        p("also,"),
                        p("Twitter ::  @john_bennett"),
                        p("LinkedIn :: https://www.linkedin.com/in/johnbennett/"),
                        p(),
                        p("This app can also be reached at ",a(href="bit.ly/jb66ct","bit.ly/jb66ct"))

                      )
               
             ) # ABOUT TAB
             
    
  ), # navbarpage
  
  
  
) #fluidpage

