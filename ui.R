### UI SCRIPT
### 
# api key bc4fef59f0cef5d4de7b8e9c7b55218eaae523fc
### LOAD PACKAGES
library(rsconnect)
library(censusapi)
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
library(RSocrata)
###
options(datatable.optimize=1)

popsicle <- c("a big bunch of text explaning a thing or two about something or another.")
# icon("globe", class = NULL, lib = "font-awesome")

###  PAGE LAYOUT :: MAIN & SIDEBAR :: SINGLE PLOT  ####
fluidPage(
  navbarPage("COVID-19 DATA TRACKING", theme = shinytheme("simplex"), 
    tabPanel("", fluid = TRUE,
             h1("Welcome to My COVID-19 Tracker", .noWS = 'before'),
             HTML("This app provides some analysis of the Covid-19 data worldwide. <br>
                  The analysis comes from a USA-centric perspective, but feel free to make suggestions. 
                    (see the About tab) <br>
                  The menu structure should be self explanatory. <br>
                  I hope you find this useful. <br>
                   - John Bennett <br><br><br>
                  <strong>NOTE: </strong>This is 'Version 2', in case it looks different from the last time you visited.")
             ),
    
  navbarMenu("The World", 
    tabPanel("Where Are We", fluid = TRUE,
             h3("The world ranked:"),
             sidebarLayout(
               sidebarPanel(
                 # shinythemes::themeSelector(),
                 selectInput('world_type', p("Which Data? Cases or Deaths", style="font-size:13px"),
                             c("Daily Cases" = "cases", "Daily Deaths" = "deaths")),
    
                 # selectInput('forecast', p("Include Forcast? Yes/No", style="font-size:13px"),
                 #             c("No" = "no", "Yes" = "yes")),
    
                 width = 2
                 ), # sidebarpanel
               mainPanel(
                 h4("Cumulative Cases"),
                 plotOutput('plot_today_world'), 
                 htmlOutput('text_today_world'),
                 h4("Cumulative Cases per 100,000 population"), 
                 plotOutput('plot_today_world_100k'), 
                 htmlOutput('text_today_world_100k'),
                 h4("Cumulative Cases per 100,000 percent change"), 
                 plotOutput('plot_today_world_100k_pctchg'),
                 # HTML(popsicle),
                 HTML("<strong>NOTE: </strong>The percent change is week over week. <br>  
                      The formula :: ( (today&lsquo;s value - last week&lsquo;s value) &#247; 
                      last week&lsquo;s value). <br> 
                      Also, not all terms are used technically. This is for a lay audience and <br> 
                      often uses terms colloquially to faciliate understanding for the target audience. 
                      <br> <br>")
                 
                 
                ) # mainpanel
             ) # sidebarlayout
    ) #tabpanel
    
  ),  # navbarmenu

  navbarMenu("The United States", 
             tabPanel("Where Are We", fluid = TRUE,
                      h3("The States ranked:"),
                      sidebarLayout(
                        sidebarPanel(
                          # shinythemes::themeSelector(),
                          selectInput('us_type', p("Which Data? Cases or Deaths", style="font-size:13px"),
                                      c("Daily Cases" = "cases", "Daily Deaths" = "deaths")),
                          
                          # selectInput('forecast', p("Include Forcast? Yes/No", style="font-size:13px"),
                          #             c("No" = "no", "Yes" = "yes")),
                          
                          width = 2
                        ), # sidebarpanel
                        mainPanel(
                          h4("Cumulative Cases"),
                          plotOutput('plot_today_us'), 
                          HTML("This chart can be misleading because it treats each country the same. 
                      We would expect countries with more people, i.e., a larger population, <br>
                      to have more cases or deaths than countries with fewer people.  
                      To be fairer in our analysis and ranking, we should compare a &lsquo;rate&lsquo;, <br>
                      which is a comparison of equal values. In this case that means how many cases 
                      per some number of population. Most countries have <br> 
                      millions of people so, using a group of 100,000 people for our rate is a good idea. <br>
                      It could be groups of 10,000 or 1,000,000 or any number. 
                      I think 100,000 works well because it makes the numbers a good size. 
                      If you pick <br>1,000,000, then some of the small countries have 0.01 cases per Millon, 
                      or if you pick 10,000, then some of the large countries have 1,000 <br> cases or more. 
                      It is an arbitray choice, I just like 100,000. <br> 
                      For these charts it does not make too much difference because they have no numbers. 
                      These charts use shading to show which country has <br> &#8220;the most&#8221;, where 
                      darker shading means more cases."),
                          h4("Cumulative Cases per 100,000 population"), 
                          plotOutput('plot_today_us_100k'), 
                          HTML("Now we are looking at the rate of cases or the rate of deaths, which is fairer 
                      and more accurate if we want to understand Covid; <br> 
                      what is happening?, where is it the worst?, where is it getting better?, etc. 
                      In the chart above the US was the worst and only a <br> 
                      couple other countries were near the top. In this chart we see that many countries 
                      are near the level of the US for &#8220;the number <br> 
                      of people who have tested positive for COVID-19 per 100,000 people in that country&#8221;. 
                      Now the US has many other countries that are also on <br> 
                      the top end of the scale."),
                          h4("Cumulative Cases per 100,000 percent change"), 
                          plotOutput('plot_today_us_100k_pctchg'),
                          HTML("In this chart we are looking at the percentage of change between a countries 
                      &#8220;rate of cases per 100,000 people last week&#8221; <br> 
                      and that same rate this week. This shows us where it is getting worse and 
                      where it is staying the same. It will not get <br> 
                      &#8220;better&#8221; because this is cumulative cases so this rate never goes 
                      in a negative direction.
                      <br><br>"),
                          # HTML(popsicle),
                          HTML("<strong>NOTE: </strong>The percent change is week over week. <br>  
                      The formula :: ( (today&lsquo;s value - last week&lsquo;s value) &#247; 
                      last week&lsquo;s value). <br> 
                      Also, not all terms are used technically. This is for a lay audience and <br> 
                      often uses terms colloquially to faciliate understanding for the target audience. 
                      <br> <br>")
                          
                          
                        ) # mainpanel
                      ) # sidebarlayout
             ), #tabpanel where we are now
             "----", 
             tabPanel("US Trends", fluid = TRUE,
                                          h3("The States ranked:"),
                                          sidebarLayout(
                                            sidebarPanel(
                                              # shinythemes::themeSelector(),
                                              selectInput('us_type', p("Which Data? Cases or Deaths", style="font-size:13px"),
                                                          c("Daily Cases" = "cases", "Daily Deaths" = "deaths")),
                                              
                                              # selectInput('forecast', p("Include Forcast? Yes/No", style="font-size:13px"),
                                              #             c("No" = "no", "Yes" = "yes")),
                                              
                                              width = 2
                                            ), # sidebarpanel
                                            mainPanel(
                                              
                                            ) 
                                          ) #tabpanel us trends 
             
  ),  # navbarmenu
  
    tabPanel("About", fluid = TRUE,
      mainPanel(
        h3("About the Data"),
        p("This data is provided from John Hopkins and other sources."),
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
        improve my skills so, I built this Shiny app for fun."),
        p("If you have suggestions for this app or want to reach
        me for any reason, you can find me here."),
        p("Github :: JohnBennett66"),
        p("also,"),
        p("Twitter ::  @john_bennett"),
        p("LinkedIn :: https://www.linkedin.com/in/johnbennett/"),
        p(),
        p("This app can also be reached at ",a(href="bit.ly/jb66ct","bit.ly/jb66ct"))
      
      ) # mainpanel
    ) # ABOUT TAB
    
  ) # navbarpage
) #fluidpage
