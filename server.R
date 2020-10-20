### SERVER SCRIPT  ####

### LOAD PACKAGES
library(rsconnect)
library(censusapi)
library(fpp2)
library(scales)
library(shiny)
library(ggplot2)
library(ggrepel)
library(stringr)
# library(wbstats)
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

###  SET SOME VARIABLES  ####
# various dates and date formatting
reporting.date <- df.tb[country == "USA"][date == max(date)][, unique(date)]
report.date <- as.character(reporting.date)
display.date <- paste(day(reporting.date), 
                      month(reporting.date, label = TRUE, abbr = FALSE),
                      year(reporting.date),
                      sep = " ")
# color values for plots, charts, graphs, etc
plot.back <- rgb(238, 238, 238, maxColorValue = 255)
plot.border <- rgb(223, 215, 202, maxColorValue = 255)
death.main <- rgb(242, 63, 33, maxColorValue = 255) 
deaths.mid <- rgb(247, 131, 110, maxColorValue = 255)
deaths.low <- rgb(252, 213, 207, maxColorValue = 255)
cases.main <- rgb(33, 150, 243, maxColorValue = 255)
cases.mid <- rgb(110, 185, 247, maxColorValue = 255)
cases.low <- rgb(207, 232, 252, maxColorValue = 255)
lines <- rgb(204, 204, 204, maxColorValue = 255)

###################### ###
###  MAIN FUNCTION    ####
###  INPUT AND OUTPUT  ###
###################### ###
function(input, output) {
  
###  GENERIC STUFF  ####  
## Current Data Date
output$current_data_date <- renderUI( { 
  p(">>", em("Data current as of :  "), strong(HTML(display.date)), "<<")
} )
  
############################################ ###
###  HOME MENU :: INTRODUCTION :: OVERVIEW  ####
############################################ ###

###  OVERVIEW  ####
# in UI script

###  MENU LEGEND #### 
# in UI script

###  WORLD  ####
## TREND VISUALS  ####
# home page :: world daily cases trend :: new_cases :: line chart
output$world_growth_cases_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_cases), colour = cases.main) + 
    labs(y = "Daily Cases", x = "") + 
    scale_y_continuous(label = comma) + 
    theme(panel.grid = element_line(colour = lines))
}, width = 400, height = 200 )
# home page :: world daily deaths trend :: new_deaths :: line chart
output$world_growth_deaths_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_deaths), color = death.main) + 
    labs(y = "Daily Deaths", x = "") + 
    scale_y_continuous(label = comma) + 
    theme(panel.grid = element_line(colour = lines))
}, width = 400, height = 200 )

###  U.S.  ####
## TREND VISUALS  ####
# home page :: us daily cases trend :: new_cases :: line chart
output$us_growth_cases_daily <- renderPlot( { 
  ggplot(data = us.allup[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_cases), colour = cases.main) + 
    labs(y = "Daily Cases", x = "") + 
    scale_y_continuous(label = comma) + 
    theme(panel.grid = element_line(colour = lines))
}, width = 400, height = 200 )
# home page :: us daily deaths trend :: new cases :: line chart
output$us_growth_deaths_daily <- renderPlot( { 
  ggplot(data = us.allup[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_deaths), color = death.main) + 
    labs(y = "Daily Deaths", x = "") + 
    scale_y_continuous(label = comma) + 
    theme(panel.grid = element_line(colour = lines))
}, width = 400, height = 200 )



################ ###
###  EDUCATION  ####
################ ###


########################### ###
###  LEARN ABOUT THE DATA  ####
########################### ###

##  MAP PLOT VISUALS  ####
# learn about data :: world cumulative rankings :: cum_cases|deaths :: world map plot
output$plot_today_world <- renderPlot( {
  if(input$learn_type == 'cases') {
    ggplot(world.today, aes(fill = cum_cases)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cumulative Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else if(input$learn_type == 'deaths') {
    ggplot(world.today, aes(fill = cum_deaths)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 500, height = 325 ) # output$plot_today_world

# learn about data :: world cumulative per 100k rankings :: ccper100k|cdper100k :: world map plot
output$plot_today_world_100k <- renderPlot( {
  if(input$learn_type == 'cases') {
    ggplot(world.today, aes(fill = ccper100k)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cumulative Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else if(input$learn_type == 'deaths') {
    ggplot(world.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 500, height = 325 ) # output$plot_today_world

# learn about data :: world per 100k percent change rankings :: cc_pctchg|cd_pctchg :: world map plot
output$plot_today_world_100k_pctchg <- renderPlot( {
  if(input$learn_type == 'cases') {
    ggplot(world.today, aes(fill = cc_pctchg)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map_w$long, y = map_w$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cumulative Cases", 
                            label = label_percent()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else if(input$learn_type == 'deaths') {
    ggplot(world.today, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_percent()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 500, height = 325 ) # output$plot_today_world

##  MAP PLOT TEXT ####
# learn about data :: cumulative companion text 
output$text_today_world <- renderUI( {
  if(input$learn_type == 'cases') {
    HTML("This chart can be misleading because it treats each country the same. 
    	We would expect countries with more people, i.e., a larger population, 
    	to have more cases or deaths than countries with fewer people. 
    	To be fairer in our analysis and ranking, we should compare a &lsquo;rate&lsquo;, 
    	which is a comparison of equal values. In this case that means how many cases 
    	per some number of population. Most countries have millions of people so, 
    	using a group of 100,000 people for our rate is a good idea. 
    	It could be groups of 10,000 or 1,000,000 or any number. 
    	I think 100,000 works well because it makes the numbers a good size. 
    	If you pick1,000,000, then some of the small countries have 0.01 cases per Millon, 
    	or if you pick 10,000, then some of the large countries have 1,000 cases or more. 
    	It is an arbitray choice, I just like 100,000. For these charts it does not 
    	make too much difference because they have no numbers. 
    	These charts use shading to show which country has &#8220;the most&#8221;, 
    	where darker shading means more cases.")
  } else if(input$learn_type == 'deaths') {
    HTML("As with the Cumulative Cases plot, this can also be misleading.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum text

# learn about data :: per 100k companion text 
output$text_today_world_100k <- renderUI( {
  if(input$learn_type == 'cases') {
    HTML("Now we are looking at the rate of cases or the rate of deaths, 
         which is fairer and more accurate if we want to understand Covid; 
         what is happening?, where is it the worst?, where is it getting better?, 
         etc. In the chart above the US was the worst and only a couple other 
         countries were near the top. In this chart we see that many countries 
         are near the level of the US for &#8220;the number of people who 
         have tested positive for COVID-19 per 100,000 people in that country&#8221;. 
         Now the US has many other countries that are also on the top end of the scale."
    )
  } else if(input$learn_type == 'deaths') {
    HTML("Here we can see that the highest per capita death rates are in the Americas and Europe.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum per captia text

# learn about data :: percent change companion text 
output$text_today_world_100k_pctchg <- renderUI( {
  if(input$learn_type == 'cases') {
    HTML("In this chart we are looking at the percentage of change between a countries 
                      &#8220;rate of cases per 100,000 people last week&#8221; <br> 
                      and that same rate this week. This shows us where it is getting worse and 
                      where it is staying the same. It will not get <br> 
                      &#8220;better&#8221; because this is cumulative cases so this rate never goes 
                      in a negative direction.
                      <br><br>")
    
  } else if(input$learn_type == 'deaths') {
    HTML("And this is the countries with the highest percentage change in their per capita death rate 
         in the last week.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum percent change text



################################# ###
###  LEARN ABOUT VISUALIZATIONS  ####
################################# ###

# chart comparision - daily vs weekly (smoothed)
output$ed_daily_line <- renderPlot( { 
  
  ggplot(us.allup) + 
    geom_line(aes(x = date, y = new_cases))
  
})

output$ed_weekly_line <- renderPlot( { 
  
  ggplot(us.wkly) + 
    geom_line(aes(x = floor_date, y = new_cases))
  
})



##  TEXT COMPARISONS  ####


############################# ###  
############################# ###
###  END OF LEARNING SECTION  ###
############################# ###
############################# ###



################ ###
###  THE WORLD  ####
################ ###

############### ###
###  OVERVIEW  ####
############### ###

##  MAP PLOT VISUALS  ####

# WORLD :: OVERVIEW :: PER 100K :: CASES AND DEATHS :: 'US_TYPE' 
output$plot_overview_world_100k <- renderPlot( {
  
  if(input$world_type == 'cases') {
    ggplot(world.today, aes(fill = ccper100k)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map_w$long, y = map_w$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cumulative Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map_w$long, y = map_w$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back))
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  } 
  
}, width = 500, height = 325 ) # output$plot_today_world

###  THE TEXT  ####

###  WORLD :: OVERVIEW :: PER 100K :: CASES AND DEATHS :: 'US_TYPE'
output$text_overview_world_100k <- renderUI( { 
  if(input$world_type == 'cases') { 
    HTML("Here we are looking at the rate of cases. This means the number of cases per
         100,000 people (in this case, it could be 10,000 or 1,000,000 or any number). 
         This compares all the countries evenly (fairly). The darker countries have more cases 
         (per 100,000 people) than the lighter countries. More cases (per capita*) means 
         one or more of these; 
           <ul>
           <li>less control of the spread of the virus</li>
           <li>less recognition of the problem (not enough testing)</li>
           <li>unaware of the problem/risk</li>
           </ul>
         We should expect the rate of cases to be fairly constant if an area has a good
         systems of control in place. There will always be some cases. <br>
         <br>
         * &#34;per capita&#34; means the same as &#34;per 100,000 people&#34;"
    )
  } else if(input$world_type == 'deaths') { 
    HTML("These are the number of deaths per 100,000 people in each country. 
           The darker countries have more deaths (per 100,000 people) than the 
           lighter countries. More deaths (per capita*) means one or more of these; 
           <ul>
           <li>excessive cases beyond hospital capacity</li>
           <li>less familiarity with the most effective treatment protocols</li>
           <li>reluctance/unability of population to seek treatment</li>
           </ul> 
           next
          <br>
          * &#34;per capita&#34; means the same as &#34;per 100,000 people&#34;"
    )
  }
} )  # output$text_overview_us_100k 



###  LEAVE OFF  ####
##  THE VISUALS
##  

# single country trend chart with stats

output$world_country_trend_stats <- renderPlot( { 
  
  ggplot(world.data[country == "Sweden"]) + 
    geom_line(aes(x = date, y = new_cases), colour = 'blue') + 
    geom_label(x = world.data[country == "Sweden",unique(min(date))] + 35, 
              y = world.data[country == "Sweden", max(new_cases)] - 150, 
              label = paste("Sweden :: Statistik",
                paste0("Kumulative Fall/100.000 människor :: ", comma(world.data[country == "Sweden" & date == max(date), ccper100k])), 
                paste0("Kumulative Dödsfall/100.000 människor :: ", comma(world.data[country == "Sweden" & date == max(date), cdper100k])), 
                paste0("Fall Procentuell Förändring (veckovis) :: ", percent(world.data[country == "Sweden" & date == max(date), cc_pctchg])), 
                paste0("Dödsfall Procentuell Förändring (veckovis) :: ", percent(world.data[country == "Sweden" & date == max(date), cd_pctchg])), 
                sep = "\n"
              ), 
              color = 'black', size = 4) + 
    geom_label(x = world.data[country == "Sweden",unique(max(date))] - 35, 
               y = world.data[country == "Sweden", max(new_cases)] - 250, 
               label = paste("Jämförelser :: ",
                             paste0("Norway :: Fall/100.000 = ", comma(world.today[country == 'Norway', ccper100k])), 
                             paste0("Denmark :: Fall/100.000 = ", comma(world.today[country == 'Denmark', ccper100k])), 
                             paste0("Portugal :: Fall/100.000 = ", comma(world.today[country == 'Portugal', ccper100k])), 
                             paste0("Greece :: Fall/100.000 = ", comma(world.today[country == 'Greece', ccper100k])), 
                             paste0("France :: Fall/100.000 = ", comma(world.today[country == 'France', ccper100k])), 
                             paste0("Italy :: Fall/100.000 = ", comma(world.today[country == 'Italy', ccper100k])), 
                             paste0("USA :: Fall/100.000 = ", comma(world.today[country == 'USA', ccper100k])), 
                             sep = "\n"), 
               color = 'black', size = 4) + 
    labs(x = "", y = "Daily New Cases", 
         title = "COVID-19 Svenska data i aggregate. Dagliga fall per 100.000 människor.", 
         caption = paste("Translation to English", 
         "Kumulative Fall/Dödsfall = Cumulative Cases/Deaths", 
         "Procentuell Förändring (veckovis) = Percentage Change (weekly)", 
         "Jämförelser = Comparison",
         sep = "\n"))
  
  })



# world rank cumulative

## WORLD OVERVIEW VISUALS ##




######################## ###
###  THE UNITED STATES  ####
######################## ###

############### ###
###  OVERVIEW  ####
############### ###

##  MAP PLOT VISUALS  ####

# US OVERVIEW :: PER 100K :: CASES AND DEATHS :: 'US_TYPE' 
output$plot_overview_us_100k <- renderPlot( {
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = ccper100k)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cumulative Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back)) 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom", 
            panel.grid = element_line(colour = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back))
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  } 
  
}, width = 500, height = 325 ) # output$plot_today_world

###  TABLES  ####
output$states_top_cases <- renderTable(
  us.top.cases
)
output$states_top_deaths <- renderTable(
  us.top.deaths
)
output$states_top_newcases <- renderTable(
  us.top.newcases
)
output$states_top_newdeaths <- renderTable(
  us.top.newdeaths
)


###  THE TEXT  ####

###  US OVERVIEW :: PER 100K :: CASES AND DEATHS :: 'US_TYPE'
output$text_overview_us_100k <- renderUI( { 
  if(input$us_type == 'cases') { 
    HTML("Here we are looking at the rate of cases. This means the number of cases per
         100,000 people (in this case, it could be 10,000 or 1,000,000 or any number). 
         This compares all the states evenly (fairly). The darker states have more cases 
         (per 100,000 people) than the lighter states. More cases (per capita*) means 
         one or more of these; 
           <ul>
           <li>less control of the spread of the virus</li>
           <li>less recognition of the problem (not enough testing)</li>
           <li>unaware of the problem/risk</li>
           </ul>
         We should expect the rate of cases to be fairly constant if an area has a good
         systems of control in place. There will always be some cases. <br>
         <br>
         * &#34;per capita&#34; means the same as &#34;per 100,000 people&#34;"
    )
  } else if(input$us_type == 'deaths') { 
    HTML("These are the number of deaths per 100,000 people in each state. 
           The darker states have more deaths (per 100,000 people) than the 
           lighter states. More deaths (per capita*) means one or more of these; 
           <ul>
           <li>excessive cases beyond hospital capacity</li>
           <li>less familiarity with the most effective treatment protocols</li>
           <li>reluctance/unability of population to seek treatment</li>
           </ul> 
           next
          <br>
          * &#34;per capita&#34; means the same as &#34;per 100,000 people&#34;"
    )
  }
} )  # output$text_overview_us_100k 


############### ###
###  RANKINGS  ####
############### ###

###
###  THE VISUALS & DATA TABLES  ####
###  

# US RANKINGS :: DATA TABLE :: 'us_rankings' 
output$us_ranking <- renderTable( { 
  if(input$us_rank == 'ccper100k')  { 
    setorder(us.table, -ccper100k)
    us.table[ , .(State, `Cases /100k`, `Deaths /100k`, 
                  `Cases Change`, `Deaths Change`, `Mortality Rate`)]
  } else if(input$us_rank == 'cdper100k')  { 
    setorder(us.table, -cdper100k)
    us.table[ , .(State, `Deaths /100k`, `Cases /100k`, 
                  `Deaths Change`, `Cases Change`, `Mortality Rate`)]
  } else if(input$us_rank == 'cc_pctchg')  { 
    setorder(us.table, -cc_pctchg)
    us.table[ , .(State, `Cases Change`, `Deaths Change`, 
                  `Cases /100k`, `Deaths /100k`, `Mortality Rate`)]
  } else if(input$us_rank == 'cd_pctchg')  { 
    setorder(us.table, -cd_pctchg)
    us.table[ , .(State, `Deaths Change`, `Cases Change`, 
                  `Deaths /100k`, `Cases /100k`, `Mortality Rate`)]
  } else if(input$us_rank == 'mortality')  { 
    setorder(us.table, -mortality)
    us.table[ , .(State, `Mortality Rate`,  
                  `Deaths /100k`, `Cases /100k`, 
                  `Deaths Change`, `Cases Change`)]
  } 
  
} , striped = TRUE, bordered = TRUE, rownames = TRUE
)

# US RANKINGS :: MAP RANKINGS :: 'US_RANK'
output$plot_today_us_rank <- renderPlot( { 
  if(input$us_rank == 'ccper100k')  { 
    ggplot(us.table, aes(fill = ccper100k)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cases per 100,000 Population", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            legend.position = 'bottom', 
            plot.background = element_rect(colour = plot.border, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            legend.background = element_rect(fill = plot.back),
            panel.grid = element_line(color = plot.back)
      )  
  } else if(input$us_rank == 'cdper100k')  { 
    ggplot(us.table, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "white", high = death.main, 
                            name = "Deaths per 100,000 Population", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            legend.position = 'bottom', 
            plot.background = element_rect(colour = plot.border, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            legend.background = element_rect(fill = plot.back),
            panel.grid = element_line(color = plot.back)
      ) 
  } else if(input$us_rank == 'cc_pctchg')  { 
    ggplot(us.table, aes(fill = cc_pctchg)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Weekly Change in Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            legend.position = 'bottom', 
            plot.background = element_rect(colour = plot.border, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            legend.background = element_rect(fill = plot.back),
            panel.grid = element_line(color = plot.back)
      ) 
  } else if(input$us_rank == 'cd_pctchg')  { 
    ggplot(us.table, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "pink", high = death.main, 
                            name = "Weekly Change in Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            legend.position = 'bottom', 
            plot.background = element_rect(colour = plot.border, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            legend.background = element_rect(fill = plot.back),
            panel.grid = element_line(color = plot.back)
      ) 
  } else if(input$us_rank == 'mortality')  { 
    ggplot(us.table, aes(fill = mortality)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Mortality Rate", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            legend.position = 'bottom', 
            plot.background = element_rect(colour = plot.border, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            legend.background = element_rect(fill = plot.back),
            panel.grid = element_line(color = plot.back)
      ) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 410, height = 270  
)


################ ###
###  US TRENDS  ####
################ ###   

#################### ###   
##  TRENDS VISUALS  ####
#################### ###  
output$us_daily_cases <- renderPlot( { 
  if(input$us_trend == "cases")  {
    ggplot(us.wkly[1:(nrow(us.wkly)-1)]) + 
    geom_line(aes(x = floor_date, y = new_cases), 
              colour = cases.main) + 
    scale_x_date(date_breaks = "1 months") + 
    scale_y_continuous(labels = comma) + 
    theme(axis.text.x = element_text(angle = 90), 
          plot.background = element_rect(colour = plot.back, fill = plot.back), 
          panel.background = element_rect(colour = plot.back, fill = plot.back), 
          panel.grid = element_line(colour = lines)) + 
    labs(y = "Weekly Total New Cases", 
         x = "")
  } else if(input$us_trend == "deaths")  {
    ggplot(us.wkly[1:(nrow(us.wkly)-1)]) + 
      geom_line(aes(x = floor_date, y = new_deaths), 
                colour = death.main) + 
      scale_x_date(date_breaks = "1 months") + 
      scale_y_continuous(labels = comma) + 
      theme(axis.text.x = element_text(angle = 90), 
            plot.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.grid = element_line(colour = lines)) + 
      labs(y = "Weekly Total New Cases", 
           x = "")
  } else if(input$us_trend == "cc_pctchg")  { 
    ggplot(us.wkly[1:(nrow(us.wkly)-1)]) + 
      geom_line(aes(x = floor_date, y = nc_pctchg), 
                colour = cases.main) + 
      scale_x_date(date_breaks = "1 months") + 
      scale_y_continuous(labels = percent) + 
      theme(axis.text.x = element_text(angle = 90), 
            plot.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.grid = element_line(colour = lines)) + 
      labs(y = "Weekly Percent Change in New Cases", 
           x = "") 
  } else if(input$us_trend == "cd_pctchg")  { 
    ggplot(us.wkly[1:(nrow(us.wkly)-1)]) + 
      geom_line(aes(x = floor_date, y = nd_pctchg), 
                colour = death.main) + 
      scale_x_date(date_breaks = "1 months") + 
      scale_y_continuous(labels = percent) + 
      theme(axis.text.x = element_text(angle = 90), 
            plot.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.grid = element_line(colour = lines)) + 
      labs(y = "Weekly Percent Change in New Deaths", 
           x = "") 
  } 
  
  
  
})


output$cases_percent_detail <- renderPlot( { 
  if(input$us_trend == 'cc_pctchg') { 
    ggplot(us.wkly[13:(nrow(us.wkly)-1)]) + 
      geom_line(aes(x = floor_date, y = nc_pctchg), 
                colour = cases.main) + 
      scale_x_date(date_breaks = "1 months") + 
      scale_y_continuous(labels = percent) + 
      labs(y = "Weekly Percent Change in Cases", 
           x = "") + 
      theme(axis.text.x = element_text(angle = 90), 
            panel.grid = element_line(color = lines),
            plot.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back))
  } else if(input$us_trend == 'cd_pctchg') {
    ggplot(us.wkly[13:(nrow(us.wkly)-1)]) + 
      geom_line(aes(x = floor_date, y = nd_pctchg), 
                colour = death.main) + 
      scale_x_date(date_breaks = "1 months") + 
      scale_y_continuous(labels = percent) + 
      labs(y = "Weekly Percent Change in Deaths", 
           x = "") + 
      theme(axis.text.x = element_text(angle = 90), 
            panel.grid = element_line(color = lines),
            plot.background = element_rect(colour = plot.back, fill = plot.back), 
            panel.background = element_rect(colour = plot.back, fill = plot.back))
  }
}, width = 400, height = 200)


################## ###
##  TRENDS  TEXT  ####
################## ###

output$us_daily_cases_text <- renderUI( {
  if(input$us_trend == 'cases') {
    HTML("This chart shows weekly total new cases for the United States. <br>
         It shows that once cases began in the US, they rose to an initial <br>
         peak and than began a decline, but in June began to rise again to a <br>
         new, higher peak in July. Once again there was a decline, but since <br>
         the beginning of September they have started to rise again.<br>
         There are some things to note here: 
         <ul>
         <li>In April, only 13 states had cumulative cases over 10,000</li>
         <li>In June, when cases rise, 33 states had over 10,000 cases <br> 
              and 5 states have over 100,000</li>
         <li>By the time the second peak crests, 42 states had over 10,000 cases <br> 
              and 10 states had over 100,000</li>
         <li>We can presume that the initial peak was a result of a new virus, <br> 
              with no clear treatment protocal, getting out of hand in the <br> 
              first few places it took hold</li>
         <li>We can presume that the second peak was a result of the virus now <br> 
              being in most states and having hit its initial peak of transmission</li>
         </ul>") 
  } else if(input$us_trend == 'deaths') {
    HTML("This chart shows weekly total new deaths for the United States. <br>
         It shows that once cases began in the US, they rose to an initial <br>
         peak and than began a decline, but unlike new cases there has not been <br>
         a second peak. We can presume that this is because in the beginning <br> 
         the appropriate treatment protocol was unknown and because the <br> 
         healthcare system had not seen the full range of symptoms and distress. <br> 
         Since, 15 May 2020 the weekly deaths have stayed in a range between <br> 
         4,000 and 8,500. 
         Here are some things to note: 
         <ul>
         <li>In mid-April, only 1 states had cumulative deaths over 10,000</li>
         <li>In June, when cases rise, 33 states had over 10,000 cases <br> 
              and 5 states have over 100,000</li>
         <li>After 15 May 2020, the weekly average is 5,789 new deaths</li>
         <li>After 15 May 2020, there is one death for each 51 cases on average</li>
         <li>If weekly cases grow too high, then we can expect deaths to exceed <br> 
              the current 8,500 per week ceiling</li>
         <li>We can presume that the initial peak was a result of a new virus, <br> 
              with no clear treatment protocal, getting out of hand in the <br> 
              first few places it took hold</li>
         </ul>")
  } else if(input$us_trend == 'cc_pctchg') { 
    HTML("The percent change in new cases each week has become fairly consistent <br> 
         after bouncing around a lot in the first couple months. However this is a <br> 
         bit misleading becasue it flattens the volitilty of the later months. <br> 
         Below is a chart showing this detail. As you can see there is still a week <br> 
         with over %40 change, and that is quite a bit.")
  } else if(input$us_trend == 'cd_pctchg') { 
    HTML("The percent change in new deaths each week has become fairly consistent <br> 
         after bouncing around some in the first couple months. However this is a <br> 
         bit misleading becasue it flattens the volitilty of the later months. <br> 
         Below is a chart showing this detail. As you can see there are still weeks <br> 
         with over %30 change, and that is quite a bit.")  
    }
  
})



#===============================================================================
#===============================================================================
#===============================================================================


##  THE VISUALS :: US  ####
##  THE OVERVIEW PANEL :: US  ####

# ranking table

# ranking plot
# stats rank cumulative
output$plot_today_us <- renderPlot( {
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cum_cases)) + 
    geom_map(aes(map_id = state), map = map_us) + 
    expand_limits(x = map_us$long, y = map_us$lat) + 
    scale_fill_continuous(low = cases.low, high = cases.main, 
                          name = "Cumulative Cases", 
                          label = label_number_si()) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),            
          legend.position = "bottom") 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cum_deaths)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom") 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 500, height = 325 ) # output$plot_today_world

# states rank cumulative 100K  

# states rank cumulative 100k % change
output$plot_today_us_100k_pctchg <- renderPlot( {
  
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cc_pctchg)) + 
    geom_map(aes(map_id = state), map = map_us) + 
    expand_limits(x = map_us$long, y = map_us$lat) + 
    scale_fill_continuous(low = cases.low, high = cases.main, 
                          name = "Cumulative Cases", 
                          label = label_number_si()) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),            
          legend.position = "bottom") 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = deaths.low, high = death.main, 
                            name = "Cumulative Deaths", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom")
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
  
}, width = 500, height = 325 ) # output$plot_today_world
# US trends
output$plot_us_trend_overall <- renderPlot( {
  
  
  # if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cc_pctchg)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = cases.low, high = cases.main, 
                            name = "Cumulative Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom") 
  # } else if(input$us_type == 'deaths') {
  #   ggplot(us.today, aes(fill = cd_pctchg)) + 
  #     geom_map(aes(map_id = state), map = map_us) + 
  #     expand_limits(x = map_us$long, y = map_us$lat) + 
                                          #     scale_fill_continuous(low = deaths.low, high = y, 
  #                           name = "Cumulative Deaths", 
  #                           label = label_number_si()) + 
  #     theme(axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           axis.ticks = element_blank(),            legend.position = "bottom")
  # } else {
  #   ggplot(mtcars) + 
  #     geom_point(aes(x = drat, y = wt)) + 
  #     labs(x = "", y = "") + 
  #     annotate("text", x = 3.75, y = 3.5, 
  #              size = 16, color = "red",
  #              label = "SOMETHING HAS GONE WRONG")
  # }
  
  
}, width = 500, height = 325 ) # output$plot_today_world

## US OVERVIEW VISUALS ##

# something else
output$world_trend_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = cases_per_100k), size = 1, colour = cases.main) + 
    geom_line(aes(x = date, y = deaths_per_100k * 40), size = 0, color = death.main) +
    labs(x = "Cases & Deaths per 100,000 population", y = "Index", 
         caption = "Cases = Blue, Deaths = Red") 
}, width = 400, height = 200 )





################# ###
###  ABOUT PAGE  ####
################# ###

## NOTHING YET :: 2020-10-12 


} # function























