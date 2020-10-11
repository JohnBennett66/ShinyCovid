### SERVER SCRIPT

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
###
options(datatable.optimize=1)

###  SETUP :: IMPORT :: TRANSFORM  ####
source('setup_anlyss.R')

###  SET SOME VARIABLES  ####
reporting.date <- df.tb[country == "USA"][date == max(date)][, unique(date)]
report.date <- as.character(reporting.date)
display.date <- paste(day(reporting.date), 
                      month(reporting.date, label = TRUE, abbr = FALSE),
                      year(reporting.date),
                      sep = " ")

###  MAIN FUNCTION  ####
function(input, output) {
  
###  GENERIC STUFF  ####  
## Current Data Date
output$current_data_date <- renderUI( { 
  p(">>", em("Data current as of :  "), strong(HTML(display.date)), "<<")
} )
  
  
###  THE WORLD  ####
###  WHERE ARE WE
  
##  THE TEXT
# cumulative chart explanation
output$text_today_world <- renderUI( {
  if(input$world_type == 'cases') {
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
  } else if(input$world_type == 'deaths') {
    HTML("As with the Cumulative Cases plot, this can also be misleading.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum text

  


output$text_today_world_100k <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("Now we are looking at the rate of cases or the rate of deaths, 
         which is fairer and more accurate if we want to understand Covid; 
         what is happening?, where is it the worst?, where is it getting better?, 
         etc. In the chart above the US was the worst and only a couple other 
         countries were near the top. In this chart we see that many countries 
         are near the level of the US for &#8220;the number of people who 
         have tested positive for COVID-19 per 100,000 people in that country&#8221;. 
         Now the US has many other countries that are also on the top end of the scale."
         )
  } else if(input$world_type == 'deaths') {
    HTML("Here we can see that the highest per capita death rates are in the Americas and Europe.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum per captia text

output$text_today_world_100k_pctchg <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("In this chart we are looking at the percentage of change between a countries 
                      &#8220;rate of cases per 100,000 people last week&#8221; <br> 
                      and that same rate this week. This shows us where it is getting worse and 
                      where it is staying the same. It will not get <br> 
                      &#8220;better&#8221; because this is cumulative cases so this rate never goes 
                      in a negative direction.
                      <br><br>")
    
  } else if(input$world_type == 'deaths') {
    HTML("And this is the countries with the highest percentage change in their per capita death rate 
         in the last week.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum percent change text

  
##  THE VISUALS
# world rank cumulative
output$plot_today_world <- renderPlot( {
  
  
  if(input$world_type == 'cases') {
    ggplot(world.today, aes(fill = cum_cases)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = label_number_si()) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(), 
          legend.position = "bottom") 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cum_deaths)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
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

# world rank cumulative 100K  
output$plot_today_world_100k <- renderPlot( {
  
  
  if(input$world_type == 'cases') {
    ggplot(world.today, aes(fill = ccper100k)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = label_number_si()) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),            
          legend.position = "bottom") 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
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
# world rank cumulative 100k % change
output$plot_today_world_100k_pctchg <- renderPlot( {
  
  
  if(input$world_type == 'cases') {
    ggplot(world.today, aes(fill = cc_pctchg)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map_w$long, y = map_w$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = label_percent()) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),            
          legend.position = "bottom") 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = label_percent()) + 
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

## WORLD OVERVIEW VISUALS ##
## TRENDS
# cum cases daily
output$world_growth_cases_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_cases), colour = 'blue') + 
    labs(y = "Daily Cases", x = "") + 
    scale_y_continuous(label = comma)
}, width = 400, height = 200 )
output$world_growth_deaths_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_deaths), color = 'red') + 
    labs(y = "Daily Deaths", x = "") + 
    scale_y_continuous(label = comma)
}, width = 400, height = 200 )
output$world_trend_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = cases_per_100k), size = 1, colour = 'blue') + 
    geom_line(aes(x = date, y = deaths_per_100k * 40), size = 0, color = 'red') +
    labs(x = "Cases & Deaths per 100,000 population", y = "Index", 
         caption = "Cases = Blue, Deaths = Red") 
}, width = 400, height = 200 )




###  THE UNITED STATES  ####
###  WHERE ARE WE

##  THE TEXT
# cumulative chart explanation
output$text_today_world <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("This chart can be misleading because it treats each country the same. 
          We would expect countries with more people, i.e., a larger population, 
          to have more cases or deaths than countries with fewer people.  
          To be fairer in our analysis and ranking, we should compare a &lsquo;rate&lsquo;, 
          which is a comparison of equal values. In this case that means how many cases per 
          some number of population. Most countries have millions of people so, using a group 
          of 100,000 people for our rate is a good idea. It could be groups of 10,000 
          or 1,000,000 or any number. I think 100,000 works well because it makes the 
          numbers a good size. If you pick <br>1,000,000, then some of the small countries 
          have 0.01 cases per Millon, or if you pick 10,000, then some of the large countries 
          have 1,000 cases or more. It is an arbitray choice, I just like 100,000. 
          For these charts it does not make too much difference because they have no numbers. 
          These charts use shading to show which country has <br> &#8220;the most&#8221;, 
          where darker shading means more cases."
    )
  } else if(input$world_type == 'deaths') {
    HTML("As with the Cumulative Cases plot, this can also be misleading.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum text

###  THE OVERVIEW PANEL :: US  ####
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
         We should expect the rate of cases to be fairly constant if an area has good
         systems of control in place. There will always be some cases. <br>
         <br>
         * &ldqou;per capita&rdqou; means the same as &ldqou;per 100,000 people&rdqou;"
    )
  } else if(input$us_type == 'deaths') { 
      HTML("These are the number of deaths per 100,000 people in each state. 
           The darker states have more deaths (per 100,000 people) than the 
           lighter states. More deaths (per capita*) means one or more of these 
           <ul>
           <li>less control of the spread of the virus</li>
           </ul> 
           next")
  }
} )

output$text_today_world_100k <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("Now we are looking at the rate of cases or the rate of deaths, 
         which is fairer and more accurate if we want to understand Covid; 
         what is happening?, where is it the worst?, where is it getting better?, etc. 
         In the chart above the US was the worst and only a couple other 
         countries were near the top. In this chart we see that many countries 
         are near the level of the US for &#8220;the number of people who have 
         tested positive for COVID-19 per 100,000 people in that country&#8221;. 
         Now the US has many other countries that are also on the top end of the scale."
         )
  } else if(input$world_type == 'deaths') {
    HTML("Here we can see that the highest per capita death rates are in the Americas and Europe.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum per captia text

output$text_today_world_100k_pctchg <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("In this chart we are looking at the percentage of change between a 
          countries &#8220;rate of cases per 100,000 people last week&#8221; 
          and that same rate this week. This shows us where it is getting worse 
          and where it is staying the same. It will not get &#8220;better&#8221; 
          because this is cumulative cases so this rate never goes in a negative direction.
          <br><br>"
         )
    
  } else if(input$world_type == 'deaths') {
    HTML("And this is the countries with the highest percentage change in 
          their per capita death rate in the last week."
         )
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum percent change text


##  THE VISUALS :: US  ####
##  THE OVERVIEW PANEL :: US  ####
output$plot_overview_us_100k <- renderPlot( {
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = ccper100k)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightblue", high = "blue3", 
                            name = "Cumulative Cases", 
                            label = label_number_si()) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),            
            legend.position = "bottom") 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
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

# ranking table
output$us_ranking <- renderTable( { 
  if(input$us_rank == 'ccper100k')  { 
    setorder(us.table, -ccper100k)
    us.table[ , .(State, `Cases /100k`, `Deaths /100k`, 
                  `Cases Change`, `Deaths Change`)]
  } else if(input$us_rank == 'cdper100k')  { 
    setorder(us.table, -cdper100k)
    us.table[ , .(State, `Cases /100k`, `Deaths /100k`, 
                  `Cases Change`, `Deaths Change`)]
  } else if(input$us_rank == 'cc_pctchg')  { 
    setorder(us.table, -cc_pctchg)
    us.table[ , .(State, `Cases Change`, `Deaths Change`, 
                  `Cases /100k`, `Deaths /100k`)]
  } else if(input$us_rank == 'cd_pctchg')  { 
    setorder(us.table, -cd_pctchg)
    us.table[ , .(State, `Deaths Change`, `Cases Change`, 
                  `Deaths /100k`, `Cases /100k`)]
  }
} , striped = TRUE, bordered = TRUE, rownames = TRUE
)
# ranking plot
plot.back <- rgb(248, 245, 240, maxColorValue = 255)
plot.border <- rgb(223, 215, 202, maxColorValue = 255)
output$plot_today_us_rank <- renderPlot( { 
  if(input$us_rank == 'ccper100k')  { 
    ggplot(us.table, aes(fill = ccper100k)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightblue", high = "blue3", 
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
      scale_fill_continuous(low = "white", high = "orangered", 
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
      scale_fill_continuous(low = "lightblue", high = "blue3", 
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
  } else if(input$us_rank == 'cd_pctchg')  { 
    ggplot(us.table, aes(fill = cd_prtchg)) + 
      geom_map(aes(map_id = State), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "pink", high = "orangered", 
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
  }
  
}, width = 410, height = 270  
)
# stats rank cumulative
output$plot_today_us <- renderPlot( {
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cum_cases)) + 
    geom_map(aes(map_id = state), map = map_us) + 
    expand_limits(x = map_us$long, y = map_us$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
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
      scale_fill_continuous(low = "lightpink", high = "orangered", 
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
    scale_fill_continuous(low = "lightblue", high = "blue3", 
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
      scale_fill_continuous(low = "lightpink", high = "orangered", 
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
      scale_fill_continuous(low = "lightblue", high = "blue3", 
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
  #     scale_fill_continuous(low = "lightpink", high = "orangered", 
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
## TRENDS
# cum cases daily
output$us_growth_cases_daily <- renderPlot( { 
  ggplot(data = us.allup[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_cases), colour = 'blue') + 
    labs(y = "Daily Cases", x = "") + 
    scale_y_continuous(label = comma)
}, width = 400, height = 200 )
# cum deaths daily
output$us_growth_deaths_daily <- renderPlot( { 
  ggplot(data = us.allup[date <= reporting.date]) + 
    geom_line(aes(x = date, y = new_deaths), color = 'red') + 
    labs(y = "Daily Deaths", x = "") + 
    scale_y_continuous(label = comma)
}, width = 400, height = 200 )
# something else
output$world_trend_daily <- renderPlot( { 
  ggplot(data = world.summary[date <= reporting.date]) + 
    geom_line(aes(x = date, y = cases_per_100k), size = 1, colour = 'blue') + 
    geom_line(aes(x = date, y = deaths_per_100k * 40), size = 0, color = 'red') +
    labs(x = "Cases & Deaths per 100,000 population", y = "Index", 
         caption = "Cases = Blue, Deaths = Red") 
}, width = 400, height = 200 )




} # function























