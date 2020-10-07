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


###  MAIN FUNCTION  ####
function(input, output) {
  
###  THE WORLD  ####
###  WHERE ARE WE
  
##  THE TEXT
# cumulative chart explanation
output$text_today_world <- renderUI( {
  if(input$world_type == 'cases') {
  HTML("This chart can be misleading because it treats each country the same. We would expect countries with more people, i.e., a larger population, <br>
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
                      darker shading means more cases.")
  } else if(input$world_type == 'deaths') {
    HTML("As with the Cumulative Cases plot, this can also be misleading.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum text

  


output$text_today_world_100k <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("Now we are looking at the rate of cases or the rate of deaths, which is fairer 
                      and more accurate if we want to understand Covid; <br> 
                      what is happening?, where is it the worst?, where is it getting better?, etc. 
                      In the chart above the US was the worst and only a <br> 
                      couple other countries were near the top. In this chart we see that many countries 
                      are near the level of the US for &#8220;the number <br> 
                      of people who have tested positive for COVID-19 per 100,000 people in that country&#8221;. 
                      Now the US has many other countries that are also on <br> 
                      the top end of the scale.")
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
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cum_deaths)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank()) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
    
}, width = 850, height = 400 ) # output$plot_today_world

# world rank cumulative 100K  
output$plot_today_world_100k <- renderPlot( {
  
  
  if(input$world_type == 'cases') {
    ggplot(world.today, aes(fill = ccper100k)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank()) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 850, height = 400 ) # output$plot_today_world
# world rank cumulative 100k % change
output$plot_today_world_100k_pctchg <- renderPlot( {
  
  
  if(input$world_type == 'cases') {
    ggplot(world.today, aes(fill = cc_pctchg)) + 
    geom_map(aes(map_id = country), map = map_w) + 
    expand_limits(x = map_w$long, y = map_w$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  } else if(input$world_type == 'deaths') {
    ggplot(world.today, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = country), map = map_w) + 
      expand_limits(x = map$long, y = map$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank()) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 850, height = 400 ) # output$plot_today_world



###  THE UNITED STATES  ####
###  WHERE ARE WE

##  THE TEXT
# cumulative chart explanation
output$text_today_world <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("This chart can be misleading because it treats each country the same. We would expect countries with more people, i.e., a larger population, <br>
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
                      darker shading means more cases.")
  } else if(input$world_type == 'deaths') {
    HTML("As with the Cumulative Cases plot, this can also be misleading.")
  } else {
    HTML("THIS IS THE ERROR CASE :-(")
  }
} ) # world cum text




output$text_today_world_100k <- renderUI( {
  if(input$world_type == 'cases') {
    HTML("Now we are looking at the rate of cases or the rate of deaths, which is fairer 
                      and more accurate if we want to understand Covid; <br> 
                      what is happening?, where is it the worst?, where is it getting better?, etc. 
                      In the chart above the US was the worst and only a <br> 
                      couple other countries were near the top. In this chart we see that many countries 
                      are near the level of the US for &#8220;the number <br> 
                      of people who have tested positive for COVID-19 per 100,000 people in that country&#8221;. 
                      Now the US has many other countries that are also on <br> 
                      the top end of the scale.")
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
# stats rank cumulative
output$plot_today_us <- renderPlot( {
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cum_cases)) + 
    geom_map(aes(map_id = state), map = map_us) + 
    expand_limits(x = map_us$long, y = map_us$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cum_deaths)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank()) 
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
}, width = 850, height = 400 ) # output$plot_today_world

# states rank cumulative 100K  
output$plot_today_us_100k <- renderPlot( {
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = ccper100k)) + 
    geom_map(aes(map_id = state), map = map_us) + 
    expand_limits(x = map_us$long, y = map_us$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cdper100k)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  } 
  
}, width = 850, height = 400 ) # output$plot_today_world
# states rank cumulative 100k % change
output$plot_today_us_100k_pctchg <- renderPlot( {
  
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cc_pctchg)) + 
    geom_map(aes(map_id = state), map = map_us) + 
    expand_limits(x = map_us$long, y = map_us$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = "Cumulative Cases", 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
  
}, width = 850, height = 400 ) # output$plot_today_world
# US trends
output$plot_us_trend_overall <- renderPlot( {
  
  
  if(input$us_type == 'cases') {
    ggplot(us.today, aes(fill = cc_pctchg)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightblue", high = "blue3", 
                            name = "Cumulative Cases", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank()) 
  } else if(input$us_type == 'deaths') {
    ggplot(us.today, aes(fill = cd_pctchg)) + 
      geom_map(aes(map_id = state), map = map_us) + 
      expand_limits(x = map_us$long, y = map_us$lat) + 
      scale_fill_continuous(low = "lightpink", high = "orangered", 
                            name = "Cumulative Deaths", 
                            label = scales::comma) + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  } else {
    ggplot(mtcars) + 
      geom_point(aes(x = drat, y = wt)) + 
      labs(x = "", y = "") + 
      annotate("text", x = 3.75, y = 3.5, 
               size = 16, color = "red",
               label = "SOMETHING HAS GONE WRONG")
  }
  
  
}, width = 850, height = 400 ) # output$plot_today_world

} # function























