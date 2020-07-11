### SERVER SCRIPT

### LOAD PACKAGES
library(rsconnect)
library(fpp2)
library(scales)
library(shiny)
library(ggplot2)
library(stringr)
library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(usmap)
library(maps)
library(shinythemes)
###

###  SETUP :: IMPORT :: TRANSFORM  ####
source('setup.R')




###  MAIN FUNCTION  ####
function(input, output) {
  
output$st.lst <- renderText({
  paste(toString(us.state.rise),"\n")
})
 
output$daily <- renderPrint({
  input$daily
})
output$forecast <- renderPrint( { 
  input$forecast
  })

##  US OVERALL DAILY LINE CHART :: CASES OR DEATHS 
md <- as_date(pred.dt[,max(date)])
mid <- as_date(us.date[,min(date)])

output$plot_dus <- renderPlot( {
  
  
  if(input$daily == 'cases' & input$forecast == "yes") ggplot(data = us.date) + 
    geom_col(aes(x = date, y = daily_cases), fill = "darkorange3") +
    geom_line(aes(x = date, y = five_day_cases), colour = 'blue', size = 1) +
    geom_line(aes(x = date, y = twowk_day_cases), colour = 'dodgerblue', size = 1) +
    geom_hline(aes(yintercept = avg.c), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Cases Covid-19 in US by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("Total US Cases :: ", format(tot.c, big.mark = ",") ),
                          paste0("Cases per million Americans :: ", format(as.integer(tot.c / 327), big.mark = ",") ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Five Day Moving Average :: Blue Line",
                          "14 Day Moving Average :: Light Blue Line",
                          sep = "\n"),
         y = "Cumulative Cases",
         x = "2020") + 
    scale_x_date(limits = c(mid,md)) + 
    geom_col(data = pred.dt, aes(x = date, y = pred), fill = "tan") + 
    geom_line(data = pred.dt, aes(x = date, y = u95), colour = "grey80") + 
    geom_line(data = pred.dt, aes(x = date, y = u80), colour = "grey40") + 
    geom_line(data = pred.dt, aes(x = date, y = l95), colour = "grey90") + 
    geom_line(data = pred.dt, aes(x = date, y = l80), colour = "grey40") else
  
  if(input$daily == 'cases' & input$forecast == "no") ggplot(data = us.date) + 
      geom_col(aes(x = date, y = daily_cases), fill = "darkorange3") +
      geom_line(aes(x = date, y = five_day_cases), colour = 'blue', size = 1) +
      geom_line(aes(x = date, y = twowk_day_cases), colour = 'dodgerblue', size = 1) +
      geom_hline(aes(yintercept = avg.c), colour = 'grey1', size = 1) +
      scale_x_date(limits = c(start.date, end.date + 1)) +
      labs(title = paste0("Daily Cases Covid-19 in US by Date — 29 February to current (",end.date,")"),
           subtitle = paste(paste0("Total US Cases :: ", format(tot.c, big.mark = ",") ),
                            paste0("Cases per million Americans :: ", format(as.integer(tot.c / 327), big.mark = ",") ),
                            paste0("Days since 29 February 2020 :: ", d[[1]]),
                            "Five Day Moving Average :: Blue Line",
                            "14 Day Moving Average :: Light Blue Line",
                            sep = "\n"),
           y = "Cumulative Cases",
           x = "2020") else
   
  
      if(input$daily == 'deaths' & input$forecast == 'yes') ggplot(data = us.date) + 
        geom_col(aes(x = date, y = daily_deaths), fill = "darkorange3") +
        geom_line(aes(x = date, y = five_day_deaths), colour = 'blue', size = 1) +
        geom_line(aes(x = date, y = twowk_day_deaths), colour = 'dodgerblue', size = 1) +
        geom_hline(aes(yintercept = avg.d), colour = 'grey1', size = 1) +
        scale_x_date(limits = c(start.date, end.date + 1)) +
        labs(title = paste0("Daily Deaths Covid-19 in US by Date — 29 February to current (",end.date,")"),
             subtitle = paste(paste0("Total US Deaths :: ", format(tot.c, big.mark = ",") ),
                              paste0("Deaths per million Americans :: ", format(as.integer(tot.c / 327), big.mark = ",") ),
                              paste0("Days since 29 February 2020 :: ", d[[1]]),
                              "Five Day Moving Average :: Blue Line",
                              "14 Day Moving Average :: Light Blue Line",
                              sep = "\n"),
             y = "Cumulative Deaths",
             x = "2020") + 
        scale_x_date(limits = c(mid,md)) + 
        geom_col(data = pred.dt.d, aes(x = date, y = pred), fill = "tan") + 
        geom_line(data = pred.dt.d, aes(x = date, y = u95), colour = "grey80") + 
        geom_line(data = pred.dt.d, aes(x = date, y = u80), colour = "grey40") + 
        geom_line(data = pred.dt.d, aes(x = date, y = l95), colour = "grey90") + 
        geom_line(data = pred.dt.d, aes(x = date, y = l80), colour = "grey40") else
      
  if(input$daily == 'deaths' & input$forecast == 'no') ggplot(data = us.date) +
    geom_col(aes(x = date, y = daily_deaths), fill = "darkorange3") +
    geom_line(aes(x = date, y = five_day_deaths), colour = 'blue', size = 1) +
    geom_line(aes(x = date, y = twowk_day_deaths), colour = 'dodgerblue', size = 1) +
    geom_hline(aes(yintercept = avg.d), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Deaths Covid-19 in US by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("Total US Deaths :: ", format(tot.d, big.mark = ",") ),
                          paste0("Deaths per million Americans :: ", format(as.integer(tot.d / 327), big.mark = ",") ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Five Day Moving Average :: Blue Line",
                          "14 Day Moving Average :: Light Blue Line",
                          sep = "\n"),
         y = "Cumulative Deaths",
         x = "2020") else {
           
           ggplot(data = us.date[date > "2020-04-15"]) + 
             geom_col(aes(date, daily_cases))
         }
  
} )
  

##  US MAP CHART :: CUMULATIVE RANKING :: CASES OR DEATHS :: DISPLAYED WITH BELOW
output$plot_st <- renderPlot( {    ## cumulative and scope cases/deaths total/change
  
  if(input$cumulative == "cases")  
    plot_usmap("states", data = us.state[date == max(date)], values = "cum_cases", color = "blue4") +
      scale_fill_continuous(low = "lightblue1", high = "blue4", name = "Total Cases", label = scales::comma) +
      theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) +
      labs(title = "Total Cases by State",
           subtitle = paste0("For ",us.date[,max(date)]) ) 
  else 
    plot_usmap("states", data = us.state[date == max(date)], values = "cum_deaths", color = "orangered") +
      scale_fill_continuous(low = "lightpink", high = "orangered", name = "Total Deaths", label = scales::comma) +
      theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) +
      labs(title = "Total Cases by State",
           subtitle = paste0("For ",us.date[,max(date)]))
  
  
  }
)

##  US MAP CHART :: PREVIOUS WEEK CHANGE RANKING :: DISPLAYED WITH ABOVE 
output$plot_chg <- renderPlot( {    ## cumulative and scope cases/deaths total/change
  
  if(input$cumulative == "cases")
    plot_usmap("states", data = us.state[date == max(date)], values = "pct_chng_lstwk", color = "slateblue4") +
      scale_fill_continuous(low = "lightblue1", high = "blue4", name = "Percent Change", label = scales::comma) +
      theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) +
      labs(title = "Percent Change for each State on Total Cases — Compared to Last Week",
           subtitle = paste0("For ",us.date[,max(date)]) )
  else 
    plot_usmap("states", data = us.state[date == max(date)], values = "pct_deaths_lstwk", color = "firebrick") +
      scale_fill_continuous(low = "lightpink", high = "orangered", name = "Percent Change", label = scales::comma) +
      theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) +
      labs(title = "Percent Change for each State on Total Deaths — Compared to Last Week",
           subtitle = paste0("For ",us.date[,max(date)]))
  
},
width = 'auto',
height = 'auto'
)


##  SINGLE STATE DAILY LINE CHART :: CASES OR DEATHS 
output$plot_dss_cases <- renderPlot( {
  ggplot(data = us.state[state == input$state]) +
    geom_col(aes(x = date, y = daily_cases), fill = "darkorange3") +
    geom_hline(aes(yintercept = us.state[state == input$state, mean(daily_cases)]), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Cases Covid-19 in ",input$state," by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("For ",input$state," ::"),
                          paste0("Total Cases :: ", format(us.state[state == input$state,max(cum_cases)], big.mark = ",") ),
                          paste0("Total Deaths :: ", format(us.state[state == input$state,max(cum_deaths)], big.mark = ",") ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Average :: Grey Line",
                          sep = "\n"),
         y = "Daily Cases",
         x = "2020")
  } ,
width = 'auto',
height = 'auto'

)
    
    
output$plot_dss_death <- renderPlot( {    
    
ggplot(data = us.state[state == input$state]) +
    geom_col(aes(x = date, y = daily_deaths), fill = "darkorange3") +
    geom_hline(aes(yintercept = us.state[state == input$state, mean(daily_deaths)]), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Deaths Covid-19 in ",input$state," by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("For ",input$state," ::"),
                          paste0("Total Cases :: ", format(us.state[state == input$state,max(cum_cases)], big.mark = ",") ),
                          paste0("Total Deaths :: ", format(us.state[state == input$state,max(cum_deaths)], big.mark = ",") ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Average :: Grey Line",
                          sep = "\n"),
         y = "Daily Deaths",
         x = "2020")
} ,
width = 'auto',
height = 'auto'
)

##  COUNTY DATA MAPPED
output$plot_county_cases <- renderPlot( {
  
  plot_usmap("counties", include = input$counties, data = counties, 
             values = "cum_cases", color = "slateblue4") +
    scale_fill_continuous(low = "lightblue", high = "blue4", 
                          name = "Cumulative Cases", label = scales::comma) +
    theme(panel.background = element_rect(colour = "black", fill = "lightyellow"),
          legend.position = 'right', 
          legend.background = element_rect(colour = "black", fill = "lightyellow")) +
    labs(title = "Cumulative Cases by County",
         subtitle = paste0("For ",us.date[,max(date)]) )  
  
} )

output$plot_county_deaths <- renderPlot( {
  
  plot_usmap("counties", include = input$counties, data = counties, 
             values = "cum_deaths", color = "darkorange3") +
    scale_fill_continuous(low = "white", high = "firebrick3", name = "Cumulative Deaths", label = scales::comma) +
    theme(panel.background = element_rect(colour = "black", fill = "lightyellow"),
          legend.background = element_rect(colour = "black", fill = "lightyellow"),
          legend.position = 'right') +
    labs(title = "Cumulative Deaths by County",
         subtitle = paste0("For ",us.date[,max(date)]) )  
  
} )

## State data table for inclusion on daily chart tab
state.table <- us.state[date == max(date), .(state, 
                                             comma(cum_cases), comma(as.integer(cases_per_mill)), 
                                             unit_format(sep = "", unit = "%", accuracy = .1)(pct_chng_lstwk), 
                                             comma(cum_deaths), comma(as.integer(deaths_per_mill)), 
                                             unit_format(sep = "", unit = "%", accuracy = .1)(pct_deaths_lstwk))]
setnames(state.table, 1:7,
         c("State", "Total Cases", "Cases/1M", "Percent Change", "Total Deaths", "Deaths/1M", "Percent Change"),
         skip_absent = FALSE
)
output$table_states <- renderTable(
  state.table
)


## Forcasting plots
forecast.c <- renderPlot({ print(p)
  # autoplot(stf.c) + 
  # geom_forecast(showgap = FALSE, PI = TRUE) + 
  # labs(title = "14 Day Forecast for Daily Cases in the US", 
  #      x = "Days Since First US Case (22 Jan 2020)",
  #      y = "Number of Cases each Day",
  #      caption = paste("These charts start with the first US case :: 22 Jan 2020",
  #                      "Most charts start with 29 Feb 2020 when cases began advancing more rapidly",
  #                      sep = "\n"))
},
height = "auto",
width = "auto"
)

forecast.d <- renderPlot({
  autoplot(stf.d) + 
    geom_forecast(showgap = FALSE, PI = TRUE) + 
    labs(title = "14 Day Forecast for Daily Death in the US", 
         x = "Days Since First US Case (22 Jan 2020)",
         y = "Number of Deaths each Day",
         caption = paste("These charts start with the first US case :: 22 Jan 2020",
                         "Most charts start with 29 Feb 2020 when cases began advancing more rapidly",
                         sep = "\n"))
})


Ms <- function(x){ number_format(accuracy = 1,
                scale = 1/1000000,
                suffix = "M")(x) }
## World Maps
output$world.c <- renderPlot({
  ggplot(world.tb[date == max(date)], aes(fill = cum_cases/1000000)) + 
    geom_map(aes(map_id = country), map = map) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "lightblue", high = "blue3", 
                          name = paste("Cumulative Cases",
                                       "in millions",
                                       sep = "\n"), 
                                       label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") + 
    labs(x="",y="")
}, width = 400, height = 325)

output$world.d <- renderPlot({
  ggplot(world.tb[date == max(date)], aes(fill = cum_deaths/1000)) + 
    geom_map(aes(map_id = country), map = map) + 
    expand_limits(x = map$long, y = map$lat) + 
    scale_fill_continuous(low = "pink", high = "red3", 
                          name = paste("Cumulative Deaths",
                                       "in thousands", 
                                       sep = "\n"), 
                          label = scales::comma) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") 
}, width = 400, height = 325)



} # function























