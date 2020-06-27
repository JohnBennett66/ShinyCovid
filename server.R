### SERVER SCRIPT

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

###  CAPTURE AND TRANSFORM DATA :: JH DATASET  ####
# data.world covid-19 datasource
us.state <- fread("https://query.data.world/s/cgpumxcw4ajvqt6334dwjhay6uhact",
                 check.names = TRUE,
                 drop = c("cumulative_cases_per_100_000",  "cumulative_deaths_per_100_000",
                          "new_cases_7_day_rolling_avg",  "new_deaths_7_day_rolling_avg",
                          "new_deaths_per_100_000", "new_cases_per_100_000"))
### DATA TRANSFORMATIONS  ####
# friendly names
colnames(us.state) <- c("state", "date", "pop",
                       "cum_cases", "cum_deaths",
                       "daily_cases", "daily_deaths")

# roll up the states for US daily totals
us.date <- us.state[
  ,.(pop = sum(pop),
     cum_cases = sum(cum_cases), cum_deaths = sum(cum_deaths),
     daily_cases = sum(daily_cases), daily_deaths = sum(daily_deaths) )
  , by = .(date)
  ]
# NA = 0
setnafill(us.state, fill = 0, cols = 6:7)
setnafill(us.date, fill = 0, cols = 5:6)
# vars
start.date = ymd(20200229)
end.date = ymd(us.date[,max(date)])
d <- end.date - start.date
# adjust data types
us.state[, date := ymd(date)]
us.date[, date := ymd(date)]
# transformations
us.date[,cases_per_capita := (cum_cases/pop)]
us.date[,deaths_per_capita := (cum_deaths/pop)]
us.date[,cases_per_mill := (cum_cases/(pop/1000000))]
us.date[,deaths_per_mill := (cum_deaths/(pop/1000000))]
us.date[,cases_per_10mill := (cum_cases/(pop/10000000))]
us.date[,deaths_per_10mill := (cum_deaths/(pop/10000000))]
# moving averages
# basic :: 5 day
us.date[ , five_day_cases := frollmean(us.date[,daily_cases], 5) ]
us.date[ , five_day_deaths := frollmean(us.date[,daily_deaths], 5) ]
# extended :: 14 day
us.date[ , twowk_day_cases := frollmean(us.date[,daily_cases], 14) ]
us.date[ , twowk_day_deaths := frollmean(us.date[,daily_deaths], 14) ]
# week over week change
us.state[,cum_cases_lstwk := shift(cum_cases,7), by = state]
us.state[,pct_chng_lstwk := ((cum_cases - cum_cases_lstwk)/cum_cases_lstwk)*100]
us.state[,cum_deaths_lstwk := shift(cum_deaths,7), by = state]
us.state[,pct_deaths_lstwk := ((cum_deaths - cum_deaths_lstwk)/cum_deaths_lstwk)*100]
# NA = 0
setnafill(us.state, fill = 0, cols = 8:11)
setnafill(us.date, fill = 0, cols = 13:16)
# variables
avg.c <- mean(us.date[,daily_cases]) 
avg.d <- mean(us.date[,daily_deaths])
tot.c <- max(us.date[,cum_cases])
tot.d <- max(us.date[,cum_deaths])




###  MAIN FUNCTION  ####
function(input, output) {
  
 # output$plot <- renderPlot( {
 #   ggplot(us.date) + 
 #     geom_col(aes(date,input$type))
 #   
 # } )
 
output$plot_dus <- renderPlot( {
  
  if(input$daily == 'cases') ggplot(data = us.date) + 
      geom_col(aes(x = date, y = daily_cases), fill = "darkorange3") +
    geom_line(aes(x = date, y = five_day_cases), colour = 'blue', size = 1) +
    geom_hline(aes(yintercept = avg.c), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Cases Covid-19 in US by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("Total US Cases :: ", format(tot.c, big.mark = ",") ),
                          paste0("Cases per million Americans :: ", format(as.integer(tot.c / 327), big.mark = ",") ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Five Day Moving Average :: Blue Line",
                          sep = "\n"),
         y = "Cumulative Deaths",
         x = "2020") 
  else {ggplot(data = us.date) +
    geom_col(aes(x = date, y = daily_deaths), fill = "darkorange3") +
    geom_line(aes(x = date, y = five_day_deaths), colour = 'blue', size = 1) +
    geom_hline(aes(yintercept = avg.d), colour = 'grey1', size = 1) +
    scale_x_date(limits = c(start.date, end.date + 1)) +
    labs(title = paste0("Daily Deaths Covid-19 in US by Date — 29 February to current (",end.date,")"),
         subtitle = paste(paste0("Total US Deaths :: ", format(tot.d, big.mark = ",") ),
                          paste0("Deaths per million Americans :: ", format(as.integer(tot.d / 327), big.mark = ",") ),
                          paste0("Days since 29 February 2020 :: ", d[[1]]),
                          "Five Day Moving Average :: Blue Line",
                          sep = "\n"),
         y = "Cumulative Deaths",
         x = "2020")}
  
  
  
  
} )
  
output$plot_st <- renderPlot( {    ## cumulative and scope
  
  plot_usmap("states", data = us.state[date == max(date)], values = "cum_cases", color = "blue4") + 
    scale_fill_continuous(low = "lightblue1", high = "blue4", name = "Total Cases", label = scales::comma) + 
    theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) + 
    labs(title = "Total Cases by State",
         subtitle = paste0("For ",us.date[,max(date)])
    )
  
  })

}
