### SERVER SCRIPT

### LOAD PACKAGES
library(rsconnect)
library(shiny)
library(ggplot2)
library(stringr)
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
df.tb <- fread("https://query.data.world/s/33aafdu2yb5fx4arlb66xhdawgkav3", 
                  check.names=TRUE, stringsAsFactors=FALSE);
colnames(df.tb) <- c("cum_cases", "county", "date", "state", "continent", "source", 
                     "new_deaths", "fips", "iso3", "country", "iso2", "new_cases", 
                     "cum_deaths")

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
# moving averages - states
# basic :: 5 day
us.state[ , five_day_cases := frollmean(us.state[,daily_cases], 5) ]
us.state[ , five_day_deaths := frollmean(us.state[,daily_deaths], 5) ]
# extended :: 14 day
us.state[ , twowk_day_cases := frollmean(us.state[,daily_cases], 14) ]
us.state[ , twowk_day_deaths := frollmean(us.state[,daily_deaths], 14) ]
# NA = 0
setnafill(us.state, fill = 0, cols = 8:15)
setnafill(us.date, fill = 0, cols = 13:16)
# variables
avg.c <- mean(us.date[,daily_cases]) 
avg.d <- mean(us.date[,daily_deaths])
tot.c <- max(us.date[,cum_cases])
tot.d <- max(us.date[,cum_deaths])
# selections
us.state.chng <- us.state[date == max(date), .(state ,pct_chng_lstwk)]
setorder(us.state.chng, -pct_chng_lstwk)
mn <- us.state.chng[,mean(pct_chng_lstwk)]
sd <- sd(us.state.chng[,pct_chng_lstwk])
sd.1 <- mn + sd
sd.2 <- mn + (sd*2)
max <- us.state.chng[,max(pct_chng_lstwk)]
cnt.mn <- count(us.state.chng[pct_chng_lstwk >= mn])
cnt.1 <- count(us.state.chng[pct_chng_lstwk >= sd.1])
ifelse(cnt.mn <= 15, us.state.rise <- us.state.chng[pct_chng_lstwk >= mn, state],  
  ifelse(cnt.1 <= 15, us.state.rise <- us.state.chng[pct_chng_lstwk >= sd.1, state],
         us.state.rise <- us.state.chng[pct_chng_lstwk >= sd.2, state]
  )
)
# counties data
us.tb <- df.tb[iso2 == "US", .(state, county, fips, date, cum_cases, cum_deaths, new_deaths, new_cases)]
counties <- us.tb[date == max(date)]
counties[state == "New York" & county == "New York City", fips := 36061]
remove(df.tb)

###  MAIN FUNCTION  ####
function(input, output) {
  
output$st.lst <- renderText({
  paste(toString(us.state.rise),"\n")
})
 
output$test <- renderText( { 
  paste(toString(us.state[state == "Washington", input$daily]))
  })

##  US OVERALL DAILY LINE CHART :: CASES OR DEATHS 
output$plot_dus <- renderPlot( {
  
  if(input$daily == 'cases') ggplot(data = us.date) + 
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
         y = "Cumulative Deaths",
         x = "2020") 
  else {ggplot(data = us.date) +
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
         x = "2020")}
  
} )
  

##  US MAP CHART :: CUMULATIVE RANKING :: CASESE OR DEATHS :: DISPLAYED WITH BELOW
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


output$plot_county_cases <- renderPlot( {
  
  plot_usmap("counties", data = counties, values = "cum_cases", color = "slateblue4") +
    scale_fill_continuous(low = "deepskyblue2", high = "red2", name = "Cumulative Cases", label = scales::comma) +
    theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) +
    labs(title = "Percent Change for each State on Total Cases — Compared to Last Week",
         subtitle = paste0("For ",us.date[,max(date)]) )  
  
} )

output$plot_county_deaths <- renderPlot( {
  
  plot_usmap("counties", include = "Utah", data = counties, values = "cum_deaths", color = "slateblue4") +
    scale_fill_continuous(low = "#DDAAAA", high = "firebrick3", name = "Cumulative Deaths", label = scales::comma) +
    theme(panel.background = element_rect(colour = "black", fill = "lightyellow")) +
    labs(title = "Percent Change for each State on Total Cases — Compared to Last Week",
         subtitle = paste0("For ",us.date[,max(date)]) )  
  
} )




} # function























