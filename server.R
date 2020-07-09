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

###  SETUP :: IMPORT :: TRANSFORM  ####
source('~/R4FUN/ShinyCovid/setup.R')




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


} # function























