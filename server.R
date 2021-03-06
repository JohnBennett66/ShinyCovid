### SERVER SCRIPT

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
             subtitle = paste(paste0("Total US Deaths :: ", format(tot.d, big.mark = ",") ),
                              paste0("Deaths per million Americans :: ", format(as.integer(tot.d / 327), big.mark = ",") ),
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
    labs(x="",y="",
         title = paste0("Worldwide Cumulative Cases & Deaths as of :: ", 
                        day(rpt.dt)," ", 
                        month(rpt.dt, label = TRUE, abbr = FALSE), " ",
                        year(rpt.dt)))
}, width = 650, height = 400)

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
}, width = 650, height = 400)

output$world.ct <- renderPlot({
  setorder(country.slct,-cum_cases)
  
  ggplot(world.ct.data[country %in% country.slct[1:10,country]]) + 
    geom_line(aes(week, new_cases, colour = country), size = 1) + 
    geom_text_repel(data = world.ct.data[country %in% country.slct[1:10,country] & 
                                      week == world.ct.data[country == "USA", max(week)]], 
                    aes(week, new_cases, label = country)) + 
    scale_y_continuous(name = "Number of New Cases by Week", labels = comma) + 
    scale_colour_viridis_d(option = "C") + 
    theme(panel.background = element_rect(colour = "black", fill = "grey70"),
        legend.position = "right", 
        legend.key = element_rect(fill = "grey65", colour = "grey40")) + 
    labs(title = paste0("Weekly Cases Covid-19 for Top Ten Countries"), 
         caption = "Top Twenty by Cumulative Cases (descending)", 
         y = "Cumulative Cases", 
         x = "2020")
  
}, width = "auto", height = "auto")

output$world.ct2 <- renderPlot({
  setorder(country.slct,-cum_cases)
  temp <- world.ct.data[country %in% country.slct[1:20,country]]
  list1 <- temp[week == max(week)]
  setorder(list1, -cum_cases)
  list1 <- list1[,country]
  temp$country <- factor(temp$country, levels = list1)
  
  
  ggplot(temp) + 
    geom_line(aes(week, new_cases), size = 1) + 
    scale_y_continuous(name = "Number of New Cases by Week", labels = comma) + 
    scale_colour_viridis_d(option = "C") + 
    theme(panel.background = element_rect(colour = "black", fill = "grey90"),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.background = element_rect(fill = "lightslategrey"), 
          strip.text = element_text(face = "bold", colour = "gold", size = 10)) + 
    labs(title = paste0("Weekly Cases Covid-19 for Top Twenty Countries"), 
         caption = "Top Twenty by Cumulative Cases (descending)", 
         y = "Cumulative Cases", 
         x = "2020") + 
    facet_wrap(.~country, scales = 'free')
  
}, width = "auto", height = "auto")

output$world.dt <- renderPlot({
  setorder(country.slct,-cum_deaths)
  
  ggplot(world.ct.data[country %in% country.slct[1:10,country]]) + 
    geom_line(aes(week, new_deaths, colour = country), size = 1) + 
    geom_text_repel(data = world.ct.data[country %in% country.slct[1:10,country] & 
                                           week == world.ct.data[country == "USA", max(week)]], 
                    aes(week, new_deaths, label = country)) + 
    scale_y_continuous(name = "Number of New Deaths by Week", labels = comma) + 
    scale_colour_viridis_d(option = "C") + 
    theme(panel.background = element_rect(colour = "black", fill = "grey70"),
        legend.position = "right",
        legend.key = element_rect(fill = "grey65", color = "grey40")) + 
    labs(title = paste0("Weekly Deaths Covid-19 for Top Ten Countries"),
         caption = "Top Twenty by Cumulative Deaths (descending)",
         y = "Cumulative Cases",
         x = "2020")
  
}, width = "auto", height = "auto")

output$world.surge <- renderPlot({
  
  ggplot(world.all[country %in% select.cntry[[1]] & situation == "Surging"]) + 
    geom_line(aes(date, new_cases), size = 1) + 
    scale_y_continuous(name = "Number of New Cases by Week", labels = comma) + 
    scale_colour_viridis_d(option = "C") + 
    theme(panel.background = element_rect(colour = "black", fill = "grey90"),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.background = element_rect(fill = "lightslategrey"), 
          strip.text = element_text(face = "bold", colour = "gold", size = 14)) + 
    labs(title = paste0("Weekly Cases Covid-19 for Top Twenty Countries"), 
         caption = "Top Twenty by Cumulative Cases (descending)", 
         y = "Cumulative Cases", 
         x = "2020") + 
    facet_wrap(vars(country), scales = 'free', ncol = 3)
  
}, width = "auto", height = 1000)

output$world.recent <- renderPlot({
  
  ggplot(world.all[country %in% select.cntry[[1]] & situation == "Recent Wave"]) + 
    geom_line(aes(date, new_cases), size = 1) + 
    scale_y_continuous(name = "Number of New Cases by Week", labels = comma) + 
    scale_colour_viridis_d(option = "C") + 
    theme(panel.background = element_rect(colour = "black", fill = "grey90"),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.background = element_rect(fill = "lightslategrey"), 
          strip.text = element_text(face = "bold", colour = "gold", size = 14)) + 
    labs(title = paste0("Weekly Cases Covid-19 for Top Twenty Countries"), 
         caption = "Top Twenty by Cumulative Cases (descending)", 
         y = "Cumulative Cases", 
         x = "2020") + 
    facet_wrap(vars(country), scales = 'free', ncol = 3)
  
}, width = "auto", height = 800)  ##  <<<<<<   ADJUST ROW HEIGHT HERE

output$world.past <- renderPlot({
  
  ggplot(world.all[country %in% select.cntry[[1]] & situation == "Wave Past"]) + 
    geom_line(aes(date, new_cases), size = 1) + 
    scale_y_continuous(name = "Number of New Cases by Week", labels = comma) + 
    scale_colour_viridis_d(option = "C") + 
    theme(panel.background = element_rect(colour = "black", fill = "grey90"),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.background = element_rect(fill = "lightslategrey"), 
          strip.text = element_text(face = "bold", colour = "gold", size = 14)) + 
    labs(title = paste0("Weekly Cases Covid-19 for Top Twenty Countries"), 
         caption = "Top Twenty by Cumulative Cases (descending)", 
         y = "Cumulative Cases", 
         x = "2020") + 
    facet_wrap(vars(country), scales = 'free', ncol = 3)
  
}, width = "auto", height = 1800)


## World data table for inclusion on world stat chart tab
setorder(world.all, country)
max.date <- world.all[country == "USA", max(date)]
world.table <- world.all[date == max.date, 
                         .(country, pop, cum_cases, cum_deaths, 
                           cases_per_mil, deaths_per_mil
                           ) ]
setnames(world.table, 1:6,
         c("Country", "Population (in Millions)", "Total Cases", "Total Deaths", 
           "Cases per Million Pop", "Deaths per Million Pop"),
         skip_absent = FALSE
)
world.table <- world.table[`Deaths per Million Pop` >= 0]
setorder(world.table, -`Deaths per Million Pop`)
output$stats_world <- renderTable(
  world.table[`Population (in Millions)` > 0], na = "—", digits = 2
)

output$other_causes <- renderPlot({
  
  ggplot(other.agg[year == 2020 & state == "United States" & !Causes %in% c("All","Natural")]) +
    geom_col(aes(Causes, Deaths, fill = Infectious)) +
    coord_flip() +
    scale_fill_manual(values = c("grey60","#ffa550", "darkorange1")) +
    scale_y_continuous(labels = comma) + 
    labs(title = paste0("Causes of Death in the US for 2020 (year-to-date)"),
         subtitle = paste(paste0("Total US Deaths (all causes) :: ", 
                                 format(other.agg[year == 2020 & state == "United States" & Causes == "All",Deaths]
                                   , big.mark = ",")),
                          paste0("Total US Deaths (natural causes) :: ", 
                                 format(other.agg[year == 2020 & state == "United States" & Causes == "Natural",Deaths]
                                        , big.mark = ",")),
                          paste0("Percent of Covid-19 among Natural Causes :: ", 
                                 format((other.agg[year == 2020 & state == "United States" & Causes == "Covid-19",Deaths] / 
                                          other.agg[year == 2020 & state == "United States" & Causes == "Natural",Deaths] ) *
                                          100,
                                          big.mark = ",", digits = 2), "%"),
                          paste0("Percent of Covid-19 among Infectious Diseases*:: ", 
                                 format((other.agg[year == 2020 & state == "United States" & Causes == "Covid-19",Deaths] / 
                                          other.agg[year == 2020 & state == "United States" & 
                                                      Causes %in% c("Covid-19 w/ Other","Influenza & Pneumonia","Sepsis"),
                                                    sum(Deaths)]) * 100 ,
                                         big.mark = ",", digits = 3), "%"),
                          sep = "\n"),
         caption = paste("* where Infectious = Yes",
                         "** always in US total descending order; your state may differ",
                         paste0("Last Reporting Date :: ",
                                other[state == "United States", max(mdy(wedate))]),
                         "Data is weekly aggregates, reporting date is last reporting week ending",
                         sep = "\n"),
         y = "Number of Deaths by Cause for 2020",
         x = "Cause of Death :: Descending Order**")
  
}, width = 800, height = 600)




} # function























