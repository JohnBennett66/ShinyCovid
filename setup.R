### SETUP SCRIPT
### SHINY COVID


### LOAD PACKAGES
library(rsconnect)
library(forecast)
library(scales)
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
# transformations - state
us.state[,cases_per_mill := (cum_cases/(pop/1000000))]
us.state[,deaths_per_mill := (cum_deaths/(pop/1000000))]
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









