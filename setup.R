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

### READ OTHER DATASETS  ####
pred.stl <- fread(file = "predstl.csv")
pred.stl[,date := as_date(date)]
pred.stl.d <- fread(file = "predstld.csv")
pred.stl.d[,date := as_date(date)]


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

### COUNTY DATA  ####
us.tb <- df.tb[iso2 == "US", .(state, county, fips, date, cum_cases, cum_deaths, new_deaths, new_cases)]
counties <- us.tb[date == max(date)]
counties[state == "New York" & county == "New York City", fips := 36061]

### WORLDS DATA  ####
world.tb <- df.tb[,.(date,continent,country,new_cases,new_deaths,cum_cases,cum_deaths)]
world.tb[,lapply(.SD,sum),.SDcols=c("new_cases","new_deaths","cum_cases","cum_deaths"), by = .(country,continent,date)]
setkey(world.tb,date)
 


# CREATE FORECASTS :: MULTIPLE MODELS
# daily cases data.table :: US
dc.dt <- us.date[,.(date, daily_cases)]
dd.dt <- us.date[,.(date, daily_deaths)]
# start and end dates and count of days
start.dt <- dc.dt[date == min(date), date]
end.dt <- dc.dt[date == max(date), date]
day.cnt <- end.dt - start.dt
# create time series objects :: Cases and Deaths
us.d.ts.c <- ts(dc.dt[,daily_cases], start = 1, frequency = 7)
us.d.ts.d <- ts(dd.dt[,daily_deaths], start = 1, frequency = 7)
# model/forecast the data :: STL method
stf.c <- stlf(us.d.ts.c)
stf.d <- stlf(us.d.ts.d)
# variables needed
pred.dt.cnt <- abs((pred.stl[,max(date)] %--% pred.stl[,min(date)]) / ddays(x=1)) + 1
# add new predictions to table
# 
# FIX WITH THIS ?? ####
# Instead of IF statement, just build the new table 
# then select dates newer than what exists in the .csv file
# now rbind those new dates to the original and then write the file
# 
# if(pred.stl[,max(date)] < (Sys.Date() + 13)) {
#   add.one <- data.table((Sys.Date()+13), as.integer(stf.c$mean[14]), 
#                         as.integer(stf.c$upper[14]), as.integer(stf.c$upper[28]), 
#                         as.integer(stf.c$lower[14]), as.integer(stf.c$lower[28]))
#   setnames(add.one,c("V1", "V2", "V3", "V4", "V5", "V6"), 
#            c("date", "pred_stl", "stl_u80", "stl_u95", "stl_l80", "stl_l95"))
# } else {
#   add.one <- data.table(NULL,NULL,NULL,NULL,NULL,NULL)
# }
# pred.stl <- rbind(pred.stl,add.one)
# fwrite(pred.stl, file = "predstl.csv")
# 
# if(pred.stl.d[,max(date)] < (Sys.Date() + 13)) {
#   add.one.d <- data.table((Sys.Date()+13), as.integer(stf.d$mean[14]), 
#                           as.integer(stf.d$upper[14]), as.integer(stf.d$upper[28]), 
#                           as.integer(stf.d$lower[14]), as.integer(stf.d$lower[28]))
#   setnames(add.one.d,c("V1", "V2", "V3", "V4", "V5", "V6"), 
#            c("date", "pred", "u80", "u95", "l80", "l95"))
# } else {
#   add.one <- data.table(NULL,NULL,NULL,NULL,NULL,NULL)
# }
# pred.stl.d <- rbind(pred.stl.d,add.one)
# fwrite(pred.stl.d, file = "predstld.csv")


# a prediction table for today
# cases
pred.dt <- data.table(as_date(Sys.Date()):as_date(Sys.Date()+13), 
                      as.integer(stf.c$mean), 
                      as.integer(stf.c$upper[1:14]), 
                      as.integer(stf.c$upper[15:28]), 
                      as.integer(stf.c$lower[1:14]), 
                      as.integer(stf.c$lower[15:28]) )
setnames(pred.dt, 
         c('V1','V2','V3','V4','V5','V6'), 
         c("date", "pred", "u80", "u95", "l80", "l95"))
pred.dt[,date := as_date(date)]
setkey(pred.dt,date)
# deaths
pred.dt.d <- data.table(as_date(Sys.Date()):as_date(Sys.Date()+13), 
                      as.integer(stf.d$mean), 
                      as.integer(stf.d$upper[1:14]), 
                      as.integer(stf.d$upper[15:28]), 
                      as.integer(stf.d$lower[1:14]), 
                      as.integer(stf.d$lower[15:28]) )
setnames(pred.dt.d, 
         c('V1','V2','V3','V4','V5','V6'), 
         c("date", "pred", "u80", "u95", "l80", "l95"))
pred.dt.d[,date := as_date(date)]
setkey(pred.dt.d,date)
pred.dt.d[l80 < 0, l80 := 0]
pred.dt.d[l95 < 0, l95 := 0]



### REMOVE  ####
remove(df.tb)
