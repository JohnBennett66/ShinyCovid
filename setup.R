### SETUP SCRIPT
### SHINY COVID


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
library(RSocrata)
###
options(datatable.optimize=1)




###  CAPTURE AND TRANSFORM DATA :: JH DATASET  ####
# data.world covid-19 datasource
us.state <- fread("https://query.data.world/s/cgpumxcw4ajvqt6334dwjhay6uhact",
                  check.names = TRUE,
                  drop = c("cumulative_cases_per_100_000",  "cumulative_deaths_per_100_000",
                           "new_cases_7_day_rolling_avg",  "new_deaths_7_day_rolling_avg",
                           "new_deaths_per_100_000", "new_cases_per_100_000"))

df.tb <- fread("https://query.data.world/s/33aafdu2yb5fx4arlb66xhdawgkav3", 
               check.names=TRUE, stringsAsFactors=FALSE);
setnames(df.tb, 
         c("PEOPLE_POSITIVE_CASES_COUNT", "COUNTY_NAME", "REPORT_DATE", "PROVINCE_STATE_NAME", "CONTINENT_NAME", 
           "DATA_SOURCE_NAME", "PEOPLE_DEATH_NEW_COUNT", "COUNTY_FIPS_NUMBER", "COUNTRY_ALPHA_3_CODE", 
           "COUNTRY_SHORT_NAME", "COUNTRY_ALPHA_2_CODE", "PEOPLE_POSITIVE_NEW_CASES_COUNT", "PEOPLE_DEATH_COUNT" ),
         c("cum_cases", "county", "date", "state", "continent", 
           "source", "new_deaths", "fips", "iso3", 
           "country", "iso2", "new_cases", "cum_deaths")
        )
sources <- df.tb[,unique(source)]

other <- read.socrata("https://data.cdc.gov/resource/muzy-jte6.csv", 
                      app_token = 'i8PjzM1xsQpCkMN4DZvWngIj5', stringsAsFactors = TRUE)
colnames(other) <- c("state", "year", "weeknum", "wedate", "all_causes", "natural_cause",
                     "septicemia", "malig_neoplasm", "diabetes", "alzheimer",
                     "influenza_pneumonia", "cl_respitory", "other_respitory",
                     "nephritis", "abnormal_unknown", "heart_disease", "cerebrovascular",
                     "covid-19_multiple", "covid-19_underlying",
                     "fl_all", "fl_nat", "fl_sept", "fl_neo", "fl_diab", "fl_alz", "fl_infl",
                     "fl_clrd", "fl_othresp", "fl_nephr", "fl_othrunk", "fl_hd", "fl_strk",
                     "fl_cov19mult", "fl_cov19undly")
setDT(other)

### READ OTHER DATASETS  ####
pred.stl <- fread(file = "predstl.csv")
pred.stl[,date := as_date(date)]
pred.stl.d <- fread(file = "predstld.csv")
pred.stl.d[,date := as_date(date)]
p2020 <- fread(file = "p2020.csv")
p2020[,pop := as.integer(pop/1000)]
jurs.lst <- fread(file = "jurslst.csv")

### DATA TRANSFORMATIONS  ####
# friendly names
colnames(us.state) <- c("state", "date", "pop",
                        "cum_cases", "cum_deaths",
                        "daily_cases", "daily_deaths")

# roll up the states for US daily totals
us.date <- us.state[
  ,.(pop = base::sum(pop),
     cum_cases = base::sum(cum_cases), cum_deaths = base::sum(cum_deaths),
     daily_cases = base::sum(daily_cases), daily_deaths = base::sum(daily_deaths) )
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
# STATE LIST
states.lst <- us.state[,state]
states.lst <- unique(states.lst)
## WORLD
df.tb[country == "United States", country := "USA"]
df.tb[country == "Antigua and Barbuda", country := "Antigua"]
df.tb[country == "Bonaire, Saint Eustatius and Saba", country := "Bonaire"]
df.tb[country == "Brunei and Darussalam", country := "Brunei"]
df.tb[country == "CuraÃ§ao", country := "Curacao"]
df.tb[country == "Czechia", country := "Czech Republic"]
df.tb[country == "Falkland Islands (Malvinas)", country := "Falkland Islands"]
df.tb[country == "Guinea Bissau", country := "Guinea-Bissau"]
df.tb[country == "Cote dIvoire", country := "Ivory Coast"]
df.tb[country == "Congo", country := "Republic of Congo"]
df.tb[country == "Saint Kitts and Nevis", country := "Saint Kitts"]
df.tb[country == "Saint Vincent and the Grenadines", country := "Saint Vincent"]
df.tb[country == "Timor Leste", country := "Timor-Leste"]
df.tb[country == "Trinidad and Tobago", country := "Trinidad"]
df.tb[country == "United Republic of Tanzania", country := "Tanzania"]
df.tb[country == "United Kingdom", country := "UK"]
world.tb <- df.tb[,.(date,continent,country,new_cases,new_deaths,cum_cases,cum_deaths)]
world.all <- df.tb[,.(date,continent,country,new_cases,new_deaths,cum_cases,cum_deaths)]
world.wk <- df.tb[,.(date,country,new_cases,new_deaths,cum_cases,cum_deaths)]
rpt.dt <- world.tb[country == "United States", unique(date)] %>% 
            max() 
world.tb <- world.tb[date == rpt.dt]
world.tb <- world.tb[,.(new_cases = max(new_cases), new_deaths = max(new_deaths), 
                        cum_cases = base::sum(cum_cases), cum_deaths = base::sum(cum_deaths)),
                     by = .(date, country)]
setkey(world.tb,date,country)
# week over week change
world.tb[,cum_cases_lstwk := shift(cum_cases,7), by = country]
world.tb[,pct_chng_lstwk := ((cum_cases - cum_cases_lstwk)/cum_cases_lstwk)*100]
world.tb[,cum_deaths_lstwk := shift(cum_deaths,7), by = country]
world.tb[,pct_deaths_lstwk := ((cum_deaths - cum_deaths_lstwk)/cum_deaths_lstwk)*100]
# moving averages - states
# basic :: 5 day
world.tb[ , five_day_cases := frollmean(world.tb[,new_cases], 5) ]
world.tb[ , five_day_deaths := frollmean(world.tb[,new_deaths], 5) ]
# extended :: 14 day
world.tb[ , twowk_day_cases := frollmean(world.tb[,new_cases], 14) ]
world.tb[ , twowk_day_deaths := frollmean(world.tb[,new_deaths], 14) ]
# NA = 0
setnafill(world.tb, fill = 0, cols = 7:14)
# population numbers
world.tb <- world.tb[p2020,on = .(country = country)]
world.tb <- world.tb[!is.na(date)]
# pop transforms :: world data
world.tb[,cases_per_mil := cum_cases/(pop/1000000)]
world.tb[,deaths_per_mil := cum_deaths/(pop/1000000)]
setorder(world.tb,date,country)
# changes :: day to day, week to week 
world.tb[,daily_change_cases := shift(cum_cases,1), by = country]
world.tb[,daily_change_deaths := shift(cum_deaths,1), by = country]
world.tb[,daily_cases_pop := (new_cases/pop)*100, by = country]
world.tb[,daily_deaths_pop := (new_deaths/pop)*100, by = country]
# world weekly
world.wk[,date := ymd(date)]
world.wk[,country := as.factor(country)]
setnafill(world.wk, fill = 0, cols = 3:6)
setorder(world.wk, country, date)
## >>>>> SUMMARIZE TABLE HERE TO DATE LEVEL, THEN PROCEED
world.wk <- world.wk[p2020, on = .(country = country)]
world.wk[,wknum := week(date)]
world.wk[,yr := year(date)]
world.wk <- world.wk[,.(new_cases = sum(new_cases), new_deaths = sum(new_deaths), 
            cum_cases = sum(cum_cases), cum_deaths = sum(cum_deaths), 
            pop = max(pop), yr = max(yr)),
         by = .(wknum, country)]
country.slct <- world.wk[,.(cum_cases = max(cum_cases), cum_deaths = max(cum_deaths)), by = .(country)]
setnafill(country.slct, fill = 0, cols = 2:3)
setorder(country.slct, -cum_deaths)

# variables
avg.c <- mean(us.date[,daily_cases]) 
avg.d <- mean(us.date[,daily_deaths])
tot.c <- max(us.date[,cum_cases])
tot.d <- max(us.date[,cum_deaths])
map <- map_data("world")
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
setkey(counties, state)

#################################### ##
###  COMPARISON WITH OTHER CAUSES  ####
#################################### ##
other.agg <- other[,1:19]
other.agg <- other.agg[,.(All = base::sum(all_causes), Natural = base::sum(natural_cause),
                          Sepsis = base::sum(septicemia), Cancer = base::sum(malig_neoplasm),
                          Diabetes = base::sum(diabetes), Alzheimer = base::sum(alzheimer),
                          `Influenza & Pneumonia` = base::sum(influenza_pneumonia),
                          `Lower Respitory` = base::sum(cl_respitory), `Other Respitory` = base::sum(other_respitory),
                          `Kidney Diseases` = base::sum(nephritis), Unknown = base::sum(abnormal_unknown),
                          `Heart Disease` = base::sum(heart_disease), Stroke = base::sum(cerebrovascular),
                          `Covid-19 w/ Other`= base::sum(`covid-19_multiple`), `Covid-19` = base::sum(`covid-19_underlying`)),
                       by = .(year, state)]
setnafill(other.agg, fill = 0, cols = 5:17)
other.agg <- melt(other.agg, id.vars = c('year','state'),
                  measure = 3:17,
                  variable.name = "Causes",
                  value.name = "Deaths")
other.agg[,Causes := as.character(Causes)]
setorder(other.agg,Causes)
other.agg[,Causes :=
            factor(Causes, levels = rev(
              c("All", "Natural", "Heart Disease", "Cancer",
                "Covid-19 w/ Other", "Covid-19", "Lower Respitory",
                "Stroke", "Alzheimer", "Diabetes", "Unknown",
                "Influenza & Pneumonia", "Kidney Diseases",
                "Other Respitory", "Sepsis")))]
infect <- data.table(Causes = factor(c("All", "Natural", "Heart Disease", "Cancer",
                                "Covid-19 w/ Other", "Covid-19", "Lower Respitory",
                                "Stroke", "Alzheimer", "Diabetes", "Unknown",
                                "Influenza & Pneumonia", "Kidney Diseases",
                                "Other Respitory", "Sepsis")),
                     Infectious = factor(c("No","No","No","No","Yes","Yes","Partial","No","No","No","No",
                           "Yes","Partial","Partial","Yes")))

other.agg <- other.agg[infect, on = .(Causes=Causes)]
other.agg[,Causes :=
            factor(Causes, levels = rev(
              c("All", "Natural", "Heart Disease", "Cancer",
                "Covid-19 w/ Other", "Covid-19", "Lower Respitory",
                "Stroke", "Alzheimer", "Diabetes", "Unknown",
                "Influenza & Pneumonia", "Kidney Diseases",
                "Other Respitory", "Sepsis")))]

setorder(other.agg,year,state,-Deaths,Causes)

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
