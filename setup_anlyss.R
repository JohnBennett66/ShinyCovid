### SETUP SCRIPT
### SHINY COVID
### V2 :: Analysis Version
### 31 AUG 2020
### 


### LOAD PACKAGES
library(rsconnect)
library(censusapi)
library(fpp2)
library(scales)
library(shiny)
library(ggplot2)
library(ggrepel)
library(stringr)
# library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(usmap)
library(maps)
library(shinythemes)
library(RSocrata)
###
options(datatable.optimize=1)

Ms <- function(x){ 
  comma(accuracy = 0.01, suffix = " M")(x) 
}

api.key.census <- 'bc4fef59f0cef5d4de7b8e9c7b55218eaae523fc'

###  CAPTURE AND TRANSFORM DATA  ####
# data.world :: covid-19 US :: john hopkins data
us.state <- fread("https://query.data.world/s/cgpumxcw4ajvqt6334dwjhay6uhact",
                  check.names = TRUE,
                  drop = c("cumulative_cases_per_100_000",  "cumulative_deaths_per_100_000",
                           "new_cases_7_day_rolling_avg",  "new_deaths_7_day_rolling_avg",
                           "new_deaths_per_100_000", "new_cases_per_100_000"))

# data.world :: covid WORLD :: multi-source
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

# CDC :: covid and other US :: multi-source(governmental)
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

# CENSUS data :: population
state.pop <- getCensus(name = "pep/population", 
                      vintage = 2019, 
                      key = api.key.census, 
                      vars = c("NAME", "POP"),  
                      region = "state:*")
setDT(state.pop)
setorder(state.pop, state)

county.pop <- getCensus(name = "pep/population", 
                       vintage = 2019, 
                       key = api.key.census, 
                       vars = c("NAME", "POP", "GEO_ID"),  
                       region = "county:*")




###  SET SOME VARIABLES  ####
map_w <- map_data("world")
map_us <- map_data("state")
map_us$region <- str_to_title(map_us$region)


### READ OTHER DATASETS  ####
pred.stl <- fread(file = "predstl.csv")
pred.stl[,date := as_date(date)]
pred.stl.d <- fread(file = "predstld.csv")
pred.stl.d[,date := as_date(date)]
p2020 <- fread(file = "p2020.csv")
p2020[,pop := (pop/1000)]
jurs.lst <- fread(file = "jurslst.csv")
select.cntry <- fread(file = "select_countries_list.csv", header = FALSE)


###  WORLD WIDE ANALYSIS  ####
###  TRANSFROM DATA       ####

# create world.data table
world.data <- df.tb
world.data[ , date := as_date(date)]
setcolorder(world.data, c(4,10,11,9,3,2,8,5,6,12,7,1,13))

##  Transform Data
# correct country names
## WORLD
world.data[country == "United States", country := "USA"]
world.data[country == "Antigua and Barbuda", country := "Antigua"]
world.data[country == "Bonaire, Saint Eustatius and Saba", country := "Bonaire"]
world.data[country == "Brunei and Darussalam", country := "Brunei"]
world.data[country == "CuraÃ§ao", country := "Curacao"]
world.data[country == "Czechia", country := "Czech Republic"]
world.data[country == "Falkland Islands (Malvinas)", country := "Falkland Islands"]
world.data[country == "Guinea Bissau", country := "Guinea-Bissau"]
world.data[country == "Cote dIvoire", country := "Ivory Coast"]
world.data[country == "Congo", country := "Republic of Congo"]
world.data[country == "Saint Kitts and Nevis", country := "Saint Kitts"]
world.data[country == "Saint Vincent and the Grenadines", country := "Saint Vincent"]
world.data[country == "Timor Leste", country := "Timor-Leste"]
world.data[country == "Trinidad and Tobago", country := "Trinidad"]
world.data[country == "United Republic of Tanzania", country := "Tanzania"]
world.data[country == "United Kingdom", country := "UK"]

# aggregate to country level
world.data <- world.data[ , .(new_cases = sum(new_cases), new_deaths = sum(new_deaths), 
                              cum_cases = sum(cum_cases), cum_deaths = sum(cum_deaths))
                          , by = .(date, country, continent)]
setorder(world.data, country, date)

##  Create Metrics and Aggregates
# add population (in Millions)
world.data <- world.data[p2020, on = .(country = country)]
world.data <- world.data[!is.na(date)]
# 100,000s of population
world.data[ , hundredk_pop := (pop * 10)]


###  ANALYSIS :: WORLD    ####
# cumulative per 100k population
world.data[ , ccper100k := cum_cases/hundredk_pop]
world.data[ , cdper100k := cum_deaths/hundredk_pop]

# last week's cumulative
world.data[ , cc100k_lswk := shift(ccper100k, 7), by = country]
world.data[ , cd100k_lswk := shift(cdper100k, 7), by = country]

# percent change (cumulative) from last week
world.data[ , cc_pctchg := (ccper100k-cc100k_lswk)/cc100k_lswk]
world.data[ , cd_pctchg := (cdper100k-cd100k_lswk)/cd100k_lswk]
setnafill(world.data, fill = 0, cols = 12:15)

##  TODAY'S DATA  ####
world.today <- world.data[date == reporting.date]






###  UNITES STATES ANALYSIS  ####
###  TRANSFROM DATA       ####

# create us.data table
us.data <- df.tb
us.data[ , date := as_date(date)]
setcolorder(us.data, c(4,10,11,9,3,2,8,5,6,12,7,1,13))

# aggregate to the US level
us.data <- us.data[country == "USA"]
us.data <- us.data[ , .(new_cases = sum(new_cases), new_deaths = sum(new_deaths), 
                              cum_cases = sum(cum_cases), cum_deaths = sum(cum_deaths))
                          , by = .(date, state)]
setorder(us.data, state, date)

##  Create Metrics and Aggregates
# add population (in Millions)
us.data <- us.data[state.pop, on = .(state = NAME)]
us.data[ , i.state := NULL]
setnames(us.data, 'POP', 'pop')
us.data <- us.data[!is.na(date)]
# 100,000s of population
us.data[ , hundredk_pop := (pop / 100000)]


###  ANALYSIS :: WORLD    ####
# cumulative per 100k population
us.data[ , ccper100k := cum_cases/hundredk_pop]
us.data[ , cdper100k := cum_deaths/hundredk_pop]

# last week's cumulative
us.data[ , cc100k_lswk := shift(ccper100k, 7), by = state]
us.data[ , cd100k_lswk := shift(cdper100k, 7), by = state]

# percent change (cumulative) from last week
us.data[ , cc_pctchg := (ccper100k-cc100k_lswk)/cc100k_lswk]
us.data[ , cd_pctchg := (cdper100k-cd100k_lswk)/cd100k_lswk]
setnafill(us.data, fill = 0, cols = 11:14)

##  TODAY'S DATA  ####
us.today <- us.data[date == reporting.date]
us.today[ , mortality := cum_deaths/cum_cases]
setorder(us.today, -ccper100k)
# rolling averages
# cases
us.today[ , one_week_cases := frollmean(us.today[,new_cases], 7)]
us.today[ , two_week_cases := frollmean(us.today[,new_cases], 14)]
us.today[ , three_week_cases := frollmean(us.today[,new_cases], 21)]
# deaths
us.today[ , one_week_cases := frollmean(us.today[,new_cases], 7)]
us.today[ , two_week_cases := frollmean(us.today[,new_cases], 14)]
us.today[ , three_week_cases := frollmean(us.today[,new_cases], 21)]
# clear NAs
setnafill(us.today, fill = 0, cols = 15:17)



##  US ALL UP FOR TRENDS ####
us.allup <- us.data[ , lapply(.SD, sum), by = .(date), 
                     .SDcols=c("new_cases", "new_deaths", 
                              "cum_cases", "cum_deaths")]
us.allup[ , pop := p2020[country == "USA", pop]]
us.allup[ , hundredk_pop := (pop * 10)]
us.allup[ , ccper100k := cum_cases/hundredk_pop]
us.allup[ , cdper100k := cum_deaths/hundredk_pop]
us.allup[ , cc100k_lswk := shift(ccper100k, 7)]
us.allup[ , cd100k_lswk := shift(cdper100k, 7)]
setnafill(us.allup, fill = 0, cols = 10:11)
us.allup[ , cc_pctchg := ((ccper100k - cc100k_lswk) / cc100k_lswk)]
us.allup[ , cd_pctchg := ((cdper100k - cd100k_lswk) / cd100k_lswk)]
setnafill(us.allup, fill = 0, cols = 12:13)
us.allup[ , nc_chg := ((new_cases - shift(new_cases,1))/shift(new_cases,1))]
us.allup[ , nd_chg := ((new_deaths - shift(new_deaths,1))/shift(new_deaths,1))]
setnafill(us.allup, fill = 0, cols = 14:15)
us.allup[nc_chg == "Inf", nc_chg := 0]
us.allup[nd_chg == "Inf", nd_chg := 0]
us.allup[cc_pctchg == "Inf", cc_pctchg := 0]
us.allup[cd_pctchg == "Inf", cd_pctchg := 0]


##  US TABLE :: TODAY  ####
us.table <- us.today[ , .(state, ccper100k, cdper100k, cc_pctchg, cd_pctchg, mortality)]
us.table[ , "Cases /100k" := comma(ccper100k, accuracy = 1)] 
us.table[ , "Deaths /100k" := comma(cdper100k, accuracy = 1)] 
us.table[ , "Cases Change" := percent(cc_pctchg, accuracy = 0.1)] 
us.table[ , "Deaths Change" := percent(cd_pctchg, accuracy = 0.1)]
us.table[ , "Mortality Rate" := percent(mortality, accuracy = 0.1)]
setnames(us.table, 1, "State")
setorder(us.table, -ccper100k)


##  US WEEKLY TABLE  ####
us.wkly <- us.data[ , lapply(.SD, sum), by = .(date), 
                    .SDcols=c("new_cases", "new_deaths", 
                              "cum_cases", "cum_deaths")]
us.wkly[ , floor_date := floor_date(date, "week")]
us.wkly <- us.wkly[ , lapply(.SD, sum), by = .(floor_date), 
                      .SDcols=c("new_cases", "new_deaths")]
us.weekly.cum <- us.allup[date %in% us.wkly[,floor_date], .(date, cum_cases, cum_deaths, pop, hundredk_pop)]
us.wkly <- us.wkly[us.weekly.cum, on = .(floor_date = date)]
remove(us.weekly.cum)
us.wkly[ , nc_pctchg := ((new_cases - shift(new_cases,1))/shift(new_cases,1))]
us.wkly[ , nd_pctchg := ((new_deaths - shift(new_deaths,1))/shift(new_deaths,1))]

##  WORLD ALL UP FOR TRENDS ####
world.allup <- world.data[ , lapply(.SD, sum), by = .(date), 
                     .SDcols=c("new_cases", "new_deaths", 
                               "cum_cases", "cum_deaths")]
world.allup[ , pop := p2020[, sum(pop)]]
world.allup[ , hundredk_pop := (pop * 10)]
world.allup[ , ccper100k := cum_cases/hundredk_pop]
world.allup[ , cdper100k := cum_deaths/hundredk_pop]
world.allup[ , cc100k_lswk := shift(ccper100k, 7)]
world.allup[ , cd100k_lswk := shift(cdper100k, 7)]
setnafill(world.allup, fill = 0, cols = 10:11)
world.allup[ , cc_pctchg := ((ccper100k - cc100k_lswk) / cc100k_lswk)]
world.allup[ , cd_pctchg := ((cdper100k - cd100k_lswk) / cd100k_lswk)]
setnafill(world.allup, fill = 0, cols = 12:13)
world.allup[ , nc_chg := ((new_cases - shift(new_cases,1))/shift(new_cases,1))]
world.allup[ , nd_chg := ((new_deaths - shift(new_deaths,1))/shift(new_deaths,1))]
setnafill(world.allup, fill = 0, cols = 14:15)
world.allup[nc_chg == "Inf", nc_chg := 0]
world.allup[nd_chg == "Inf", nd_chg := 0]
world.allup[cc_pctchg == "Inf", cc_pctchg := 0]
world.allup[cd_pctchg == "Inf", cd_pctchg := 0]    


###  FRONT PAGE QUICK UPDATE  ####
# world numbers
world.cases <- world.data[date == reporting.date , sum(cum_cases)]
world.deaths <- world.data[date == reporting.date , sum(cum_deaths)]
world.cases.lastweek <- world.data[date == reporting.date -7 , sum(cum_cases)]
world.deaths.lastweek <- world.data[date == reporting.date -7 , sum(cum_deaths)]
world.pop <- p2020[, sum(pop)*10]
world.c100k.lastweek <-  world.data[date == reporting.date - 7 , sum(cum_cases)/world.pop]
world.d100k.lastweek <-  world.data[date == reporting.date - 7 , sum(cum_deaths)/world.pop]
world.ccper100k <- world.cases/world.pop
world.cdper100k <- world.deaths/world.pop
world.c100k.lastweek <-  world.data[date == reporting.date - 7 , sum(cum_cases)/world.pop]
world.d100k.lastweek <-  world.data[date == reporting.date - 7 , sum(cum_deaths)/world.pop]
world.cases.increase <- ((world.ccper100k - world.c100k.lastweek) / 
                            world.c100k.lastweek) 
world.deaths.increase <- ((world.cdper100k - world.d100k.lastweek) / 
                            world.d100k.lastweek) 



# us numbers
us.cases <- us.allup[date == reporting.date, cum_cases]
us.deaths <- us.allup[date == reporting.date, cum_deaths]
us.cases.lastweek <- us.allup[date == reporting.date - 7, cum_cases]
us.cases.increase <- ((us.cases - us.cases.lastweek) / 
                        us.cases.lastweek)
us.deaths.lastweek <- us.allup[date == reporting.date - 7, cum_deaths]
us.deaths.increase <- ((us.deaths - us.deaths.lastweek) / 
                         us.deaths.lastweek)







