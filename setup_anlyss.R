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

Ms <- function(x){ 
  number_format(accuracy = 0.01, suffix = " M")(x) 
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
world.today <- world.data[date == (max(date) - 1)]






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
us.today <- us.data[date == max(date)]

##  US ALL UP FOR TRENDS ####
us.allup <- us.data[, 
                    lapply(.SD, sum), 
                    by = .(date), 
                    .SDcols=c("new_cases", "new_deaths", "cum_cases", "cum_deaths",    
                              "pop", "hundredk_pop", "ccper100k", "cdper100k", 
                              "cc100k_lswk", "cd100k_lswk", "cc_pctchg", "cd_pctchg")]
us.allup[ , pop := p2020[country == "USA", pop]]
us.allup[ , hundredk_pop := (pop * 10)]





























