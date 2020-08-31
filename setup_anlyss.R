### SETUP SCRIPT
### SHINY COVID
### V2 :: Analysis Version
### 31 AUG 2020
### 


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
library(RSocrata)
###
options(datatable.optimize=1)

Ms <- function(x){ 
  number_format(accuracy = 0.01, suffix = " M")(x) 
}


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
p2020[,pop := (pop/1000)]
jurs.lst <- fread(file = "jurslst.csv")
select.cntry <- fread(file = "select_countries_list.csv", header = FALSE)


###  WORLD WIDE ANALYSIS  ####
###  TRANSFROM DATA       ####

# create world.data table
world.data <- df.tb
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
world.data <- world.data[ , .(new_cases = sum(new_cases), new_deaths = sum(new_deaths), cum_cases = sum(cum_cases), cum_deaths = sum(cum_deaths)), by = .(date, country, continent)]
setorder(world.data, country, date)

##  Create Metrics and Aggregates
# add population (in Millions)
world.data <- world.data[p2020, on = .(country = country)]
world.data <- world.data[!is.na(date)]
# 100,000s of population
world.data[ , hundredk_pop := (pop * 10)]


###  ANALYSIS :: WORLD    ####












































