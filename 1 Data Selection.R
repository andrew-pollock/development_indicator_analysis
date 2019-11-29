
# Load the required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#### DATA PREPARATION ####

# Load the data from the excel file
raw_data       <- read_xls("data/API_21_DS2_en_excel_v2.xls", sheet = 1, skip = 3)  
country_data   <- read_xls("data/API_21_DS2_en_excel_v2.xls", sheet = 2)
extra_data     <- read.csv("data/extra_country_data.csv", stringsAsFactors = FALSE, na.strings = "")
indicator_data <- read_xls("data/API_21_DS2_en_excel_v2.xls", sheet = 3)

# Clean up the column names
names(raw_data)       <- tolower(gsub("\\s", "_", names(raw_data)))
names(country_data)   <- c("country_code", "region", "income_group", "notes", "country_name")
country_data          <- country_data %>% select(country_name, everything())
names(indicator_data) <- tolower(gsub("\\s", "_", names(indicator_data)))

# Combining country data into a single table
country_data <- left_join(country_data, extra_data[, -2], by = "country_code", all.x = TRUE)
raw_data[raw_data$country_name == "Korea, Rep.",1] <- "South Korea"
rm(extra_data)

# Check if any of the existing countries don't appear in the extra_data table
country_data %>% filter(is.na(continent), !is.na(region))
# Only 2 small countries are impacted so we'll exclude them from our analysis


#### DATA EXPLORATION ####

# First look at the data
summary(raw_data)


# Gather the data into a long format to make it easier to analyse & graph
long_data <- raw_data %>% gather(key = "year", value="value", -country_name, -country_code, -indicator_name, -indicator_code)


# Which years have the most missing values?
long_data %>% group_by(year) %>% summarise(num_missing = sum(is.na(value)), perc_missing = num_missing/n()) %>% arrange(-perc_missing) %>% 
  ggplot(aes(x=as.integer(year), y = perc_missing)) + 
  geom_line() + lims(y = c(0,1)) + theme_bw()
# 2016 data is almost completely missing & 77% is missing from 2015, otherwise data availability slowly improves from 19060 onwards


# Dropping 2016, Channel Islands and Kosovo from the data
long_data <- long_data %>% filter(year != "2016", !(country_code %in% c("CHI", "KSV")))


#### SELECTING COUNTRIES ####
# We'll focus on high income countries to simplify our comparisons

# Counting countries in each region
country_data %>% filter(income_group == "High income") %>% group_by(region) %>% summarise(num_countries = n()) %>% arrange(-num_countries) %>% View()
# Sub Saharan Africa only has 1 high income country so we'll exclude it

# Removing Sub-Saharan Africa, Channel Islands and Kosovo
country_data <- country_data %>% filter(income_group == "High income", region != "Sub-Saharan Africa", !(country_code %in% c("CHI", "KSV")))


# Lets compare countries by Region & Continent
country_data %>% select(country_name, region, continent) %>% arrange(region, continent) %>% View()
# Within East Asia & Pacific we'll look a AS
# Within Europe & Central Asia we'll look at EU
# Within Middle East & North Africa we'll look at AS
# All North America is contained within NA

# Filtereing down to one continent per region
country_data <- country_data %>% filter((region == "East Asia & Pacific" & continent == "AS") | 
                                        (region == "Europe & Central Asia" & continent == "EU") |
                                        (region == "Middle East & North Africa" & continent == "AS") | 
                                        (region == "North America"))


# Selecting the 2 most populous countries from each region
country_filter <- country_data %>% select(region, country_code, population) %>% arrange(region, -population) %>% 
  group_by(region) %>% slice(1:2) %>% ungroup() %>% select(country_code)


long_data <- long_data %>% filter(country_code %in% country_filter$country_code)



#### SELECTING INDICATORS ####
indicator_selection <- long_data %>% select(indicator_name, year, value)

# Remove any indicators with more than 40% of data points missing
indicator_selection <- indicator_selection %>% group_by(indicator_name) %>% summarise(percent_missing = sum(is.na(value))/n()) %>% 
  arrange(-percent_missing) %>% filter(percent_missing < 0.4) %>% 
  select(indicator_name) %>% left_join(indicator_selection, by = "indicator_name")


# Visually inspect remaining indicators
indicator_selection %>% group_by( year) %>% summarise(percent_missing = sum(is.na(value))/n())  %>% filter(year != "2015") %>% 
  ggplot(aes(x=as.integer(year), y = percent_missing)) +
  geom_line() + theme_bw() + ylim(0,1)

# Data availability for our chosen countries improve significantly after 1975
indicator_selection %>% filter(year > 1975) %>% group_by(indicator_name)  %>% summarise(percent_missing = sum(is.na(value))/n()) %>% View()


# We'll use the below 15 indicators to compare our chosen countries
indicators <- c('Arms imports (SIPRI trend indicator values)',
  'Energy imports, net (% of energy use)',
  'Exports of goods and services (% of GDP)',
  'Food exports (% of merchandise exports)',
  'Food imports (% of merchandise imports)',
  'Fuel exports (% of merchandise exports)',
  'Fuel imports (% of merchandise imports)',
  'Imports of goods and services (% of GDP)',
  'Merchandise exports (current US$)',
  'Merchandise exports to high-income economies (% of total merchandise exports)',
  'Merchandise imports (current US$)',
  'Merchandise imports from high-income economies (% of total merchandise imports)',
  'Merchandise trade (% of GDP)',
  'Ores and metals exports (% of merchandise exports)',
  'Ores and metals imports (% of merchandise imports)',
  'Trade (% of GDP)')


long_data <- long_data %>% filter(indicator_name %in% indicators)


#### SELECTING A TIME FRAME ####

# What period of time should we consider?
indicator_selection %>% group_by(year) %>% summarise(percent_missing = sum(is.na(value))/n()) %>%
  ggplot(aes(x=as.integer(year), y=percent_missing)) + 
  geom_line() + theme_bw() + ylim(0,1)
# data availability improves rapidly up to the mid-1970s but is poor again in 2015

# To allow for the most stable period for comparisons we'll look at 1975 until 2014
dashboard_data <- long_data %>% filter(year >= 1975, year <= 2014) %>% left_join(select(country_data, country_code, region), by = "country_code") %>%
  select(country_name, country_code, region, year, indicator_name, value)


write.csv(dashboard_data, file = "data/dashboard_data.csv", row.names = FALSE)


