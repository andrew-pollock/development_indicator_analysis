
# Load the required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

#### DATA PREPARATION ####

# Load the data from the excel file
raw_data       <- read_xls("API_21_DS2_en_excel_v2.xls", sheet = 1, skip = 3)  ## README Note: Excel file must be in our working directory
country_data   <- read_xls("API_21_DS2_en_excel_v2.xls", sheet = 2)
extra_data     <- read.csv("extra_country_data.csv", stringsAsFactors = FALSE, na.strings = "")
indicator_data <- read_xls("API_21_DS2_en_excel_v2.xls", sheet = 3)

# Clean up the column names
names(raw_data)       <- tolower(gsub("\\s", "_", names(raw_data)))
names(country_data)   <- c("country_code", "region", "income_group", "notes", "country_name")
country_data          <- country_data %>% select(country_name, everything())
names(indicator_data) <- tolower(gsub("\\s", "_", names(indicator_data)))

# Combining country data into a single table
country_data <- left_join(country_data, extra_data[, -2], by = "country_code", all.x = TRUE)
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
# Sub Saharan Africa only has 1 country so we'll exclude it

# Removing Sub-Saharan Africa, Channel Islands and Kosovo
country_data <- country_data %>% filter(income_group == "High income", region != "Sub-Saharan Africa", !(country_code %in% c("CHI", "KSV")))


# Lets compare countries by Region & Continent
country_data %>% group_by(region, continent) %>% summarise(num_countries = n()) %>% arrange(region) #%>% View()

country_data %>% select(country_name, region, continent) %>% arrange(region, continent) #%>% View()
# Within East Asia & Pacific we'll look a AS
# Within Europe & Central Asia we'll look at EU
# Within Latin America & Caribbean we'll look at NA
# Within Middle East & North Africa we'll look at AS
# All North America is contained within NA

# Filtereing down to one continent per region
country_data <- country_data %>% filter((region == "East Asia & Pacific" & continent == "AS") | 
                                        (region == "Europe & Central Asia" & continent == "EU") |
                                        (region == "Latin America & Caribbean" & continent == "NA") | 
                                        (region == "Middle East & North Africa" & continent == "AS") | 
                                        (region == "North America"))


# Selecting a subset of countries from each region
raw_subset <- country_data %>% select(country_code, region, area_in_km, population) %>%
                left_join(long_data, by = "country_code", all.x=TRUE)


# Comparing countries by: missing_prec, populaton, area
raw_subset %>% group_by(country_name, region, area_in_km, population) %>% summarise(perc_missing = sum(is.na(value))/n()) %>% arrange(region, perc_missing) %>% View()

# Removing countries that are very small or have a small population
raw_subset2 <- raw_subset %>% filter(population > 100000, area_in_km > 1000)

raw_subset2 %>% group_by(country_name, region, area_in_km, population) %>% summarise(perc_missing = sum(is.na(value))/n()) %>% arrange(region, perc_missing) %>% View()

# normalise area and population within each group
region_avgs <- raw_subset2 %>% select(country_code, region, population, area_in_km) %>% distinct %>% group_by(region) %>% 
    summarise(min_pop = min(population), 
              range_pop = max(population) - min(population),
              min_size = min(area_in_km),
              range_size = max(area_in_km) - min(area_in_km))

region_avgs <- raw_subset2 %>% select(country_code, region, population, area_in_km) %>% distinct %>% group_by(region) %>% 
  summarise(pop_mean = mean(population), 
            pop_sd = sd(population),
            size_mean = mean(area_in_km),
            size_sd = sd(area_in_km))



raw_subset2 <- left_join(raw_subset2, region_avgs, by = "region")


normalised_data <- raw_subset2 %>% mutate(norm_area = abs((area_in_km-size_mean)/size_sd),
                       norm_pop = abs((population-pop_mean)/pop_sd),
                       total_norm = norm_area+norm_pop) %>% select(country_code, norm_area, norm_pop, total_norm) %>% distinct()

country_data2 <- left_join(country_data, normalised_data, by = "country_code")
# Most representative countries in each region, based on size and population
country_data2 %>% group_by(region) %>% arrange(total_norm) %>% slice(1:2)


raw_subset2 %>% group_by(year) %>% summarise(num_missing = sum(is.na(value)), perc_missing = num_missing/n()) %>% arrange(-perc_missing) %>% 
  ggplot(aes(x=as.integer(year), y = perc_missing)) + 
  geom_line() + lims(y = c(0,1))










# Which indicators 


# Which countries should we choose?

# Counting number of countries in each region


###  In order to make countries more comparable, we'll focus on high income countries from different regions
# Sub saharan africa only has 1 high income country (Seychelles, a series of small islands with low population), so we'll exclude it
country_data %>% filter(!is.na(region), region != "Sub-Saharan Africa", income_group == "High income") %>% select(region, country_name) %>%  arrange(region) %>% View()





country_data %>% filter(!is.na(region),  income_group == "High income") %>% select(region, country_name) %>% distinct() %>% group_by(region) %>% summarise(num_countries = n()) %>% View()


country_data %>% filter(!is.na(region),  income_group == "High income") %>% View()


?read.csv

###  In order to make countries more comparable, we'll focus on high income countries from different regions
# Sub saharan africa only has 1 high income country (Seychelles, a series of small islands with low population), so we'll exclude it
country_data %>% filter(!is.na(region), region != "Sub-Saharan Africa", income_group == "High income") %>% select(region, country_name) %>%  arrange(region) %>% View()

# To simplify the analysis further, we'll pick comparable countries from each region (i.e 2 European countries from Europe & Central Asia, )


## Number of missing rows by country
countries_to_consider <- country_data %>% filter(!is.na(region), region != "Sub-Saharan Africa",  income_group == "High income") %>% select(country_code, region, country_name)
missing_data <- long_data %>% group_by(country_code) %>% summarise(percent_missing = sum(is.na(value))/n())

countries_to_consider <- left_join(countries_to_consider, missing_data, by = "country_code", allx. = TRUE) %>% arrange(percent_missing)

countries_to_consider %>% group_by(region) %>% slice(1:5) %>% View()




country_data %>% group_by(region, continent) %>% summarise(n()) %>% View()


country_data %>% filter(is.na(continent), !is.na(region))  # %>% View()




















