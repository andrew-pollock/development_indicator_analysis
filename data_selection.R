
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
country_data <- country_data %>% filter(!(country_code %in% c("CHI", "KSV")))


#### DATA EXPLORATION ####

# First look at the data
summary(raw_data)


# Gather the data into a long format to make it easier to analyse & graph
long_data <- raw_data %>% gather(key = "year", value="value", -country_name, -country_code, -indicator_name, -indicator_code)


# Which years have the most missing values?
(yearly_missing_values <- long_data %>% group_by(year) %>% summarise(num_missing = sum(is.na(value)), perc_missing = num_missing/n()) %>% arrange(-perc_missing) %>% 
  ggplot(aes(x=as.integer(year), y = perc_missing)) + 
  geom_line() + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1) +
  labs(title = "Percentage of Data Points Missing by Year", x = "Year", y = "% of Data Missing"))
ggsave("plots/yearly_missing_values.png", yearly_missing_values)
# 2016 data is almost completely missing & 77% is missing from 2015, otherwise data availability slowly improves from 19060 onwards
# We'll look at the date range from 1980 until 2014 to ensure we have the best data availability possible


# Dropping 2016, Channel Islands and Kosovo from the data
long_data <- long_data %>% filter(year <= 2014, year >= 1980, !(country_code %in% c("CHI", "KSV")))


#### SELECTING COUNTRIES ####
# We're only interested in countries with are current high income countries

# Filtering down to the 3 regions we're interested in (EU, NA & East Asia)
country_data <- country_data %>% filter(country_code == "WLD" | 
                                        (income_group == "High income" & region  %in% c("East Asia & Pacific", 
                                                                                        "Europe & Central Asia",
                                                                                        "North America"))) %>%
                                mutate(region = case_when(country_code == "WLD" ~ "World", TRUE ~ region))

# Counting countries in each region
country_data %>% group_by(region) %>% summarise(num_countries = n()) %>% arrange(-num_countries)


# Lets compare countries by Region & Continent
country_data %>% select(country_name, region, continent) %>% arrange(region, continent) %>% View()
# We want to   filter to just the Asian countries in East Asia & Pacific (i.e excluding Aus and New Zealand)

# Filtering down to one continent per region
country_data <- country_data %>% filter((region == "East Asia & Pacific" & continent == "AS") | 
                                        (region == "Europe & Central Asia" & continent == "EU") |
                                        (region == "North America" & continent == "NA") |
                                        (country_code == "WLD"))


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
(filtered_missing_values <- indicator_selection %>% group_by(year) %>% summarise(percent_missing = sum(is.na(value))/n())  %>% filter(year != "2015") %>% 
  ggplot(aes(x=as.integer(year), y = percent_missing)) +
  geom_line() + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1) +
  labs(title = "Percentage of Data Points Missing After Filtering", x = "Year", y = "% of Data Missing"))
ggsave("plots/filtered_missing_values.png", filtered_missing_values)

indicator_selection %>% group_by(indicator_name) %>% summarise(percent_missing = sum(is.na(value))/n()) %>% View()


# We'll use the below 15 indicators to compare our chosen countries
indicators <- c('Exports of goods and services (% of GDP)',
                'Imports of goods and services (% of GDP)',
                'Food exports (% of merchandise exports)',
                'Food imports (% of merchandise imports)',
                'Fuel exports (% of merchandise exports)',
                'Fuel imports (% of merchandise imports)',
                'Agricultural raw materials exports (% of merchandise exports)',
                'Agricultural raw materials imports (% of merchandise imports)',
                'Ores and metals exports (% of merchandise exports)',
                'Ores and metals imports (% of merchandise imports)',
                'Manufactures exports (% of merchandise exports)',
                'Manufactures imports (% of merchandise imports)',
                'Merchandise trade (% of GDP)',
                'Merchandise exports (current US$)',
                'Merchandise imports (current US$)')
# These are all available for our chose countries from 1980 onwards

long_data <- long_data %>% filter(indicator_name %in% indicators)


#### SELECTING A TIME FRAME ####

# Can we look earlier than 1980 but maintain good availability of data?
final_data <- raw_data %>% gather(key = "year", value="value", -country_name, -country_code, -indicator_name, -indicator_code) %>% filter(indicator_name %in% indicators) %>% 
  filter(country_code %in% country_filter$country_code)

(selected_missing_values <- final_data %>% group_by(year) %>% summarise(percent_missing = sum(is.na(value))/n()) %>%
  ggplot(aes(x=as.integer(year), y=percent_missing)) + 
  geom_line() + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0,1) +
  labs(title = "Percentage of Selected Variables Missing by Year", x = "Year", y = "% of Data Missing"))
ggsave("plots/selected_vars_missing_values.png", selected_missing_values)
# Our chosen data is fully available from 1970 up until 2014

# To allow for the most stable period for comparisons we'll look at 1975 until 2014
dashboard_data <- final_data %>% filter(year >= 1970, year <= 2014) %>% left_join(select(country_data, country_code, region), by = "country_code") %>%
  select(country_name, country_code, region, year, indicator_name, value) %>% 
  mutate(value = case_when(indicator_name %in% c('Merchandise exports (current US$)',
                                                 'Merchandise imports (current US$)') ~ value,
                           TRUE ~ round(value/100,4)))

world_data <-  raw_data %>% gather(key = "year", value="value", -country_name, -country_code, -indicator_name, -indicator_code) %>% 
  filter(year >= 1970, year <= 2014, indicator_name %in% indicators, !is.na(value)) %>% 
  mutate(value = case_when(indicator_name %in% c('Merchandise exports (current US$)',
                                                 'Merchandise imports (current US$)') ~ value,
                           TRUE ~ round(value/100,4)))  %>% select(country_code, indicator_name, year, value) %>%
  filter(!(country_code %in% c('ARB','CEB','CHI','CSS','EAP','EAR','EAS','ECA','ECS','EMU','EUU','FCS','GIB','HIC','HPC','IBD',
                               'IBT','IDA','IDB','IDX','INX','KSV','LAC','LCN','LDC','LIC','LMC','LMY','LTE','MEA','MIC','MNA',
                               'NAC','OED','OSS','PRE','PSS','PST','SAS','SSA','SSF','SST','TEA','TEC','TLA','TMN','TSA','TSS',
                               'UMC', 'WLD')))

write.csv(dashboard_data, file = "data/dashboard_data.csv", row.names = FALSE)
write.csv(world_data, file = "data/world_data.csv", row.names = FALSE)


