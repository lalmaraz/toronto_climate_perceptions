#### Preamble ####
# Purpose: City and province demographic comparison for Climate Perceptions project
# Author: Lorena Almaraz De La Garza
# Date: April 2021
# Contact: l.almaraz@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - install packages if needed

library(here)
library(tidyverse)
library(RCurl)
library(janitor)

# Get data
census_2016 <- getURL("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/current-actuelle.cfm?Lang=E&Geo1=CSD&Code1=3520005&Geo2=PR&Code2=35&B1=All&type=0&FILETYPE=CSV")

# Skip not-too-useful header and sub-column name (thanks to https://stackoverflow.com/questions/39110755/skip-specific-rows-using-read-csv-in-r)
census_2016_headers <- read.csv(text = census_2016, skip = 1, header = F, nrows = 1, as.is = T,  sep = ',')
census_2016_data <- read.csv(text = census_2016, skip = 3, header = F, sep = ',')
colnames(census_2016_data) = census_2016_headers

head(census_2016_data)

class(census_2016_data)

# Save
write_csv(census_2016_data, "outputs/data/census_2016_data.csv")

# Clean
census_2016_data <- 
  census_2016_data %>%
  rename(topic = 1, characteristics = 2, flag_total_to = 3, total_to = 4, flag_male_to = 5, male_to = 6, flag_female_to = 7, female_to = 8, 
         flag_total_on = 9, total_on = 10, flag_male_on = 11,  male_on = 12, flag_female_on = 13, female_on = 14) %>% # Rename to unique names
  select(1, 2, 4, 6, 8, 10, 12, 14)

age_data <- census_2016_data %>% 
  filter(topic == "Age characteristics")

# Create age groups
young <- c("    20 to 24 years", "    25 to 29 years", "    30 to 34 years")
youngish <- c("    35 to 39 years", "    40 to 44 years", "    45 to 49 years")
oldish <- c("    50 to 54 years", "    55 to 59 years", "    60 to 64 years")
old <- c("  65 years and over")

# Make column with age group
age_data <- age_data %>%
  mutate(age_group = case_when(
    characteristics %in% young ~ 0,
    characteristics %in% youngish ~ 1,
    characteristics %in% oldish ~ 2,
    characteristics %in% old ~ 3
  )) %>% 
  na.omit() %>% 
  slice(1:(n()-1)) # Remove irrelevant row

# Make city/province table
totals_table_data <- age_data %>%
  select(4, 5, 7, 8, 9) %>%
  group_by(age_group) %>%
  summarise(male_to = sum(male_to),
            female_to = sum(female_to),
            male_on = sum(male_on),
            female_on = sum(female_on))

city_province_totals <- census_2016_data[8,]

# TO DO: make graphs w percentages/proportion of male / female per age group and compare to study sample
