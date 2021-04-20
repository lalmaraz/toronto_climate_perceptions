#### Preamble ####
# Purpose: Map respondents' regions for Climate Perceptions project
# Author: Lorena Almaraz De La Garza
# Date: April 2021
# Contact: l.almaraz@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - install packages if needed
# - run EDA script first

library(here)
library(rvest)
library(RCurl)
library(tidyverse)
library(xml2)

# Get data
postal_codes <- read_html("https://www.zipcodesonline.com/2020/06/postal-code-of-toronto-in-2020.html")

# Save
write_html(postal_codes, "inputs/data/raw_postal_code_data.html")

# Select html container and grab text
postal_codes <- postal_codes %>%
  html_nodes("span") %>%
  html_text()

website_text <- tibble(postal_codes = postal_codes)

head(postal_codes)

# Slice only relevant info
website_text <- website_text %>% 
  slice(52:871)

# Make dataframe
postal_codes_clean <- data.frame(website_text) # Assign website_text to new vector
postal_codes_clean <- as.character(postal_codes_clean[,1]) # Convert contents of column 1 into character type
postal_codes_clean <- matrix(postal_codes_clean, ncol = 4, byrow=TRUE) # Create a matrix w 4 columns and fill by row
postal_codes_clean <- data.frame(postal_codes_clean) # Turn matrix into dataframe

# Remove leading/trailing space
postal_codes_clean$X4 <-  gsub("^\\s+|\\s+$", "", postal_codes_clean$X4)
postal_codes_clean$X3 <-  gsub("^\\s+|\\s+$", "", postal_codes_clean$X3) 

# Create regions based on original report
toronto_east_york <- c("Central Toronto", "Downtown Toronto", "East Toronto", "East York", "West Toronto")
etobicoke_york <- c("Etobicoke", "Mississauga", "York")
north_york <- c("North York", "North York,")
scarborough <- c("Scarborough")

# Create region column
postal_codes_clean <- postal_codes_clean %>% 
  mutate(region = case_when(
    X4 %in% toronto_east_york ~ "Toronto/East York",
    X4 %in% etobicoke_york ~ "Etobicoke-York",
    X4 %in% north_york ~ "North York",
    X4 %in% scarborough ~ "Scarborough"
  ))

# Clean some more
postal_codes_clean <- postal_codes_clean %>% 
  select("X3", "region") %>% 
  rename(postal_code = "X3") %>% 
  mutate(postal_code = str_sub(postal_code,0,3)) %>%   # Keep only first postal code if multiple
  unique()

# Reminder: Run EDA script first, it generates the file read here:
climate_perceptions_clean <- read.csv("outputs/data/climate_data_clean.csv")

# Get latitude and longitude data
# CA_ON_full.txt is an excerpt of CA_full, available as .zip here: https://download.geonames.org/export/zip/
# CC GeoNames (www.geonames.org)
coordinates_on <- read.table("inputs/data/CA_ON_full.txt", header = FALSE, sep = "\t") %>% 
  select(2,3,4,10,11) 

coordinates_to <- coordinates_on %>% 
  filter(substr(V2,1,1) == "M")%>% 
  mutate(V2 = str_sub(V2,0,3)) %>%   # Keep only first postal code if multiple
  unique() %>% 
  group_by(V2) %>% 
  summarise(V2 = V2,
            V10 = ave(V10),
            V11 = ave(V11)) %>% 
  unique() %>% 
  rename(
    postal_code = V2,
    latitude = V10,
    longitude = V11
  )

# TO DO: Check postal codes in climate_perceptions_clean against postal_codes_clean,
# make column in 1st that assigns the appropriate region




