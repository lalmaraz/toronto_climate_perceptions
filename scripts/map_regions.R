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
library(ggmap)
library(osmdata)
library(sf)

# Get data
postal_codes <- read_html("https://www.zipcodesonline.com/2020/06/postal-code-of-toronto-in-2020.html")

# Save
write_html(postal_codes, "inputs/data/raw_postal_code_data.html")

# Select html container and grab text
postal_codes <- postal_codes %>%
  html_nodes("span") %>%
  html_text()

website_text <- tibble(postal_codes = postal_codes)

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
            V11 = ave(V11)) %>% # Don't need exact location, an average is fine
  unique() %>% 
  rename(
    postal_code = V2,
    latitude = V10,
    longitude = V11
  )

# Join dataframes and add missing postal codes and coordinates manually
postal_codes_coordinates <- inner_join(postal_codes_clean, coordinates_to, by = "postal_code") %>% 
  add_row(postal_code = "M2N", region = "North York", latitude = 43.77093629736116, longitude = -79.4132495121774) %>% 
  add_row(postal_code = "M2R", region = "North York", latitude = 43.77913556475642, longitude = -79.4442928405029) %>% 
  add_row(postal_code = "M3C", region = "North York", latitude = 43.722655855005605, longitude = -79.34073514475693) %>% 
  add_row(postal_code = "M3L", region = "North York", latitude = 43.73590254273471, longitude = -79.51381572378213) %>% 
  add_row(postal_code = "M3N", region = "North York", latitude = 43.756459368146075, longitude = -79.5212712573707)


postal_codes_clean <- postal_codes_clean %>% 
  mutate(region = case_when(
    X4 %in% toronto_east_york ~ "Toronto/East York",
    X4 %in% etobicoke_york ~ "Etobicoke-York",
    X4 %in% north_york ~ "North York",
    X4 %in% scarborough ~ "Scarborough"
  ))


# Now to the study sample data!
# Reminder: Run EDA script first, it generates the file read here:
climate_perceptions <- read.csv("outputs/data/climate_data.csv")

# Expand dataframe with coordinates and region
climate_perceptions$postal_code = toupper(climate_perceptions$postal_code)
sample_locations <- left_join(climate_perceptions, postal_codes_coordinates, by = "postal_code")

# Map!
# Create map-friendly dataframe:
map_locations_data <- sample_locations %>% 
  group_by(postal_code) %>% 
  summarise(postal_code = postal_code,
            region = region,
            latitude = latitude,
            longitude = longitude,
            count = as.numeric(table(postal_code))) %>% 
  unique()

map_locations <- get_map(getbb("Toronto"),maptype = "terrain")
ggmap(map_locations)+
  geom_point(data = map_locations_data,
             aes(x = longitude, y = latitude, size = count, color = region)) +
  labs(x = " ", y = "  ", fill = "Region",
       title = "Survey Respondents' Locations", subtitle = "Climate Perceptions Survey 2018")

