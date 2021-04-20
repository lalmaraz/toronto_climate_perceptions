#### Preamble ####
# Purpose: Exploratory data analysis for Climate Perceptions project
# Author: Lorena Almaraz De La Garza
# Date: April 2021
# Contact: l.almaraz@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - install packages if needed

library(opendatatoronto)
library(RCurl)
library(here)
library(tidyverse)

# Based on Michael Chong's excellent work, as published in Telling Stories With Data (Alexander, 2021) here:
# https://www.tellingstorieswithdata.com/exploratory-data-analysis.html#case-study---opinions-about-a-casino-in-toronto


# The nice way* to import dataset:

# search_packages("climate") %>% 
#   list_package_resources() %>% 
#   filter(grepl("v1", name)) %>% 
#   get_resource()

# * this is commented out because there seems to be an issue with the original file extension (listed as .xls but behaves as .xlsx)
# contacted data owner 29032021, issue persists as of 17042021 with error:
#   "filepath: /var/folders/ff/yckwhmhx69jbjdlphv588lv80000gn/T//RtmpAD7G0d/file14a6c14da63d9.xls"
#   "libxls error: Unable to open file"

# Here's a less elegant but functional workaround:
climate_perceptions_raw <- getURL("https://raw.githubusercontent.com/lalmaraz/toronto_climate_perceptions/main/inputs/data/csv_city-of-toronto-climate-perceptions-spss-v1.csv")
climate_perceptions_csv <- read.csv(text = climate_perceptions_raw)

head(climate_perceptions_csv)

class(climate_perceptions_csv)

# Save
write_csv(climate_perceptions_csv, "outputs/data/climate_data.csv")

# TO DO: Make some graphs/tables of data as found

# Select variables of interest
climate_perceptions <- 
  climate_perceptions_csv %>%
  select(QS1, HID25W, QS2, HIDAGE1, Q1r1, Q1r2, Q2, QD5, QD6) %>%
  rename(postal_code = QS1, ward_name = HID25W, gender = QS2, age = HIDAGE1, concern_local = Q1r1, concern_global = Q1r2, information = Q2, education = QD5, income = QD6)

# Clean up for model:
# Remove invalid responses
climate_perceptions_clean <- climate_perceptions %>%
  filter(gender != "Prefer not to say", concern_local != "Donâ€™t know", concern_global != "Donâ€™t know", education != "Prefer not to answer", income != "Prefer not to answer")

# Curious about those invalid responses, actually 
climate_perceptions_dirty <- climate_perceptions %>%
mutate(invalid = case_when(
  gender == "Prefer not to say" ~ TRUE,
  concern_local == "Donâ€™t know" ~ TRUE,
  concern_global == "Donâ€™t know"~ TRUE,
  education == "Prefer not to answer" ~ TRUE,
  income == "Prefer not to answer"~ TRUE
)) %>% 
  na.omit()

# Create binary options for concern_local, concern_global, and information
concerned_local <- c("Extremely concerned", "Very concerned")
not_concerned_local <- c("Not very concerned", "Not at all concerned")

concerned_global <- c("Extremely concerned", "Very concerned")
not_concerned_global <- c("Not very concerned", "Not at all concerned")

informed <- c("Extremely informed", "Very informed")
not_informed <- c("Not very informed", "Not at all informed")

# Add column for each binary to dataframe
climate_perceptions_clean <- climate_perceptions_clean %>%
  mutate(concerned_local_binary = case_when(
    concern_local %in% concerned_local ~ TRUE,
    concern_local %in% not_concerned_local ~ FALSE
  ))

climate_perceptions_clean <- climate_perceptions_clean %>%
  mutate(concerned_global_binary = case_when(
    concern_global %in% concerned_global ~ TRUE,
    concern_global %in% not_concerned_global ~ FALSE  
  ))

climate_perceptions_clean <- climate_perceptions_clean %>%
  mutate(informed_binary = case_when(
    information %in% informed ~ TRUE,
    information %in% not_informed ~ FALSE 
  ))

# Add column for age groups as determined in original report
climate_perceptions_clean <- 
  climate_perceptions_clean %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 34 ~ 0,
    age >= 35 & age <= 49 ~ 1,
    age >= 50 & age <= 64 ~ 2,
    age >= 65 ~ 3
  ))

# Determine Boomer status
climate_perceptions_clean <- 
  climate_perceptions_clean %>%
  mutate(boomer_status = case_when(
    age <= 54 ~ FALSE,
    age >= 55 ~ TRUE
  ))

# Convert income and education to numeric variables
climate_perceptions_clean <- 
  climate_perceptions_clean %>%
  mutate(education_group = case_when(
    education == "High school or less" ~ 0,
    education == "Some community college, vocational, trade school" ~ 1,
    education == "Completed community college, vocational, trade school" ~ 2,
    education == "Some university" ~ 3,
    education == "Completed undergraduate degree" ~ 4,
    education == "Post graduate/professional school" ~ 5,
  ))

climate_perceptions_clean <- 
  climate_perceptions_clean %>%
  mutate(income_group = case_when(
    income == "Under $40,000" ~ 0,
    income == "$40,001 to $60,000" ~ 1,
    income == "$60,001 to $80,000" ~ 2,
    income == "$80,001 to $100,000" ~ 3,
    income == "$100,001 to $150,000" ~ 4,
    income == "More than $150,000" ~ 5,
  ))

# Save
write_csv(climate_perceptions_clean, "outputs/data/climate_data_clean.csv")

# Model time!
# Univar & multivar models: local concern
u_concern_local_glm <- glm(concerned_local_binary ~ age_group, data = climate_perceptions_clean, family = "binomial")
summary(u_concern_local_glm)

m_concern_local_glm <- glm(concerned_local_binary ~ age_group + gender + education_group + income_group, data = climate_perceptions_clean, family = "binomial")
summary(m_concern_local_glm)

# Univar & multivar models: global concern
u_concern_global_glm <- glm(concerned_global_binary ~ age_group, data = climate_perceptions_clean, family = "binomial")
summary(u_concern_global_glm)

m_concern_global_glm <- glm(concerned_global_binary ~ age_group + gender + education_group + income_group, data = climate_perceptions_clean, family = "binomial")
summary(m_concern_global_glm)

# Univar & multivar models: informed
u_informed_glm <- glm(informed_binary ~ age_group, data = climate_perceptions_clean, family = "binomial")
summary(u_informed_glm)

m_informed_glm <- glm(informed_binary ~ age_group + gender + education_group + income_group, data = climate_perceptions_clean, family = "binomial")
summary(m_informed_glm)

# Boomer Special Edition models:
# Local concern
boomer_concern_local_glm <- glm(concerned_local_binary ~ boomer_status + gender + education_group + income_group, data = climate_perceptions_clean, family = "binomial")
summary(boomer_concern_local_glm)

# Global concern
boomer_concern_global_glm <- glm(concerned_global_binary ~ boomer_status + gender + education_group + income_group, data = climate_perceptions_clean, family = "binomial")
summary(boomer_concern_global_glm)

# Informed
boomer_informed_glm <- glm(informed_binary ~ boomer_status + gender + education_group + income_group, data = climate_perceptions_clean, family = "binomial")
summary(boomer_informed_glm)