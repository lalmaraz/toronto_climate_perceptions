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

# Make city/province demographics table
totals_table_data <- age_data %>%
  select(4, 5, 7, 8, 9) %>%
  group_by(age_group) %>%
  summarise(male_to = sum(male_to),
            female_to = sum(female_to),
            male_on = sum(male_on),
            female_on = sum(female_on))

# Make sample demographics table
climate_perceptions <- read.csv("outputs/data/climate_data.csv")

# Add column for age groups as determined in original report
climate_perceptions <- 
  climate_perceptions %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 34 ~ 0,
    age >= 35 & age <= 49 ~ 1,
    age >= 50 & age <= 64 ~ 2,
    age >= 65 ~ 3
  ))

# Vector with male counts per age group from survey
ms <- climate_perceptions %>% 
  select (gender, age, age_group) %>% 
  group_by(age_group) %>% 
  filter(gender == "Man") %>% summarize(male_s = n())

# Vector with female counts per age group from survey
fs <- climate_perceptions %>% 
  select (gender, age, age_group) %>% 
  group_by(age_group) %>% 
  filter(gender == "Woman") %>% summarize(female_s = n())

# Join
ms_fs <- inner_join(ms, fs, by = "age_group")

totals_table_data <- inner_join(ms_fs, totals_table_data, by = "age_group") %>% 
  mutate(total_s = 0,
         total_to = 0,
         total_on = 0)

# New columns and rows for total counts (not separated by gender)
totals_table_data$total_s=rowSums(cbind(totals_table_data$male_s,totals_table_data$female_s))
totals_table_data$total_to=rowSums(cbind(totals_table_data$male_to,totals_table_data$female_to))
totals_table_data$total_on=rowSums(cbind(totals_table_data$male_on,totals_table_data$female_on))

totals_row <- c("All",
                sum(totals_table_data$male_s), sum(totals_table_data$female_s),
                sum(totals_table_data$male_to), sum(totals_table_data$female_to),
                sum(totals_table_data$male_on), sum(totals_table_data$female_on),
                sum(totals_table_data$total_s), sum(totals_table_data$total_to), sum(totals_table_data$total_on))

totals_table_data <- rbind(totals_table_data,totals_row)

# Reorder columns
totals_table_data <- totals_table_data %>%  
  subset(select = c(1, 2, 3, 8, 4, 5, 9, 6, 7, 10))

# TO DO: Make table? Maybe?

# Now to plot!
# First, demographics from survey:
# Select relevant columns and rows
totals_splot_data <- totals_table_data %>% 
  select(1:3) %>% 
  slice(1:(n() -1))

# New data frame with data suited to graph
splot_data <- data.frame(age_group = c(0,0,1,1,2,2,3,3), gender = rep(c("M", "F"),4))

splot_data$count <- c(totals_splot_data[1, 2], totals_splot_data[1, 3],
               totals_splot_data[2, 2], totals_splot_data[2, 3],
               totals_splot_data[3, 2], totals_splot_data[3, 3],
               totals_splot_data[4, 2], totals_splot_data[4, 3])

splot_data$percentage <- as.numeric(splot_data$count) / sum(as.numeric(splot_data$count)) * 100

# Voila.
splot<-ggplot(splot_data, aes(x = age_group,
                              fill = gender,
                              y = ifelse(test = gender == "M", yes = -percentage, no = percentage))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = c(-20,20)) +
  coord_flip()

# Second, demographics from city:
# Select relevant columns and rows
totals_toplot_data <- totals_table_data %>% 
  select(1, 5, 6) %>% 
  slice(1:(n() -1))

# New data frame with data suited to graph
toplot_data <- data.frame(age_group = c(0,0,1,1,2,2,3,3), gender = rep(c("M", "F"),4))

toplot_data$count <- c(totals_toplot_data[1, 2], totals_toplot_data[1, 3],
                       totals_toplot_data[2, 2], totals_toplot_data[2, 3],
                       totals_toplot_data[3, 2], totals_toplot_data[3, 3],
                       totals_toplot_data[4, 2], totals_toplot_data[4, 3])

toplot_data$percentage <- as.numeric(toplot_data$count) / sum(as.numeric(toplot_data$count)) * 100

# Voila again.
toplot<-ggplot(toplot_data, aes(x = age_group,
                                fill = gender,
                                y = ifelse(test = gender == "M",yes = -percentage, no = percentage))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = c(-20,20)) +
  coord_flip()

toplot

# Third, demographics from province:
# Select relevant columns and rows
totals_onplot_data <- totals_table_data %>% 
  select(1, 8, 9) %>% 
  slice(1:(n() -1))

# New data frame with data suited to graph
onplot_data <- data.frame(age_group = c(0,0,1,1,2,2,3,3), gender = rep(c("M", "F"),4))

onplot_data$count <- c(totals_onplot_data[1, 2], totals_onplot_data[1, 3],
                       totals_onplot_data[2, 2], totals_onplot_data[2, 3],
                       totals_onplot_data[3, 2], totals_onplot_data[3, 3],
                       totals_onplot_data[4, 2], totals_onplot_data[4, 3])

onplot_data$percentage <- as.numeric(onplot_data$count) / sum(as.numeric(onplot_data$count)) * 100

# A final voila.
onplot<-ggplot(onplot_data, aes(x = age_group,
                                fill = gender,
                                y = ifelse(test = gender == "M", yes = -percentage, no = percentage))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = c(-20,20)) +
  coord_flip()

splot + toplot + onplot

demographicsplot <- plot_grid(splot, toplot, onplot, align = "h", nrow = 1, rel_widths = c(0.555,0.445))
title <- ggdraw() + draw_label("Employment distributions were lower for the treatment sample")
plot_grid(title, demographicsplot, ncol=1, rel_heights=c(0.1, 1))


# Cleaner graph attempt:
splot_data$scale <- "Sample"
toplot_data$scale <- "Toronto"
onplot_data$scale <- "Ontario"

demographicsplot2 <- rbind(splot_data,toplot_data,onplot_data) %>% 
  mutate(age_group = case_when(
    age_group ==  0  ~ "18-34",
    age_group ==  1  ~ "35-49",
    age_group ==  2  ~ "50-64",
    age_group ==  3  ~ "65+")) %>% 
 transform(demographicsplot2,
          scale=factor(scale,levels=c("Sample","Toronto","Ontario")))


ggplot(demographicsplot2,
       aes(width = .5,
           x = age_group,
           fill = gender,
           y = ifelse(test = gender == "M", yes = -percentage, no = percentage))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = c(-20,20)) +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  facet_wrap(~scale) +
  theme_minimal() +
  theme(aspect.ratio=9/16)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey")) +
  scale_fill_manual(values = c("#ff9933", "#99cccc")) +
  labs(title = "Demographics Comparison", y = "Percentage", x = "Age group", fill = "Gender" )

