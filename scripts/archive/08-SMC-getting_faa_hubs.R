library(arrow)
library(tidyverse)
library(dplyr)
library(splines)
library(purrr)

# Doing this for the year 2017
pv <- read.csv("passenger_info/T_T100D_SEGMENT_ALL_CARRIER_2017.csv")

# Get total passengers for 2017
tot_pas <- pv %>%
  dplyr::select(PASSENGERS) %>%
  sum(na.rm = T)

# Define thresholds
# https://www.faa.gov/airports/planning_capacity/categories
large_hub_min <- tot_pas * 0.01
med_hub_min <- tot_pas * 0.0025
small_hub_min <- tot_pas * 0.0005

# Get categories
hub_df <- pv %>%
  group_by(DEST) %>%
  summarise(yearly_passengers = sum(PASSENGERS, na.rm = T)) %>%
  mutate(hub_type = case_when(yearly_passengers > large_hub_min ~ 'Large',
                              yearly_passengers > med_hub_min ~ 'Medium',
                              yearly_passengers > small_hub_min ~ 'Small',
                              TRUE ~ 'Nonhub')) %>%
  rename(dest = DEST) %>%
  arrange(desc(yearly_passengers))

# Write output
write.csv(hub_df, "data/hub_status_2017.csv")
