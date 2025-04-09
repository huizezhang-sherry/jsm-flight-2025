# https://data.transportation.gov/Aviation/Consumer-Airfare-Report-Table-1a-All-U-S-Airport-P/tfrh-tu9e/about_data
library(tidyverse)
airport_pairs <- read_csv(file.choose()) |>
  janitor::clean_names() |>
  arrange(year, quarter, airportid_1, airportid_2)

write_csv(airport_pairs, here::here("data/airport_pairs.csv"))

