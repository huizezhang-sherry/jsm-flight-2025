library(tidyverse)
library(janitor)
# https://hub.arcgis.com/documents/f74df2ed82ba4440a2059e8dc2ec9a5d/explore
airports <- read_csv(here::here("data/airport.csv")) |>
  clean_names()

write_csv(airports, file = here::here("data/airports.csv"))
