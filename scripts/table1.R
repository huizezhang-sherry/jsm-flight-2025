library(tidyverse)

# aa_df is created in the fig3.R script
aa_df <- read_csv('data/aa_df.csv')

short_list <- c("DFW", "CLT", "ORD", "PHL", "LGA", "LAX", "DCA")

# For table
aa_df |>
  mutate(airport_name = str_extract(airline_airport,"(?<=/ ).*(?= \\()"),
         n = abs(n),
         hub = if_else(airport %in% short_list, 'Yes', 'No')) |>
  group_by(airport, airport_name, hub) |>
  summarise(total = sum(n), .groups = 'drop') |>
  mutate(total = scales::comma(total))
