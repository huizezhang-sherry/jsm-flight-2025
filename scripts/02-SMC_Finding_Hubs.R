########################
### Read in packages ###
########################

library(arrow) # to read parquet
library(tidyverse)
library(ggridges) # for ridgeline plots
library(ggplot2)

#########################################
### Read in datasets and clean/format ###
#########################################

## Read dataset for JSM project
# Data dictionary here https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FGJ
# Column Dest has the destination airport, FAA code
flight_df <- read_parquet("Year=2024/data_0.parquet")

## Read Sherry's dataset - direction invariant
cost_df <- read.csv("./data/airport_pairs.csv")

############################################
### Find large marketshare routes for DL ###
############################################

# Define function to do all this for me!
plot_pairs_by_airline <- function(airline, thresh, flight_df, cost_df){
  # Filter dataframe such that either large carrier is airline AND market share is > threshold OR
  # Low carrier is airline AND low market share is > threshold
  t_cost_df <- cost_df %>%
    filter((carrier_lg == airline & large_ms > thresh) | (carrier_low == airline & lf_ms > thresh),
           year == 2024) %>% # for now. passengers = per day
    group_by(airport_1, airport_2) %>%  # remove quarter distinction
    summarise(passengers = mean(passengers, na.rm = TRUE),
              large_ms = mean(large_ms, na.rm = TRUE),
              lf_ms = mean(large_ms, na.rm = TRUE))

  t_flight_df <- flight_df %>%
    filter(Reporting_Airline == airline) %>%
    mutate(airport_1 = pmin(Origin, Dest),
           airport_2 = pmax(Origin, Dest)) %>%
    group_by(airport_1, airport_2) %>%
    summarise(avg_delay = mean(DepDelay, na.rm = TRUE))

  merged_df <- merge(t_cost_df, t_flight_df, by = c("airport_1", "airport_2")) %>%
    mutate(Pair = paste0(airport_1,"-",airport_2))

  p <- merged_df %>%
    ggplot(aes(x = large_ms, y = avg_delay, size = passengers)) +
    geom_point() +
    geom_label(aes(label = Pair), position = position_jitter()) +
    labs(title = paste("Airline", airline),
         x = "Large Market Share",
         y = "Average Delay Time (minutes)")
return(p)
}

thresh <- 0.5

p <- plot_pairs_by_airline("AA", thresh, flight_df, cost_df)
ggsave(filename ='./figures/02-SMC_airline_aa.png',plot = p)
p <- plot_pairs_by_airline("DL", thresh, flight_df, cost_df)
ggsave(filename = './figures/02-SMC_airline_dl.png',plot = p)
p <- plot_pairs_by_airline("WN", thresh, flight_df, cost_df)
ggsave(filename = './figures/02-SMC_airline_wn.png',plot = p)
p <- plot_pairs_by_airline("UA", thresh, flight_df, cost_df)
ggsave(filename = './figures/02-SMC_airline_ua.png',plot = p)
