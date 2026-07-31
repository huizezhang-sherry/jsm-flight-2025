library(arrow)
library(tidyverse)
library(patchwork)
library(sf)
source(here::here("scripts/00-SH-shared-functions.R"))

dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA") # Source https://news.delta.com/corporate-stats-and-facts
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA") # Source https://www.aa.com/i18n/customer-service/about-us/american-airlines-group.jsp and wikipedia
# note southwest says they don't do the hub and spoke thing so i used wikipedia
ua_hubs <- c("ORD", "DEN", "GUM", "IAH", "LAX", "EWR", "SFO", "IAD") # Source https://www.united.com/en/us/fly/travel/airport/maps.html
hubs <- c(aa_hubs, dl_hubs, ua_hubs)
airline_airport_vec <- c(paste0("AA/ ", aa_hubs),
                         paste0("DL/ ", dl_hubs),
                         paste0("UA/ ", ua_hubs))

flight_2023 <- read_parquet("Year=2023/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)
flight_2022 <- read_parquet("Year=2022/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)
flight_2021 <- read_parquet("Year=2021/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)
flight_2020 <- read_parquet("Year=2020/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)
flight_2019 <- read_parquet("Year=2019/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)

flight_df <- bind_rows(flight_2019, flight_2020, flight_2021, flight_2022, flight_2023) |>
  arrange(Reporting_Airline, FlightDate, DepTime)
readr::write_csv(flight_df, file = here::here("data/flight_df.csv"))


flight_hubs_df <- flight_df |> summarize_count(airports = hubs)
readr::write_csv(flight_hubs_df, file = here::here("data/flight_hubs_df.csv"))

flight_hubs_df |>
  filter(airline_airport %in% airline_airport_vec) |>
  filter(airline == "AA") |>
  plot_dep_arv_pattern() +
  ggtitle("AA Hubs across 2019-2023")
ggsave(filename = here::here("figures/07-SH-aa-hubs.png"), height = 10, width = 15, bg = "white")

flight_hubs_df |>
  filter(airline_airport %in% airline_airport_vec) |>
  filter(airline == "UA") |>
  plot_dep_arv_pattern() +
  ggtitle("UA Hubs across 2019-2023")
ggsave(filename = here::here("figures/07-SH-ua-hubs.png"), height = 10, width = 15, bg = "white")

flight_hubs_df |>
  filter(airline_airport %in% airline_airport_vec) |>
  filter(airline == "DL") |>
  plot_dep_arv_pattern() +
  ggtitle("DL Hubs across 2019-2023")
ggsave(filename = here::here("figures/07-SH-dl-hubs.png"), height = 10, width = 15, bg = "white")
