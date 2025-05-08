library(arrow)
library(tidyverse)
library(patchwork)
library(sf)
source(here::here("scripts/00-SH-shared-functions.R"))

dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA") # Source https://news.delta.com/corporate-stats-and-facts
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA") # Source https://www.aa.com/i18n/customer-service/about-us/american-airlines-group.jsp and wikipedia
# note southwest says they don't do the hub and spoke thing so i used wikipedia
ua_hubs <- c("ORD", "DEN", "GUM", "IAH", "LAX", "EWR", "SFO", "IAD") # Source https://www.united.com/en/us/fly/travel/airport/maps.html

flight_2023 <- read_parquet("Year=2023/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)

flight_1995 <- read_parquet("Year=1995/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN", "US", "NW", "CO") &
           (Origin %in% hubs | Dest %in% hubs)) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest)


# ghost flights!
count_df |> group_by(airline, airport) |> summarize(total = sum(n)) |> arrange(-total)

url <- "https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_the_United_States"
raw_tables <- rvest::read_html(url) |> rvest::html_table()
large_airports <- raw_tables[[2]] |> janitor::clean_names()
mid_airports <- raw_tables[[3]] |> janitor::clean_names()
airports_vec <- c(large_airports$iata_code, mid_airports$iata_code) |> unique()

count_df <- flight_2023 |> summarize_count(airports = airports_vec)
count_df_net <- count_df |>
  pivot_wider(names_from = type, values_from = n) |>
  mutate(net = dep + arr)

count_df |>
  filter(airport %in% setdiff(large_airports$iata_code, aa_hubs)) |>
  filter(airline == "AA") |>
  plot_dep_arv_pattern() +
  ggtitle("AA for large airports in 2023")

count_df |>
  filter(airport %in% setdiff(mid_airports$iata_code, aa_hubs)) |>
  filter(airline == "AA") |>
  plot_dep_arv_pattern() +
  ggtitle("AA for mid airports in 2023")
ggsave(filename = here::here("figures/07-SH-aa-hubs.png"), height = 10, width = 15, bg = "white")

count_df |>
  filter(airport %in% setdiff(large_airports$iata_code, ua_hubs)) |>
  filter(airline == "UA") |>
  plot_dep_arv_pattern() +
  ggtitle("UA for large airports in 2023")

count_df |>
  filter(airport %in% setdiff(mid_airports$iata_code, ua_hubs)) |>
  filter(airline == "UA") |>
  plot_dep_arv_pattern() +
  ggtitle("UA for mid airports in 2023")
ggsave(filename = here::here("figures/07-SH-ua-hubs.png"), height = 10, width = 15, bg = "white")

count_df |>
  filter(airport %in% setdiff(large_airports$iata_code, dl_hubs)) |>
  filter(airline == "DL") |>
  plot_dep_arv_pattern() +
  ggtitle("DL for large airports in 2023")

count_df |>
  filter(airport %in% setdiff(mid_airports$iata_code, dl_hubs)) |>
  filter(airline == "DL") |>
  plot_dep_arv_pattern() +
  ggtitle("DL for mid airports in 2023")
ggsave(filename = here::here("figures/07-SH-dl-hubs.png"), height = 10, width = 15, bg = "white")


