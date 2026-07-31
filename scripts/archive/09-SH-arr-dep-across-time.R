library(arrow)
library(tidyverse)
library(patchwork)
library(sf)
source(here::here("scripts/00-SH-shared-functions.R"))

flight_aa <- read_csv(here::here("data/AA_flights.csv"))
count_df_aa <- flight_aa |>
  group_split(year = year(FlightDate)) |>
  map_dfr(~.x |> summarize_count(airports = c("DFW", "CLT", "ORD", "MIA")), .id = "year")
write_csv(count_df_aa, file = here::here("data/flight_df_aa.csv"))

count_df_aa |>
  mutate(year = as.numeric(year) + 1994) |>
  filter(airline_airport == "AA/ DFW") |>
  plot_dep_arv_pattern() +
  facet_wrap(vars(year), ncol = 5) +
  ggtitle("Arrival/ Departure pattern for AA at DFW")
ggsave(filename = here::here("figures/09-SH-aa-dfw.png"), height = 10, width = 25, bg = "white")

count_df_aa |>
  mutate(year = as.numeric(year) + 1994) |>
  filter(airline_airport == "AA/ ORD") |>
  plot_dep_arv_pattern() +
  facet_wrap(vars(year), ncol = 5) +
  ggtitle("Arrival/ Departure pattern for AA at ORD")
ggsave(filename = here::here("figures/09-SH-aa-ord.png"), height = 10, width = 25, bg = "white")

count_df_aa |>
  mutate(year = as.numeric(year) + 1994) |>
  filter(airline_airport == "AA/ MIA") |>
  plot_dep_arv_pattern() +
  facet_wrap(vars(year), ncol = 5) +
  ggtitle("Arrival/ Departure pattern for AA at MIA")
ggsave(filename = here::here("figures/09-SH-aa-mia.png"), height = 10, width = 25, bg = "white")

count_df_aa |>
  mutate(year = as.numeric(year) + 1994) |>
  filter(airline_airport == "AA/ CLT") |>
  plot_dep_arv_pattern() +
  facet_wrap(vars(year), ncol = 5) +
  ggtitle("Arrival/ Departure pattern for AA at CLT")
ggsave(filename = here::here("figures/09-SH-aa-clt.png"), height = 10, width = 25, bg = "white")

flight_ua <- read_csv(here::here("data/UA_flights.csv"))
count_df_ua <- flight_ua |>
  group_split(year = year(FlightDate)) |>
  map_dfr(~.x |> summarize_count(airports = c("SFO", "ORD")), .id = "year")
write_csv(count_df_ua, file = here::here("data/flight_df_ua.csv"))

count_df_ua |>
  mutate(year = as.numeric(year) + 1994) |>
  filter(airline_airport == "UA/ ORD") |>
  plot_dep_arv_pattern() +
  facet_wrap(vars(year), ncol = 5) +
  ggtitle("Arrival/ Departure pattern for UA at ORD")
ggsave(filename = here::here("figures/09-SH-ua-ord.png"), height = 10, width = 25, bg = "white")


count_df_ua |>
  mutate(year = as.numeric(year) + 1994) |>
  filter(airline_airport == "UA/ SFO") |>
  plot_dep_arv_pattern() +
  facet_wrap(vars(year), ncol = 5) +
  ggtitle("Arrival/ Departure pattern for UA at SFO")
ggsave(filename = here::here("figures/09-SH-ua-sfo.png"), height = 10, width = 25, bg = "white")
