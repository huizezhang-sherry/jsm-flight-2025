library(arrow)
library(tidyverse)
library(usmap)
library(sf)

setwd("/Users/smc4578/Library/CloudStorage/Box-Box/Research/jsm-flight-2025/")

us_map_sf <- us_map(regions = 'states')
flight_df <- read_parquet("Year=2024/data_0.parquet")
airports <- read_csv("data/airports.csv")
all_routes <- flight_df |>
  select(Reporting_Airline, Tail_Number, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  mutate(DepTime = as_datetime(paste0(FlightDate, "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0(FlightDate, "-", ArrTime, "-00"))) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(FlightDate, Reporting_Airline, Tail_Number),
               names_to = c("type", ".value"), names_sep = "_" ) |>
  arrange(Reporting_Airline, Tail_Number, time)

missing_airports <- all_routes |>
  left_join(airports, by = c("airport" = "ident")) 

missing_airports <- missing_airports |>
  filter(is.na(x)) |>
  pull(airport) |>
  unique()

all_routes2 <- all_routes |>
  left_join(airports, by = c("airport" = "ident")) |>
  filter(!airport %in% missing_airports) |> # for some reasons these two airports are not available
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(st_crs(usmap::us_map())) |>
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) |>
  as_tibble() |>
  filter(between(x, -3000000, 3000000), y < 700000) # better filter before the coordinate transformation

all_routes2_count_df_big4 <- all_routes2 |>
  group_by(Reporting_Airline) |>
  count(airport) |>
  left_join(all_routes2 |> select(airport:y) |> distinct()) |>
  mutate(Reporting_Airline = factor(Reporting_Airline, c("OO", "UA", "WN", "AA", "DL",
                                                         "MQ", "F9", "G4", "NK", "B6",
                                                         "AS", "9E", "OH", "YX", "HA"))) |>
  filter(Reporting_Airline %in% c("UA", "WN", "AA", "DL")) |>
  group_by(Reporting_Airline) |>
  mutate(`Route Count` =  50 * n/sum(n),
         Reporting_Airline = factor(Reporting_Airline, 
                                    levels = c("AA", "DL","UA", "WN"),
                                    labels = c("American Airlines",
                                               "Delta Airlines",
                                               "United Airlines",
                                               "Southwest Airlines"),
                                    ordered = T))

# all_routes2_count_df |>
#   ggplot(aes(x = size)) +
#   geom_histogram() +
#   facet_wrap(vars(Reporting_Airline), scales = "free", nrow = 5, dir = "v")

# all_routes2_count_df |>
#   ggplot() +
#   geom_sf(data = us_map_sf, color = "white", fill = "grey90") +
#   geom_point(aes(x = x, y = y, size = size), color = "red") +
#   geom_path(aes(x = x, y = y), alpha = 0.1, color = "red") +
#   theme_void() +
#   facet_wrap(vars(Reporting_Airline), nrow = 5, dir = "v")
# ggsave(filename = here::here("figures/airline-2024.png"), height = 10, width = 10, bg = "white")

all_routes2_count_df_big4 |>
  ggplot() +
  geom_sf(data = us_map_sf, color = "white", fill = "grey90") +
  geom_point(aes(x = x, y = y, size = `Route Count`), color = "red") +
  geom_path(aes(x = x, y = y), alpha = 0.1, color = "red") +
  theme_void() +
  facet_wrap(vars(Reporting_Airline), nrow = 2, dir = "v")
ggsave(filename = here::here("figures/airline-2024-big-4.png"), height = 6, width = 10, bg = "white")
