library(arrow)
library(tidyverse)
library(patchwork)
library(sf)
source(here::here("figures/00-SH-shared-functions.R"))
flight_df_2024 <- read_parquet("Year=2024/data_0.parquet")
flight_df <- read_parquet("Year=2023/data_0.parquet")
flight_df2 <- read_parquet("Year=2022/data_0.parquet")
flight_df_2021 <- read_parquet("Year=2021/data_0.parquet")
airports <- read_csv("data/airports.csv")

# 2022 christmas period is also a good one: https://en.wikipedia.org/wiki/2022_Southwest_Airlines_scheduling_crisis
res <- flight_df2 |>
  filter(Month == 12, DayofMonth > 20, Reporting_Airline == "WN") |>
  group_by(DayofMonth) |>
  count(Cancelled) |>
  mutate(prop = n/sum(n))

christmas_2022 <- flight_df2 |> filter(Month == 12, DayofMonth > 20)

ch22_pairs <- christmas_2022 |> group_by(Origin, Dest) |> summarise(delay = mean(DepDelayMinutes, na.rm = TRUE))
flight_df2 |> filter(Reporting_Airline == "WN", FlightDate == as.Date("2022-12-25")) |> count(DepartureDelayGroups)
ch22_pairs |>
  ggplot(aes(x = delay)) +
  geom_density()


christmas_2 <- christmas_2022 |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN")) |>
  mutate(DepTime = as_datetime(paste0(FlightDate, "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0(FlightDate, "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, Tail_Number, FlightDate,DepTime, ArrTime, DepDelay, ArrDelay,
         Origin, Dest, DepartureDelayGroups) |>
  filter(ArrDelay < 1000)

# the arrival delay doesn't show much pattern
christmas_2 |>
  ggplot(aes(x = DepTime, y = ArrDelay, color = Reporting_Airline,
             group = Tail_Number)) +
  geom_line(alpha = 0.4) +
  facet_wrap(vars(Reporting_Airline))

####################################################################################
####################################################################################
# EDA on FAA system outage
# a case study on 2023 Jan 11 FAA system outage: https://en.wikipedia.org/wiki/2023_FAA_system_outage
flight_df |> filter(Origin == "JFK", FlightDate == as.Date("2023-01-11")) |> count(DepartureDelayGroups)
flight_df |> filter(Origin == "ORD", FlightDate == as.Date("2023-01-11")) |> count(DepartureDelayGroups)
flight_df |> filter(Origin == "CLT", FlightDate == as.Date("2023-01-11")) |> count(DepartureDelayGroups)
flight_df |> filter(Origin == "DFW", FlightDate == as.Date("2023-01-11")) |> count(DepartureDelayGroups)

outage_df <- flight_df |>
  filter(FlightDate == as.Date("2023-01-11")) |>
  filter(!is.na(Tail_Number)) |>
  filter(Cancelled == 0)


cancel_df <- flight_df |>
  filter(FlightDate == as.Date("2023-01-11")) |>
  filter(!is.na(Tail_Number)) |>
  filter(Cancelled == 1)
# There are also cancelled flight
outage_df |> count(Cancelled)
count_df <- outage_df |> count(Reporting_Airline, Tail_Number, sort = TRUE)


outage_df |> filter(Tail_Number == "N7889A") |>
  mutate(DepTime = as_datetime(paste0("2023-01-11", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2023-01-11", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate,DepTime, ArrTime, DepDelay, ArrDelay,
         Origin, Dest, DepartureDelayGroups) |>
  arrange(DepTime)


delay_df <- outage_df |>
  mutate(DepTime = as_datetime(paste0("2023-01-11", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2023-01-11", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, Tail_Number, FlightDate,DepTime, ArrTime, DepDelay, ArrDelay,
         Origin, Dest, DepartureDelayGroups) |>
  arrange(DepTime) |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"))

chain_tn <- delay_df |> count(Reporting_Airline, Tail_Number) |> filter(n >= 5) |> pull(Tail_Number)
delay_chain_df <- delay_df |>
  filter(Tail_Number %in% chain_tn) |>
  arrange(Reporting_Airline, Tail_Number, DepTime)

# this shows the distribution of departure delay by airport
delay_chain_df |>
  ggplot(aes(x = DepDelay, fill = Reporting_Airline)) +
  geom_density(alpha = 0.1) +
  facet_wrap(vars(Reporting_Airline))

####################################################################################
####################################################################################
# look at the delay performance of each major airline on the map
# delta is doing well
outrage_2023 <- flight_df |>
  filter(Reporting_Airline %in% c("AA", "WN", "DL", "UA"), FlightDate == as.Date("2023-01-11")) |>
  group_by(Reporting_Airline, Origin, Dest) |>
  summarise(delay = mean(DepDelayMinutes, na.rm = TRUE))

# the distribution also doesn't show much interesting
outrage_2023 |>
  ggplot(aes(x = delay, group = Reporting_Airline)) +
  geom_density()

all <- outrage_2023 |>
  rename(dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(Reporting_Airline, delay),
               names_to = c("type", ".value"), names_sep = "_" )

nodes <- all |>
  filter(type == "dep") |>
  group_by(Reporting_Airline, airport) |>
  summarise(delay = mean(delay, na.rm = TRUE))

missing_airports <- all |>
  left_join(airports, by = c("airport" = "ident")) |>
  filter(is.na(x)) |>
  pull(airport) |>
  unique()

all_routes2 <- all  |>
  left_join(airports |> select(ident, x, y), by = c("airport" = "ident")) |>
  filter(!airport %in% missing_airports) |> # for some reasons these two airports are not available
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(st_crs(usmap::us_map())) |>
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) |>
  as_tibble() |>
  filter(between(x, -3000000, 3000000), y < 700000) # better filter before the coordinate transformation

nodes_df <- nodes |> left_join(airports |> select(ident, x, y), by = c("airport" = "ident")) |>
  filter(!airport %in% missing_airports) |> # for some reasons these two airports are not available
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(st_crs(usmap::us_map())) |>
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) |>
  as_tibble() |>
  filter(between(x, -3000000, 3000000), y < 700000) |> # better filter before the coordinate transformation
  mutate(delay_cat = case_when(
    delay < 15 ~ 1,
    between(delay, 16, 60) ~ 2,
    between(delay, 61, 120) ~ 3,
    between(delay, 121, 180) ~ 4,
    TRUE ~ 5))

us_map_sf <- usmap::us_map() |>
  filter(!full %in% c("Alaska", "Hawaii")) |>
  rmapshaper::ms_simplify()

all_routes2 |>
  ggplot() +
  geom_sf(data = us_map_sf, color = "white", fill = "grey90") +
  geom_path(aes(x = x, y = y), alpha = 0.5, color = "white", size = 0.1) +
  geom_point(data = nodes_df |> filter(delay < 300), aes(x = x, y = y, color = delay)) +
  theme_void() +
  scale_color_distiller(palette = "YlOrRd", name = "Delay (min)", direction = 1) +
  facet_wrap(vars(Reporting_Airline), dir = "v")

####################################################################################
####################################################################################
# look at departure/ arrival pattern of major hubs
dt <- flight_df |>
  filter(Reporting_Airline %in% c("AA", "WN", "DL", "UA"), FlightDate == as.Date("2023-01-11")) |>
  summarize_count()
good_pairs <- dt |> count(airline_airport) |> filter(n > 100) |> pull(airline_airport)

dt |>
  filter(airline_airport %in% good_pairs) |>
  plot_dep_arv_pattern() +
  ggtitle("airport/airline pair on 2023-01-11")

flight_df |>
  filter(FlightDate == as.Date("2023-01-11")) |>
  filter(Reporting_Airline == "AA", (Origin == "DFW" | Dest == "DFW")) |>
  summarize_count(airports = "DFW") |>
  plot_dep_arv_pattern() +
  ggtitle("DFW - AA on 2023-01-11")


flight_df_2021 |>
  filter(Month == 2, between(DayofMonth, 15, 16), Reporting_Airline == "AA",
         (Origin == "DFW" | Dest == "DFW")) |>
  summarize_count(airports = "DFW") |>
  plot_dep_arv_pattern() +
  ggtitle("DFW/ AA on 2021 Feb 15-16")


####################################################################################
####################################################################################
# MSP storm at the end of March 2024
p1 <- flight_df_2024 |>
  filter(Month == 3, between(DayofMonth, 23, 24), Reporting_Airline == "DL",
         (Origin == "MSP" | Dest == "MSP")) |>
  summarize_count(airports = "MSP") |>
  plot_dep_arv_pattern() +
  ggtitle("MSP - DL on 2024 Mar 23-24")

p2 <- flight_df_2024 |>
  filter(Month == 3, between(DayofMonth, 01, 20), Reporting_Airline == "DL",
         (Origin == "MSP" | Dest == "MSP")) |>
  summarize_count(airports = "MSP") |>
  plot_dep_arv_pattern() +
  ggtitle("MSP - DL on 2024 Mar 01-20")
(p1 / p2) + plot_layout(guides = 'collect')

####################################################################################
####################################################################################
# 2022 Christmas period
flight_df2 |>
  filter(Month == 12, between(DayofMonth, 15, 25), Reporting_Airline %in% c("AA", "DL", "UA")) |>
  summarize_count() |>
  plot_dep_arv_pattern() +
  facet_grid(airport ~ airline) +
  ggtitle("airport/airline pair on 2022-12-15 to 25")




# March 24th (Sun), 2024,
# Portland - Feb 2 - 13, 2021
