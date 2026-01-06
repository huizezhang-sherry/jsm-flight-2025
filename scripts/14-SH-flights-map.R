library(tidyverse)
library(usmap)
library(sf)
library(arrow)

#flight_df_raw <- read_csv("data/flight_df.csv") |> select(-DepTime, -ArrTime)

airports_raw <- read_csv("data/airports.csv")
us_map_sf <-  us_map(regions = 'states') |> filter(!abbr %in% c("AK", "HI"))
airports <- airports_raw |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(crs = st_crs(us_map_sf)) |>
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) |>
  filter(between(x, -3000000, 3000000), y < 700000) |>
  rename(airport = ident) |>
  as_tibble() |>
  select(airport, x, y)

flight_df_raw <- read_parquet("Year=2017/data_0.parquet") |> 
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"))

all_routes <- flight_df_raw |> 
  select(Reporting_Airline, Tail_Number, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  mutate(DepTime = as_datetime(paste0(FlightDate, "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0(FlightDate, "-", ArrTime, "-00"))) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(FlightDate, Reporting_Airline, Tail_Number),
               names_to = c("type", ".value"), names_sep = "_" ) |>
  arrange(Reporting_Airline, Tail_Number, time)

nodes_df <- flight_df_raw |>
  select(Reporting_Airline, DepTime, ArrTime, Origin, Dest) |>
  janitor::clean_names() |>
  select(-dep_time, -arr_time) |>
  mutate(id = row_number()) |>
  rename(dep_airport = origin, arr_airport = dest) |>
  pivot_longer(cols = -c(reporting_airline, id),
               names_to = "type", values_to = "airport") |>
  group_by(reporting_airline) |>
  count(airport) |>
  #mutate(n = n / 100000) |>
  mutate(
    reporting_airline = as.factor(reporting_airline),
    reporting_airline = fct_relevel(reporting_airline, c("AA", "DL", "WN", "UA")),
    reporting_airline = fct_recode(
    reporting_airline,
    "American Airlines" = "AA", "Delta Air Lines" = "DL",
    "Southwest Airlines" = "WN", "United Airlines" = "UA")) |>
  left_join(airports, by = "airport")

routes_df <- flight_df_raw |>
  janitor::clean_names() |>
  distinct(reporting_airline, origin, dest) |>
  mutate(id = row_number()) |>
  pivot_longer(cols = -c(reporting_airline, id),
               names_to = "type", values_to = "airport") |>
  mutate(
    reporting_airline = as.factor(reporting_airline),
    reporting_airline = fct_relevel(reporting_airline, c("AA", "DL", "WN", "UA")),
    reporting_airline = fct_recode(
      reporting_airline,
      "American Airlines" = "AA", "Delta Air Lines" = "DL",
      "Southwest Airlines" = "WN", "United Airlines" = "UA")) |>
  left_join(airports, by = "airport")


# missing_airports <- nodes_df |>
#   filter(is.na(x)) |>
#   pull(airport) |>
#   unique()
#
# nodes_df2 <- nodes_df |>
#   filter(!airport %in% missing_airports)

ggplot() +
  geom_sf(data = us_map_sf, color = "white", fill = "grey90") +
  geom_point(data = nodes_df, aes(x = x, y = y, size = n), color = "red") +
  geom_line(data = routes_df, aes(x = x, y = y, group = id), alpha = 0.03, color = "red") +
  theme_void() + 
  facet_wrap(vars(reporting_airline), ncol = 2) + 
  scale_size_continuous(name = "Num. of Flights", labels = scales::label_comma()) + 
  theme(legend.position = "bottom",text = element_text(colour = "black", size = 10))
ggsave(filename = "figures/14-flights-map.png", height = 14, width = 20, unit = "cm", bg = "white")

 source('scripts/00-SH-shared-functions.R')
two_df <- flight_df_raw |>
  filter(Reporting_Airline == "AA", Year == 2017,
         (Origin %in% c("AUS", "DFW") | Dest %in% c("AUS", "DFW"))) |>
  summarize_count(airports = c("AUS", "DFW"), block_size = 1) |> 
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |> 
   mutate(airline_airport = factor(airline_airport,
                                  labels = c("American / Austin-Bergstrom International Airport (AUS)",
                                             "American / Dallas/Fort Worth International Airport (DFW)")))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
two_df |> plot_dep_arv_pattern() + 
   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "6 hour") + 
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) + 
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) + 
  xlab("Binned time (1 minute intervals)") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10), aspect.ratio = 0.2 ) 
ggsave(filename = "figures/14-AUS-DFW.png", height = 8, width = 2-0, unit = "cm", bg = "white")
   