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
  summarize_count(airports = c("AUS", "DFW"), block_size = 30) |> 
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |> 
   mutate(airline_airport = factor(airline_airport,
                                  labels = c("American / Austin-Bergstrom International Airport (AUS)",
                                             "American / Dallas/Fort Worth International Airport (DFW)")))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
two_df |> plot_dep_arv_pattern() + 
   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour") + 
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) + 
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) + 
  xlab("Binned time (10 minute intervals)") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10)) 
ggsave(filename = "figures/14-AUS-DFW.png", height = 8, width = 20, unit = "cm", bg = "white")

###############################################################
aa_hubs <- c("DFW", "CLT", "ORD", "PHX", "LGA", "LAX", "AUS", "ATL","MCI", "PDX", "ALB", "OAK")
airport_df <- tibble(
  aa_hubs=aa_hubs, 
  airline_airport = c(
    "American / Dallas/Fort Worth International Airport (DFW)",
    "American / Charlotte Douglas International Airport (CLT)",
    "American / Chicago O'Hare International Airport (ORD)",
    "American / Phoenix Sky Harbor International Airport (PHX)",
    "American / LaGuardia Airport (LGA)",
    "American / Los Angeles International Airport (LAX)",
    "American / Austin-Bergstrom International Airport (AUS)",
    "American / Hartsfield-Jackson Atlanta International Airport (ATL)",
    "American / Kansas City International Airport (MCI)",
    "American / Portland International Airport (PDX)",
    "American / Albany International Airport (ALB)",
    "American / Oakland International Airport (OAK)"

  ))

aa_df <- flight_df_raw |>
  filter(Reporting_Airline == "AA", Year == 2017, (Origin %in% aa_hubs | Dest %in% aa_hubs)) |>
  summarize_count(airports = aa_hubs, block_size = 10) |> 
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |> 
  select(-airline_airport) |> 
  left_join(airport_df, by = c("airport" = "aa_hubs")) |> 
  mutate(airline_airport = factor(airline_airport, levels = airport_df$airline_airport))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
aa_df |> plot_dep_arv_pattern() + 
   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour") + 
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2, dir = "v") + 
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) + 
  xlab("Binned time (10 minute intervals)") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10)) 
ggsave(filename = "figures/14-aa-all.png", height = 18, width = 20, unit = "cm", bg = "white")
     
######################################################################
  aaairport_df <- tibble(
  aa_hubs=aa_hubs, 
  airline_airport = c(
    "Delta Air Lines / Dallas/Fort Worth International Airport (DFW)",
    "Delta Air Lines / Charlotte Douglas International Airport (CLT)",
    "Delta Air Lines / Chicago O'Hare International Airport (ORD)",
    "Delta Air Lines / Philadelphia International Airport (PHL)",
    "Delta Air Lines / Phoenix Sky Harbor International Airport (PHX)",
    "Delta Air Lines / Los Angeles International Airport (LAX)",
    "Delta Air Lines / Ronald Reagan Washington National Airport (DCA)",
    "Delta Air Lines / John F. Kennedy International Airport (JFK)",
    "Delta Air Lines / Raleigh-Durham International Airport (RDU)",
    "Delta Air Lines / Kansas City International Airport (MCI)",
    "Delta Air Lines / Albany International Airport (ALB)",
    "Delta Air Lines / Omaha Eppley Airfield (OMA)"
  ))
dl_df <- flight_df_raw |>
  filter(Reporting_Airline == "DL", Year == 2017, (Origin %in% aa_hubs | Dest %in% aa_hubs)) |>
  summarize_count(airports = aa_hubs, block_size = 10) |> 
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |> 
  select(-airline_airport) |> 
  left_join(airport_df, by = c("airport" = "aa_hubs")) |> 
  mutate(airline_airport = factor(airline_airport, levels = airport_df$airline_airport))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
dl_df |> plot_dep_arv_pattern() + 
   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour") + 
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2, dir=  "v") + 
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) + 
  xlab("Binned time (10 minute intervals)") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10)) 
ggsave(filename = "figures/14-dl-aa-hubs.png", height = 18, width = 20, unit = "cm", bg = "white")
      