library(tidyverse)
library(usmap)
library(sf)
library(arrow)

us_map_sf <-  us_map(regions = 'states') |> filter(!abbr %in% c("AK", "HI", "PR"))

airports <- read_csv("data/airports.csv") |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(crs = st_crs(us_map_sf)) |>
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) |>
  filter(between(x, -3000000, 3000000), y < 700000) |>
  rename(airport = ident) |>
  as_tibble() |>
  select(airport, x, y)

write_csv(airports, file = here::here("data/airports_in_cont_us.csv"))
airports <- read_csv('data/airports_in_cont_us.csv')

st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI

flight_df_raw <- read_parquet("Year=2017/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"),
         OriginStateFips %in% st_fips,
         DestStateFips %in% st_fips)

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

write_csv(all_routes, file = here::here("data/all_routes.csv"))
write_csv(nodes_df, file = here::here("data/nodes_df.csv"))
write_csv(routes_df, file = here::here("data/routes_df.csv"))

fig1 <- ggplot() +
  geom_sf(data = us_map_sf, color = "grey70", fill = "grey90") +
  geom_line(data = routes_df, aes(x = x, y = y, group = id), alpha = 0.03, color = '#005f86') +
  geom_point(data = nodes_df, aes(x = x, y = y, size = n), alpha = 0.5, color = '#005f86') +
  theme_void() +
  facet_wrap(vars(reporting_airline), ncol = 2) +
  scale_size_continuous(name = "Number of Flights", labels = scales::label_comma()) +
  theme(legend.position = "bottom",text = element_text(colour = "black", size = 10))

ggsave(plot = fig1, filename = "figures/fig1.png",
       height = 12, width = 18, unit = "cm",
       bg = "white")
