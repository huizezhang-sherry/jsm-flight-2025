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

write_csv(airports, file = here::here("data/airports_in_cont_us.csv"))

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
  geom_point(data = nodes_df, aes(x = x, y = y, size = n), color = '#005f86') +
  geom_line(data = routes_df, aes(x = x, y = y, group = id), alpha = 0.03, color = '#005f86') +
  theme_void() +
  facet_wrap(vars(reporting_airline), ncol = 2) +
  scale_size_continuous(name = "Number of Flights", labels = scales::label_comma()) +
  theme(legend.position = "bottom",text = element_text(colour = "black", size = 10))
ggsave(filename = "figures/14-flights-map.png", height = 14, width = 20, unit = "cm", bg = "white")

source('scripts/00-SH-shared-functions.R')
two_df <- flight_df_raw |>
  filter(Reporting_Airline == "AA", Year == 2017,
         (Origin %in% c("AUS", "DFW") | Dest %in% c("AUS", "DFW"))) |>
  summarize_count(airports = c("AUS", "DFW"), block_size = 10) |>
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
   mutate(airline_airport = factor(airline_airport,
                                  labels = c("American / Austin-Bergstrom International Airport (AUS)",
                                             "American / Dallas/Fort Worth International Airport (DFW)")))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
two_df |> plot_dep_arv_pattern() +
  scale_x_datetime(labels = function(x) {
    lab <- format(x, "%I:%M %p")
    lab <- sub("^0", "", lab)   # remove leading zero
    tolower(lab)               # am/pm in lowercase
  },
                   date_breaks = "4 hour") +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  xlab("Binned time (10 minute intervals)") +
  ylab("Annual Flight Count") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10))
ggsave(filename = "figures/14-AUS-DFW.png", height = 8, width = 20, unit = "cm", bg = "white")

###############################################################
aa_hubs <- c("DFW", "CLT", "ORD", "PHL", "LGA", "LAX", "DCA", "AUS", "ATL", "PDX", "EGE", "ALB", "OAK", "AMA")
airport_df <- tibble(
  aa_hubs=aa_hubs,
  airline_airport = c(
    "American / Dallas/Fort Worth International Airport (DFW)",
    "American / Charlotte Douglas International Airport (CLT)",
    "American / Chicago O'Hare International Airport (ORD)",
    "American / Philadelphia International Airport (PHL)",
    "American / LaGuardia Airport (LGA)",
    "American / Los Angeles International Airport (LAX)",
    "American / Ronald Reagan Washington National Airport (DCA)",
    "American / Austin-Bergstrom International Airport (AUS)",
    "American / Hartsfield-Jackson Atlanta International Airport (ATL)",
    "American / Portland International Airport (PDX)",
    "American / Eagle County Regional Airport (EGE)",
    "American / Albany International Airport (ALB)",
    "American / Oakland International Airport (OAK)",
     "American / Rick Husband Amarillo International Airport (AMA)"
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
   scale_x_datetime(labels = function(x) {
     lab <- format(x, "%I:%M %p")
     lab <- sub("^0", "", lab)   # remove leading zero
     tolower(lab)               # am/pm in lowercase
   }, date_breaks = "4 hour") +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2, dir = "v") +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  xlab("Binned time (10 minute intervals)") +
  ylab("Annual Flight Count") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10))
ggsave(filename = "figures/14-aa-all.png", height = 18, width = 20, unit = "cm", bg = "white")


######################################################################f
entropy_df <- readRDS('./data-raw/entropy_df_11-SH') # fft

entropy_american <- entropy_df |>
  filter(airline == "American") |>
  filter(airport %in% airports$airport) |>  # remove noncontinental: see line 10 for the `airports` object
  ungroup() |>
  filter(!airport %in% c("SJU", "STT")) # remove territories

highlight_df <- entropy_american |> filter(arr < 2.07, dep < 3.1) |>
  mutate(group = ifelse(airport %in% aa_hubs, "highlighted", "others")) |>
  bind_rows(entropy_american |> filter(airport %in% aa_hubs) |> filter(arr > 2.1) |>
    mutate(group = "highlighted"))

color_df <- c(highlighted = "black", others = "grey60")
entropy_american |>
  ggplot(aes(x = arr, y = dep)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(color = "#00a9b7", size = 2) +
  geom_point(data = highlight_df, aes(color = group), size = 2) +
  ggrepel::geom_text_repel(
    data = highlight_df, aes(label = airport, color = group), size = 3,
    max.overlaps = 6) +
  scale_x_continuous(breaks = seq(1.5, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(1.5, 4, by = 0.5)) +
  coord_cartesian(xlim = c(1.5, 4), ylim = c(1.5, 4)) +
  scale_color_manual(name = 'Type', values = color_df) +
  theme_minimal(base_size = 10) +
  theme(aspect.ratio = 1,
        legend.position = "none") +
  guides(shape = 'none') +
  xlab("Arrival entropy") +
  ylab("Departure entropy")
ggsave(filename = here::here("figures/14-american-entropy.png"),
       units = 'cm', width = 9, height = 9, bg = "white")


######################################################################
#   aaairport_df <- tibble(
#   aa_hubs=aa_hubs,
#   airline_airport = c(
#     "Delta Air Lines / Dallas/Fort Worth International Airport (DFW)",
#     "Delta Air Lines / Charlotte Douglas International Airport (CLT)",
#     "Delta Air Lines / Chicago O'Hare International Airport (ORD)",
#     "Delta Air Lines / Philadelphia International Airport (PHL)",
#     "Delta Air Lines / Phoenix Sky Harbor International Airport (PHX)",
#     "Delta Air Lines / Los Angeles International Airport (LAX)",
#     "Delta Air Lines / Ronald Reagan Washington National Airport (DCA)",
#     "Delta Air Lines / John F. Kennedy International Airport (JFK)",
#     "Delta Air Lines / Raleigh-Durham International Airport (RDU)",
#     "Delta Air Lines / Kansas City International Airport (MCI)",
#     "Delta Air Lines / Albany International Airport (ALB)",
#     "Delta Air Lines / Omaha Eppley Airfield (OMA)"
#   ))
# dl_df <- flight_df_raw |>
#   filter(Reporting_Airline == "DL", Year == 2017, (Origin %in% aa_hubs | Dest %in% aa_hubs)) |>
#   summarize_count(airports = aa_hubs, block_size = 10) |>
#   mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
#   select(-airline_airport) |>
#   left_join(airport_df, by = c("airport" = "aa_hubs")) |>
#   mutate(airline_airport = factor(airline_airport, levels = airport_df$airline_airport))

# color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
# dl_df |> plot_dep_arv_pattern() +
#    scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour") +
#   facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2, dir=  "v") +
#   scale_fill_manual(name = "Flight type", values = color_list) +
#   scale_color_manual(name = "Flight type", values = color_list) +
#   xlab("Binned time (10 minute intervals)") +
#   theme(legend.position = "bottom", text = element_text(colour = "black", size = 10))
# ggsave(filename = "figures/14-dl-aa-hubs.png", height = 18, width = 20, unit = "cm", bg = "white")




######################################################################
hub_df <- read_csv(here::here("data/hub_status_2017.csv")) |> select(-...1)
nonhub_dest <- hub_df |> filter(hub_type == "Nonhub") |> pull(dest)
nonhub_vec <- c("AMA", "EGE")

nonhub_df <- tibble(
  aa_hubs=nonhub_vec,
  airline_airport = c(
    "American / Amarillo, TX",
    "American / Eagle, CO"

  ))

dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA") # Source https://news.delta.com/corporate-stats-and-facts
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA") # Source https://www.aa.com/i18n/customer-service/about-us/american-airlines-group.jsp and wikipedia
wn_hubs <- c("ATL", "BWI", "MDW", "DAL", "DEN", "HOU", "LAS", "LAX", "BNA", "OAK", "MCO", "PHX") # source https://en.wikipedia.org/wiki/Southwest_Airlines
# note southwest says they don't do the hub and spoke thing so i used wikipedia
ua_hubs <- c("ORD", "DEN", "GUM", "IAH", "LAX", "EWR", "SFO", "IAD") # Source https://www.united.com/en/us/fly/travel/airport/maps.html

hub_df |> filter(dest %in% aa_hubs)
hub_df |> filter(dest %in% dl_hubs)
hub_df |> filter(dest %in% wn_hubs)
hub_df |> filter(dest %in% ua_hubs)
hub_df |> filter(dest %in% c("AUS", "PDX"))
hub_df |> filter(dest %in% c("DFW", "CLT", "ORD", "PHX", "LGA", "LAX", "DCA", "AUS", "ATL", "PDX", "EGE", "ALB", "OAK", "AMA"))

nonhub_df <- flight_df_raw |>
  #filter(Reporting_Airline == "AA", Year == 2017, Origin %in% nonhub_dest | Dest %in% nonhub_dest) |>
  filter(Reporting_Airline == "AA", Year == 2017, Origin %in% nonhub_vec | Dest %in% nonhub_vec) |>
  summarize_count(airports = nonhub_vec, block_size = 10) |>
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
  select(-airline_airport) |>
  left_join(nonhub_df, by = c("airport" = "aa_hubs")) |>
  mutate(airline_airport = factor(airline_airport, levels = nonhub_df$airline_airport))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
nonhub_df |> plot_dep_arv_pattern() +
   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour") +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1, dir = "v") +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  xlab("Binned time (10 minute intervals)") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10))


