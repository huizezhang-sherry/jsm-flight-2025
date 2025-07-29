library(arrow)
library(tidyverse)
library(patchwork)
source(here::here("scripts/00-SH-shared-functions.R"))
flight_df <- read_parquet("Year=2017/data_0.parquet")

hub_df <- read_csv(here::here("data/hub_status_2017.csv")) |> select(-...1)

airport_vec <- hub_df |> pull(dest)

# flight_hubs_spokes <- flight_df |>
#   filter(Reporting_Airline %in% c("DL", "AA", "WN", "UA")) |>
#   #dplyr::filter((Origin == airport_vec | Dest == airport_vec)) |>
#   mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
#          weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
#          DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
#          ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
#   select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
#   rename(dep_time = DepTime, arr_time = ArrTime,
#          dep_airport = Origin, arr_airport = Dest,
#          airline = Reporting_Airline) |>
#   pivot_longer(cols = -c(FlightDate, airline),
#                names_to = c("type", ".value"), names_sep = "_") |>
#   dplyr::filter(airport %in% airport_vec) |>
#   arrange(time)
#
# binned_data <- flight_hubs_spokes |>
#   dplyr::filter(!is.na(time)) |>
#   mutate(hour_minute = format(time, "%H:%M"),
#          time_numeric = as.numeric(difftime(time, as.POSIXct("2017-01-01 00:00:00", tz = "UTC"), units = "mins"))) |>
#   group_by(airline, airport, type, time_numeric) |>
#   summarise(n = n(), .groups = "drop")

flight_hubs_spokes <- flight_df |>
  filter(Reporting_Airline %in% c("DL", "AA", "WN", "UA")) |>
  summarize_count(block_size = 30, airports = airport_vec)

binned_data <- flight_hubs_spokes |>
  complete(airline, airport, type, block, fill = list(n = 0)) |>
  mutate(airline_airport = paste(airline, airport, sep = "/ "))

# Fit a smooth spline for each airport and type

calc_smooth <- function(data, spar = 0.5){
  res <- smooth.spline(data$block, data$n, spar = spar)
  tibble(x = res$x, fitted = res$y)
}

splines_df <- binned_data |>
  mutate(n = abs(n)) |>
  nest(data = -c(airline, airport, type)) |>
  rowwise() |>
  #filter(nrow(data) > 48) |>
  mutate(smooth_res = list(calc_smooth(data, spar = 0.05))) |>
  unnest(smooth_res) |>
  select(-data) |>
  mutate(block = as_datetime(x)) |>
  select(-x)


# dt <-  splines_df |>
#   filter(airline == "UA") |>
#   filter((airport %in% airports)) |>
#   mutate(fitted = ifelse(type == "dep", fitted, -fitted))
#
# p2 <- flight_df |>
#   filter(Reporting_Airline %in% c("UA")) |>
#   filter((Origin %in% airports| Dest %in% airports)) |>
#   summarize_count(airports = airports) |>
#   ggplot(aes(x = block, y = n, color = type, fill = type)) +
#   geom_col() +
#   facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
#   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
#   theme_minimal() +
#   xlab("Time of the day") +
#   ylab("Count")
#
# p_smooth <- dt |>
#   ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
#   geom_line() +
#   facet_wrap(vars(airport), ncol = 1, scale = "free_y") +
#   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
#   theme_minimal() +
#   xlab("Time of the day") +
#   ylab("Standardized spline fit")
#
# (p2 | p_smooth) + plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')


calc_fft <- function(dt){
  signal <- dt$fitted
  n <- length(signal)

  fft_result <- fft(signal) # actually doing fft
  modulus <- Mod(fft_result)[1:(n*0.5)] # amplitudes
  freqs <- (1:(n*0.5)) / n # frequencies
  periods_in_minutes <- 1 / freqs

  tibble(
    period_mins = periods_in_minutes,
    amplitude = modulus
  ) |>
    dplyr::filter(is.finite(period_mins), period_mins <= 1440) # up to 24 hours
}

fft_all <- splines_df |>
  nest(data = -c(airline, airport, type)) |>
  rowwise() |>
  mutate(fft_res = list(calc_fft(data))) |>
  select(-data) |>
  unnest(fft_res)

# Get entropy!
entropy_df <- fft_all |>
  group_by(airline, airport, type) %>%
  summarise(prob = amplitude^2 / sum(amplitude^2)) %>%
  summarise(entropy = sum(-prob*log(prob), na.rm = T)) |>
  pivot_wider(names_from = type, values_from = entropy) |>
  left_join(hub_df, by = c("airport" = "dest")) |>
  filter(arr != 0)

################################################################
################################################################
#airports <- c("TUS", "AUS", "DEN", "PHX", "HNL", "DFW", "CLT", "IAH", "PBI", "MEM", "DCA", "MCI")
airports <-  ua_hubs#c("ORD", "DEN", "IAH", "LAX", "EWR")
smallest_ten <- entropy_df |> filter(airline == "UA") |> mutate(a= sum(arr + dep)) |> arrange(a) |> head(3)
airports <- c(smallest_ten$airport, "ORD", "DEN", "IAH")
entropy_two <- entropy_df |> filter(airline == "UA") |> filter(airport %in% airports)
p1 <- entropy_df |>
  filter(airline == "UA") |>
  ggplot(aes(x = arr, y = dep)) +
  geom_point() +
  geom_point(data = entropy_two, color = "red", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  ggrepel::geom_label_repel(data = entropy_two,
    aes(label = airport), min.segment.length = 0) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlab("Total arrival entropy") +
  ylab("Total departure entropy")

p2 <- flight_df |>
  filter(Reporting_Airline %in% c("UA")) |>
  filter((Origin %in% airports| Dest %in% airports)) |>
  summarize_count(airports = airports) |>
  mutate(airline_airport = factor(airline_airport, levels = c("UA/ DEN", "UA/ IAH", "UA/ ORD",
                                                              "UA/ EWR", "UA/ LAX", "UA/ SFO"))) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Count")

dt <-  splines_df |>
  filter(airline == "UA") |>
  filter((airport %in% airports)) |>
  mutate(fitted = ifelse(type == "dep", fitted, -fitted)) |>
  mutate(airline_airport = as.factor(paste(airline, airport, sep = "/ "))) |>
  mutate(airline_airport = factor(airline_airport, levels = c("UA/ DEN", "UA/ IAH", "UA/ ORD",
                                                              "UA/ EWR", "UA/ LAX", "UA/ SFO")))

p_smooth <- dt |>
  ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
  geom_line() +
  facet_wrap(vars(airline_airport), ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Spline fit")

(p1 | p2 | p_smooth) + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
#

################################################################
################################################################
dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA") # Source https://news.delta.com/corporate-stats-and-facts
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA") # Source https://www.aa.com/i18n/customer-service/about-us/american-airlines-group.jsp and wikipedia
wn_hubs <- c("ATL", "BWI", "MDW", "DAL", "DEN", "HOU", "LAS", "LAX", "BNA", "OAK", "MCO", "PHX") # source https://en.wikipedia.org/wiki/Southwest_Airlines
# note southwest says they don't do the hub and spoke thing so i used wikipedia
ua_hubs <- c("ORD", "DEN", "GUM", "IAH", "LAX", "EWR", "SFO", "IAD") # Source https://www.united.com/en/us/fly/travel/airport/maps.html
hubs <- tibble(airline = "AA", airport = aa_hubs) |>
  bind_rows(tibble(airline = "DL", airport = dl_hubs)) |>
  bind_rows(tibble(airline = "WN", airport = wn_hubs)) |>
  bind_rows(tibble(airline = "UA", airport = ua_hubs)) |>
  group_by(airline) |>
  slice_head(n = 10) |>
  mutate(airlineairport = paste(airline, airport, sep = "_"))


p3 <- entropy_df |>
  mutate(hub_type = factor(hub_type, levels = c("Large", "Medium", "Small", "Nonhub"))) |>
  ggplot(aes(x = arr, y = dep, color = hub_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point() +
  # geom_point(
  #   data = entropy_df |> mutate(airlineairport = paste(airline, airport, sep = "_")) |>
  #     filter(airlineairport %in% hubs$airlineairport),
  #   color = "red") +
  # ggrepel::geom_label_repel(
  #   data = entropy_df |> mutate(airlineairport = paste(airline, airport, sep = "_")) |>
  #     filter(airlineairport %in% hubs$airlineairport),
  #   aes(label = airport), min.segment.length = 0) +
  #ggforce::geom_mark_hull(expand = unit(2, "mm"), concavity = 10 ) +
  facet_wrap(vars(airline)) +
  scale_color_brewer(palette = "Dark2", name = "Hub type") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlab("Total arrival entropy") +
  ylab("Total departure entropy")
p3
#ggsave(p3, filename = here::here("figures/11-SH-four-airlines.png"), height = 10, width = 10 , bg = "white")

################################################################
################################################################
smallest_ten <- entropy_df |> filter(airline == "AA") |> mutate(a= sum(arr + dep)) |> arrange(a) |> head(3)
airports <- c(smallest_ten$airport, "DFW", "CLT", "ORD")
entropy_two <- entropy_df |> filter(airline == "AA") |> filter(airport %in% airports)
p1 <- entropy_df |>
  filter(airline == "AA") |>
  ggplot(aes(x = arr, y = dep)) +
  geom_point() +
  geom_point(data = entropy_two, color = "red", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  ggrepel::geom_label_repel(data = entropy_two,
                            aes(label = airport), min.segment.length = 0) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlab("Total arrival entropy") +
  ylab("Total departure entropy")

p2 <- flight_df |>
  filter(Reporting_Airline %in% c("AA")) |>
  filter((Origin %in% airports| Dest %in% airports)) |>
  summarize_count(airports = airports) |>
  mutate(airline_airport = factor(airline_airport, levels = c("AA/ LGA", "AA/ BOS", "AA/ LAX",
                                                              "AA/ DFW", "AA/ CLT", "AA/ ORD"))) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Count")

dt <-  splines_df |>
  filter(airline == "AA") |>
  filter((airport %in% airports)) |>
  mutate(fitted = ifelse(type == "dep", fitted, -fitted)) |>
  mutate(airline_airport = as.factor(paste(airline, airport, sep = "/ "))) |>
  mutate(airline_airport = factor(airline_airport, levels = c("AA/ LGA", "AA/ BOS", "AA/ LAX",
                                                              "AA/ DFW", "AA/ CLT", "AA/ ORD")))

p_smooth <- dt |>
  ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
  geom_line() +
  facet_wrap(vars(airline_airport), ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Spline fit")

(p1 | p2 | p_smooth) + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')
