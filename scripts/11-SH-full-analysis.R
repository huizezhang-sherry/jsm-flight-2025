library(arrow)
library(tidyverse)
library(patchwork)
source(here::here("scripts/00-SH-shared-functions.R"))
flight_df <- read_parquet("Year=2017/data_0.parquet")

hub_df <- read_csv(here::here("data/hub_status_2017.csv")) |> select(-...1)

airport_vec <- hub_df |> head(100) |> pull(dest)

flight_hubs_spokes <- flight_df |>
  filter(Reporting_Airline %in% c("DL", "AA", "WN", "UA")) |>
  dplyr::filter((Origin == airport_vec | Dest == airport_vec)) |>
  mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
         weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
         DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) |>
  pivot_longer(cols = -c(FlightDate, airline),
               names_to = c("type", ".value"), names_sep = "_") |>
  dplyr::filter(airport %in% airport_vec) |>
  arrange(time)

binned_data <- flight_hubs_spokes |>
  dplyr::filter(!is.na(time)) |>
  mutate(hour_minute = format(time, "%H:%M"),
         time_numeric = as.numeric(difftime(time, as.POSIXct("2017-01-01 00:00:00", tz = "UTC"), units = "mins"))) |>
  group_by(airline, airport, type, time_numeric) |>
  summarise(n = n(), .groups = "drop")

# Fit a smooth spline for each airport and type
library(purrr)
splines_df <- binned_data |>
  nest(data = -c(airline, airport, type)) |>
  rowwise() |>
  filter(nrow(data) > 48) |>
  unnest() |>
  group_by(airline, airport, type) |>
  group_modify(~{
    fit <- smooth.spline(.x$time_numeric, .x$n, spar = 0.5)
    tibble(
      time_numeric = fit$x,
      fitted = fit$y,
      airport = unique(.x$airport),
      type = unique(.x$type)
    )
  }) |>
  ungroup() |>
  mutate(time = as.POSIXct("2017-01-01", tz = "UTC") + time_numeric * 60)

binned_data <- binned_data |>
  mutate(time = as.POSIXct("2017-01-01", tz = "UTC") + time_numeric * 60)

splines_df_std <- splines_df |>
  group_by(airline, airport, type) |>
  mutate(fitted = (fitted - min(fitted)) / (max(fitted) - min(fitted)))

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
    dplyr::filter(is.finite(period_mins), period_mins <= 1440) |>  # up to 24 hours
    mutate(airport = unique(dt$airport), type = unique(dt$type))
}

fft_all <- splines_df_std |>
  nest(-c(airline, airport, type)) |>
  rowwise() |>
  mutate(fft_res = list(calc_fft(data))) |>
  unnest(fft_res) |>
  ungroup()
# Get entropy!
entropy_df <- fft_all |>
  group_by(airline, airport, type) %>%
  summarise(prob = amplitude^2 / sum(amplitude^2)) %>%
  summarise(entropy = sum(-prob*log(prob), na.rm = T)) |>
  pivot_wider(names_from = type, values_from = entropy) |>
  left_join(hub_df, by = c("airport" = "dest"))

airports <- c("TUS", "AUS", "DEN", "PHX", "HNL", "DFW", "CLT", "IAH", "PBI", "MEM", "DCA", "MCI")
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
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Count")

dt <-  splines_df |>
  group_by(airline, airport, type) |>
  mutate(fitted = (fitted - min(fitted)) / (max(fitted) - min(fitted))) |>
  filter(airline == "AA") |>
  filter((airport %in% airports | airport %in% airports)) |>
  mutate(fitted = ifelse(type == "dep", fitted, -fitted))

p_smooth <- dt |>
  ggplot(aes(x = time, y = fitted, color = type, fill = type, group = type)) +
  geom_line() +
  facet_wrap(vars(airport), ncol = 2) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Standardized spline fit")

(p2 | p_smooth) + plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

(p1 | p2) + plot_layout(widths = c(0.3, 0.7))
#ggsave(filename = here::here("figures/11-SH-aa-aus-dfw.png"), height = 4, width = 12, bg = "white")


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
  ggplot(aes(x = arr, y = dep)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point() +
  geom_point(
    data = entropy_df |> mutate(airlineairport = paste(airline, airport, sep = "_")) |>
      filter(airlineairport %in% hubs$airlineairport),
    color = "red") +
  # ggrepel::geom_label_repel(
  #   data = entropy_df |> mutate(airlineairport = paste(airline, airport, sep = "_")) |>
  #     filter(airlineairport %in% hubs$airlineairport),
  #   aes(label = airport), min.segment.length = 0) +
  facet_wrap(vars(airline)) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlab("Total arrival entropy") +
  ylab("Total departure entropy")
p3
ggsave(p3, filename = here::here("figures/11-SH-four-airlines.png"), height = 8, width = 8 , bg = "white")

dl_airports <- c("JFK", "ATL", "MSP", "SLC", "DTW")
dl_jfk <- splines_df |>
  group_by(airline, airport, type) |>
  mutate(fitted = fitted / max(fitted)) |>
  filter(airline == "DL") |>
  filter((airport %in% dl_airports | airport %in% dl_airports)) |>
  mutate(fitted = ifelse(type == "arr", fitted, -fitted)) |>
  ggplot(aes(x = time, y = fitted, color = type, fill = type)) +
  geom_area() +
  facet_wrap(vars(airport), ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Standardized spline fit")

# we can decide whether to use the smoothed or non-smoothed data
p_dl <- flight_df |>
  filter(Reporting_Airline %in% c("DL")) |>
  filter((Origin %in% dl_airports | Dest %in% dl_airports)) |>
  summarize_count(airports = dl_airports) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Count")

p3 | p_dl

UA_airports <- c("DEN", "SFO", "ORD", "IAH", "IAD")
p_ua <- splines_df |>
  group_by(airline, airport, type) |>
  mutate(fitted = fitted / max(fitted)) |>
  filter(airline == "UA") |>
  filter((airport %in% UA_airports | airport %in% UA_airports)) |>
  mutate(fitted = ifelse(type == "arr", fitted, -fitted)) |>
  ggplot(aes(x = time, y = fitted, color = type, fill = type)) +
  geom_area() +
  facet_wrap(vars(airport), ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Standardized spline fit")


WN_airports <- c("STL", "HOU", "DEN")
p_wn <- splines_df |>
  group_by(airline, airport, type) |>
  mutate(fitted = fitted / max(fitted)) |>
  filter(airline == "WN") |>
  filter((airport %in% WN_airports | airport %in% WN_airports)) |>
  mutate(fitted = ifelse(type == "arr", fitted, -fitted)) |>
  ggplot(aes(x = time, y = fitted, color = type, fill = type)) +
  geom_area() +
  facet_wrap(vars(airport), ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Standardized spline fit")

AA_airports <- c("DFW", "CLT", "PHL", "MIA", "PHX", "LAX")
p_aa <- splines_df |>
  group_by(airline, airport, type) |>
  mutate(fitted = fitted / max(fitted)) |>
  filter(airline == "AA") |>
  filter((airport %in% AA_airports | airport %in% AA_airports)) |>
  mutate(fitted = ifelse(type == "arr", fitted, -fitted)) |>
  ggplot(aes(x = time, y = fitted, color = type, fill = type)) +
  geom_area() +
  facet_wrap(vars(airport), ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Standardized spline fit")

arr_dep_df <- flight_df |>
  filter(Reporting_Airline %in% c("DL", "AA", "WN", "UA")) |>
  filter((Origin %in% airport_vec | Dest %in% airport_vec)) |>
  summarize_count(airports = airport_vec) |>
  left_join(airport_df, by = c("airport" = "origin"))

arr_dep_df |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey95") +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "6 hour") +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Count")



