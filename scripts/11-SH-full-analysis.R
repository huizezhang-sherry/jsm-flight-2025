library(tidyverse)
library(patchwork)
source(here::here("scripts/00-SH-shared-functions.R"))
flight_df <- read_parquet("Year=2017/data_0.parquet")

hub_df <- read_csv(here::here("data/hub_status_2017.csv")) |> select(-...1)

airport_vec <- hub_df |> head(50) |> pull(dest)

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
  mutate(fitted = fitted / max(fitted))

fft_all <- splines_df_std |>
  group_by(airline, airport, type) |>
  group_modify(~{
    signal <- .x$fitted
    n <- length(signal)
#     fft_result <- fft(signal)
#     modulus <- Mod(fft_result)[1:(n/2)]
#     freqs <- (0:(n/2 - 1)) / n
#     periods_in_minutes <- 1 / freqs


    fft_result <- fft(signal) # acutally doing fft
    modulus <- Mod(fft_result)[1:(n*0.5)] # amplitudes
    freqs <- (1:(n*0.5)) / n #
    periods_in_minutes <- 1 / freqs

    tibble(
      period_mins = periods_in_minutes,
      amplitude = modulus
    ) |>
      dplyr::filter(is.finite(period_mins), period_mins <= 1440) |>  # up to 24 hours
      mutate(airport = unique(.x$airport), type = unique(.x$type))
  }) |>
  ungroup()

# Get entropy!
entropy_df <- fft_all |>
  group_by(airline, airport, type) %>%
  summarise(prob = amplitude^2 / sum(amplitude^2)) %>%
  summarise(entropy = sum(-prob*log(prob), na.rm = T)) |>
  pivot_wider(names_from = type, values_from = entropy) |>
  left_join(hub_df, by = c("airport" = "dest")) |>
  mutate(sum = arr + dep)

entropy_two <- entropy_df |> filter(airline == "AA") |> filter(airport %in% c("AUS", "DFW"))
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
  filter((Origin %in% c("AUS", "DFW") | Dest %in% c("AUS", "DFW"))) |>
  summarize_count(airports = c("AUS", "DFW")) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  theme_minimal() +
  xlab("Time of the day") +
  ylab("Count")

p1 | p2
ggsave(filename = here::here("figures/11-SH-aa-aus-dfw.png"), height = 4, width = 12, bg = "white")


p3 <- entropy_df |>
  ggplot(aes(x = arr, y = dep)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point() +
  ggrepel::geom_label_repel(
    data= entropy_df |> group_by(airline) |> arrange(-sum) |> filter(sum > 0.5),
    aes(label = airport), min.segment.length = 0) +
  facet_wrap(vars(airline)) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlab("Total arrival entropy") +
  ylab("Total departure entropy")
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



