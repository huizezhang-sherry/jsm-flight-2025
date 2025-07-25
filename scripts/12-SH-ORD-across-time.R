library(tidyverse)
library(patchwork)
source(here::here("scripts/00-SH-shared-functions.R"))
flight_df <- read_parquet("Year=2017/data_0.parquet")

hub_df <- read_csv(here::here("data/hub_status_2017.csv")) |> select(-...1)

airport_vec <- hub_df |> head(50) |> pull(dest)

flight_aa <- read_csv(here::here("data/AA_flights.csv"))
flight_ua <- read_csv(here::here("data/UA_flights.csv"))

flight_ord <- bind_rows(flight_aa, flight_ua) |> filter(Origin == "ORD" | Dest == "ORD")

ord_hubs_spokes <- flight_ord |>
  mutate(year = year(FlightDate),
         DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest, year) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) |>
  pivot_longer(cols = -c(FlightDate, airline, year),
               names_to = c("type", ".value"), names_sep = "_") |>
  dplyr::filter(airport %in% "ORD") |>
  arrange(time)

ord_binned_data <- ord_hubs_spokes |>
  dplyr::filter(!is.na(time)) |>
  mutate(hour_minute = format(time, "%H:%M"),
         time_numeric = as.numeric(difftime(time, as.POSIXct("2017-01-01 00:00:00", tz = "UTC"), units = "mins"))) |>
  group_by(airline, airport, type, time_numeric, year) |>
  summarise(n = n(), .groups = "drop")

# Fit a smooth spline for each airport and type
library(purrr)
ord_splines_df <- ord_binned_data |>
  group_by(airline, airport, type, year) |>
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

ord_binned_data <- ord_binned_data |>
  mutate(time = as.POSIXct("2017-01-01", tz = "UTC") + time_numeric * 60)

ord_splines_df_std <- ord_splines_df |>
  group_by(airline, airport, type, year) |>
  mutate(fitted = fitted / max(fitted))

ord_fft_all <- ord_splines_df_std |>
  group_by(airline, airport, type, year) |>
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
ord_entropy_df <- ord_fft_all |>
  group_by(airline, airport, type, year) %>%
  mutate(prob = amplitude^2 / sum(amplitude^2)) %>%
  summarise(entropy = sum(-prob*log(prob), na.rm = T)) |>
  pivot_wider(names_from = type, values_from = entropy)

ord_entropy_df |>
  ggplot(aes(x = year, y = dep, color = airline)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1995, 2024, 2)) +
  ylab("Total departure entropy")
ggsave(filename = here::here("figures/12-SH-ord-dep-entropy.png"), height = 3, width = 10, bg = "white")
