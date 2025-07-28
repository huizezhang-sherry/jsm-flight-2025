library(tidyverse)
library(patchwork)
library(arrow)
library(purrr)

# Copied from Sherry's script 11, to debug entropy

flight_df <- read_parquet("Year=2017/data_0.parquet")
hub_df <- read_csv(here::here("data/hub_status_2017.csv"),
                   show_col_types = FALSE) %>%
  dplyr::select(-...1) %>%
  filter(hub_type != 'Nonhub')

airport_vec <- hub_df %>% pull(dest)
airline_vec <- c("DL", "AA", "WN", "UA")

flight_hubs_spokes <- flight_df %>%
  filter(Reporting_Airline %in% airline_vec) %>%
  filter(Origin %in% airport_vec | Dest %in% airport_vec) %>%
  drop_na(ArrTime, DepTime) %>%
  mutate(ArrTime = as.numeric(substr(ArrTime, 1, 2)) + as.numeric(substr(ArrTime, 3, 4))/60,
         DepTime = as.numeric(substr(DepTime, 1, 2)) + as.numeric(substr(DepTime, 3, 4))/60) %>%
  dplyr::select(Reporting_Airline, DepTime, ArrTime, Origin, Dest) %>%
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) %>%
  pivot_longer(cols = -c(airline),
               names_to = c("type", ".value"), names_sep = "_") %>%
  mutate(binTime = floor(time / 0.25) * 0.25)

# Bin dataframe
binned_data <- flight_hubs_spokes %>%
  group_by(airline, airport, type, time, binTime) %>%
  summarise(n = n(), .groups = "drop")

# Fit a smooth spline for each airport and type
splines_df <- binned_data %>%
  group_by(airline, airport, type) %>%
  group_modify(~{
    if (n_distinct(.x$time) >= 48) { # if there are at least 48 obs
      fit <- smooth.spline(.x$time, .x$n, spar = 0.5)
      tibble(
        time = fit$x,
        fitted = fit$y,
        airport = unique(.x$airport),
        type = unique(.x$type),
        airline = unique(.x$airline)
      )
    } else {
      tibble()  # return empty tibble for groups that don't meet requirement
    }
  }) %>%
  ungroup()

splines_df_std <- splines_df %>%
  group_by(airline, airport, type) %>%
  mutate(fitted = fitted / max(fitted)) # standardize amplitudes

# Checking the number of results we have
check <-  splines_df_std %>%
  group_by(airline, airport, type) %>%
  summarise(n_rows = n())

fft_all <- splines_df_std %>%
  group_by(airline, airport, type) %>%
  group_modify(~{
    signal <- .x$fitted
    n <- length(signal)
    fft_result <- fft(signal) # actually doing fft
    modulus <- Mod(fft_result)[1:(n*2)] # amplitudes
    freqs <- (1:(n*2)) / n
    periods_in_minutes <- 1 / freqs
    tibble(
      period_mins = periods_in_minutes,
      amplitude = modulus
    ) %>%
      dplyr::filter(is.finite(period_mins), period_mins <= 1440) %>%  # up to 24 hours
      mutate(airport = unique(.x$airport), type = unique(.x$type))
  }) %>%
  ungroup() %>% drop_na()

# Get entropy!
entropy_df <- fft_all %>%
  group_by(airline, airport, type) %>%
  mutate(prob = amplitude^2 / sum(amplitude^2)) %>%
  summarise(entropy = sum(-prob*log(prob), na.rm = T)) %>%
  pivot_wider(names_from = type, values_from = entropy) %>%
  left_join(hub_df, by = c("airport" = "dest")) %>%
  mutate(sum = arr + dep) %>% drop_na() %>% ungroup()

p <- entropy_df %>% ggplot() +
  geom_violin(aes(x = hub_type, y = dep)) +
  facet_wrap(~airline)

q <- entropy_df %>% ggplot() +
  geom_violin(aes(x = hub_type, y = arr)) +
  facet_wrap(~airline)

lm.1 <- lm(arr ~ hub_type - 1, data = entropy_df)
lm.2 <- lm(dep ~ hub_type - 1, data = entropy_df)

