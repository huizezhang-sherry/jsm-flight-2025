library(tidyverse)
library(patchwork)
library(arrow)
library(purrr)

# Copied from Sherry's script 11, to debug entropy

# Read in data
flight_df <- read_parquet("Year=2017/data_0.parquet")
hub_df <- read_csv(here::here("data/hub_status_2017.csv"),
                   show_col_types = FALSE) %>%
  dplyr::select(-...1)

# Define what airports and airlines I'm screeing with
airport_vec <- hub_df %>% pull(dest)
airline_vec <- c("DL", "AA", "WN", "UA")

# Mutate dataframe correctly
flight_hubs_spokes <- flight_df %>%
  filter(Reporting_Airline %in% airline_vec) %>% # get only big 4 airlines
  filter(Origin %in% airport_vec | Dest %in% airport_vec) %>% # get only airports with hub status
  drop_na(ArrTime, DepTime) %>% # clean up
  mutate(ArrTime = as.numeric(substr(ArrTime, 1, 2)) + as.numeric(substr(ArrTime, 3, 4))/60, # get numeric time
         DepTime = as.numeric(substr(DepTime, 1, 2)) + as.numeric(substr(DepTime, 3, 4))/60) %>% # ""
  dplyr::select(Reporting_Airline, DepTime, ArrTime, Origin, Dest) %>% # only important vars
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) %>%
  pivot_longer(cols = -c(airline),
               names_to = c("type", ".value"), names_sep = "_") %>%
  mutate(binTime = floor(time / 0.25) * 0.25) %>% # bin time
  group_by(airline, airport, type, binTime) %>% # group it
  summarise(n = n(), .groups = "drop") %>% # summarise
  rename(time = binTime) # rename this for downstream cohesion

# OPTION 1: FIT A SMOOTH SPLINE
# Fit a smooth spline for each airport and type and standardize amp
splines_df_std <- binned_data %>%
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
  mutate(fitted = fitted / max(fitted, na.rm = TRUE))

# OPTION 2: JUST GOING WITH THE DATA
# What if I didn't fit a spline?
nonspline_df_std <- binned_data %>%
  rename(fitted = n) %>%
  mutate(fitted = fitted / max(fitted, na.rm = TRUE)) %>%
  complete( # make sure that zero observations are also recorded
    airline = unique(airline),
    airport = unique(airport),
    type = unique(type),
    time = seq(0, 24, by = 0.25),
    fill = list(fitted = 0)
  ) %>%
  group_by(airline, airport, type) %>%
  filter(sum(fitted > 0) >= 48) # at least half data are not zero

# Doing the FFT
fft_all <- nonspline_df_std %>%
  group_by(airline, airport, type) %>%
  group_modify(~{
    signal <- .x$fitted
    n <- length(signal)
    fft_result <- fft(signal) # actually doing fft
    modulus <- Mod(fft_result)[1:(n*2)] # amplitudes
    freqs <- (1:(n*2)) / n
    periods_in_hrs <- 1 / freqs
    tibble(
      period_hrs = periods_in_hrs,
      amplitude = modulus
    ) %>%
      dplyr::filter(is.finite(periods_in_hrs), period_hrs <= 24) %>%  # up to 24 hours
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
  mutate(sum = arr + dep) %>%
  drop_na() %>%
  ungroup() %>%
  mutate(hub_type = factor(hub_type, levels = c('Nonhub', 'Small', 'Medium', 'Large')))

# Visualize entropy by airline and hub
p <- entropy_df %>% ggplot() +
  geom_violin(aes(x = hub_type, y = dep)) +
  facet_wrap(~airline)

q <- entropy_df %>% ggplot() +
  geom_violin(aes(x = hub_type, y = arr)) +
  facet_wrap(~airline)

# Some model fits and contrasts
lm.1 <- aov(arr ~ hub_type - 1, data = entropy_df)
lm.2 <- aov(dep ~ hub_type - 1, data = entropy_df)

# Do pairwise compairsons
pairwise_1 <- TukeyHSD(lm.1, "hub_type")
pairwise_2 <- TukeyHSD(lm.2, "hub_type")

# Output results
print(pairwise_1)
print(pairwise_2)

getwd()

# Write file
write.csv(entropy_df, "data/09-SMC-entropy.csv", row.names = F)
