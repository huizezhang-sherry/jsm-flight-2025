library(arrow)
library(tidyverse)
library(dplyr)
library(splines)
library(purrr)

# Note: Most/all of this is copied from 05-SR-AA_2017.... (Saptarshi's)

###########################
### Get data and format ###
###########################

# Get airport/airline pairs
df_airports <- read.csv("./data/pairs80_v2_SMC.csv")
airlines <- unique(df_airports$airline)

# Make a list where each index is the specific airlines
airports_airlines_list <- lapply(airlines, function(air){
  df <- df_airports %>%
    dplyr::filter(airline %in% air)
})
names(airports_airlines_list) <- airlines

# Get 2017 FFT data
flight_df <- read_parquet("Year=2017/data_0.parquet")

# Transform for FFT
flight_hubs_spokes_list <- lapply(airports_airlines_list, function(df){
  airline <- unique(df$airline)
  airports <- unique(df$origin)

  # Get only the right airline/airport pairs
  df_0 <- flight_df %>%
    dplyr::filter(Reporting_Airline == airline & (Origin %in% airports | Dest %in% airports))

  # Formatting stuff
  df_1 <- df_0 %>%
    mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
           weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
           DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
           ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) %>%
    select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) %>%
    rename(dep_time = DepTime, arr_time = ArrTime,
           dep_airport = Origin, arr_airport = Dest) %>%
    pivot_longer(cols = -c(FlightDate, Reporting_Airline),
                 names_to = c("type", ".value"), names_sep = "_") %>%
    dplyr::filter(airport %in% airports) %>%
    arrange(time)
})

# Visualize
plot_list <- lapply(flight_hubs_spokes_list, function(df){
  p <- flight_hubs_spokes_list[[1]] |>
    dplyr::filter(!is.na(time)) |>
    ggplot(aes(time, color = type, fill = type)) +
    geom_density(alpha = 0.4) +
    #geom_histogram(bins = 23 * 6) +
    scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
    facet_wrap(vars(airport), scales = "free_y", ncol = 2) +
    theme_minimal() +
    theme(strip.text = element_blank()) +
    labs(title = unique(df$Reporting_Airline))
})

# Define time block function
assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time + floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

# Visualize again (with time block function)
plot_list_2 <- lapply(flight_hubs_spokes_list, function(df){
  p <- df |>
    dplyr::filter(!is.na(time)) |>
    mutate(block = assign_time_blocks(time)) |>
    count(airport, type, block) |>
    mutate(n = ifelse(type == "dep", n, -n)) |>
    ggplot(aes(x = block, y = n, color = type, fill = type)) +
    geom_col() +
    scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
    facet_wrap(vars(airport), scales = "free", ncol = 5) +
    theme_minimal() +
    labs(title = unique(df$Reporting_Airline))
})

######################
### Fitting spline ###
######################

# Make time numeric
binned_data_list <- lapply(flight_hubs_spokes_list, function(df){
  airline <- unique(df$Reporting_Airline)
  df <- df |>
    dplyr::filter(!is.na(time)) |>
    mutate(hour_minute = format(time, "%H:%M"),
           time_numeric = as.numeric(difftime(time, as.POSIXct("2024-01-01 00:00:00"), units = "mins"))) |>
    group_by(airport, type, time_numeric) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(airline = airline)
})

# Fit a spline to data
splines_df_list <- lapply(binned_data_list, function(df){
  airline <- unique(df$airline)
  df <- df |>
    group_by(airport, type) |>
    group_modify(~{
      fit <- smooth.spline(.x$time_numeric, .x$n, spar = 0.5)
      tibble(
        time_numeric = fit$x,
        fitted = fit$y,
        airport = unique(.x$airport),
        type = unique(.x$type)
      )
    }) |>
    ungroup() %>%
    mutate(airline = airline)
})

# skipping plot/vis for now

###########
### FFT ###
###########

sample_rate <- 0.5

fft_all_list <- lapply(splines_df_list, function(df){
  airline <- unique(df$airline)
  df <- df |>
    group_by(airport, type) |>
    group_modify(~{
      signal <- .x$fitted
      n <- length(signal) # number of rows

      fft_result <- fft(signal) # acutally doing fft
      modulus <- Mod(fft_result)[1:(n*sample_rate)] # amplitudes
      freqs <- (1:(n*sample_rate)) / n #
      periods_in_minutes <- 1 / freqs

      tibble(
        period_mins = periods_in_minutes,
        amplitude = modulus
      ) |>
        dplyr::filter(is.finite(period_mins), period_mins <= 120) |>  # up to 2 hours
        mutate(airport = unique(.x$airport), type = unique(.x$type))
    }) |>
    ungroup() %>%
    mutate(airline = airline)
})

plot_list_3 <- lapply(fft_all_list, function(df){
  p <- df %>%
    ggplot(aes(x = period_mins, y = amplitude)) +
    geom_point() +
    facet_grid(rows = vars(type), cols = vars(airport), scale = 'free')
})

# Saptarshi's data
# dat <- read.csv('./data/top2_fft_2017.csv')

# Get entropy!
l_0 <- lapply(fft_all_list, function(df){
  Airline <- unique(df$airline)
  df_0 <- df %>%
    group_by(airport, type) %>%
    summarise(prob = amplitude^2 / sum(amplitude^2)) %>%
    summarise(entropy = sum(-prob*log(prob), na.rm = T)) %>%
    pivot_wider(names_from = type, values_from = entropy) %>%
    rename(origin = airport)

  df_1 <- df_airports %>%
    filter(airline == Airline) %>%
    left_join(df_0, by = 'origin')
})

# Write to file
if (TRUE){
  write.csv(l_0 %>% bind_rows(), './data/fft_entropy_all_80.csv')
}

# Plot entropy
p_entropy <- lapply(l_0, function(df){
  Airline <- unique(df$airline)
  p <- df %>%
    #mutate(arr = log(abs(arr)), dep = log(abs(dep)))%>%
    ggplot(aes(x = arr, y = dep, color = factor(quartile))) +
    geom_point() +
    geom_text(aes(label = origin), nudge_y = 0.1) +
    labs(title = Airline)
  if (TRUE){
    ggsave(paste0('./figures/07-SMC_fft_entropy_',Airline,'.png'))
  }
  p
})

p <- bind_rows(l_0) %>% as_tibble() %>%
  mutate(airline_airport = paste0(airline, "-", origin)) %>%
  ggplot(aes(x = arr, y = dep, color = as.factor(quartile))) +
  #geom_point() +
  geom_text(aes(label = airline_airport), nudge_y = 0.1)

ggsave(paste0('./figures/07-SMC_fft_entropy_all.png'))
