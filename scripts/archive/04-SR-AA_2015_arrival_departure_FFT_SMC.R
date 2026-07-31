library(arrow)
library(tidyverse)
library(dplyr)

# Get airport/airline pairs
df_airports <- read.csv("./data/pairs80.csv")
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

# Fitting spline
