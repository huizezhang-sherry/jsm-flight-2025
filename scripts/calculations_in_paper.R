library(tidyverse)
library(arrow)

############################################################
### Getting the total number of flights across all years ###
############################################################

# Note that this includes all flights as this is a measure of the data breadth
# and therefore may include non-contingous states flights

years <- 1987:2024
files <- paste0("Year=", years, "/data_0.parquet")
total <- 0

for (file in files){
  n <- arrow::open_dataset(file)
  total <- total + n$num_rows
}

# total is 217,389,694, rounds to 217 million over 38 years
print(scales::comma(total))

##############################################
### Calculating the FAA hub status in 2017 ###
##############################################

# Define file location and valid state fips
file <- "passenger_info/T_T100D_SEGMENT_ALL_CARRIER_2017.csv"
st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI

# Read data and filter for only contigious states
pv <- read_csv(file, show_col_types = F) |>
  filter(ORIGIN_STATE_FIPS %in% st_fips,
         DEST_STATE_FIPS %in% st_fips)

# Get total passengers for 2017
tot_pas <- pv |>
  pull(PASSENGERS) |>
  sum(na.rm = T)

# Define thresholds for hub classification
# https://www.faa.gov/airports/planning_capacity/categories
large_hub_min <- tot_pas * (1/100)
med_hub_min <- tot_pas * (0.25/100)
small_hub_min <- tot_pas * (0.05/100)

# Get categories
hub_df <- pv |>
  group_by(ORIGIN) |>
  summarise(yearly_passengers = sum(PASSENGERS, na.rm = T)) |>
  mutate(hub_type = case_when(yearly_passengers > large_hub_min ~ 'Large',
                              yearly_passengers > med_hub_min ~ 'Medium',
                              yearly_passengers > small_hub_min ~ 'Small',
                              TRUE ~ 'Nonhub')) |>
  rename(origin = ORIGIN) |>
  arrange(desc(yearly_passengers))

# Write output
write_csv(hub_df, "data/hub_status_2017.csv")

#################################
### Performing the spline fit ###
#################################

# Define contigious state fips and airports
st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI
airport_vec <- read_csv(here::here('data/hub_status_2017.csv'),
                        show_col_types = F) |> pull(origin)

# Define function to get time blocks from a time vector
assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time +
    floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

# Process the raw flight data to obtain flight counts
flight_hubs_spokes <- read_parquet("Year=2017/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"),
         OriginStateFips %in% st_fips,
         DestStateFips %in% st_fips) |>
  filter(!is.na(DepTime), !is.na(ArrTime)) |>
  mutate(DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  rename(dep_time = DepTime, arr_time = ArrTime, airline = Reporting_Airline,
         dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(FlightDate, airline),
               names_to = c("type", ".value"), names_sep = "_") |>
  filter(airport %in% airport_vec) |>
  mutate(block = assign_time_blocks(time)) |>
  count(airline, airport, type, block) |>
  mutate(airline_airport = paste(airline, airport, sep = "/ ")) |>
  mutate(n = ifelse(type == "dep", n, -n)) |>
  mutate(type = factor(type, levels = c('dep', 'arr'),
                       labels = c("dep" = 'Departure', "arr" = 'Arrival')))

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
  mutate(smooth_res = list(calc_smooth(data, spar = 0.05))) |>
  unnest(smooth_res) |>
  select(-data) |>
  mutate(block = as_datetime(x)) |>
  select(-x)

write_csv(splines_df, 'data/splines_df.csv')

##########################
### Performing the FFT ###
##########################

splines_df <- read_csv(here::here('data/splines_df.csv'), show_col_types = F)

calc_fft <- function(dt, block_size = 10){
  # Get signal and number of observations
  signal <- dt$fitted
  n <- length(signal)

  # Perform fft
  fft_result <- fft(signal) # actually doing fft, observations are in intervals of block_size
  modulus <- Mod(fft_result)[2:(n/2)]
  amplitude <- 2 * modulus / n
  freqs <- (1:(n/2 - 1)) / (n * block_size)
  periods_in_minutes <- 1 / freqs

  # Format result
  result <- tibble(
    period_mins = periods_in_minutes,
    amplitude = amplitude
  ) |>
    dplyr::filter(is.finite(period_mins), period_mins <= 1440) # up to 24 hours

  # Get inverse (for visualization)
  inverse <- Re(fft(fft_result, inverse = TRUE)) / (n/2)  # normalize
  reconstruct <- tibble(
    block = dt$block,
    reconstructed = inverse
  )
  return(list('calc' = result, 'reconstructed' = reconstruct))
}

fft_all <- splines_df |>
  nest(data = everything(), .by = c(airline, airport, type)) |>
  rowwise() |>
  mutate(fft_res = list(calc_fft(data)[['calc']])) |>
  select(-data) |>
  unnest(fft_res)

write_csv(fft_all, 'data/fft_df.csv')

###########################
### Calculating entropy ###
###########################

hub_df <- read_csv("data/hub_status_2017.csv", show_col_types = F)
fft_all <- read_csv(here::here('data/fft_df.csv'), show_col_types = F)

entropy_df <- fft_all |>
  group_by(airline, airport, type) |>
  mutate(prob = amplitude^2 / sum(amplitude^2)) |>
  summarise(entropy = sum(-prob*log(prob), na.rm = T), .groups = 'drop') |>
  pivot_wider(names_from = type, values_from = entropy) |>
  left_join(hub_df, by = c("airport" = "origin")) |>
  filter(Arrival != 0, Departure != 0) |>
  mutate(hub_type = factor(hub_type, levels = c("Large", "Medium", "Small", "Nonhub"))) |>
  mutate(airline = factor(airline,
                          levels = c("AA", "DL", "UA", "WN"),
                          labels = c("American", "Delta", "United", "Southwest")))

write_csv(entropy_df, 'data/entropy_df.csv')

########################################################
### Getting ORD flight counts over time for Figure 8 ###
########################################################

Years <- 1995:2024
st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI

ord_flight_counts <- lapply(Years, function(Year){
  parquet_name <- paste0("Year=", Year, "/data_0.parquet")
  parquet <- read_parquet(parquet_name) |>
    filter(Reporting_Airline %in% c("AA", "UA"),
           OriginStateFips %in% st_fips,
           DestStateFips %in% st_fips) |>
    filter(!is.na(DepTime), !is.na(ArrTime)) |>
    filter(Origin == "ORD") |> # only flights departing from ORD
    group_by(Reporting_Airline) |>
    summarise(flight_count = n(), .groups = 'drop') |>
    mutate(Year = Year)
}) |> bind_rows()

write_csv(ord_flight_counts, 'data/ord_flight_counts.csv')

######################################################
### Calculating ORD entropy over time for Figure 8 ###
######################################################

# Part 1: Get flight counts

Years <- 1995:2024
st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI

# Define function to get time blocks from a time vector
assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time +
    floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

ord_hubs_spokes <- lapply(Years, function(Year){
  parquet_name <- paste0("Year=", Year, "/data_0.parquet")
  parquet <- read_parquet(parquet_name) |>
    filter(Reporting_Airline %in% c("AA", "UA"),
           OriginStateFips %in% st_fips,
           DestStateFips %in% st_fips) |>
    filter(!is.na(DepTime), !is.na(ArrTime)) |>
    mutate(DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
           ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
    select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
    rename(dep_time = DepTime, arr_time = ArrTime, airline = Reporting_Airline,
           dep_airport = Origin, arr_airport = Dest) |>
    pivot_longer(cols = -c(FlightDate, airline),
                 names_to = c("type", ".value"), names_sep = "_") |>
    filter(airport == 'ORD') |>
    mutate(block = assign_time_blocks(time)) |>
    count(airline, airport, type, block) |>
    mutate(airline_airport = paste(airline, airport, sep = "/ ")) |>
    mutate(n = ifelse(type == "dep", n, -n)) |>
    mutate(type = factor(type, levels = c('dep', 'arr'),
                         labels = c("dep" = 'Departure', "arr" = 'Arrival')),
           year = Year)
}) |> bind_rows()

write_csv(ord_hubs_spokes, file = here::here("data/ord_hubs_spokes.csv"))
ord_hubs_spokes <- read_csv('data/ord_hubs_spokes.csv', show_col_types = F)

# Part 2: Bin data, create smoothing spline

ord_binned_data <- ord_hubs_spokes |>
  complete(airline, airport, type, block, year, fill = list(n = 0)) |>
  mutate(airline_airport = paste(airline, airport, sep = "/ "))

calc_smooth <- function(data, spar = 0.5){
  res <- smooth.spline(data$block, data$n, spar = spar)
  tibble(x = res$x, fitted = res$y)
}

ord_splines_df <- ord_binned_data |>
  mutate(n = abs(n)) |>
  nest(data = -c(airline, airport, type, year)) |>
  rowwise() |>
  filter(dplyr::n_distinct(data$block) >= 4) |>
  mutate(smooth_res = list(calc_smooth(data, spar = 0.05))) |>
  unnest(smooth_res) |>
  select(-data) |>
  mutate(block = as_datetime(x)) |>
  select(-x)

# Part 3: Perform fft

calc_fft <- function(dt, block_size = 10){
  # Get signal and number of observations
  signal <- dt$fitted
  n <- length(signal)

  # Perform fft
  fft_result <- fft(signal) # actually doing fft, observations are in intervals of block_size
  modulus <- Mod(fft_result)[2:(n/2)]
  amplitude <- 2 * modulus / n
  freqs <- (1:(n/2 - 1)) / (n * block_size)
  periods_in_minutes <- 1 / freqs

  # Format result
  result <- tibble(
    period_mins = periods_in_minutes,
    amplitude = amplitude
  ) |>
    dplyr::filter(is.finite(period_mins), period_mins <= 1440) # up to 24 hours

  # Get inverse (for visualization)
  inverse <- Re(fft(fft_result, inverse = TRUE)) / (n/2)  # normalize
  reconstruct <- tibble(
    block = dt$block,
    reconstructed = inverse
  )
  return(list('calc' = result, 'reconstructed' = reconstruct))
}

ord_fft_all <- ord_splines_df |>
  nest(data = -c(airline, airport, type, year)) |>
  rowwise() |>
  mutate(fft_res = list(calc_fft(data)[['calc']])) |>
  select(-data) |>
  unnest(fft_res)

# Part 4: Calculate entropy

ord_entropy_df <- ord_fft_all |>
  group_by(airline, airport, type, year) |>
  mutate(prob = amplitude^2 / sum(amplitude^2)) |>
  summarise(entropy = sum(-prob*log(prob), na.rm = T), .groups = 'drop') |>
  pivot_wider(names_from = type, values_from = entropy)

write_csv(ord_entropy_df, 'data/ord_entropy_df.csv')
