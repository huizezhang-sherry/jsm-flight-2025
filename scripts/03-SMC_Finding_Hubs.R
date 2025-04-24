########################
### Read in packages ###
########################

rm(list = ls()) # clean old variables
library(arrow) # to read parquet
library(tidyverse)
library(ggridges) # for ridgeline plots
library(ggplot2)

#########################################
### Read in datasets and clean/format ###
#########################################

## Read dataset for JSM project
# Data dictionary here https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FGJ
# Column Dest has the destination airport, FAA code
flight_df <- read_parquet("Year=2024/data_0.parquet")
big_four <- c("DL","AA","WN","UA") # Delta American Southwest United
flight_df <- flight_df %>%
  filter(Reporting_Airline %in% big_four)

###############################
### Modify Saptarshi's Code ###
###############################

aa <- flight_df |>
  filter(!is.na(DepTime), !is.na(ArrTime)) |>
  mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
         weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
         DepTime = as_datetime(paste0("2024-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2024-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest, DepDelay) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) |>
  mutate(a = row_number()) |>
  arrange(a)

assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time + floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

aa_df <- aa |>
  mutate(block = assign_time_blocks(dep_time, block_size = 10)) |>
  group_by(block, airline, dep_airport) |>
  summarise(DepDelay = mean(DepDelay, na.rm = TRUE)) |>
  mutate(airline_airport = paste0(airline, "_", dep_airport)) |>
  filter(hour(block) >= 8)

#### FFT of delay ####

library(splines)
library(ggplot2)

min_count_for_spline <- 8
# I included the above because i got this error
# "Error in smooth.spline(.x$minutes, .x$DepDelay, spar = 0.5) :
# need at least four unique 'x' values"
# without it

spline_df <- aa_df |>
  group_by(airline, dep_airport) |>
  filter(n() > min_count_for_spline) |>
  arrange(block) |>
  mutate(
    minutes = as.numeric(difftime(block, min(block), units = "mins"))
  ) |>
  group_modify(~ {
    fit <- smooth.spline(.x$minutes, .x$DepDelay, spar = 0.5)
    tibble(
      minutes = .x$minutes,
      fitted = predict(fit, .x$minutes)$y
    ) |>
      mutate(airline = unique(.x$airline), dep_airport = unique(.x$dep_airport))
  }) |>
  ungroup()

# Step 2: FFT on spline-fitted delay
fft_delay <- spline_df |>
  group_by(airline, dep_airport) |>
  group_modify(~ {
    signal <- .x$fitted
    n <- length(signal)
    fft_result <- fft(signal)
    modulus <- Mod(fft_result)[1:(n/2)]
    freqs <- (0:(n/2 - 1)) / n
    period_mins <- 1 / freqs

    tibble(
      period_mins = period_mins,
      amplitude = modulus
    ) |>
      filter(is.finite(period_mins), period_mins <= 1440) |>  # Up to 24 hours
      mutate(airline = unique(.x$airline), dep_airport = unique(.x$dep_airport))
  }) |>
  ungroup()

###################################
### Use my own filtering method ###
###################################

for (working_airline in big_four){
  p <- fft_delay %>%
    filter(airline == working_airline) %>%
    group_by(dep_airport) %>%
    filter(period_mins == max(period_mins)) %>%
    ggplot(aes(x = period_mins, y = amplitude)) +
    geom_point() +
    geom_text(aes(label = dep_airport), nudge_y = 100) +
    labs(title = paste0("FTT Delay Results for Airline ", working_airline),
         x = "Period (minutes) at Max Amplitude",
         y = "Max Amplitude")

  print(p)
}
