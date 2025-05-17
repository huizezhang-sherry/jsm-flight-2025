library(tidyverse)
library(lubridate)

# Load the data
flight_df <- read_csv("flight_df_ua.csv")

# Clean and format
flight_df <- flight_df |>
  mutate(
    FlightDate = as.Date(FlightDate),
    year = year(FlightDate)
  ) |>
  filter(dep_airport == "ORD" | arr_airport == "ORD") |>
  mutate(
    time = if_else(dep_airport == "ORD", dep_time, arr_time),
    type = if_else(dep_airport == "ORD", "departure", "arrival"),
    time = as.POSIXct(time)
  ) |>
  filter(!is.na(DepDelay))

# Bin into 10-minute intervals and compute mean delay per block
delay_df <- flight_df |>
  mutate(
    block = floor_date(time, unit = "10 minutes")
  ) |>
  group_by(year, block, type) |>
  summarise(delay = mean(DepDelay, na.rm = TRUE), .groups = "drop") |>
  arrange(year, block)

# Add "minutes since start of year" for FFT prep
delay_df <- delay_df |>
  group_by(year, type) |>
  mutate(minutes = as.numeric(difftime(block, min(block), units = "mins"))) |>
  ungroup()

# Fit splines and compute FFT for each year and type
fft_df <- delay_df |>
  group_by(year, type) |>
  group_modify(~ {
    signal <- .x$delay
    time_vec <- .x$minutes
    
    # Fit smoothing spline
    fit <- smooth.spline(time_vec, signal, spar = 0.5)
    fitted <- predict(fit, time_vec)$y
    
    # Perform FFT
    n <- length(fitted)
    fft_result <- fft(fitted)
    modulus <- Mod(fft_result)[1:(n/2)]
    freqs <- (0:(n/2 - 1)) / n
    period_mins <- 1 / freqs
    
    tibble(
      period_mins = period_mins,
      amplitude = modulus
    ) |>
      filter(is.finite(period_mins), period_mins <= 1440) |>
      mutate(year = unique(.x$year), type = unique(.x$type))
  }) |>
  ungroup()

# Plot FFT for each year
ggplot(fft_df, aes(x = period_mins, y = amplitude, color = type)) +
  geom_line() +
  facet_wrap(vars(year), scales = "free_y") +
  scale_x_log10(
    breaks = c(30, 60, 120, 240, 480, 720, 1440),
    labels = scales::label_number(suffix = " min")
  ) +
  labs(
    x = "Period (minutes)",
    y = "Amplitude",
    title = "Yearly FFT of UA Delay Patterns at ORD (1995–2023)"
  ) +
  geom_vline(xintercept = 120, linetype = "dashed", color = "red") +
  theme_minimal()