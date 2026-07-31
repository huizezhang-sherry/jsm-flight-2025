library(arrow)
library(tidyverse)

flight_df <- read_parquet("~/Documents/GitHub/jsm-flight-2025/scripts/Year=2022/data_0.parquet")

dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA")
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA")
ua_hubs <- c("ORD", "DEN", "IAH", "LAX", "EWR", "SFO", "IAD")
wn_hubs <- c("ATL", "BWI", "MDW", "DAL", "DEN", "HOU", "LAS", "LAX", "BNA", "OAK", "MCO", "PHX")

aa <- flight_df |>
  filter((Reporting_Airline == "AA" & Origin %in% aa_hubs) |
           (Reporting_Airline == "DL" & Origin %in% dl_hubs) |
           (Reporting_Airline == "UA" & Origin %in% ua_hubs) |
           (Reporting_Airline == "WN" & Origin %in% wn_hubs) ,
         !is.na(DepTime), !is.na(ArrTime)) |>
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

p1 <- aa_df |>
  ggplot(aes(x = block, y = DepDelay)) +
  geom_col() +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2) +
  theme_minimal()

p_aa <- p1 %+% (aa_df |> filter(airline == "AA"))
p_ua <- p1 %+% (aa_df |> filter(airline == "UA"))
p_dl <- p1 %+% (aa_df |> filter(airline == "DL"))
p_wn <- p1 %+% (aa_df |> filter(airline == "WN"))

#### FFT of delay ####

library(splines)
library(ggplot2)

spline_df <- aa_df |>
  group_by(airline, dep_airport) |>
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

# Step 3: Plot FFT analysis by airline hub group
library(scales)

# Airline hub definitions
dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA")
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA")
ua_hubs <- c("ORD", "DEN", "IAH", "LAX", "EWR", "SFO", "IAD")
wn_hubs <- c("ATL", "BWI", "MDW", "DAL", "DEN", "HOU", "LAS", "LAX", "BNA", "OAK", "MCO", "PHX")

plot_fft <- function(hub_list, airline_label) {
  fft_delay |>
    filter(dep_airport %in% hub_list, airline == airline_label) |>
    ggplot(aes(x = period_mins, y = amplitude, color = dep_airport)) +
    geom_line() +
    facet_wrap(vars(dep_airport), scales = "free_y") +
    scale_x_continuous(trans = "log10", breaks = c(30, 60, 120, 240, 480, 960, 1440),
                       labels = label_number(suffix = " min")) +
    labs(x = "Period (minutes)", y = "Amplitude",
         title = paste("FFT of Delay Time Patterns -", airline_label)) +
    theme_minimal() +
    geom_vline(xintercept = 120, linetype = "dashed", color = "red")
}

plot_fft(dl_hubs, "DL")
plot_fft(aa_hubs, "AA")
plot_fft(ua_hubs, "UA")
plot_fft(wn_hubs, "WN")




# plot_fft <- function(hub_list, airline_label) {
#   key_periods <- c(30, 60, 120, 240, 480, 960, 1440)
#   
#   # Create a data frame to hold label positions
#   label_df <- fft_delay |>
#     filter(dep_airport %in% hub_list, airline == airline_label) |>
#     group_by(dep_airport) |>
#     slice(1) |>
#     ungroup() |>
#     expand_grid(period_mins = key_periods) |>
#     mutate(amplitude = NA)  # Just a placeholder for text labels
#   
#   fft_delay |>
#     filter(dep_airport %in% hub_list, airline == airline_label) |>
#     ggplot(aes(x = period_mins, y = amplitude, color = dep_airport)) +
#     geom_line() +
#     geom_vline(xintercept = 120, linetype = "dashed", color = "red") +
#     geom_text(
#       data = label_df,
#       aes(x = period_mins, y = 0, label = paste0(period_mins, " min")),
#       inherit.aes = FALSE,
#       vjust = 1.5,
#       size = 3,
#       color = "black"
#     ) +
#     facet_wrap(vars(dep_airport), scales = "free_y") +
#     scale_x_continuous(
#       trans = "log10",
#       breaks = key_periods,
#       labels = label_number(suffix = " min")
#     ) +
#     labs(x = "Period (minutes)", y = "Amplitude",
#          title = paste("FFT of Delay Time Patterns -", airline_label)) +
#     theme_minimal()
# }
