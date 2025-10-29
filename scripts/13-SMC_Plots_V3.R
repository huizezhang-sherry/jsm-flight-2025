# Load in plotting things
library(tidyverse)
library(purrr)
library(ggrepel)
library(ggplot2)
library(arrow)
source('scripts/00-SH-shared-functions.R')

# Read in the below DFs for plotting (from Sherry script)
flight_df <- read_parquet("Year=2017/data_0.parquet") # hist
dt <-  readRDS('data-raw/dt_11-SH') # smooth

### FIRST PLOT: HISTOGRAM ###

airports <- c("DEN", "ORD", "IAH", "LAX")
color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
legend_name <- "Flight type"

p1 <- flight_df |>
  filter(Reporting_Airline == 'UA') |>
  filter((Origin %in% airports| Dest %in% airports)) |>
  summarize_count(block_size = 15, airports = airports) |>
  mutate(airline_airport = factor(airline_airport,
                                  levels = paste("UA/", airports),
                                  labels = c("United / Denver Airport (DEN)",
                                             "United / Chicago O'Hare International Airport (ORD)",
                                             "United / George Bush Intercontinental Airport (IAH)",
                                             "United / Los Angeles International Airport (LAX)")),
         type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M",
                   date_breaks = "4 hour",
                   limits = as.POSIXct(c("1970-01-01 02:00:00", NA), tz = 'UTC')) +
  theme_minimal() +
  scale_fill_manual(name = legend_name, values = color_list) +
  scale_color_manual(name = legend_name, values = color_list) +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10)) +
  xlab("Binned time (15 minute intervals)") +
  scale_y_continuous(labels = NULL) +
  ylab("Yearly flight count (arb. units)")

# Save image
ggsave("figures/13-histogram.png",
       plot = p1,
       units = 'cm',
       width = 8.5,
       height = 10)

### SECOND PLOT: SPLINE ###

airports <- c("DEN", "ORD", "IAH", "LAX")
color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")

dt <- readRDS('data-raw/splines_df_11-SH') |>
  filter(airline == 'UA' & airport %in% airports) |>
  mutate(airline_airport = factor(airport,
                                  levels = airports,
                                  labels = c("United / Denver Airport (DEN)",
                                             "United / Chicago O'Hare International Airport (ORD)",
                                             "United / George Bush Intercontinental Airport (IAH)",
                                             "United / Los Angeles International Airport (LAX)")),
         fitted = if_else(type == 'arr', -fitted, fitted),
         type = factor(type,
                       levels = c('dep', 'arr'),
                       labels = c('Departure', 'Arrival')))

p2 <- dt |>
  ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(airline_airport), ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour",
                   limits = as.POSIXct(c("1970-01-01 02:00:00", NA), tz = 'UTC')) +
  scale_color_manual(name = 'Flight type', values = color_list) +
  theme_minimal() +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10)) +
  xlab("Binned time (15 minute intervals)") +
  ylab("Spline fit (arb. units)")

# Save image
ggsave("figures/13-smooth.png",
       plot = p2,
       units = 'cm',
       width = 8.5,
       height = 10)

### THIRD PLOT: FFT ###

airports <- c("DEN", "ORD", "IAH", "LAX")
color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
r_df <- readRDS('./data-raw/fft_reconstructed_11-SH') |>
  filter(airline == 'UA' & airport %in% airports) |>
  mutate(airline_airport = factor(airport,
                                  levels = airports,
                                  labels = c("United / Denver Airport (DEN)",
                                             "United / Chicago O'Hare International Airport (ORD)",
                                             "United / George Bush Intercontinental Airport (IAH)",
                                             "United / Los Angeles International Airport (LAX)")),
         reconstructed = if_else(type == 'arr', -reconstructed, reconstructed),
         type = factor(type,
                       levels = c('dep', 'arr'),
                       labels = c('Departure', 'Arrival')))

p3 <- r_df |>
  ggplot(aes(x = block, y = reconstructed, color = type, fill = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(airline_airport), ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour",
                   limits = as.POSIXct(c("1970-01-01 02:00:00", NA), tz = 'UTC')) +
  scale_color_manual(name = 'Flight type', values = color_list) +
  theme_minimal() +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10)) +
  xlab("Binned time (15 minute intervals)") +
  ylab("FFT Reconstruction")


### FOURTH PLOT: ENTROPY ###
entropy_df <- readRDS('./data-raw/entropy_df_11-SH') # fft
p4 <- entropy_df |>
  mutate(hub_type = factor(hub_type, levels = c("Nonhub", "Small", "Medium", "Large"))) |>
  ggplot(aes(x = arr, y = dep, color = hub_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 1) +
  facet_wrap(vars(airline)) +

  scale_color_manual(name = 'Hub type',
                     values = c('Nonhub' = '#005f86',
                               'Small' = '#00a9b7',
                               'Medium' = '#f8971f',
                               'Large' = '#bf5700')) +
  theme_bw() +
  theme(aspect.ratio = 1,
        legend.position = "bottom",
        text = element_text(colour = "black", size = 10)) +
  guides(color = guide_legend(nrow = 2)) +
  xlab("Arrival entropy") +
  ylab("Departure entropy")

# Save image
ggsave("figures/13-entropy.png",
       plot = p4,
       units = 'cm',
       width = 8.5,
       height = 12)

