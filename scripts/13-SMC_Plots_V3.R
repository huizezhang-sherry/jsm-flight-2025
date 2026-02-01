# Load in plotting things
library(tidyverse)
library(purrr)
library(ggrepel)
library(ggplot2)
library(patchwork)
library(scales)
library(arrow)
source('scripts/00-SH-shared-functions.R')

# Read in the below DFs for plotting (from Sherry script)
flight_df <- read_parquet("Year=2017/data_0.parquet") # hist
dt <-  readRDS('data-raw/dt_11-SH') # smooth

### ZEROTH PLOT: PASSENGER FLUX ###

years <- 1996:2024 # 1995 doesn't have passenger counts
airlines <- c('AA', 'DL', 'WN', 'UA')
cols <- c("American" = "#36495A", "Delta" = "#9B1631",
          "Southwest" = "#f9b612", "United" = "#1414D4")

# pas_df <- lapply(years, function(year){
#   # Define file
#   file <- paste0("passenger_info/T_T100D_SEGMENT_ALL_CARRIER_",year,".csv")
#
#   # Read df and summarize
#   df <- read.csv(file) |>
#     filter(CARRIER %in% airlines) |>
#     group_by(CARRIER) |>
#     summarise(Count = sum(PASSENGERS, na.rm = TRUE), .groups = 'drop') |>
#     mutate(Year = year)
# }) |> bind_rows()
#
# write.csv(pas_df, 'data/big_four_yearly_passenger_counts.csv', row.names = FALSE)
pas_df <- read.csv('data/big_four_yearly_passenger_counts.csv')

p0 <- pas_df |>
  mutate(Airline = factor(CARRIER,
                          levels = airlines,
                          labels = names(cols)),
         Count = Count/10**6) |>
  ggplot(aes(x = Year, y = Count, color = Airline, fill = Airline)) +
  geom_area(alpha = 0.8) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(1996, 2024, 4)) +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        text = element_text(colour = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank()) +
  labs(x = 'Year', y = 'Million Enplanements') +
  guides(color = guide_legend(nrow = 2))

# Save image
ggsave("figures/13-airline_passenger_count.png",
       plot = p0,
       units = 'cm',
       width = 8.5,
       height = 7)

### FIRST PLOT: FAA HUB VS AIRLINE HUB FOR UNITED ###

year <- 2017
airline <- c("UA")
airline_hubs <- c('DEN', 'EWR', 'GUM', 'IAD', 'IAH', 'LAX', 'ORD', 'SFO')
other_airports <- c('PAE', 'ATL','DFW')

# import passenger count data
pv <- read.csv(paste0("./passenger_info/T_T100D_SEGMENT_ALL_CARRIER_", year, ".csv"))

# get total passengers
tot <- pv |>
  summarise(Total = sum(PASSENGERS)) |>
  pull(Total)

# get FAA hub classification thresholds
l_thresh <- 1/100 * tot
m_thresh <- 0.25/100 * tot
s_thresh <- 0.05/100 * tot

# get total passengers for each airport (ORIGIN) and FAA hub
tot_airport <- pv |>
  group_by(ORIGIN) |>
  summarise(Total_Airport = sum(PASSENGERS)) |>
  mutate(Hub = factor(case_when(Total_Airport > l_thresh ~ 'Large',
                                Total_Airport > m_thresh ~ 'Medium',
                                Total_Airport > s_thresh ~ 'Small',
                                TRUE ~ 'Nonhub'),
                      levels = c('Nonhub', 'Small', 'Medium', 'Large')))

# get fraction of passengers for that airport that were in that airline
airline_frac <- pv |>
  group_by(ORIGIN, CARRIER) |>
  summarise(Carrier_Airport = sum(PASSENGERS), .groups = 'drop') |>
  full_join(tot_airport, by = 'ORIGIN') |>
  filter(CARRIER %in% airline) |>
  mutate(Frac = Carrier_Airport/Total_Airport*100,
         Total_Airport = Total_Airport/10^6,
         Carrier_Airport = Carrier_Airport/10^6,
         United_Hub = factor(if_else(ORIGIN %in% airline_hubs,
                                     'Yes',
                                     'No'), levels = c('Yes', 'No')))

# define colors
hub_colors = c('Large' = '#bf5700',
               'Medium' = '#f8971f',
               'Small' = '#00a9b7',
               'Nonhub' = '#005f86')

label_df <- airline_frac |>
  filter(ORIGIN %in% c(airline_hubs, other_airports))

# plot
p1 <- airline_frac |>
  ggplot(aes(x = Total_Airport, y = Frac, color = Hub, shape = United_Hub)) +
  geom_point() +
  geom_text_repel(aes(label = ORIGIN), data = label_df, show.legend = FALSE,
                  max.overlaps = 3) +
  scale_color_manual(name = 'FAA Classification', values = hub_colors) +
  scale_shape_manual(name = "United Airlines Hub", values = c('Yes' = 17, 'No' = 19)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        text = element_text(colour = "black", size = 10),
        panel.grid.minor = element_blank(),
        legend.box = "vertical") +
  labs(x = 'Million Enplanements', y = 'United Airlines Enplanements (%)') +
  guides(color = guide_legend(nrow = 2))

# Save image
ggsave("figures/13-united-faa-percent.png",
       plot = p1,
       units = 'cm',
       width = 8.5,
       height = 10)

### SECOND PLOT: HISTOGRAM ###

flight_df <- read_parquet("Year=2017/data_0.parquet")
#airports <- c("DEN", "ORD", "IAH", "LAX")
airports <- c('DEN', 'ORD', 'IAH', 'LAX', 'SFO', 'EWR', 'IAD','GUM')
color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
legend_name <- "Flight type"

p2 <- flight_df |>
  filter(Reporting_Airline == 'UA') |>
  filter((Origin %in% airports| Dest %in% airports)) |>
  summarize_count(block_size = 15, airports = airports) |>
  mutate(airline_airport = factor(airline_airport,
                                  levels = paste("UA/", airports),
                                  labels = c("United / Denver Airport (DEN)",
                                             "United / Chicago O'Hare International Airport (ORD)",
                                             "United / George Bush Intercontinental Airport (IAH)",
                                             "United / Los Angeles International Airport (LAX)",
                                             "United / San Francisco International Airport (SFO)",
                                             "United / Newark Liberty International Airport (EWR)",
                                             "United / Washington Dulles International Airport (IAD)",
                                             "United / Guam International Airport (GUM)")),
         type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  scale_x_datetime(date_labels =  "%H:%M",
                   date_breaks = "4 hour",
                   limits = as.POSIXct(c("1970-01-01 02:00:00", NA), tz = 'UTC')) +
  theme_minimal(base_size = 10) +
  scale_fill_manual(name = legend_name, values = color_list) +
  scale_color_manual(name = legend_name, values = color_list) +
  theme(legend.position = "bottom",
        text = element_text(colour = "black", size = 10),
        panel.grid.minor = element_blank()) +
  xlab("Binned time (15 minute intervals)") +
  scale_y_continuous(labels = NULL) +
  ylab("Yearly flight count (arb. units)")

# Save image
ggsave("figures/13-histogram.png",
       plot = p2,
       units = 'cm',
       width = 8.5,
       height = 15)


### THIRD PLOT: CONCEPTUAL FFT ###

f <- 10000  #frequency sampling intervals; 1/s
t <- 10 # time; s (arbitrary)
n <- f*t # number of samples (dimensionless)
x <- seq(0,t, length.out = n)
ya <- 6*sin(1*2*pi*x)
yb <- 4*sin(2*2*pi*x)
yc <- 2*sin(4*2*pi*x)
y <- ya + yb + yc

df_x <- data.frame('x' = x, 'y' = y)

p_x <- df_x |>
  filter(x < 2) |>
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  theme_minimal(base_size = 10) +
  labs(x = 'time (s)', y = 'Signal')

df_y <- data.frame('x' = x, 'ya' = ya, 'yb' = yb, 'yc' = yc) |>
  pivot_longer(cols = starts_with('y'),
               names_to = 'component',
               names_prefix = 'y',
               values_to = 'y') |>
  mutate(component = factor(component,
                            levels = c('a','b','c'),
                            labels = c('Component 1','Component 2', 'Component 3')))

p_y <- df_y |>
  filter(x < 2) |>
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(color = component), show.legend = FALSE) +
  theme_minimal(base_size = 10) +
  facet_wrap(~component, nrow = 3, scales = 'free_y') +
  scale_color_manual(values = c('Component 1' = '#f8971f',
                                'Component 2' = '#bf5700',
                                'Component 3' = '#00a9b7')) +
  labs(x = 'time (s)', y = 'Signal \n components')

fft_result <- fft(y)
freq <- (0:(n/2-1))/t # units: per second
amp <- Mod(fft_result)[1:(n/2)]/(n/2)

df_f <- data.frame('x' = freq, 'y' = amp) |>
  mutate(component = as.factor(case_when(x == 1 ~ '1',
                                         x == 2 ~ '2',
                                         x == 4 ~ '3')))

p_f <- df_f |>
  filter(x < 5) |>
  ggplot(aes(x = x, y = y)) +
  geom_col(aes(fill = component), show.legend = FALSE) +
  theme_minimal(base_size = 10) +
  labs(x = 'Frequency (1/s)', y = 'Amplitude') +
  scale_fill_manual(values = c('1' = '#f8971f',
                               '2' = '#bf5700',
                               '3' = '#00a9b7'))

p3 <- (p_x/p_f/p_y) +
  plot_layout(heights = c(2, 1, 4)) +
  plot_annotation(tag_levels = 'a')

# Save image
ggsave("figures/13-fft_explained.png",
       plot = p3,
       units = 'cm',
       width = 8.5,
       height = 15)

### FOURTH PLOT: PSD CONCEPTUALLY ###

# Define basics
f <- 10000  #frequency sampling intervals; 1/s
t <- 10 # time; s (arbitrary)
n <- f*t # number of samples (dimensionless)
x <- seq(0,t, length.out = n)
y_og <- 6*sin(1*2*pi*x) + 4*sin(2*2*pi*x) + 2*sin(4*2*pi*x)
y_s <- 8*sin(1*2*pi*x)
y_c <- 3*sin(1*2*pi*x) + 2*sin(1.5*2*pi*x) +
       2*sin(2*2*pi*x) + 2*sin(2.5*2*pi*x) +
       4*sin(3*2*pi*x) + 1*sin(4*2*pi*x)

factor_orders <- c('Simple', 'Medium', 'Complex')
factor_colors <- c('Simple' = "#00a9b7",
                   'Medium' = 'black',
                   'Complex' = '#bf5700')

# Format data into df
x_y_df <- data.frame(x = rep(x, times = 3),
                     y = c(y_og, y_s, y_c),
                     type = c(rep('Medium', n),
                              rep('Simple', n),
                              rep('Complex', n))) |>
  mutate(type = factor(type, levels = factor_orders))

# First subplot: data
p_data <- x_y_df |>
  filter(x < 2) |>
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(color = type), show.legend = FALSE) +
  facet_wrap(~type) +
  labs(x = 'time (s)', y = 'Signal') +
  scale_color_manual(values = factor_colors) +
  theme_minimal(base_size = 10) +
  scale_x_continuous(breaks = c(0, 1, 2))

# Define fft function
get_amp_freq <- function(data, t){
  y <- data |> pull(y)
  n <- length(y)
  fft_result <- fft(y)
  freq <- (0:(n/2-1))/t # units: per second
  amp <- Mod(fft_result)[1:(n/2)]/(n/2)

  df <- data.frame('freq' = freq, 'amp' = amp) |>
    filter(freq < 5)
  return(df)
}

# Use fft function
fft_df <- x_y_df |>
  nest(data = c(x, y), .by = type) |>
  rowwise() |>
  mutate(entropy = list(get_amp_freq(data, t = 10))) |>
  select(-data) |>
  unnest(cols = c(entropy))

# Plot that
p_fft <- fft_df |> ggplot(aes(x = freq, y = amp, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, ncol = 3) +
  labs(x = 'Frequency (1/s)', y = 'Amplitude') +
  scale_fill_manual(values = factor_colors) +
  theme_minimal(base_size = 10)

# Get probability
prob_df <- fft_df |>
  group_by(type) |>
  mutate(prob = amp**2/sum(amp**2)) |>
  ungroup()

# Plot that
p_prob <- prob_df |> ggplot(aes(x = freq, y = prob, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, ncol = 3) +
  labs(x = 'Frequency (1/s)', y = 'Spectral \n Density') +
  scale_fill_manual(values = factor_colors) +
  theme_minimal(base_size = 10)

# Calculate entropy
entropy <- prob_df |>
  group_by(type) |>
  summarise(entropy = sum(-prob*log(prob)))

# make plot
p4 <- p_data/p_fft/p_prob + plot_annotation(tag_levels = 'a')

# Save image
ggsave("figures/13-fft_to_psd.png",
       plot = p4,
       units = 'cm',
       width = 8.5,
       height = 11)

### UNUSED PLOT: SPLINE ###

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

p_unused <- dt |>
  ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(airline_airport), ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour",
                   limits = as.POSIXct(c("1970-01-01 02:00:00", NA), tz = 'UTC')) +
  scale_color_manual(name = 'Flight type', values = color_list) +
  theme_minimal(base_size = 10) +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "bottom",
        text = element_text(colour = "black", size = 10),
        panel.grid.minor = element_blank()) +
  xlab("Binned time (15 minute intervals)") +
  ylab("Spline fit (arb. units)")

# Save image
ggsave("figures/13-smooth.png",
       plot = p_unused,
       units = 'cm',
       width = 8.5,
       height = 10)


### SIXTH PLOT: ENTROPY ###
entropy_df <- readRDS('./data-raw/entropy_df_11-SH') # fft

airports <- read_csv(here::here("data/airports_in_cont_us.csv"))

aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA") # Source https://www.aa.com/i18n/customer-service/about-us/american-airlines-group.jsp and wikipedia
dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA") # Source https://news.delta.com/corporate-stats-and-facts
ua_hubs <- c("ORD", "DEN", "GUM", "IAH", "LAX", "EWR", "SFO", "IAD") # Source https://www.united.com/en/us/fly/travel/airport/maps.html

airline_names <- list('American' = 'American Airlines',
                      'Delta' = 'Delta Air Lines',
                      'United' = 'United Airlines',
                      'Southwest' = 'Southwest Airlines')

hubs <- tibble(airline = "American", airport = aa_hubs) |>
  bind_rows(tibble(airline = "Delta", airport = dl_hubs)) |>
  bind_rows(tibble(airline = "United", airport = ua_hubs)) |>
  mutate(hub = 'Yes')

p6 <- entropy_df |>
  left_join(hubs, by = c('airline', 'airport')) |>
  replace_na(list(hub = 'No')) |>
  mutate(hub = factor(hub, levels = c("Yes", "No")),
         hub_type = factor(hub_type, levels = c("Nonhub", "Small", "Medium", "Large")),
         airline = airline_names[[airline]]) |>
  filter(airport %in% airports$airport,
         !(airport %in% c("SJU", "STT"))) |>
  ggplot(aes(x = arr, y = dep, color = hub_type, shape = hub)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2) +
  facet_wrap(vars(airline)) +
  scale_color_manual(name = 'FAA Classification',
                     values = c('Nonhub' = '#005f86',
                               'Small' = '#00a9b7',
                               'Medium' = '#f8971f',
                               'Large' = '#bf5700')) +
  scale_shape_manual(name = "Airline Hub", values = c('Yes' = 17, 'No' = 19)) +
  theme_minimal(base_size = 10) +
  theme(aspect.ratio = 1,
        legend.position = "right",
        text = element_text(colour = "black", size = 10),
        panel.grid.minor = element_blank(),
        legend.box = 'vertical') +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(ncol = 2)) +
  xlab("Arrival entropy") +
  ylab("Departure entropy")

# Save image
ggsave("figures/13-entropy.png",
       plot = p6,
       units = 'cm',
       width = 18,
       height = 12,
       bg = 'white')

### SEVENTH PLOT: ZOOMED IN HUBS ###

entropy_df <- readRDS("./data-raw/entropy_df_11-SH")
united_hubs <- c('DEN', 'EWR', 'GUM', 'IAD', 'IAH', 'LAX', 'ORD', 'SFO')

entropy_hubs <- entropy_df |>
  filter(airline == "United") |> filter(airport %in% united_hubs)
smallest_three <- entropy_df |>
  filter(airline == "United") |> mutate(a= sum(arr + dep)) |> arrange(a) |> head(3)

colored_point_size <- 2
other_point_size <- 1
faa_color <- "#008000"
airline_color <- "#1414D4"

p7 <- entropy_df |>
  filter(airline == "United") |>
  ggplot(aes(x = arr, y = dep)) +
  geom_point(color = "grey80", size = other_point_size) +
  geom_point(data = entropy_hubs, aes(color = 'United hub'),
             size = colored_point_size) +
  geom_point(data = smallest_three, aes(color = 'Smallest entropy'),
             size = colored_point_size) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  ggrepel::geom_text_repel(
    data = entropy_hubs ,
    aes(label = airport), color = airline_color,
    direction = "y", nudge_x = 0, hjust = 0,
    segment.curvature = -1e-20, segment.size = 1, segment.linetype = "dotted") +
  ggrepel::geom_text_repel(
    data = smallest_three ,
    aes(label = airport), color = faa_color, angle = 90,
    direction = "x", nudge_y = 0 , hjust = 0,
    segment.curvature = -1e-20, segment.size = 1, segment.linetype = "dashed") +
  scale_x_continuous(breaks = seq(1.5, 3.5, by = 0.5)) +
  scale_y_continuous(breaks = seq(1.5, 3.5, by = 0.5)) +
  coord_cartesian(xlim = c(1.5, 3), ylim = c(1.5, 3)) +
  facet_wrap(vars(airline)) +
  scale_color_manual(name = 'Type', values = c('United hub' = airline_color,
                                           'Smallest entropy' = faa_color)) +
  theme_minimal(base_size = 10) +
  theme(aspect.ratio = 1,
        legend.position = "bottom") +
  guides(shape = 'none') +
  xlab("Arrival entropy") +
  ylab("Departure entropy")

ggsave(p7,
       filename = here::here("figures/13-United_ZoomedIn.png"),
       units = 'cm',
       width = 8.5,
       height = 8.5)

### EIGTH PLOT: CASE STUDY ###
ord_entropy_df <- readRDS("./data-raw/ord_entropy_df-12-SH") |>
  mutate(airline = factor(airline, levels = c("AA", "UA"), labels = c("American", "United"))) |>
  mutate(group = ifelse(airline == "American", 1, ifelse(
    airline == "United" & year < 2001, 2, 3
  )))

col_list <- c("American" = "#36495A", "United" = "#1414D4")

events_df <- tibble(year = 2001, reason = "9/11",
                    airline = "American") |>
  bind_rows(
    tibble(year = 2013, reason = "US Airway \n merger", airline = "American")) |>
  bind_rows(
    tibble(year = 2021, reason = "COVID", airline = "United")) |>
  bind_rows(
    tibble(year = 2003, reason = "United \n bankruptcy", airline = "United")) |>
  left_join(ord_entropy_df |> select(airline, airport, year, dep)) |>
  filter(year != 2001) |>
  filter(!year %in% c(2001, 2021))

p8 <- ord_entropy_df |>
  filter(!(airline == "United" & year == 2001)) |>
  ggplot(aes(x = year, y = dep, group = airline, color = airline), alpha = 0.6) +
  geom_line(aes(group = group), linewidth = 1, alpha = 0.6) +
  geom_point(size = 1, aes(shape = airline)) +
  geom_point(data = events_df, size = 2, aes(shape = airline), alpha = 0.6) +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year != 2013), aes(label = reason), color = "black",
    nudge_x = 7, nudge_y = 0.3, segment.curvature = 0.2,
    arrow = arrow(length = unit(0.05, "npc")),
    segment.linetype = "solid",
    size = 3,
    segment.size = 0.5,
    min.segment.length = 0) +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year == 2013),
    aes(label = reason), color = "black",
    nudge_x = 7, nudge_y = 0, segment.curvature = 0.2,
    arrow = arrow(length = unit(0.05, "npc")),
    segment.linetype = "solid",
    size = 3,
    segment.size = 0.5,
    min.segment.length = 0) +
  scale_color_manual(name = 'Airline', values = col_list) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom', axis.text.y = element_blank(),
        text = element_text(colour = "black", size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(shape = 'none',
         color = guide_legend(override.aes = list(shape = c(16, 17)))) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  ylab("Departure \n Entropy") +
  xlab("Year")

ggsave(p8,
       filename = "figures/13-ord-dep-entropy.png",
       units = 'cm',
       width = 19,
       height = 10)
