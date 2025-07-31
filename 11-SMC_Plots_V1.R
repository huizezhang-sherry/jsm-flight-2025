# Load in plotting things
library(tidyverse)
library(purrr)
library(ggrepel)
library(ggplot2)
library(arrow)
source('scripts/00-SH-shared-functions.R')

# Read in the below DF for plotting (this comes from Sherry's script 11 - ran on 7/30/25)
entropy_df <- read.csv('data/11-SH-full-analysis-entropy-df.csv') %>%
  mutate(hub_type = factor(hub_type, levels = c("Nonhub", "Small", "Medium", "Large")))

splines_df <- read.csv("data/11-SH-full-analysis-splines-df.csv") %>%
  mutate(block = as.POSIXct(block))

flight_df <- read_parquet("Year=2017/data_0.parquet")

# Plot 1 - ARRIVAL
# Some model fits and contrasts
lm_arr <- aov(arr ~ hub_type - 1, data = entropy_df)

# Factor mapping
factor_map <- c('Nonhub' = '1', 'Small' = '2', 'Medium' = '3', 'Large' = '4')

# Manual
n_y <- 3.1
s_y <- 2.5
m_y <- 2
l_y <- 2
lab_start <- c(n_y, s_y, m_y, l_y)
lab_heights <- max(lab_start) + c(0.15, 0.3, 0.45, 0.6) # individs, then 1-3, 2-4, 1-4

names(lab_start) <- names(factor_map)

# Make horizontal x dataframe
positions_x <- data.frame(x = c(1.025, 2.025, 3.025, 1, 2, 0.975),
                          xend = c(1.975, 2.975, 3.975, 3, 4, 4.025),
                          comp_spans = c(1, 1, 1, 2, 2, 3),
                          y = c(rep(lab_heights[1],3), lab_heights[2:4])) %>%
  mutate(x_to_merge = round(x),
         x_to_merge_end = round(xend))

# Make vertical y dataframe
positions_y <- positions_x %>%
  select(-x_to_merge, -x_to_merge_end) %>%
  pivot_longer(cols = c(x, xend), names_to = "position_type", values_to = "x")

positions_y <- positions_y %>%
  mutate(yend = lab_start[round(positions_y$x)])

# Do pairwise compairsons
pairwise_1 <- TukeyHSD(lm_arr, "hub_type")$hub_type %>%
  as.data.frame() %>%
  rownames_to_column('contrast') %>%
  separate(contrast, into = c("group1", "group2"), sep = "-") %>%
  mutate(group1 = as.numeric(factor_map[group1]),
         group2 = as.numeric(factor_map[group2]),
         p.value = case_when(`p adj` < 0.001 ~ "***",
                             `p adj` < 0.01 ~ "**",
                             `p adj` < 0.05 ~ "*",
                             `p adj` < 0.1 ~ ".",
                             TRUE ~ "n.s."),
         adjust = case_when(`p adj` < 0.05 ~ 0,
                            TRUE ~ 0.05)) %>%
  full_join(positions_x, by = c('group1' = 'x_to_merge_end', 'group2' = 'x_to_merge'))

# Making this for plotting
factor_conversion <- c('1' = 'Nonhub', '2' = 'Small', '3' = 'Medium', '4' = 'Large')

# Plotting
p <- ggplot(entropy_df, aes(x = hub_type, y = arr)) +
  geom_violin(alpha = 0) +
  geom_segment( # this plots the bars which span the x direction
    data = positions_x,
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE) +
  geom_segment( # this plots the bars which span the y direction
    data = positions_y,
    aes(x = x, xend = x, y = y, yend = yend),
    inherit.aes = FALSE) +
  geom_text( # adds text labels
    data = pairwise_1,
    aes(x = (x + xend)/2 + c(0, -0.2, 0, 0, 0, 0), y = y + adjust + 0.05, label = p.value),
    size = 20/.pt) +
  geom_violin(data = entropy_df, aes(fill = hub_type), show.legend = FALSE, color = 'black') +
  stat_summary(fun = "median", geom = "crossbar", color = "black", size = 0.25) +
  labs(
    y = "Arrival entropy",
    x = "Hub classification"
  ) +
  scale_fill_manual(values = c('Nonhub' = '#005f86',
                               'Small' = '#00a9b7',
                               'Medium' = '#f8971f',
                               'Large' = '#bf5700')) +
  theme_minimal() +
  theme(text = element_text(colour = "black", size = 20))

# Save image
ggsave("figures/11-SMC_entropy_ar.png",
       plot = p,
       units = 'in',
       width = 5.5,
       height = 5.5)


# Plot 2 - DEPARTURE
# Some model fits and contrasts
lm_dep <- aov(dep ~ hub_type - 1, data = entropy_df)

# Factor mapping
factor_map <- c('Nonhub' = '1', 'Small' = '2', 'Medium' = '3', 'Large' = '4')

# Manual
n_y <- 3.1
s_y <- 2.5
m_y <- 2
l_y <- 2
lab_start <- c(n_y, s_y, m_y, l_y)
lab_heights <- max(lab_start) + c(0.15, 0.3, 0.45, 0.6) # individs, then 1-3, 2-4, 1-4

names(lab_start) <- names(factor_map)

# Make horizontal x dataframe
positions_x <- data.frame(x = c(1.025, 2.025, 3.025, 1, 2, 0.975),
                          xend = c(1.975, 2.975, 3.975, 3, 4, 4.025),
                          comp_spans = c(1, 1, 1, 2, 2, 3),
                          y = c(rep(lab_heights[1],3), lab_heights[2:4])) %>%
  mutate(x_to_merge = round(x),
         x_to_merge_end = round(xend))

# Make vertical y dataframe
positions_y <- positions_x %>%
  select(-x_to_merge, -x_to_merge_end) %>%
  pivot_longer(cols = c(x, xend), names_to = "position_type", values_to = "x")

positions_y <- positions_y %>%
  mutate(yend = lab_start[round(positions_y$x)])

# Do pairwise compairsons
pairwise_2 <- TukeyHSD(lm_dep, "hub_type")$hub_type %>%
  as.data.frame() %>%
  rownames_to_column('contrast') %>%
  separate(contrast, into = c("group1", "group2"), sep = "-") %>%
  mutate(group1 = as.numeric(factor_map[group1]),
         group2 = as.numeric(factor_map[group2]),
         p.value = case_when(`p adj` < 0.001 ~ "***",
                             `p adj` < 0.01 ~ "**",
                             `p adj` < 0.05 ~ "*",
                             `p adj` < 0.1 ~ ".",
                             TRUE ~ "n.s."),
         adjust = case_when(`p adj` < 0.05 ~ 0,
                            TRUE ~ 0.05)) %>%
  full_join(positions_x, by = c('group1' = 'x_to_merge_end', 'group2' = 'x_to_merge'))

# Making this for plotting
factor_conversion <- c('1' = 'Nonhub', '2' = 'Small', '3' = 'Medium', '4' = 'Large')

# Plotting
p <- ggplot(entropy_df, aes(x = hub_type, y = dep)) +
  geom_violin(alpha = 0) +
  geom_segment( # this plots the bars which span the x direction
    data = positions_x,
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE) +
  geom_segment( # this plots the bars which span the y direction
    data = positions_y,
    aes(x = x, xend = x, y = y, yend = yend),
    inherit.aes = FALSE) +
  geom_text( # adds text labels
    data = pairwise_2,
    aes(x = (x + xend)/2 + c(0, -0.2, 0, 0, 0, 0), y = y + adjust + 0.05, label = p.value),
    size = 20/.pt) +
  geom_violin(data = entropy_df, aes(fill = hub_type), show.legend = FALSE, color = 'black') +
  stat_summary(fun = "median", geom = "crossbar", color = "black", size = 0.25) +
  labs(
    y = "Departure entropy",
    x = "Hub classification"
  ) +
  scale_fill_manual(values = c('Nonhub' = '#005f86',
                               'Small' = '#00a9b7',
                               'Medium' = '#f8971f',
                               'Large' = '#bf5700')) +
  theme_minimal() +
  theme(text = element_text(colour = "black", size = 20))

# Save image
ggsave("figures/11-SMC_entropy_dp.png",
       plot = p,
       units = 'in',
       width = 5.5,
       height = 5.5)

# Plot 3 - Sherry's (for methods)
flight_df <- read_parquet("Year=2017/data_0.parquet")
airports <- c("DEN", "ORD", "IAH", "LAX")

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
legend_name <- "Flight type"

p2 <- flight_df |>
  filter(Reporting_Airline %in% c("UA")) |>
  filter((Origin %in% airports| Dest %in% airports)) |>
  summarize_count(airports = airports) |>
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
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 20)) +
  xlab("Binned time (30 minute intervals)") +
  scale_y_continuous(labels = NULL) +
  ylab("Yearly flight count (arb. units)")

# Save image
ggsave("figures/11-SMC_flight_histogram.png",
       plot = p2,
       units = 'in',
       width = 6,
       height = 7)

dt <-  splines_df |>
  filter(airline == "UA") |>
  filter((airport %in% airports)) |>
  mutate(fitted = ifelse(type == "dep", fitted, -fitted)) |>
  mutate(airline_airport = as.factor(paste(airline, airport, sep = "/ "))) |>
  mutate(airline_airport = factor(airline_airport,
                                  levels = paste("UA/", airports),
                                  labels = c("United / Denver Airport (DEN)",
                                             "United / Chicago O'Hare International Airport (ORD)",
                                             "United / George Bush Intercontinental Airport (IAH)",
                                             "United / Los Angeles International Airport (LAX)")),
         type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival')))

p_smooth <- dt |>
  ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
  geom_line(size = 2) +
  facet_wrap(vars(airline_airport), ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour",
                   limits = as.POSIXct(c("1970-01-01 08:00:00", NA), tz = 'UTC')) +
  scale_color_manual(name = legend_name, values = color_list) +
  theme_minimal() +
  scale_y_continuous(labels = NULL) +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 20)) +
  xlab("Binned time (30 minute intervals)") +
  ylab("Spline fit (arb. units)")

# Save image
ggsave("figures/11-SMC_flight_spline_smoothed.png",
       plot = p_smooth,
       units = 'in',
       width = 6,
       height = 7)

# Plot 4 - Sherry's (results)
p3 <- entropy_df |>
  mutate(hub_type = factor(hub_type, levels = c("Nonhub", "Small", "Medium", "Large")),
         airline = factor(airline,
                          levels = c("AA", "DL", "UA", "WN"),
                          labels = c("American", "Delta", "United", "Southwest"))) |>
  ggplot(aes(x = arr, y = dep, color = hub_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 10) +
  # geom_point(
  #   data = entropy_df |> mutate(airlineairport = paste(airline, airport, sep = "_")) |>
  #     filter(airlineairport %in% hubs$airlineairport),
  #   color = "red") +
  # ggrepel::geom_label_repel(
  #   data = entropy_df |> mutate(airlineairport = paste(airline, airport, sep = "_")) |>
  #     filter(airlineairport %in% hubs$airlineairport),
  #   aes(label = airport), min.segment.length = 0) +
  #ggforce::geom_mark_hull(expand = unit(2, "mm"), concavity = 10 ) +
  facet_wrap(vars(airline)) +

  scale_color_manual(name = 'Hub type',
                     values = c('Nonhub' = '#005f86',
                               'Small' = '#00a9b7',
                               'Medium' = '#f8971f',
                               'Large' = '#bf5700')) +
  theme_bw() +
  theme(aspect.ratio = 1,
        legend.position = "right",
        text = element_text(colour = "black", size = 50)) +
  xlab("Arrival entropy") +
  ylab("Departure entropy")

# Save image
ggsave("figures/11-SMC_all_four.png",
       plot = p3,
       units = 'in',
       width = 24,
       height = 20)

# Delay correlation plot
airport_vec <- entropy_df %>% pull(airport)
airline_vec <- c("DL", "AA", "WN", "UA")

flight_hubs_spokes <- flight_df %>%
  dplyr::select(Reporting_Airline, Origin, Dest, DepDel15) %>% # only important vars
  drop_na(DepDel15) %>%
  rename(dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) %>%
  pivot_longer(cols = -c(airline, DepDel15),
               names_to = c("type", ".value"), names_sep = "_") %>%
  group_by(airline, type, airport) %>%
  summarise(percDelay = sum(DepDel15)/n() * 100, .groups = "drop") %>%  # summarise
  pivot_wider(names_from = type, values_from = percDelay) %>%
  rename(arr_del = arr,
         dep_del = dep) %>%
  filter(airline %in% airline_vec) %>%  # get only big 4 airlines
  filter(airport %in% airport_vec) # get only airports with hub status

df_delay <- full_join(entropy_df, flight_hubs_spokes, by = c('airline', 'airport')) %>%
  drop_na() %>%
  mutate(airline = factor(airline,
                          levels = c("AA", "DL","WN", "UA"),
                          labels = c("American", "Delta", "Southwest", "United")))

lm_1 <- lm(dep_del ~ airline * hub_type * poly(dep,2) - 1, data = df_delay)

p_delay <- df_delay %>%
  ggplot(aes(x = dep, y = dep_del,
             shape = airline, color = airline)) +
  geom_point(size = 5) +
  stat_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = FALSE, aes(group = airline, color = airline)) +
  facet_wrap(~hub_type, scale = 'free') +
  scale_color_manual(
    name = "Airline",
    values = c("American" = "#0033A0",
               "Delta" = "#C8102E",
               "United" = "#FFC72C",
               "Southwest" = "#92278F")) +
  scale_shape_manual(
    name = "Airline",
    values = c("American" = 16,
               "Delta" = 17,
               "United" = 15,
               "Southwest" = 18)) +
  theme_bw() +
  labs(x = 'Departure entropy', y = 'Percentage of flights delayed') +
  theme(legend.position = 'bottom', text = element_text(colour = "black", size = 30)) +
  guides(shape = "none",
         color = guide_legend(override.aes = list(shape = c(16, 17, 15, 18))))

# Save image
ggsave("figures/11-SMC_delays.png",
       plot = p_delay,
       units = 'in',
       width = 20,
       height = 11)
