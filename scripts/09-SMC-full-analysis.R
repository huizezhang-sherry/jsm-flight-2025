library(tidyverse)
library(patchwork)
library(arrow)
library(purrr)
library(ggrepel)
library(ggplot2)

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
binned_data <- flight_df %>%
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
  rename(time = binTime) %>% # rename this for downstream cohesion
  filter(airline %in% airline_vec) %>% # get only big 4 airlines
  filter(airport %in% airport_vec) # get only airports with hub status

# Standardizing data
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

# Some model fits and contrasts
lm.1 <- aov(arr ~ hub_type - 1, data = entropy_df)
lm.2 <- aov(dep ~ hub_type - 1, data = entropy_df)

# Factor mapping
factor_map <- c('Nonhub' = '1', 'Small' = '2', 'Medium' = '3', 'Large' = '4')

# Manual
n_y <- 4
s_y <- 4.2
m_y <- 4.1
l_y <- 4.1
lab_start <- c(n_y, s_y, m_y, l_y)
lab_heights <- max(lab_start) + c(0.2, 0.4, 0.6, 0.8) # individs, then 1-3, 2-4, 1-4

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
pairwise_2 <- TukeyHSD(lm.2, "hub_type")$hub_type %>%
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
    aes(x = (x + xend)/2 + c(0, -0.2, 0, 0, 0, 0), y = y + adjust + 0.075, label = p.value),
    size = 10/.pt) +
  geom_violin(data = entropy_df, aes(fill = hub_type), show.legend = FALSE) +
  stat_summary(fun = "median", geom = "crossbar", color = "black", size = 0.25) +
  labs(
    y = "Departure entropy",
    x = "Hub type"
  ) +
  scale_fill_manual(values = c('Nonhub' = '#005f86',
                                'Small' = '#00a9b7',
                                'Medium' = '#f8971f',
                                'Large' = '#bf5700')) +
  theme_minimal(base_size = 10)

# Save image
# ggsave("figures/09-SMC_entropy_dp.png",
#        plot = p,
#        units = 'in',
#        width = 4,
#        height = 3)

# Write file
# write.csv(entropy_df, "data/09-SMC-entropy.csv", row.names = F)
