library(tidyverse)
library(patchwork)
library(arrow)
library(purrr)

# Read in data
entropy_df <- read.csv("data/11-SH-full-analysis-entropy-df.csv") %>%
  rename(arr_ent = arr,
         dep_ent = dep) %>%
  mutate(hub_type = factor(hub_type, levels = c('Nonhub', 'Small', 'Medium', 'Large')))
flight_df <- read_parquet("Year=2017/data_0.parquet")

# Filter data
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

# Merge df
df_delay <- full_join(entropy_df, flight_hubs_spokes, by = c('airline', 'airport')) %>% drop_na()

# Write csv
#write.csv(df_delay, 'data/10-SMC-df-delay.csv', row.names = F)

# Check association
lm_1 <- lm(dep_del ~ airline + hub_type + dep_ent, data = df_delay)
lm_2 <- lm(arr_del ~ airline + hub_type + arr_ent, data = df_delay)

# Plot it
p <- df_delay %>% ggplot(aes(x = dep_del, y = dep_ent, color = hub_type, shape = airline)) +
  geom_point() +
  labs(x = 'Percent of flights delayed 15 minutes or more from an airport',
       y = 'Airport departure entropy') +
  scale_color_manual(values = c('Nonhub' = '#005f86',
                               'Small' = '#00a9b7',
                               'Medium' = '#f8971f',
                               'Large' = '#bf5700')) +
  theme_minimal()
