library(arrow)
library(tidyverse)

flight_df <- read_parquet("Year=2024/data_0.parquet")
# waves
aa_hub <- c("DFW", "CLT")
dl_hub <- c("ATL", "DTW")
ua_hub <- c("ORD", "DEN")
rand_spoke <- c("AUS")

# I didn't see much of a weekday/ weekend effect, other than just volume
flight_hubs_spokes <- flight_df |>
  filter(Reporting_Airline == "AA" & (Origin == aa_hub | Dest == aa_hub) |
           Reporting_Airline == "DL" & (Origin == dl_hub | Dest == dl_hub) |
           Reporting_Airline == "UA" & (Origin == ua_hub | Dest == ua_hub) |
           Reporting_Airline == "AA" & (Origin == "AUS" | Dest == "AUS") |
           Reporting_Airline == "UA" & (Origin == "MEM" | Dest == "MEM") |
           Reporting_Airline == "DL" & (Origin == "STL" | Dest == "STL") |
           Reporting_Airline == "DL" & (Origin == "OMA" | Dest == "OMA")) |>
  mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
         weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
         DepTime = as_datetime(paste0("2024-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2024-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(FlightDate, Reporting_Airline),
               names_to = c("type", ".value"), names_sep = "_") |>
  filter(airport %in% c(aa_hub, dl_hub, ua_hub, "AUS", "MEM", "STL", "OMA")) |>
  arrange(time)

flight_hubs_spokes |>
  filter(!is.na(time)) |>
  ggplot(aes(time, color = type, fill = type)) +
  geom_density(alpha = 0.4) +
  #geom_histogram(bins = 23 * 6) +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  facet_wrap(vars(airport), scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(strip.text = element_blank())

assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time + floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

flight_hubs_spokes |>
  filter(!is.na(time)) |>
  mutate(block = assign_time_blocks(time)) |>
  count(airport, type, block) |>
  mutate(n = ifelse(type == "dep", n, -n)) |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  facet_wrap(vars(airport), scales = "free", ncol = 2) +
  theme_minimal()
ggsave(filename = here::here("figures/schedule-waves.png"), height = 10, width = 15, bg = "white")
