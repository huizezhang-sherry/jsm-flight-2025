library(arrow)
library(tidyverse)

flight_df <- read_parquet("Year=2024/data_0.parquet")

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

ggsave(p_aa, filename = here::here("figures/delay-aa-hubs.png"), height = 10, width = 15, bg = "white")
ggsave(p_ua, filename = here::here("figures/delay-ua-hubs.png"), height = 10, width = 15, bg = "white")
ggsave(p_dl, filename = here::here("figures/delay-dl-hubs.png"), height = 10, width = 15, bg = "white")
ggsave(p_wn, filename = here::here("figures/delay-wn-hubs.png"), height = 10, width = 15, bg = "white")
