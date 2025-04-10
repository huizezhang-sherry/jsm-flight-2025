library(arrow)
library(tidyverse)

flight_df <- read_parquet("Year=2024/data_0.parquet")
aa_hub <- c("DFW", "CLT")
dl_hub <- c("ATL", "DTW")
ua_hub <- c("ORD", "DEN")
aa <- flight_df |>
  filter(Reporting_Airline == "AA" & (Origin == aa_hub | Dest == aa_hub) |
           Reporting_Airline == "DL" & (Origin == dl_hub | Dest == dl_hub) |
           Reporting_Airline == "UA" & (Origin == ua_hub | Dest == ua_hub) |
           Reporting_Airline == "AA" & (Origin == "AUS" | Dest == "AUS") |
           Reporting_Airline == "UA" & (Origin == "MEM" | Dest == "MEM") |
           Reporting_Airline == "DL" & (Origin == "STL" | Dest == "STL") |
           Reporting_Airline == "DL" & (Origin == "OMA" | Dest == "OMA")) |>
  #filter(Reporting_Airline == "DL", Origin == "JFK") |>
  #filter(Reporting_Airline == "AA", Origin == "JFK") |>
  #filter(Reporting_Airline == "AA", Origin == "DFW") |>
  #filter(Reporting_Airline == "DL", Origin == "ATL") |>
  mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
         weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
         DepTime = as_datetime(paste0("2024-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2024-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest, DepDelay) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest) |> mutate(a = row_number()) |>
  pivot_longer(cols = -c(FlightDate, Reporting_Airline, DepDelay, a),
               names_to = c("type", ".value"), names_sep = "_") |>
  arrange(time)  |>
  filter(airport %in% c(aa_hub, dl_hub, ua_hub, "AUS", "MEM", "STL", "OMA"))


aa_df <- aa |>
  filter(!is.na(time)) |>
  mutate(block = assign_time_blocks(time, block_size = 10)) |>
  group_by(block, airport) |>
  summarise(DepDelay = mean(DepDelay, na.rm = TRUE))

aa_df |>
  filter(hour(block) >= 8) |>
  ggplot(aes(x = block, y = DepDelay)) +
  geom_col() +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  facet_wrap(vars(airport), scales = "free_y", ncol = 2) +
  theme_minimal()
ggsave(filename = here::here("figures/delay.png"), height = 10, width = 15, bg = "white")
