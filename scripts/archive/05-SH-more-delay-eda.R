library(arrow)
library(tidyverse)

flight_df <- read_parquet("Year=2023/data_0.parquet")

dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA")
aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA")
ua_hubs <- c("ORD", "DEN", "IAH", "LAX", "EWR", "SFO", "IAD")
wn_hubs <- c("ATL", "BWI", "MDW", "DAL", "DEN", "HOU", "LAS", "LAX", "BNA", "OAK", "MCO", "PHX")

assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time + floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

delay_group_df <- flight_df |>
  filter((Reporting_Airline == "AA" & Origin %in% aa_hubs) |
           (Reporting_Airline == "DL" & Origin %in% dl_hubs) |
           (Reporting_Airline == "UA" & Origin %in% ua_hubs) |
           (Reporting_Airline == "WN" & Origin %in% wn_hubs)) |>
  filter(!is.na(DepTime), !is.na(ArrTime)) |>
  mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
         weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
         DepTime = as_datetime(paste0("2024-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2024-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest, DepartureDelayGroups) |>
  rename(dep_time = DepTime, arr_time = ArrTime,
         dep_airport = Origin, arr_airport = Dest,
         airline = Reporting_Airline) |>
  mutate(a = row_number()) |>
  arrange(a) |>
  mutate(block = assign_time_blocks(dep_time, block_size = 20)) |>
  group_by(block, airline, dep_airport) |>
  count(DepartureDelayGroups) |>
  mutate(airline_airport = paste0(airline, "_", dep_airport)) |>
  filter(hour(block) >= 8) |>
  group_by(airline, dep_airport, block) |>
  mutate(prop = n/sum(n),
         delay_cat = case_when(
           DepartureDelayGroups %in% c(-2, -1, 0) ~ 0,
           DepartureDelayGroups %in% c(1, 2, 3) ~ 1,
           DepartureDelayGroups %in% c(4, 5, 6) ~ 2,
           TRUE ~ 3))

p1 <- delay_group_df |>
  ggplot(aes(x = block, y = prop, fill = as.factor(delay_cat))) +
  geom_col(position = "fill") +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(vars(airline_airport), scales = "free_y") +
  theme_minimal()

# if you leave from JFK, LGA, ORD, or PHL with AA after 10PM, you're doomed
p1 %+% (delay_group_df |> filter(airline == "AA"))
# if you leave from BOS, JFK, or LGA with DL after 10PM, you're doomed
p1 %+% (delay_group_df |> filter(airline == "DL"))
# if you leave from EWR, ORD, or IAH with UA after 10PM, you're doomed
p1 %+% (delay_group_df |> filter(airline == "UA"))
# if you leave from any "hub" with WN after 10PM, you're doomed
p1 %+% (delay_group_df |> filter(airline == "WN"))

# Is this really interesting? If you leave from a hub after 10pm - it is pretty sure you're doomed (otherwise you would've already been home)....


