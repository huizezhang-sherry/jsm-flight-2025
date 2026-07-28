library(tidyverse)
library(arrow)

flight_df_raw <- read_parquet("Year=2017/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"))

airports <- c('AUS', 'DFW')
block_size <- 10

assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time +
    floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

two_df <- flight_df_raw |>
  filter(Reporting_Airline == "AA", Year == 2017,
         (Origin %in% c("AUS", "DFW") | Dest %in% c("AUS", "DFW"))) |>
  filter(!is.na(DepTime), !is.na(ArrTime)) |>
  mutate(DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  rename(dep_time = DepTime, arr_time = ArrTime, airline = Reporting_Airline,
         dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(FlightDate, airline),
               names_to = c("type", ".value"), names_sep = "_") |>
  filter(airport %in% airports) |>
  mutate(block = assign_time_blocks(time, block_size)) |>
  count(airline, airport, type, block) |>
  mutate(airline_airport = paste(airline, airport, sep = "/ ")) |>
  mutate(n = ifelse(type == "dep", n, -n)) |>
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
   mutate(airline_airport = factor(airline_airport,
                                  labels = c("American / Austin-Bergstrom International Airport (AUS)",
                                             "American / Dallas/Fort Worth International Airport (DFW)")))

write_csv(two_df, file = here::here("data/two_df.csv"))
two_df <- read_csv("data/two_df.csv")

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")

fig2 <- two_df |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Count") +
  scale_x_datetime(labels = function(x) {
    lab <- format(x, "%H:%M")
    lab <- sub("^0", "", lab)},
                   date_breaks = "4 hour") +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  xlab("Binned time (10 minute intervals)") +
  ylab("Annual Flight Count") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10))

ggsave(plot = fig2, filename = "figures/fig2.png",
       height = 8, width = 18, unit = "cm", bg = "white")
