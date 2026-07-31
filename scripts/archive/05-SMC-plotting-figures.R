
# Load in packages

rm(list = ls())
library(arrow)
library(tidyverse)

# Get colors
colors <- read.csv("data/poster_color_palette.csv")
arr_color <- colors %>%
  filter(color == "light_orange") %>% select("hex") |> as.character()
dep_color <- colors %>%
  filter(color == "light_blue") %>% select("hex") |> as.character()

###########################################################################
### First figure: arrival departure waves (copied from 02-SH-arr-dep.R) ###
###########################################################################

remake <- FALSE

if (remake){
  flight_df <- read_parquet("Year=2024/data_0.parquet")
  # waves
  aa_hub <- c("DFW")
  dl_hub <- c("ATL")

  # I didn't see much of a weekday/ weekend effect, other than just volume
  flight_hubs_spokes <- flight_df |>
    filter(Reporting_Airline == "AA" & (Origin == aa_hub | Dest == aa_hub) |
           Reporting_Airline == "DL" & (Origin == aa_hub | Dest == aa_hub) |
           Reporting_Airline == "AA" & (Origin == dl_hub | Dest == dl_hub) |
           Reporting_Airline == "DL" & (Origin == dl_hub | Dest == dl_hub)) |>
    mutate(weekday = wday(FlightDate, label = TRUE, week_start = 1),
           weekday = ifelse(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"),
           DepTime = as_datetime(paste0("2024-01-01", "-", DepTime, "-00")),
           ArrTime = as_datetime(paste0("2024-01-01", "-", ArrTime, "-00"))) |>
    select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
    rename(dep_time = DepTime, arr_time = ArrTime,
           dep_airport = Origin, arr_airport = Dest) |>
    pivot_longer(cols = -c(FlightDate, Reporting_Airline),
                 names_to = c("type", ".value"), names_sep = "_") |>
    filter(airport %in% c(aa_hub, dl_hub)) |>
    arrange(time)

  assign_time_blocks <- function(time_vector, block_size = 10) {
    start_time <- min(time_vector)
    block_start <- start_time + floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
    return(block_start)
  }

  plot_df <- flight_hubs_spokes |>
    filter(!is.na(time)) |>
    group_by(airport, Reporting_Airline) |>
    mutate(block = assign_time_blocks(time)) |>
    count(airport, type, block) |>
    mutate(n = ifelse(type == "arr", n, -n),
           AirportAirline = factor(paste0(airport, "-",Reporting_Airline),
                                   labels = list("ATL-AA" = "American - Atlanta",
                                                 "ATL-DL" = "Delta - Atlanta",
                                                 "DFW-AA" = "American - Dallas",
                                                 "DFW-DL" = "Delta - Dallas"),
                                   levels = c("ATL-AA","ATL-DL", "DFW-AA", "DFW-DL")))  %>%
    ungroup()

  AirportAirline <- levels(plot_df$AirportAirline)
  ymax <- c("200", "2000", "200", "2000")

  facet_limits <- data.frame(AirportAirline, ymax)

  plot_df <- left_join(plot_df, facet_limits, by = "AirportAirline")
  #saveRDS(plot_df, "data/05-SMC-plot_df")
}

plot_df <- readRDS("data/05-SMC-plot_df")

p <- plot_df |> ggplot(aes(x = block, y = pmin(n,ymax), color = type, fill = type)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_col(data = plot_df) +
  scale_x_datetime(date_labels =  "%I:00%p", date_breaks = "4 hour") +
  facet_wrap(~AirportAirline, scales = "free_y", nrow = 2, ncol = 2) +
  scale_color_manual(name = "Type", values = c("arr" = arr_color,
                                               "dep" = dep_color)) +
  scale_fill_manual(name = "Type", values = c("arr" = arr_color,
                                               "dep" = dep_color)) +
  theme_minimal() +
  guides(color = 'none') +
  labs(x = "", y = "Flight Count")
