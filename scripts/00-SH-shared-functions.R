assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time + floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

summarize_count <- function(data, block_size = 10, airports = c("DFW", "CLT", "ATL", "DTW", "ORD", "DEN")){
  data |>
    filter(!is.na(DepTime), !is.na(ArrTime)) |>
    mutate(DepTime = as_datetime(paste0("1970-01-01", "-", DepTime, "-00")),
           ArrTime = as_datetime(paste0("1970-01-01", "-", ArrTime, "-00"))) |>
    select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
    rename(dep_time = DepTime, arr_time = ArrTime, airline = Reporting_Airline,
           dep_airport = Origin, arr_airport = Dest) |>
    pivot_longer(cols = -c(FlightDate, airline),
                 names_to = c("type", ".value"), names_sep = "_") |>
    filter(airport %in% airports) |>
    mutate(block = assign_time_blocks(time, block_size)) |>
    count(airline, airport, type, block) |>
    mutate(airline_airport = paste(airline, airport, sep = "/ ")) |>
    mutate(n = ifelse(type == "dep", n, -n))
}


plot_dep_arv_pattern <- function(data){
  data |>
    ggplot(aes(x = block, y = n, color = type, fill = type)) +
    geom_col() +
    facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2) +
    scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
    theme_minimal()
}

###############################################################################
###############################################################################
# Usage
library(arrow)
flight_df_2024 <- read_parquet("Year=2024/data_0.parquet")

# `summarize_count()` gives the count summary
# check the `block_size` and `airports` argument
msp_df <- flight_df_2024 |>
  filter(Month == 3, between(DayofMonth, 23, 24), Reporting_Airline == "DL",
         (Origin == "MSP" | Dest == "MSP")) |>
  summarize_count(airports = "MSP")

# plot with `plot_dep_arv_pattern()`
msp_df |> plot_dep_arv_pattern()
# you can add more ggplot syntax to customize the title, etc:
msp_df |> plot_dep_arv_pattern() + ggtitle("MSP - DL on 2024 Mar 23-24")
