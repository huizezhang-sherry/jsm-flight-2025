library(tidyverse)
library(patchwork)
source(here::here("scripts/00-SH-shared-functions.R"))
flight_2001 <- read_parquet("Year=2001/data_0.parquet")

# flight_aa <- read_csv(here::here("data/AA_flights.csv"))
# flight_ua <- read_csv(here::here("data/UA_flights.csv"))
#
# flight_ord <- bind_rows(flight_aa, flight_ua) |>
#   filter(Origin == "ORD" | Dest == "ORD") |>
#   filter(!is.na(DepTime), !is.na(ArrTime))

# ord_hubs_spokes <- flight_ord |>
#   mutate(year = year(FlightDate),
#          DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
#          ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
#   select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest, year) |>
#   rename(dep_time = DepTime, arr_time = ArrTime,
#          dep_airport = Origin, arr_airport = Dest,
#          airline = Reporting_Airline) |>
#   pivot_longer(cols = -c(FlightDate, airline, year),
#                names_to = c("type", ".value"), names_sep = "_") |>
#   dplyr::filter(airport %in% "ORD") |>
#   arrange(time) |>
#   mutate(block = assign_time_blocks(time, 15)) |>
#   count(airline, airport, type, block, year) |>
#   mutate(airline_airport = paste(airline, airport, sep = "/ ")) |>
#   mutate(n = ifelse(type == "dep", n, -n))
# write_csv(ord_hubs_spokes, file = here::here("data/ord_hubs_spokes.csv"))

ord_hubs_spokes <- read_csv(here::here("data/ord_hubs_spokes.csv"))
binned_data <- ord_hubs_spokes |>
  complete(airline, airport, type, block, year, fill = list(n = 0)) |>
  mutate(airline_airport = paste(airline, airport, sep = "/ "))

# Fit a smooth spline for each airport and type
calc_smooth <- function(data, spar = 0.5){
  res <- smooth.spline(data$block, data$n, spar = spar)
  tibble(x = res$x, fitted = res$y)
}

ord_splines_df <- ord_binned_data |>
  mutate(n = abs(n)) |>
  nest(data = -c(airline, airport, type, year)) |>
  rowwise() |>
  #filter(nrow(data) > 48) |>
  mutate(smooth_res = list(calc_smooth(data, spar = 0.05))) |>
  unnest(smooth_res) |>
  select(-data) |>
  mutate(block = as_datetime(x)) |>
  select(-x)

# diagnostic on spline fit
# dt <-  splines_df |>
#   filter(airline == "UA") |>
#   filter((airport %in% airports)) |>
#   mutate(fitted = ifelse(type == "dep", fitted, -fitted))
#
# p2 <- flight_df |>
#   filter(Reporting_Airline %in% c("UA")) |>
#   filter((Origin %in% airports| Dest %in% airports)) |>
#   summarize_count(airports = airports) |>
#   ggplot(aes(x = block, y = n, color = type, fill = type)) +
#   geom_col() +
#   facet_wrap(vars(airline_airport), scales = "free_y", ncol = 1) +
#   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
#   theme_minimal() +
#   xlab("Time of the day") +
#   ylab("Count")
#
# p_smooth <- dt |>
#   ggplot(aes(x = block, y = fitted, color = type, fill = type, group = type)) +
#   geom_line() +
#   facet_wrap(vars(airport), ncol = 1, scale = "free_y") +
#   scale_x_datetime(date_labels =  "%H:%M", date_breaks = "2 hour") +
#   theme_minimal() +
#   xlab("Time of the day") +
#   ylab("Standardized spline fit")
#
# (p2 | p_smooth) + plot_layout(guides = 'collect') &
#   theme(legend.position = 'bottom')


calc_fft <- function(dt){
  signal <- dt$fitted
  n <- length(signal)

  fft_result <- fft(signal) # actually doing fft
  modulus <- Mod(fft_result)[1:(n*0.5)] # amplitudes
  freqs <- (1:(n*0.5)) / n # frequencies
  periods_in_minutes <- 1 / freqs

  tibble(
    period_mins = periods_in_minutes,
    amplitude = modulus
  ) |>
    dplyr::filter(is.finite(period_mins), period_mins <= 1440) # up to 24 hours
}

ord_fft_all <- ord_splines_df |>
  nest(data = -c(airline, airport, type, year)) |>
  rowwise() |>
  mutate(fft_res = list(calc_fft(data))) |>
  select(-data) |>
  unnest(fft_res)

# Get entropy!
ord_entropy_df <- ord_fft_all |>
  group_by(airline, airport, type, year) %>%
  summarise(prob = amplitude^2 / sum(amplitude^2)) %>%
  summarise(entropy = sum(-prob*log(prob), na.rm = T)) |>
  pivot_wider(names_from = type, values_from = entropy)

events_df <- tibble(year = 2001, reason = "post 9/11",
       airline = "AA") |>
  bind_rows(
    tibble(year = 2014, reason = "AA-US Airway \n merger in 2013", airline = "AA")) |>
  bind_rows(
    tibble(year = 2021, reason = "COVID", airline = "UA")) |>
  bind_rows(
    tibble(year = 2003, reason = "Retrenchment after \n Chapter 11 bankruptcy", airline = "UA")) |>
  left_join(ord_entropy_df |> select(airline, year, dep))


p_ord <- ord_entropy_df |>
  filter(!(airline == "UA" & year == 2001)) |>
  ggplot(aes(x = year, y = dep, group = airline)) +
  geom_point(data = tibble(year = 2001, dep = 1.09, airline = "UA"), shape = 1, stroke = 1, size = 10) +
  geom_point(color = "#bf5700", size = 10) +
  geom_line(color = "#bf5700") +
  ggrepel::geom_label_repel(
    data = events_df, aes(label = reason), color = "black", size = 10,
    nudge_x = 3, nudge_y = 0.1, segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.015, "npc")),
    segment.linetype = "dotted",
    min.segment.length = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none', axis.text.y = element_blank(),
        text = element_text(colour = "black", size = 30)) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  ylab("Departure \n Entropy")


ggsave(p_ord, filename = here::here("figures/12-SH-ord-dep-entropy.png"), height = 5, width = 25, bg = "white")


