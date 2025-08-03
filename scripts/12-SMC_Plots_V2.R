# Load in plotting things
library(tidyverse)
library(purrr)
library(ggrepel)
library(ggplot2)
library(arrow)

entropy_df <- readRDS("./data-raw/entropy_df_11-SH") # made in 11-SH script

# FIRST PLOT
p1 <- entropy_df |>
  ggplot(aes(x = arr, y = dep, color = hub_type)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 4) +
  facet_wrap(vars(airline), ncol = 4) +
  scale_color_manual(name = 'Hub type',
                     values = c('Nonhub' = '#005f86',
                                'Small' = '#00a9b7',
                                'Medium' = '#f8971f',
                                'Large' = '#bf5700')) +
  theme_bw(base_size = 32) +
  theme(aspect.ratio = 1, legend.position = "left",
        text = element_text(colour = "black", size = 32)) +
  xlab("Arrival entropy") +
  ylab("Departure entropy")
ggsave(p1,
       filename = here::here("figures/12-SMC-four-airlines.png"),
       units = 'in',
       width =  18,
       height = 5.5,
       bg = "white")

# Second plot
entropy_two <- entropy_df |> # american color
  filter(airline == "American") |> filter(airport %in% c("ORD", "DFW", "CLT"))
smallest_three <- entropy_df |> # faa color
  filter(airline == "American") |> mutate(a= sum(arr + dep)) |> arrange(a) |> head(3)

point_size <- 9
faa_color <- "#008000"
american_color <- "#0078D2"

p2 <- entropy_df |>
  filter(airline == "American") |>
  ggplot(aes(x = arr, y = dep, shape = hub_type)) +
  geom_point(color = "grey80", size = 5) +
  geom_point(data = entropy_two, color = american_color, size = point_size) +
  geom_point(data = smallest_three, color = faa_color, size = point_size) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  ggrepel::geom_text_repel(
    data = entropy_two ,
    aes(label = airport),  color = american_color, size = 10,
    direction = "y", nudge_x = 2.5 - entropy_two$arr, hjust = 0,
    segment.curvature = -1e-20, segment.size = 1, segment.linetype = "dotted") +
  ggrepel::geom_text_repel(
    data = smallest_three ,
    aes(label = airport),  color = faa_color, size = 10, angle = 90,
    direction = "x", nudge_y = -2 , hjust = 0,
    segment.curvature = -1e-20, segment.size = 1, segment.linetype = "dashed") +
  scale_shape_manual(values = c("Nonhub" = 15,
                                "Small" = 16,
                                "Medium" = 17,
                                "Large" = 18)) +
  scale_x_continuous(breaks = seq(0.5, 1.75, by = 0.5)) +
  scale_y_continuous(breaks = seq(0.5, 1.75, by = 0.5)) +
  coord_cartesian(xlim = c(0.5, 1.75), ylim = c(0.5, 1.75)) +
  facet_wrap(vars(airline)) +
  theme_bw(base_size = 32) +
  theme(aspect.ratio = 1) +
  guides(shape = 'none') +
  xlab("Arrival entropy") +
  ylab("Departure entropy")

ggsave(p2,
       filename = here::here("figures/12-SMC-American_ZoomedIn.png"),
       units = 'in',
       width = 5.5,
       height = 5.5,
       bg = "white")

# PLOT 3
dt <- readRDS("./data-raw/dt_11-SH") |>
  filter(airport %in% c("ORD", "LGA")) |>
  mutate(airline_airport = factor(airline_airport, levels = c("American/ Chicago O'Hare Int. Airport (ORD)", "American/ LaGuardia Airport (LGA)")),
         plotCols = paste0(airport,type))
count_df <- readRDS("./data-raw/count_df_11-SH") |>
  filter(airport %in% c("ORD", "LGA")) |>
  mutate(airline_airport = factor(airline_airport, levels = c("American/ Chicago O'Hare Int. Airport (ORD)", "American/ LaGuardia Airport (LGA)")),
         plotCols = paste0(airport,type))
color_list <- c("Departure" = "#353F47", "Arrival" = "#00a9b7")
color_list_2 <- c("LGAArrival" = "#008000",
                  "LGADeparture" = "#353F47",
                  "ORDArrival" = "#0078D2",
                  "ORDDeparture" = "#353F47")
legend_name <- "Flight type"

p3 <- ggplot() +
  # Plot using plotCols, but suppress legend
  geom_col(data = count_df, aes(x = block, y = n, fill = plotCols), alpha = 0.5, show.legend = FALSE) +
  geom_line(data = dt, aes(x = block, y = fitted, color = plotCols), size = 2, show.legend = FALSE) +

  # Add invisible layer to force legend for 'type'
  geom_col(data = count_df, aes(x = block, y = n, fill = type), alpha = 0, show.legend = FALSE) +

  facet_wrap(vars(airline_airport), ncol = 1) +
  scale_x_datetime(
    date_labels = "%H:%M",
    date_breaks = "3 hour",
    limits = c(
      ymd_hms("1970-01-01 04:00:00"),
      ymd_hms("1970-01-01 23:59:00"))
  ) +
  scale_fill_manual(name = NULL, values = color_list_2) +
  scale_color_manual(name = NULL, values = color_list_2) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
  theme_minimal(base_size = 32) +
  theme(
    axis.text.y = element_blank(),
    legend.position = 'bottom'
  ) +
  xlab("Time") +
  ylab("Count")

ggsave(p3,
       filename = here::here("figures/12-SMC-histograms_with_spline.png"),
       units = 'in',
       width = 12,
       height = 5,
       bg = "white")

# Last plot
ord_entropy_df <- readRDS("./data-raw/ord_entropy_df-12-SH") %>%
  mutate(airline = factor(airline, levels = c("AA", "UA"), labels = c("American", "United")))

# cols <- brewer.pal(10, 'Paired')
# colU <- cols[6]
# colA <- cols[10]
col_list <- c("American" = "#C30019", "United" = "#1414D4")

events_df <- tibble(year = 2001, reason = "9/11",
                    airline = "American") |>
  bind_rows(
    tibble(year = 2014, reason = "US Airway merger", airline = "American")) |>
  bind_rows(
    tibble(year = 2021, reason = "COVID", airline = "United")) |>
  bind_rows(
    tibble(year = 2003, reason = "United bankruptcy", airline = "United")) |>
  left_join(ord_entropy_df |> select(airline, airport, year, dep))

p4 <- ord_entropy_df |>
  filter(!(airline == "United" & year == 2001)) |>
  ggplot(aes(x = year, y = dep, group = airline, color = airline), alpha = 0.6) +
  geom_line(size = 3, alpha = 0.6) +
  geom_point(size = 5, aes(shape = airline)) +
  geom_point(data = events_df, size = 10, aes(shape = airline), alpha = 0.6) +
  geom_point(data = tibble(year = 2001, dep = 1.09, airline = "United"),
             shape = 17, stroke = 1, size = 13, color = 'grey60') +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year != 2014), aes(label = reason), color = "black", size = 32/.pt,
    nudge_x = 2, nudge_y = 0.3, segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.05, "npc")),
    segment.linetype = "solid",
    segment.size = 1.5,
    min.segment.length = 0) +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year == 2014), aes(label = reason), color = "black", size = 32/.pt,
    nudge_x = 5, nudge_y = 0, segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.05, "npc")),
    segment.linetype = "solid",
    segment.size = 1.5,
    min.segment.length = 0) +
  scale_color_manual(name = 'Airline', values = col_list) +
  theme_bw(base_size = 32) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'right', axis.text.y = element_blank(),
        text = element_text(colour = "black", size = 32)) +
  guides(shape = 'none',
         color = guide_legend(override.aes = list(shape = c(16, 17)))) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  ylab("Departure \n Entropy") +
  xlab("Year")


ggsave(p4,
       filename = here::here("figures/12-SMC-ord-dep-entropy.png"),
       units = 'in',
       height = 4, width = 25,
        bg = "white")


