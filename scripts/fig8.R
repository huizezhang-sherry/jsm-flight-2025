library(tidyverse)
library(patchwork)

col_list <- c("American Airlines" = "black", "United Airlines" = "#00a9b7")

ord_flight_counts <- read_csv(here::here('data/ord_flight_counts.csv'),
                              show_col_types = F)

count_plot <- ord_flight_counts |>
  mutate(airline = factor(Reporting_Airline,
                          levels = c("AA", "UA"),
                          labels = c("American Airlines", "United Airlines"))) |>
  ggplot(aes(x = Year, y = flight_count, fill = airline)) +
  geom_col() +
  facet_wrap(~airline, nrow = 2) +
  scale_fill_manual(name = '', values = col_list) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 150000, 50000)) +
  labs(y = 'Annual Departing Flights') +
  theme_minimal(base_size = 10) +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(1995, 2025), ylim = c(0, 150000))

ord_entropy_df <- read_csv(here::here('data/ord_entropy_df.csv'),
                           show_col_types = F) |>
  mutate(airline = factor(airline, levels = c("AA", "UA"),
                          labels = c("American Airlines", "United Airlines"))) |>
  mutate(group = ifelse(airline == "American Airlines", 1, ifelse(
    airline == "United" & year < 2001, 2, 3
  )))

events_df <- tibble(year = 2001, reason = "9/11",
                    airline = "American") |>
  bind_rows(
    tibble(year = 2013, reason = "US Airway \n merger",
           airline = "American Airlines")) |>
  bind_rows(
    tibble(year = 2002, reason = "United \n bankruptcy",
           airline = "United Airlines")) |>
  left_join(ord_entropy_df |> select(airline, airport, year, Departure),
            by = join_by('year', 'airline')) |>
  filter(year != 2001)

event_plot <- ord_entropy_df |>
  mutate(Departure = if_else(Departure == 0, NA, Departure)) |>
  ggplot(aes(x = year, y = Departure, group = airline, color = airline)) +
  geom_line(aes(group = group), linewidth = 1, alpha = 0.6) +
  geom_point(size = 1, aes(shape = airline)) +
  geom_point(data = events_df, size = 2, aes(shape = airline), alpha = 0.6) +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year != 2013), aes(label = reason), color = "black",
    nudge_x = 1, nudge_y = 1, segment.curvature = 0.2,
    arrow = arrow(length = unit(0.05, "npc")),
    segment.linetype = "solid",
    size = 3,
    segment.size = 0.5,
    min.segment.length = 0) +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year == 2013),
    aes(label = reason), color = "black",
    nudge_x = 5, nudge_y = 0.3, segment.curvature = 0.2,
    arrow = arrow(length = unit(0.05, "npc")),
    segment.linetype = "solid",
    size = 3,
    segment.size = 0.5,
    min.segment.length = 0) +
  scale_color_manual(name = '', values = col_list) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        text = element_text(colour = "black", size = 10)) +
  guides(shape = 'none',
         color = guide_legend(override.aes = list(shape = c(16, 17)))) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(breaks = seq(1, 3.5, 0.5)) +
  labs(y = "Departure Entropy", x = "Year") +
  coord_cartesian(xlim = c(1995, 2025), ylim = c(1, 3.75))

fig8 <- (event_plot / count_plot) +
  patchwork::plot_layout(nrow = 2, heights = c(1, 1.5)) +
  patchwork::plot_annotation(tag_levels = 'a')

ggsave(fig8,
       filename = "figures/fig8.png",
       units = 'cm',
       width = 18,
       height = 14,
       bg = 'white')
