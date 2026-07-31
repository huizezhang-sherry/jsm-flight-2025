library(tidyverse)

ord_entropy_df <- read_csv(here::here('data/ord_entropy_df.csv'),
                           show_col_types = F) |>
  mutate(airline = factor(airline, levels = c("AA", "UA"), labels = c("American", "United"))) |>
  mutate(group = ifelse(airline == "American", 1, ifelse(
    airline == "United" & year < 2001, 2, 3
  )))

col_list <- c("American" = "black", "United" = "#00a9b7")

events_df <- tibble(year = 2001, reason = "9/11",
                    airline = "American") |>
  bind_rows(
    tibble(year = 2013, reason = "US Airway \n merger", airline = "American")) |>
  bind_rows(
    tibble(year = 2021, reason = "COVID", airline = "United")) |>
  bind_rows(
    tibble(year = 2003, reason = "United \n bankruptcy", airline = "United")) |>
  left_join(ord_entropy_df |> select(airline, airport, year, Departure)) |>
  filter(year != 2001) |>
  filter(!year %in% c(2001, 2021))

fig8 <- ord_entropy_df |>
  filter(Departure > 0) |>
  ggplot(aes(x = year, y = Departure, group = airline, color = airline)) +
  geom_line(aes(group = group), linewidth = 1, alpha = 0.6) +
  geom_point(size = 1, aes(shape = airline)) +
  geom_point(data = events_df, size = 2, aes(shape = airline), alpha = 0.6) +
  ggrepel::geom_label_repel(
    data = events_df |> filter(year != 2013), aes(label = reason), color = "black",
    nudge_x = 5, nudge_y = 0.3, segment.curvature = 0.2,
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
  scale_color_manual(name = 'Airline', values = col_list) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        text = element_text(colour = "black", size = 10)) +
  guides(shape = 'none',
         color = guide_legend(override.aes = list(shape = c(16, 17)))) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous() +
  labs(y = "Departure Entropy", x = "Year")

ggsave(fig8,
       filename = "figures/fig8.png",
       units = 'cm',
       width = 19,
       height = 8,
       bg = 'white')
