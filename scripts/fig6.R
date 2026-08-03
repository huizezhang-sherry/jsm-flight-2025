library(tidyverse)
library(ggrepel)

airports <- read_csv(here::here("data/airports_in_cont_us.csv"),
                     show_col_types = F)

entropy_american <- read_csv(here::here('data/entropy_df.csv'),
                             show_col_types = F) |>
  filter(airline == "American") |>
  filter(airport %in% airports$airport)

aa_fig3 <- c("DFW", "CLT", "ORD", "PHL", "LGA", "LAX", "DCA", "AUS", "ATL", "PDX", "EGE", "ALB", "OAK", "AMA")
aa_other <- c("SFO", "JFK", "BOS")

highlight_df <- entropy_american |>
  filter(airport %in% aa_fig3 | airport %in% aa_other) |>
  mutate(group = ifelse(airport %in% aa_fig3, "highlighted", "others"))

color_df <- c(highlighted = "black", others = "grey60")

fig6 <- entropy_american |>
  ggplot(aes(x = Arrival, y = Departure)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(color = "#00a9b7", size = 2) +
  geom_point(data = highlight_df, aes(color = group), size = 2) +
  ggrepel::geom_text_repel(
    data = highlight_df, aes(label = airport, color = group), size = 3,
    max.overlaps = Inf) +
  scale_x_continuous(breaks = seq(1.5, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(1.5, 4, by = 0.5)) +
  coord_cartesian(xlim = c(1.5, 4), ylim = c(1.5, 4)) +
  scale_color_manual(name = 'Type', values = color_df) +
  theme_minimal(base_size = 10) +
  theme(aspect.ratio = 1,
        legend.position = "none") +
  guides(shape = 'none') +
  xlab("Arrival entropy") +
  ylab("Departure entropy")
ggsave(plot = fig6, filename = here::here("figures/fig6.png"),
       units = 'cm', width = 9, height = 9, bg = "white")
