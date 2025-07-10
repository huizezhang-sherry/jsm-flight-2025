library(tidyverse)

revenue <- read_csv(here::here("data/DB1B-texas.csv"))

df_aa_airports <- read.csv("data/pairs80.csv")
quantile_df <- tibble(df_aa_airports) |>
  filter(airline == "AA") |>
  group_by(quartile) |>
  mutate(id = row_number())

df <- splines_df |>
  filter(type == "arr") |>
  left_join(quantile_df, by = c("airport" = "origin"))

df |>
  ggplot() +
  geom_line(aes(x = time, y = fitted)) +
  geom_text(data = df |> group_by(airport, quartile, id) |> filter(row_number() == 1),
            aes(x = as_datetime("2005-12-31 18:00:00") + 100, y = 12, label = airport)) +
  facet_grid(id ~ quartile, scales = "free_y") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
  labs(title = "Smoothed Flight Activity (Spline Fit)",
       x = "Time of Day", y = "Flight Count")
ggsave(file = "figures/pair20.png", height = 8, width = 16)

