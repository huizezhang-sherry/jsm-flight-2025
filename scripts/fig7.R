library(tidyverse)
library(arrow)

aa_hubs <- c("CLT","ORD","DFW","LAX","MIA","JFK","LGA","PHL","PHX","DCA")
dl_hubs <- c("ATL","BOS","DTW","LAX","MSP","JFK","LGA","SLC","SEA")
ua_hubs <- c("ORD", "DEN", "IAH", "LAX", "EWR", "SFO", "IAD")

# 2017 UA source: https://www.usatoday.com/story/travel/flights/todayinthesky/2017/01/26/fleet-and-hubs-united-airlines-numbers/96983530/
# 2017 DL source: https://www.travelandleisure.com/airlines-airports/delta/delta-hubs-around-the-world
# 2017 AA source: https://news.aa.com/centennial/our-stories/9-hubs-9-stories/
# and https://s202.q4cdn.com/986123435/files/doc_news/2018/04/1/CRR-Report-2017.pdf
# and https://www.forbes.com/sites/tedreed/2023/02/17/american-airlines-says-laguardia-airport-is-its-newest-hub/

entropy_df <- read_csv(here::here("data/entropy_df.csv"),
                       show_col_types = F)
airports <- read_csv(here::here("data/airports_in_cont_us.csv"),
                     show_col_types = F)

airline_names <- list('American' = 'American Airlines',
                      'Delta' = 'Delta Air Lines',
                      'United' = 'United Airlines',
                      'Southwest' = 'Southwest Airlines')

hubs <- tibble(airline = "American", airport = aa_hubs) |>
  bind_rows(tibble(airline = "Delta", airport = dl_hubs)) |>
  bind_rows(tibble(airline = "United", airport = ua_hubs)) |>
  mutate(hub = 'Yes')

fig7 <- entropy_df |>
  left_join(hubs, by = c('airline', 'airport')) |>
  replace_na(list(hub = 'No')) |>
  rowwise() |>
  mutate(hub = factor(hub, levels = c("Yes", "No")),
         hub_type = factor(hub_type, levels = c("Nonhub", "Small", "Medium", "Large")),
         airline = airline_names[[airline]]) |>
  ungroup() |>
  filter(airport %in% airports$airport,
         !(airport %in% c("SJU", "STT"))) |>
  ggplot(aes(x = Arrival, y = Departure, color = hub_type, shape = hub)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(vars(airline)) +
  scale_shape_manual(name = "Airline Hub", values = c('Yes' = 17, 'No' = 19)) +
  scale_color_manual(name = 'FAA Classification',
                     values = c('Nonhub' = '#005f86',
                                'Small' = '#00a9b7',
                                'Medium' = '#f8971f',
                                'Large' = '#bf5700')) +
  theme_minimal(base_size = 10) +
  theme(aspect.ratio = 1,
        legend.position = "right",
        text = element_text(colour = "black", size = 10),
        panel.grid.minor = element_blank(),
        legend.box = 'vertical') +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(ncol = 2)) +
  xlab("Arrival entropy") +
  ylab("Departure entropy")

# Save image
ggsave("figures/fig7.png",
       plot = fig7,
       units = 'cm',
       width = 18,
       height = 12,
       bg = 'white')
