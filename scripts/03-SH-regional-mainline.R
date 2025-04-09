library(tidyverse)
library(tidygraph)
library(ggraph)
library(sf)
library(broom)
airport_pairs <- read_csv(here::here("data/airport_pairs.csv")) |> filter(year == 2024, quarter == 1)
################################################################################################
################################################################################################
# we know regional airline (9E for Delta, OH for American, YX and OO for American, Delta, and United)
# operates on smaller routes - can we tell the difference of them from those standard routes??
# If we can find a good reason to tell them apart, then we can propose route to big airline company
# to outsource those to smaller regional carrier
regionals <- c("9E", "YX", "OH", "OO")
mainlines <- c("AA", "DL", "UA", "WN")
flight_mr <- flight_df |>
  filter(Reporting_Airline %in% c(regionals, mainlines)) |>
  distinct(Reporting_Airline, Origin, Dest, Distance, DistanceGroup) |>
  mutate(regional = as.factor(ifelse(Reporting_Airline %in% regionals, 1, 0))) |>
  group_by(Origin, Dest) |>
  filter(row_number() == 1) |>
  ungroup()

flight_mr |>
  ggplot(aes(x = Distance, fill = Reporting_Airline)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~regional, ncol = 1)

#############################################################################
joined_df1 <- flight_mr |>
  mutate(pair = paste0(Origin, Dest) ) |>
  left_join(airport_pairs |> mutate(pair = paste0(airport_1, airport_2)), by = "pair" )

joined_df2 <- flight_mr |> filter(is.na(year)) |>
  mutate(pair = paste0(Origin, Dest) ) |>
  select(Reporting_Airline: pair) |>
  left_join(airport_pairs |> mutate(pair = paste0(airport_2, airport_1)), by = "pair" )

mainlines_vs_regionals <- joined_df1 |> filter(!is.na(year)) |>
  bind_rows(joined_df2) |>
  filter(!is.na(airport_1))

########################################################################################
route_count_df <- flight_df |>
  select(Reporting_Airline, Origin, Dest) |>
  group_by(Reporting_Airline) |>
  count(Origin, Dest, sort = TRUE) |>
  rename(from = Origin, to = Dest)

airports_trans <- airports |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(st_crs(usmap::us_map())) |>
  mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) |>
  as_tibble() |>
  filter(between(x, -3000000, 3000000), y < 700000)

route_nodes <- tibble(airport = c(route_count_df$from, route_count_df$to)) |>
  left_join(airports_trans |> select(ident, x, y) |> rename(airport = ident)) |>
  filter(!is.na(x)) # I exclude Hawaii and Alaska
route_count_df <- route_count_df |> filter(from %in% route_nodes$airport & to %in% route_nodes$airport)

route_graph <- tbl_graph(nodes = route_nodes |> distinct(), edges = route_count_df) |>
  activate(nodes) |>
  mutate(degree = centrality_degree())

airport_centrality_df <- route_graph |> as_tibble() |> select(airport, degree) |> arrange(airport)

########################################################################################
mainlines_vs_regionals2 <- mainlines_vs_regionals |>
  select(Reporting_Airline:regional, city1, city2, airport_1, airport_2, nsmiles, passengers) |>
  left_join(airport_centrality_df |> rename(degree_1 = degree), by = c("airport_1" = "airport")) |>
  left_join(airport_centrality_df |> rename(degree_2 = degree), by = c("airport_2" = "airport"))

mainlines_vs_regionals_balance <- mainlines_vs_regionals2 |>
  filter(regional == 0) |>
  head(526) |>
  bind_rows(mainlines_vs_regionals2 |> filter(regional == 1))

res_glm <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(regional ~ nsmiles + passengers + degree_1 + degree_2, data = mainlines_vs_regionals_balance)
aug_glm <- augment(res_glm, new_data = mainlines_vs_regionals_balance)
aug_glm |> conf_mat(truth = regional, estimate = .pred_class)

library(tidymodels)
res_rf <- rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(regional ~ nsmiles + passengers + degree_1 + degree_2, data = mainlines_vs_regionals_balance)
aug_rf <- augment(res_rf, mainlines_vs_regionals_balance)
aug_rf |> conf_mat(truth = regional, estimate = .pred_class)

aug_rf |> filter(.pred_class == 1, regional == 0) |> View()
aug_rf |> filter(.pred_class == 0, regional == 1) |> View()
