library(arrow)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(sf)
library(broom)
library(tidymodels)
flight_df <- read_parquet("Year=2024/data_0.parquet")
################################################################################################
################################################################################################
# we know regional airline (9E for Delta, OH for American, YX and OO for American, Delta, and United)
# operates on smaller routes - can we tell the difference of them from those standard routes??
# If we can find a good reason to tell them apart, then we can propose route to big airline company
# to outsource those to smaller regional carrier
regionals <- c("9E", "YX", "OH", "OO")
mainlines <- c("AA", "DL", "UA", "WN")

flight_raw <- flight_df |>
  janitor::clean_names() |>
  filter(reporting_airline %in% c(regionals, mainlines)) |>
  filter(!is.na(dep_time))

flight_mr <- flight_raw |>
  distinct(reporting_airline, origin, dest, distance, distance_group) |>
  mutate(regional = as.factor(ifelse(reporting_airline %in% regionals, 1, 0))) |>
  rowwise() |>
  mutate(concat = sort(c(origin, dest)) |> paste0(collapse = "-")) |>
  group_by(concat, reporting_airline) |>
  filter(row_number() == 1) |>
  ungroup()

res <- flight_mr |>
  nest(-concat) |>
  rowwise() |>
  mutate(airline = list(unique(data$reporting_airline)))


flight_assignment <- res |> filter(length(airline) == 1) |>
  select(-airline) |>
  unnest(data) |>
  select(-concat) |>
  rowwise() |>
  mutate(airline_origin_dest = paste0(c(reporting_airline, origin, dest), collapse = "-"))


flight_final <- flight_raw |>
  rowwise() |>
  mutate(airline_origin_dest = paste0(c(reporting_airline, origin, dest), collapse = "-")) |>
  filter(airline_origin_dest %in% flight_assignment$airline_origin_dest)
flight_final2 <- flight_final |>
  ungroup() |>
  select(-(first_dep_time: div4wheels_off), -(div4tail_num:column109), -airline_origin_dest)

write_csv(flight_final2, file = "data/flights_2024.csv")

pair_used <- flight_mr |> count(concat, sort = TRUE) |> filter(n == 1)

flight_mr2 <- flight_mr |> filter(concat %in% pair_used$concat) |> select(-concat)


flight_mr2 <- flight_assignment |>
  rowwise() |>
  mutate(concat = sort(c(origin, dest)) |> paste0(collapse = "-")) |>
  group_by(concat, reporting_airline) |>
  filter(row_number() == 1) |>
  ungroup()



# doesn't seem that regional airline will take long routes
# distance is useful to predict whether a flight would be operated by mainstream or regional airline
flight_mr2 |>
  count(distance_group, regional) |>
  ggplot() +
  geom_col(aes(x = distance_group, y = n, fill = regional), position = "fill")
#############################################################################
# Would you think its a good idea to add a collection of binary variables of the main airport hubs for each airline?
# Binary variable for whether the route pass DFW, CLT, ORD, etc

# A: it could work because regional routes tend to be from a hub to a low-profile airport
#############################################################################
# Would you think coordinates of the Origin and Destination airport would be useful information to predict?
# If so, how would you use it?

# A: Given how much they've learnt, coordinates are not good predictors to be directly used to predict.
########################################################################################

########################################################################################
# find the centrality measure for the Origin/ Dest
route_count_df <- flight_df |>
  filter(Reporting_Airline %in% c(regionals, mainlines)) |>
  select(Origin, Dest) |>
  count(Origin, Dest, sort = TRUE) |>
  rename(from = Origin, to = Dest)

route_nodes <- tibble(airport = c(route_count_df$from, route_count_df$to))

route_graph <- tbl_graph(nodes = route_nodes |> distinct(), edges = route_count_df) |>
  activate(nodes) |>
  mutate(degree = centrality_degree())

airport_centrality_df <- as_tibble(route_graph) |> arrange(-degree)

########################################################################################
flight_mr3 <- flight_mr2 |>
  left_join(airport_centrality_df |> rename(degree_1 = degree), by = c("origin" = "airport")) |>
  left_join(airport_centrality_df |> rename(degree_2 = degree), by = c("dest" = "airport"))

flight_mr3 |>
  ggplot(aes(x = degree_1, y = degree_2, color = regional)) +
  geom_point() +
  theme(aspect.ratio = 1)

# mainlines_vs_regionals2 <- mainlines_vs_regionals |>
#   select(Reporting_Airline:regional, city1, city2, airport_1, airport_2, nsmiles, passengers) |>
#   left_join(airport_centrality_df |> rename(degree_1 = degree), by = c("airport_1" = "airport")) |>
#   left_join(airport_centrality_df |> rename(degree_2 = degree), by = c("airport_2" = "airport"))
#
# mainlines_vs_regionals2 |>
#   ggplot(aes(x = degree_1, y = degree_2, color = regional)) +
#   geom_point() +
#   theme(aspect.ratio = 1)
#
# airport_centrality_df |> filter(airport %in% c("SEA", "MIA"))
# flight_mr2 |> filter(origin %in% c("SEA", "MIA") & dest %in% c("SEA", "MIA")) |> View()
# mainlines_vs_regionals2 |> filter(Origin %in% c("SEA", "MIA") & Dest %in% c("SEA", "MIA")) |> View()
#

# training-testing split
set.seed(123)
split <- flight_mr3 |> initial_split(prop = 0.8, strata = regional)
flight_train <- training(split)
flight_test <- testing(split)

# fit a logistic regression
data_recipe <- recipe(regional ~ distance + degree_1 + degree_2, data = flight_train)
flight_folds <- vfold_cv(flight_train, v = 10)

glm_mod <- logistic_reg(engine = "glm", mode = "classification")
glm_wf <- workflow() |> add_recipe(data_recipe) |>  add_model(glm_mod)
aug_glm <- glm_wf |> fit(data = flight_train) |> augment(flight_test)

# fit a decision tree
dt_mod <- decision_tree(engine = "rpart", mode = "classification")
dt_wf <- workflow() |> add_recipe(data_recipe) |> add_model(dt_mod)
aug_dt <- dt_wf |> fit(data = flight_train) |> augment(flight_test)

# fit a random forest
rf_mod <- rand_forest(engine = "ranger", mode = "classification", trees = 1000)
rf_wf <- workflow() |> add_recipe(data_recipe) |> add_model(rf_mod)
aug_rf <- rf_wf |> fit(data = flight_train) |> augment(flight_test)

# accuracy
# glm_wf |> fit_resamples(flight_folds) |> collect_metrics()
# dt_wf |> fit_resamples(flight_folds) |> collect_metrics()
# rf_wf |> fit_resamples(flight_folds) |> collect_metrics()

aug_glm |> conf_mat(truth = regional, estimate = .pred_class)
aug_dt |> conf_mat(truth = regional, estimate = .pred_class)
aug_rf |> conf_mat(truth = regional, estimate = .pred_class)

aug_rf |> mutate(model = "rf") |>
  bind_rows(aug_glm |> mutate(model = "logistic")) |>
  bind_rows(aug_dt |> mutate(model = "decisiontree")) |>
  group_by(model) |>
  yardstick::roc_curve(truth = regional,  .pred_0) |>
  autoplot()



# figure out how to deal with imbalanced data




aug_rf |> filter(.pred_class == 1, regional == 0) |> View()
aug_rf |> filter(.pred_class == 0, regional == 1) |> View()
