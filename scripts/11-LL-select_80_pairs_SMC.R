rm(list = ls())
library(arrow)
library(dplyr)
library(ggplot2)

# # To gggplot2# # To get all years of data
# base_url <- "https://blobs.duckdb.org/flight-data-partitioned/"
# files <- paste0("Year=", 1987:2023, "/data_0.parquet")
# for (dir in dirname(files)) dir.create(dir, showWarnings = FALSE)
# out <- curl::multi_download(paste0(base_url, files), files, resume = TRUE)

# Below is Lydia's code
# import passenger count data
y <- 2015
pv <- read.csv(paste0(getwd(), "/passenger_info/T_T100D_SEGMENT_ALL_CARRIER_", y, ".csv")) # prev "unzipped"
pv <- subset(pv, subset = UNIQUE_CARRIER %in% c("AA", "DL", "WN", "UA"),
             select = c("UNIQUE_CARRIER", "ORIGIN", "QUARTER", "PASSENGERS"))
colnames(pv) <- c("airline", "origin", "quarter", "passengers")
# get yearly passenger counts for airline/airport pairs
pvYear <- pv %>%
  group_by(airline, origin) %>%
  summarise(count = sum(passengers)) %>%
  as.data.frame()

pvYearO <- pv %>%
  group_by(origin) %>%
  mutate(count = sum(passengers)) %>%
  ungroup()

# import flight data
flights <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
flights <- subset(flights, Reporting_Airline %in% c("AA", "DL", "WN", "UA"))
uniPairs <- unique(flights[ ,c("Reporting_Airline", "Origin")])
# get unique airline/airport pairs
colnames(uniPairs) <- c("airline", "origin")

# merge flights pairs with yearly passenger counts
df <- merge(uniPairs, pvYear)

# group into quartiles (for yearly passenger counts), by airline
dfQ <- df %>%
  group_by(airline) %>%
  mutate(quantile = ntile(count, 4)) %>%
  ungroup()

# sort by airline and yearly passenger count (so it is in descending order)
# and then take the top five airports from each airline quartile
df80 <- dfQ %>%
  group_by(airline, quantile) %>%
  arrange(desc(count)) %>%
  slice_head(n = 5)

# # Sarah edits of Lydia's script start here ( i moved stuff around)
y <- 2015
pv <- read.csv(paste0(getwd(),
                      "/passenger_info/T_T100D_SEGMENT_ALL_CARRIER_",# prev /unzipped/
                      y, ".csv")) %>%
  select(UNIQUE_CARRIER, ORIGIN, QUARTER, PASSENGERS) %>%
  rename(airline=UNIQUE_CARRIER, origin=ORIGIN,
         quarter=QUARTER, passengers = PASSENGERS)

pvNew <- pv %>%
  group_by(origin) %>%
  mutate(AirportCount = sum(passengers, na.rm = T)) %>%
  ungroup() %>%
  group_by(origin, airline) %>%
  mutate(count = sum(passengers, na.rm = T),
         Perc = count / AirportCount * 100) %>%
  ungroup() %>%
  select(origin, airline, count, AirportCount, Perc) %>%
  right_join(uniPairs, by = c("airline", "origin")) %>%
  distinct() %>%
  filter(airline %in% c("AA", "DL", "UA", "WN")) %>%
  group_by(airline) %>%
  mutate(quantile = ntile(count, 4)) %>%
  ungroup() %>%
  group_by(airline, quantile) %>%
  arrange(desc(count)) %>%
  slice_head(n = 5)

# # Sarah sanity check
# test <- left_join(df80, pvNew, by = c("airline", "origin"))

## Sarah plots
# By airline, what is the percentage of passengers for an airport/airline pair?
p <- ggplot(pvNew) +
  geom_histogram(aes(x = Perc)) +
  facet_wrap(~airline)

# By airline, what are the passenger count distributions?
q <- ggplot(pvNew) +
  geom_histogram(aes(x = count)) +
  facet_wrap(~airline)

# What about total passenger amount?
r <- ggplot(pvNew) +
  geom_histogram(aes(x = AirportCount)) +
  facet_grid(airline~quantile)

# What about fraction?
s <- ggplot(pvNew) +
  geom_histogram(aes(x = Perc)) +
  facet_grid(airline~quantile)
