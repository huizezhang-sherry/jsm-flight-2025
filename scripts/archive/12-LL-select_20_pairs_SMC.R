library(arrow)
library(dplyr)

y <- 2017

# import passenger count data
pv <- read.csv(paste0("./passenger_info/T_T100D_SEGMENT_ALL_CARRIER_", y, ".csv"))
pv <- subset(pv, subset = UNIQUE_CARRIER %in% c("AA","DL","UA","WN"),
             select = c("UNIQUE_CARRIER", "ORIGIN", "QUARTER", "PASSENGERS"))
colnames(pv) <- c("airline", "origin", "quarter", "passengers")
# get yearly passenger counts for airline/airport pairs
pvYear <- pv %>%
  group_by(airline, origin) %>%
  summarise(count = sum(passengers)) %>%
  as.data.frame()

# import flight data
flights <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
flights <- subset(flights, Reporting_Airline %in% c("AA","DL","UA","WN"))
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

colnames(df80) <- c("airline", "origin", "passenger_count", "quartile")
write.csv(df80, "./data/pairs80_v2_SMC.csv", row.names = FALSE)
