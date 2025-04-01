library(arrow)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(naniar)

# the import code from JSM website, changed to only year 2024
base_url <- "https://blobs.duckdb.org/flight-data-partitioned/"
files <- paste0("Year=", 1987:2024, "/data_0.parquet")
for (dir in dirname(files)) dir.create(dir, showWarnings = FALSE)
out <- curl::multi_download(paste0(base_url, files), files, resume = TRUE)

# read in parquet file
df <- read_parquet("Year=2024/data_0.parquet")

# make some simple exploratory plots
f886 <- subset(df, Flight_Number_Reporting_Airline == 886)
f886 <- subset(df, Flight_Number_Reporting_Airline == 886 & Reporting_Airline == "DL" & OriginAirportID == 12478)
ggplot() +
  geom_hline(yintercept = 0, color = "lightgray", linewidth = .25) +
  geom_hline(yintercept = mean(f886$ArrDelay, na.rm = T), color = "#FF5733", linewidth = .25) +
  geom_text(aes(x = f886$FlightDate[1], y = mean(f886$ArrDelay, na.rm = T) + 5, label = "Average arrival delay", hjust = 0, colour = "#FF5733")) +
  scale_color_manual(values = "#FF5733") +
  guides(color = "none") +
  geom_point(data = f886, aes(x = FlightDate, y = ArrDelay)) +
  labs(x = "Date", y = "Arrival Delay (Minutes)", title = "Arrival Delays from Jan-Mar 2024 for Delta Flight 886 from New York City to Miami") +
  theme_few()

#########

f400 <- subset(df, Flight_Number_Reporting_Airline == 400 & Reporting_Airline == "AA" & OriginAirportID == 11298)
ggplot() +
  geom_hline(yintercept = 0, color = "lightgray", linewidth = .25) +
  geom_hline(yintercept = mean(f400$ArrDelay, na.rm = T), color = "#FF5733", linewidth = .25) +
  geom_text(aes(x = f400$FlightDate[1], y = mean(f400$ArrDelay, na.rm = T) + 5, label = "Average arrival delay", hjust = 0, colour = "#FF5733")) +
  scale_color_manual(values = "#FF5733") +
  guides(color = "none") +
  geom_point(data = f400, aes(x = FlightDate, y = ArrDelay)) +
  labs(x = "Date", y = "Arrival Delay (Minutes)", title = "Arrival Delays from Jan-Feb 2024 for AA Flight 400 from Dallas/Fort Worth to Salt Lake City") +
  theme_few()

#########

names <-
  data.frame(
    Airline = unique(df$Reporting_Airline),
    Name = c(
      "Endeavor",
      "American",
      "Alaska",
      "JetBlue",
      "Delta",
      "Frontier",
      "Allegiant",
      "Hawaiian",
      "Envoy",
      "Spirit",
      "Republic",
      "PSA",
      "Skywest",
      "Southwest",
      "United"
    )
  )

cancel <- aggregate(df[ ,c("Cancelled")], list(Airline = df$Reporting_Airline), sum)
cancel <- merge(cancel, names)
cancel <- cancel[order(-cancel$Cancelled), ]
rownames(cancel) <- NULL
cancel$Name <- factor(cancel$Name, levels = cancel$Name)
ggplot(data = cancel, aes(x = Name, y = Cancelled)) + geom_bar(stat = "identity", fill = "#6a6a6a") +
  theme_few() +
  labs(x = "Airline", y = "Cancelled Flights", title = "Number of Cancelled Flights by Airline in 2024")

total <- table(df$Reporting_Airline)
total <- as.data.frame(total)
colnames(total) <- c("Airline", "Total")
props <- merge(total, cancel)
props$Cancelled_Proportion <- props$Cancelled / props$Total
props <- props[order(-props$Cancelled_Proportion), ]
rownames(props) <- NULL
props$Name <- factor(props$Name, levels = props$Name)
ggplot(data = props, aes(x = Name, y = Cancelled_Proportion*100)) + geom_bar(stat = "identity", fill = "#6a6a6a") +
  theme_few() +
  labs(x = "Airline", y = "Percentage of Cancelled Flights", title = "Percentage of Cancelled Flights by Airline in 2024")

#########

get_unique_count <- function(y) {
  df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
  tns <- data.frame(year = c(y),
                    n = c(nrow(df)),
                    na = c(sum(is.na(df$Tail_Number))),
                    unique = I(list(unique(df$Tail_Number))),
                    unique_n = c(length(unique(df$Tail_Number))))
  return(tns)
}

counts_list <- lapply(1987:2024, get_unique_count)
counts_df <- bind_rows(counts_list, .id = "column_label")
counts_df$unique_n <- ifelse(is.na(counts_df$unique), 0, counts_df$unique_n)

ggplot() + geom_point(data = counts_df, aes(x = year, y = unique_n)) +
  labs(x = "Year", y = "Number of Unique Tail Numbers") +
  scale_y_continuous(breaks = seq(0, 6000, 1000)) +
  theme_few()

#########

get_tail_numbers_by_flight <- function(y) {
  df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
  df$flight <- paste0(df$Reporting_Airline, df$Flight_Number_Reporting_Airline)
  df <- as.data.frame(df[, c("flight", "Tail_Number")])
  df <- df %>% group_by(flight) %>% mutate(unique_types = n_distinct(Tail_Number))
  df <- unique(df[, c("flight", "unique_types")])
  df$year <- y

  return(df)
}

counts2_list <- lapply(2000:2024, get_tail_numbers_by_flight)
counts2_df <- bind_rows(counts2_list, .id = "column_label")

airlines <- read.csv("L_UNIQUE_CARRIERS.csv")
colnames(airlines) <- c("airline", "name")
counts2_df$airline <- substr(counts2_df$flight, 1, 2)
counts2_df <- merge(counts2_df, airlines)

counts2_df <- subset(counts2_df, name %in% c("Alaska Airlines Inc.",
                                             "American Airlines Inc.",
                                             "Delta Air Lines Inc.",
                                             "Frontier Airlines Inc.",
                                             "JetBlue Airways",
                                             "Southwest Airlines Co.",
                                             "Spirit Air Lines",
                                             "United Air Lines Inc.",
                                             "Virgin America"))

ggplot(data = counts2_df, aes(x = year, y = unique_types, group = year, fill = year)) +
  geom_violin() +
  labs(x = "Year", y = "Number of Unique Tail Numbers by Flight") +
  theme_few() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(1987, 2024, 4)) +
  facet_wrap(~name)

#########

get_na_by_airline <- function(y) {
  df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
  df <- as.data.frame(df[, c("Reporting_Airline", "Tail_Number")])
  df <- df %>% group_by(Reporting_Airline) %>% summarise(missing_count = sum(is.na(Tail_Number)))
  df$year <- y

  return(df)
}

counts3_list <- lapply(2000:2024, get_na_by_airline)
counts3_df <- bind_rows(counts3_list, .id = "column_label")

View(counts3_df)
