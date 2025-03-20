library(arrow)
library(ggplot2)
library(ggthemes)

# the import code from JSM website, changed to only year 2024
base_url <- "https://blobs.duckdb.org/flight-data-partitioned/"
files <- paste0("Year=", 2024:2024, "/data_0.parquet")
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

names <- data.frame(Airline = unique(df$Reporting_Airline), Name = c("Endeavor", "American", "Alaska", 
                                                                  "JetBlue", "Delta", "Frontier", 
                                                                  "Allegiant", "Hawaiian", "Envoy", 
                                                                  "Spirit", "Republic", "PSA", "Skywest", 
                                                                  "Southwest", "United"))

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
