library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

y = 2024
df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
df <- subset(df, Reporting_Airline %in% c("DL", "UA", "WN", "AA"))
df$flight <- paste0(df$Reporting_Airline, df$Flight_Number_Reporting_Airline)

## proportion of delays/cancellations per flight
df$dc <- ifelse(df$DepDelay > 0 | df$Cancelled == 1, T, F)
df_flight <- df %>% 
  group_by(flight) %>% 
  summarise(mean = mean(dc))
df_flight$airline <- substr(df_flight$flight, start = 1, stop = 2)

ggplot(data = df_flight, aes(x = mean)) + 
  geom_histogram(colour = "black", fill = "cornflowerblue") + 
  labs(x = "Proportion of Delayed/Cancelled Flights (for a particular flight number)", 
       y = "Number of Flights (for a particular flight number)") + 
  facet_wrap(~airline) + 
  theme_few()

## proportion of delay>=15/cancellations per flight
df$dc <- ifelse(df$DepDel15 == 1 | df$Cancelled == 1, T, F)
df_flight <- df %>% 
  group_by(flight) %>% 
  summarise(mean = mean(dc))
df_flight$airline <- substr(df_flight$flight, start = 1, stop = 2)

ggplot(data = df_flight, aes(x = mean)) + 
  geom_histogram(colour = "black", fill = "cornflowerblue") + 
  labs(x = "Proportion of Flights Delayed (>=15min) or Cancelled (for a particular flight number)", 
       y = "Number of Flights (for a particular flight number)") + 
  facet_wrap(~airline) + 
  theme_few()


## association between delay time and plane size?
aircraft <- read.csv("aircraft.csv")
df$tail_number <- df$Tail_Number
df2 <- merge(df, aircraft[, c("tail_number", "no_seats")], by = "tail_number", all.x = TRUE)
df2 <- df2[ ,c("flight", "DepDelay", "ArrDelay", "no_seats")]
df2$size <- ifelse(df2$no_seats <= 100, "<100", 
                   ifelse(df2$no_seats > 100 & df2$no_seats <= 200, "100<x<=200", 
                          ifelse(df2$no_seats > 200 & df2$no_seats <= 300, "200<x<=300",
                                 ifelse(df2$no_seats > 300 & df2$no_seats <= 400, "300<x<=400",
                                        ifelse(df2$no_seats > 400 & df2$no_seats <= 500, "400<x<=500",
                                               ifelse(df2$no_seats > 500, ">500", NA))))))
df2$size <- factor(df2$size, levels = c("<100", "100<x<=200", "200<x<=300", "300<x<=400", "400<x<=500", ">500"))
df2$airline <- substr(df2$flight, start = 1, stop = 2)

df_flight <- df2 %>% 
  group_by(airline, size) %>% 
  summarise(mean = mean(DepDelay, na.rm = T))
ggplot(data = df_flight, aes(x = size, y = mean)) + 
  geom_bar(stat = "identity") +
  labs(x = "Number of Plane Seats", y = "Average Departure Delay (in minutes)") + 
  facet_wrap(~airline) + 
  theme_few()

df_flight <- df2 %>% 
  group_by(airline, size) %>% 
  summarise(mean = mean(ArrDelay, na.rm = T))
ggplot(data = df_flight, aes(x = size, y = mean)) + 
  geom_bar(stat = "identity") +
  labs(x = "Number of Plane Seats", y = "Average Arrival Delay (in minutes)") + 
  facet_wrap(~airline) + 
  theme_few()



