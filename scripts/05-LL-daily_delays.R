library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

y = 2023
df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
df <- subset(df, 
             subset = Reporting_Airline %in% c("DL", "UA", "WN", "AA"), 
             select = c("FlightDate", "Month", "Reporting_Airline", "Origin",
                        "Tail_Number", "CRSDepTime", "CRSElapsedTime", 
                        "DepDelay", "ArrDelay", "CarrierDelay", "WeatherDelay", 
                        "NASDelay", "SecurityDelay", "LateAircraftDelay", "Cancelled"))
df <- subset(df, Cancelled != 1)

hubs <- data.frame(Reporting_Airline = c("AA", "AA", "DL", "DL"), 
                   Origin = c("DFW", "ORD", "ATL", "MSP"), 
                   hub = c(T, T, T, T))

##########################################################
#### LOOK AT DELAY DIFFERENCE BETWEEN HUB AND NON-HUB ####
##########################################################

#### DEPARTURE DELAY ####

dfsub <- subset(df, Reporting_Airline %in% c("AA", "DL"))
dfsub <- merge(dfsub, hubs, all.x = T)
dfsub$hub <- ifelse(is.na(dfsub$hub), F, dfsub$hub)

dfgroup <- dfsub %>% group_by(FlightDate, Reporting_Airline, hub) %>% summarize("aveDelay" = mean(DepDelay, na.rm=TRUE))
dfwide <- dfgroup %>% pivot_wider(names_from = hub, values_from = aveDelay)
colnames(dfwide) <- c("day", "airline", "notHub", "Hub")
dfwide$dif <- dfwide$notHub - dfwide$Hub
dfwide$col <- ifelse(dfwide$dif < 0, "negative", "positive")

ggplot(data = dfwide, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", y = "Difference between average departure delay from non-hub and hub (as origin)", 
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  guides(fill = "none") + 
  scale_y_continuous(breaks = seq(-80, 50, 10)) +
  facet_wrap(~airline)

#### ARRIVAL DELAY ####

dfsub <- subset(df, Reporting_Airline %in% c("AA", "DL"))
dfsub <- merge(dfsub, hubs, all.x = T)
dfsub$hub <- ifelse(is.na(dfsub$hub), F, dfsub$hub)

dfgroup <- dfsub %>% group_by(FlightDate, Reporting_Airline, hub) %>% summarize("aveDelay" = mean(ArrDelay, na.rm=TRUE))
dfwide <- dfgroup %>% pivot_wider(names_from = hub, values_from = aveDelay)
colnames(dfwide) <- c("day", "airline", "notHub", "Hub")
dfwide$dif <- dfwide$notHub - dfwide$Hub
dfwide$col <- ifelse(dfwide$dif < 0, "negative", "positive")

ggplot(data = dfwide, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", y = "Difference between average arrival delay from non-hub and hub (as origin)", 
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  scale_y_continuous(breaks = seq(-80, 50, 10)) + 
  theme_hc() + 
  guides(fill = "none") + 
  facet_wrap(~airline)

####################################
#### CHECK DELAYS BY DELAY TYPE ####
####################################

dfsub <- subset(df, Reporting_Airline %in% c("AA", "DL"))
dfsub <- dfsub %>% pivot_longer(cols = c("CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay"))
dfsub$value <- as.numeric(dfsub$value)
dfsub <- subset(dfsub, value > 0)
dfsub <- merge(dfsub, hubs, all.x = T)
dfsub$hub <- ifelse(is.na(dfsub$hub), F, dfsub$hub)

dfgroup <- dfsub %>% group_by(FlightDate, Reporting_Airline, name, hub) %>% summarize("aveDelay" = mean(value, na.rm=TRUE))
dfwide <- dfgroup %>% pivot_wider(names_from = hub, values_from = aveDelay)
colnames(dfwide) <- c("day", "airline", "cause", "notHub", "Hub")
dfwide$dif <- dfwide$notHub - dfwide$Hub
dfwide <- subset(dfwide, !is.na(dif))
dfwide$col <- ifelse(dfwide$dif < 0, "negative", "positive")

ggplot(data = dfwide, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  scale_y_continuous(breaks = seq(-1100, 1000, 100)) +
  guides(fill = "none") + 
  facet_wrap(~airline)

ggplot(data = dfwide, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_few() + 
  guides(fill = "none") + 
  facet_grid(cause~airline)

dfwideCarrier <- subset(dfwide, cause == "CarrierDelay")
ggplot(data = dfwideCarrier, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)", 
       title = "Carrier Delay",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  guides(fill = "none") + 
  facet_wrap(~airline)

dfwideLate <- subset(dfwide, cause == "LateAircraftDelay")
ggplot(data = dfwideLate, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)", 
       title = "Late Aircraft Delay",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  guides(fill = "none") + 
  facet_wrap(~airline)

dfwideNAS <- subset(dfwide, cause == "NASDelay")
ggplot(data = dfwideNAS, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)", 
       title = "NAS Delay",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  guides(fill = "none") + 
  facet_wrap(~airline)

dfwideSecurity <- subset(dfwide, cause == "SecurityDelay")
ggplot(data = dfwideSecurity, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)", 
       title = "Security Delay",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  guides(fill = "none") + 
  facet_wrap(~airline)

dfwideWeather <- subset(dfwide, cause == "WeatherDelay")
ggplot(data = dfwideWeather, aes(x = day, y = dif, fill = col)) + 
  geom_col() + 
  labs(x = "", 
       y = "Difference between average delay (among delays > 0) from non-hub and hub (as origin)", 
       title = "Weather Delay",
       caption = "AA Hubs: DFW & ORD, DL Hubs: ATL & MSP") + 
  theme_hc() + 
  guides(fill = "none") + 
  scale_y_continuous(breaks = seq(-1100, 1000, 100)) +
  facet_wrap(~airline)
