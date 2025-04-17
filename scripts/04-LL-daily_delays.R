library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

y = 2024
df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
df <- subset(df, 
             subset = Reporting_Airline %in% c("DL", "UA", "WN", "AA"), 
             select = c("FlightDate", "Month", "Reporting_Airline", 
                        "Tail_Number", "CRSDepTime", "CRSElapsedTime", 
                        "DepDelay"))
df <- subset(df, !is.na(Tail_Number))

#######################################################
##### LOOK AT ONE DAY IN A MONTH FOR EACH AIRLINE #####
#######################################################
dfDay <- subset(df, FlightDate == "2024-02-01")

dfDayUses <- dfDay %>% group_by(FlightDate, Reporting_Airline, Tail_Number) %>% count(name = "dailyUses")
dfDayUsesTotals <- dfDayUses %>% group_by(dailyUses, Reporting_Airline) %>% count()
ggplot(data = dfDayUsesTotals, aes(x = dailyUses, y = n)) + 
  geom_bar(stat = "identity", fill = "#5DB4CD") + 
  labs(x = "Number of times a plane is used in a day", y = "Number of planes") + 
  scale_x_discrete(limits = as.character(seq(1, 10, 1))) + 
  facet_wrap(~Reporting_Airline) + 
  theme_few()

dfDayDels <- dfDay %>% group_by(Reporting_Airline, Tail_Number) %>% summarize("totDelay" = sum(DepDelay, na.rm=TRUE))
dfDayUsesDels <- merge(dfDayUses, dfDayDels)
dfDayUsesDelsTotals <- dfDayUsesDels %>% group_by(dailyUses, Reporting_Airline) %>% summarize("aveDelay" = mean(totDelay, na.rm=TRUE))
ggplot(data = dfDayUsesDelsTotals, aes(x = dailyUses, y = aveDelay)) + 
  geom_bar(stat = "identity", fill = "#5DB4CD") + 
  labs(x = "Number of times a plane is used in a day", y = "Average Total Delay") + 
  scale_x_discrete(limits = as.character(seq(1, 10, 1))) + 
  facet_wrap(~Reporting_Airline) + 
  theme_few() 

############################################################
#### LOOK AT EVERYDAY IN A MONTH FOR A SPECIFIC AIRLINE ####
############################################################

dfSub <- subset(df, Month == 4 & Reporting_Airline == "AA")
dfDayUses <- dfSub %>% group_by(FlightDate, Reporting_Airline, Tail_Number) %>% count(name = "dailyUses")
dfDayUsesTotals <- dfDayUses %>% group_by(dailyUses, Reporting_Airline, FlightDate) %>% count()

ggplot(data = dfDayUsesTotals, aes(x = dailyUses, y = n)) + 
  geom_bar(stat = "identity", fill = "#5DB4CD") + 
  labs(x = "Number of times a plane is used in a day", y = "Number of planes") + 
  scale_x_discrete(limits = as.character(seq(1, 10, 1))) + 
  facet_wrap(~FlightDate) + 
  theme_few()

dfDayDels <- dfSub %>% group_by(Reporting_Airline, Tail_Number, FlightDate) %>% summarize("totDelay" = sum(DepDelay, na.rm=TRUE))
dfDayUsesDels <- merge(dfDayUses, dfDayDels)
dfDayUsesDelsTotals <- dfDayUsesDels %>% group_by(dailyUses, Reporting_Airline, FlightDate) %>% summarize("aveDelay" = mean(totDelay, na.rm=TRUE))
  
ggplot(data = dfDayUsesDelsTotals, aes(x = dailyUses, y = aveDelay)) + 
  geom_bar(stat = "identity", fill = "#5DB4CD") + 
  labs(x = "Number of times a plane is used in a day", y = "Average Total Delay") + 
  scale_x_discrete(limits = as.character(seq(1, max(dfDayUsesDelsTotals$dailyUses), 1))) + 
  facet_wrap(~FlightDate) + 
  theme_few() 



