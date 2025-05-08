library(arrow)
library(readr)
library(dplyr)

y = 2024
df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))

# each month of data was downloaded as a csv file from here: 
# https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGJ&QO_fu146_anzr=b0-gvzr

import_month_data <- function(month) {
  
  filename <- paste0(month, "2024.csv")
  newDat <- read_csv(filename)
  # "column109" appears in the provided parquet files, so adding it for 
  # consistency and to help with merges
  newDat$column109 <- NA
  colnames(newDat) <- colnames(df)
  
  newDat$FlightDate <- as.POSIXct(newDat$FlightDate, 
                                  format = "%m/%d/%Y %I:%M:%S %p")
  newDat$FlightDate <- format(newDat$FlightDate, "%Y-%m-%d")
  
  newDat$TaxiOut <- ifelse(is.na(newDat$TaxiOut), 
                           NA, 
                           format(newDat$TaxiOut, nsmall = 2, trim = TRUE))
  
  newDat$TaxiIn <- ifelse(is.na(newDat$TaxiIn), 
                          NA, 
                          format(newDat$TaxiIn, nsmall = 2, trim = TRUE))
  
  newDat$CarrierDelay <- ifelse(is.na(newDat$CarrierDelay), 
                                NA, 
                                format(newDat$CarrierDelay, 
                                       nsmall = 2, trim = TRUE))
  
  newDat$WeatherDelay <- ifelse(is.na(newDat$WeatherDelay), 
                                NA, 
                                format(newDat$WeatherDelay, 
                                       nsmall = 2, trim = TRUE))
  
  newDat$NASDelay <- ifelse(is.na(newDat$NASDelay), 
                            NA, 
                            format(newDat$NASDelay, nsmall = 2, trim = TRUE))
  
  newDat$SecurityDelay <- ifelse(is.na(newDat$SecurityDelay),
                                 NA, 
                                 format(newDat$SecurityDelay, 
                                        nsmall = 2, trim = TRUE))
  
  newDat$LateAircraftDelay <- ifelse(is.na(newDat$LateAircraftDelay),
                                     NA, 
                                     format(newDat$LateAircraftDelay, 
                                            nsmall = 2, trim = TRUE))
  
  newDat$Div1WheelsOn <- ifelse(is.na(newDat$Div1WheelsOn), 
                                NA, 
                                format(newDat$Div1WheelsOn, trim = TRUE))
  
  newDat$Div1WheelsOff <- ifelse(is.na(newDat$Div1WheelsOff), 
                                 NA, 
                                 format(newDat$Div1WheelsOff, trim = TRUE))
  
  newDat$FirstDepTime <- ifelse(is.na(newDat$FirstDepTime), 
                                NA, 
                                format(newDat$FirstDepTime, trim = TRUE))
  
  return(newDat)
  
}

months <- list("july", "august", "september", "october", "november", "december")
preppedDat <- lapply(months, import_month_data)
julDecDat <- bind_rows(preppedDat)

# # check if new data merges okay with old data
# df10 <- df[1:10,]
# julDecDat10 <- julDecDat[1:10,]
# checkMerge <- rbind(df10, julDecDat10)

write_parquet(julDecDat, "flight_data_jul_dec_2024.parquet")
# julDecDatCheck <- read_parquet("flight_data_jul_dec_2024.parquet")
# View(head(julDecDatCheck))
