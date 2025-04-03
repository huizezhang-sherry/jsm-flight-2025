library(arrow)

na_results <- data.frame()
for (y in 1995:2024) {
  
  print(y)
  df <- read_parquet(paste0("Year=", y, "/data_0.parquet"))
  df <- df[,(colnames(df) %in% c("Tail_Number", "Cancelled"))]
  
  if ("UNKNOW" %in% df$Tail_Number) {
    na_tn <- subset(df, Tail_Number == "UNKNOW")
    indicator <- "UNKNOW"
    missing_n <- nrow(na_tn)
    missing_p <- round((missing_n / nrow(df)) * 100, 2)
    cancelled_n <- sum(na_tn$Cancelled)
    cancelled_p <- round((cancelled_n / missing_n) * 100, 2)
    cancelled_p2 <- round((cancelled_n / nrow(subset(df, Cancelled == 1))) * 100, 2)
  } else if (NA %in% df$Tail_Number) {
    na_tn <- subset(df, is.na(Tail_Number))
    tab <- table(na_tn$Cancelled)
    indicator <- "NA"
    missing_n <- nrow(na_tn)
    missing_p <- round((missing_n / nrow(df)) * 100, 2)
    cancelled_n <- sum(na_tn$Cancelled)
    cancelled_p <- round((cancelled_n / missing_n) * 100, 2)
    cancelled_p2 <- round((cancelled_n / nrow(subset(df, Cancelled == 1))) * 100, 2)
  } else {
    indicator <- "don't know any indicator yet"
    missing_n <- "--"
    missing_p <- "--"
    cancelled_n <- "--"
    cancelled_p <- "--"
    cancelled_p2 <- "--"
  }
  
  na_result <- data.frame(year = c(y),
                          indicator = c(indicator),
                          missing_count = c(missing_n),
                          missing_perc = c(missing_p),
                          cancelled_count = c(cancelled_n),
                          cancelled_perc_na = c(cancelled_p),
                          cancelled_perc_na_total = c(cancelled_p2)
                          )
  na_results <- rbind(na_results, na_result)
  
}

attr(na_results$year, "label") <- "Year"
attr(na_results$indicator, "label") <- "Identified indicator for a missing value (there could be others)"
attr(na_results$missing_count, "label") <- "Number of missing tail numbers"
attr(na_results$missing_perc, "label") <- "% of flights with a missing tail number"
attr(na_results$cancelled_count, "label") <- "Number of flights with a missing tail number that were cancelled"
attr(na_results$cancelled_perc_na, "label") <- "% of flights with a missing tail number that were cancelled"
attr(na_results$cancelled_perc_na_total, "label") <- "% of cancelled flights with a missing tail number"

View(na_results)
