knitr::opts_chunk$set(echo = TRUE)

df_aa_airports <- read.csv("data/pairs80.csv")
aa_airports <- df_aa_airports$origin[which(df_aa_airports$airline  == "AA")]
print(aa_airports)


library(arrow)
library(tidyverse)
library(dplyr)

