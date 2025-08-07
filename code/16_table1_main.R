# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Table 1.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

# Load data & results
dengue_yearly <- read_rds("data/clean/dengue_yearly_panels.rds")
table1 <- read_rds("results/main_text_results/table1_main.rds")

#####################
## Table 1
#####################
table1 <- table1[c(3:9,10:13,17,16,15),2:7]
table1[14,] <- c(rep(length(unique(dengue_yearly$connected_buffered$key)),6))
colnames(table1) <- c('Dengue Yearly', 'Dengue Biannual Dry', 'Dengue Biannual Rainy','Leish Yearly','Leish Biannual Dry', 'Leish Biannual Rainy')
rownames(table1) <- c("Urban", "Agriculture", "Precipitation", "Temperature", "Temperature^2", "5km x Post-2008 Yearly", "5km x Post-2008 Biannual", "FEs", "Unit", "Year", "Biannual", "R^2", "N", "Units")
table1 <- table1[c(6:7,1:5,8:14),]
table1[] <- lapply(table1, function(col) {
  ifelse(col == "Yes", "X", ifelse(col == "No", "", col))
})
table1[12,] <- round(as.numeric(table1[12,]),3)
table1
write.csv(table1, "figures/table1.csv")
