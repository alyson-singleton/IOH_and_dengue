# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Table S3.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

# Load data & results
dengue_yearly <- read_rds("data/clean/dengue_yearly_panels.rds")
stable3 <- read_rds("results/supplementary_text_results/stable3_precip_quad_df.rds")

#####################
## STable 3
#####################
stable3 <- stable3[c(3:10,11:14,18,17,16),2:7]
stable3[15,] <- c(rep(length(unique(dengue_yearly$connected_buffered$key)),6))
colnames(stable3) <- c('Dengue Yearly', 'Dengue Biannual Dry', 'Dengue Biannual Rainy',
                       'Leish Yearly','Leish Biannual Dry', 'Leish Biannual Rainy')
rownames(stable3) <- c("Urban", "Agriculture", "Precipitation", "Precipitation^2", "Temperature", 
                       "Temperature^2", "5km x Post-2008 Yearly", "5km x Post-2008 Biannual", "FEs", 
                       "Unit", "Year", "Biannual", "R^2", "N", "Units")
stable3 <- stable3[c(7:8,1:6,9:15),]
stable3[] <- lapply(stable3, function(col) {
  ifelse(col == "Yes", "X", ifelse(col == "No", "", col))
})
stable3[13,] <- round(as.numeric(stable3[13,]),3)
stable3
write.csv(stable3, "figures/stable3.csv")
