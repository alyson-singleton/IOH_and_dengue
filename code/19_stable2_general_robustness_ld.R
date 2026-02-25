# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Table S2.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

# Load data & results
stable2 <- read_rds("results/supplementary_text_results/stable2_general_robustness_ld.rds")

#####################
## STable 2
#####################

stable2 <- stable2[c(3:10,17,16,15),2:11]
stable2[11,] <- (as.numeric(gsub(",","",stable2[10,])))/15
colnames(stable2) <- c('Main model', 'No PM', 'No LUC', 'Pop weight','1km boundary',
                       '10km boundary','No buffer (<5km v >5km)', 'Large buffer (<5km v >20km)', 
                       'Conf & prob cases', 'Units w dengue')
rownames(stable2) <- c("Urban", "Agriculture", "Precipitation", "Temperature", 
                       "Temperature^2", "5km x Post-2008", "1km x Post-2008", 
                       "10km x Post-2008", "R^2", "N", "Units")
stable2 <- stable2[c(6:8, 1:5, 9:11),]
stable2[9,] <- round(as.numeric(stable2[9,]),3)
stable2
write.csv(stable2, "figures/stable2.csv")
