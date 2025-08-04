# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Table S6.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

# Load results
summary_table_glmmTMB <- read_rds("results/supplementary_text_results/stable6_summary_table_glmmTMB.rds")

#####################
## STable 6
#####################

stable6 <- summary_table_glmmTMB[,c(2:7)]
colnames(stable6) <- c('Dengue Yearly', 'Dengue Biannual Dry', 'Dengue Biannual Rainy','Leish Yearly','Leish Biannual Dry', 'Leish Biannual Rainy')
stable6 <- as.data.frame(rbind(stable6, stable3[c(7:10),]))
stable6 <- stable6 %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
rownames(stable6) <- c("5km x Post-2008 Yearly", "5km x Post-2008 Biannual", "Urban", "Agriculture", "Precipitation", "Temperature^2", "FEs", "Unit", "Year", "Biannual")
stable6
write.csv(stable6, "figures/stable6.csv")