# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build STable 1.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

# Load data & results
dengue_yearly <- read_rds("data/clean/dengue_yearly_panels.rds")
stable1 <- read_rds("results/supplementary_text_results/stable1_yearly_estimates.rds")

#####################
## STable 1
#####################
stable1 <- stable1[c(3:10,37,11:31,35,34,36),2:3]
stable1[33,] <- c(rep(length(unique(dengue_yearly$connected_buffered$key)),2))
stable1[9,] <- c("----","----")
colnames(stable1) <- c('Dengue Yearly', 'Leish Yearly')
rownames(stable1) <- c("5km x 2000", "5km x 2001", "5km x 2002", "5km x 2003", "5km x 2004", "5km x 2005", "5km x 2006", "5km x 2007", "5km x 2008", "5km x 2009", "5km x 2010", "5km x 2011", "5km x 2012", "5km x 2013", "5km x 2014", "5km x 2015", "5km x 2016", "5km x 2017", "5km x 2018", "5km x 2019", "5km x 2020", "5km x 2021", "5km x 2022", "Urban", "Agriculture", "Precipitation", "Temperature^2", "FEs", "Unit", "Year", "R^2", "N", "Units")
stable1[] <- lapply(stable1, function(col) {
  ifelse(col == "Yes", "X", ifelse(col == "No", "", col))
})
stable1[31,] <- round(as.numeric(stable1[31,]),3)
stable1
write.csv(stable1, "figures/stable1.csv")