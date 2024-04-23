
#read in required packages
require(readxl)
require(tidyverse)
library(ggplot2)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

#################################### 
### load worldpop data (downloaded from google earth engine)
#################################### 

### add population data
population_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data/mdd_population_yearly.csv")
population_mdd$cluster <- population_mdd$layer
population_mdd <- population_mdd[,c(2:22,25)]
colnames(population_mdd) <- c(as.character(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="years")), 'cluster')
population_mdd_long <- population_mdd %>%
  pivot_longer(cols = c(1:21), 
               names_to = "year", 
               values_to = "population")
population_mdd_long$year <- as.Date(population_mdd_long$year)

#################################### 
### load cleaned diresa pop data
#################################### 

cleaned_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_all_years_final.csv")
cleaned_diresa_pop <- cleaned_diresa_pop[,c(2:20)]
colnames(cleaned_diresa_pop)[c(7:15)] <- c(as.character(seq(as.Date("2009-01-01"), as.Date("2017-01-01"), by="years")))
colnames(cleaned_diresa_pop)[c(16:19)] <- c(as.character(seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by="years")))
colnames(cleaned_diresa_pop)[c(6)] <- "cluster"

#################################### 
## impute missing population values from 2000 to 2020 based on worldpop ratio
#################################### 
cleaned_diresa_pop <- cleaned_diresa_pop[,c(6:19)]

# change into long format and group into clusters 
# (need to use clusters to have shapefiles w which to download world pop data, can change these to be whatever groupings you'd prefer)
cleaned_diresa_pop <- cleaned_diresa_pop %>%
  pivot_longer(cols = c(2:14), 
               names_to = "year", 
               values_to = "population")%>%
  group_by(cluster,year) %>%
  summarize(population=sum(population, na.rm=T),
            year=max(year))
cleaned_diresa_pop$year <- as.Date(cleaned_diresa_pop$year)

#full join with worldpop data
all_pop <- full_join(population_mdd_long, cleaned_diresa_pop, by=c('cluster','year'))
colnames(all_pop) <- c('cluster', 'year', 'worldpop', 'diresapop')

#change zeros to NAs
all_pop$diresapop[which(all_pop$diresapop==0)] <- NA

#get average ratio between the diresa and worldpop for EACH CLUSTER
all_pop$ratio <- all_pop$worldpop/all_pop$diresapop
all_pop <- all_pop %>%
  group_by(cluster) %>%
  mutate(average_ratio = mean(ratio, na.rm=T))
all_pop$worldpop_ratio <- all_pop$worldpop/all_pop$average_ratio

#individual ratios
all_pop$individual_ratio <- NA
for(i in 1:dim(all_pop)[1]){
  all_pop$individual_ratio[i] <- all_pop$worldpop[i+1]/all_pop$worldpop[i]
}

for(i in 1:dim(all_pop)[1]){
  all_pop$individual_ratio[i] <- all_pop$worldpop[i+1]/all_pop$worldpop[i]
}

#fill in the blanks, keep the "known" values
#start here.. should it be the average or a year by year linear interpolation ? ie missing 2008 is the ratio of world pop 2008 to just 2009 (most recent year, like below)
adjusted_diresa_pop <- all_pop[,c(1,2,7)]
colnames(adjusted_diresa_pop) <- c('cluster', 'year', 'population')

#linearly interpolate for 2021 and 2022
adjusted_diresa_pop <- adjusted_diresa_pop %>%
  pivot_wider(names_from = "year",
              values_from = "population")
adjusted_diresa_pop <- adjusted_diresa_pop[,c(1:22)]
adjusted_diresa_pop <- adjusted_diresa_pop[,c(1:22)]
adjusted_diresa_pop$ratio_20_19 <- adjusted_diresa_pop$`2020-01-01`/adjusted_diresa_pop$`2019-01-01`
adjusted_diresa_pop$`2021-01-01` <- adjusted_diresa_pop$ratio_20_19*adjusted_diresa_pop$`2020-01-01`
adjusted_diresa_pop$`2022-01-01` <- adjusted_diresa_pop$ratio_20_19*adjusted_diresa_pop$`2021-01-01`
adjusted_diresa_pop <- adjusted_diresa_pop[,c(1:22,24,25)]
adjusted_diresa_pop <- adjusted_diresa_pop[which(adjusted_diresa_pop$cluster!=66),]

#export imputed population data
write.csv(adjusted_diresa_pop, "~/Desktop/doctorate/ch2 mdd highway/data/population_data_adjusted.csv")
