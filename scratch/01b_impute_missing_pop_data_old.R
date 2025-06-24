# ID ----------------------------------------------------------------------
## Aly Singleton

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

population_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_7.5km/mdd_population_yearly.csv")
population_mdd$cluster <- population_mdd$layer
population_mdd <- population_mdd[,c(2:22,25)]
colnames(population_mdd) <- c(as.character(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="years")), 'cluster')
population_mdd$`2021-01-01` <- NA
population_mdd$`2022-01-01` <- NA
population_mdd$`2023-01-01` <- NA
population_mdd_long <- population_mdd %>%
  pivot_longer(cols = c(1:21,23:25), 
               names_to = "year", 
               values_to = "population")
population_mdd_long$year <- as.Date(population_mdd_long$year)

#################################### 
### load cleaned diresa pop data
#################################### 

cleaned_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_all_years_final.csv")
cleaned_diresa_pop <- cleaned_diresa_pop[,c(2:19)]
colnames(cleaned_diresa_pop)[c(6:14)] <- c(as.character(seq(as.Date("2009-01-01"), as.Date("2017-01-01"), by="years")))
colnames(cleaned_diresa_pop)[c(15:18)] <- c(as.character(seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by="years")))

#################################### 
### link to clusters
# (need to use clusters to have shapefiles w which to download world pop data, can change these to be whatever groupings you'd prefer)
#################################### 

e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_E_Salud_Coordinates_Key - best_choice.csv")
id_cluster_key <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_7.5km/idClusterKey_7.5km.csv")
linked_ids_codes <- left_join(e_salud_codes, id_cluster_key, by = 'key')
cleaned_diresa_pop <- full_join(cleaned_diresa_pop, linked_ids_codes[,c(1,2,6)], by = 'key')
colnames(cleaned_diresa_pop)[c(19)] <- "cluster"
cleaned_diresa_pop <- cleaned_diresa_pop[,c(1:5,19,6:18)]

#################################### 
## impute missing population values from 2000 to 2020 based on worldpop ratio
#################################### 

cleaned_diresa_pop <- cleaned_diresa_pop[,c(6:19)]

# change into long format and group into clusters 
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
all_pop$individual_ratio <- all_pop$worldpop/all_pop$diresapop
all_pop <- all_pop %>%
  group_by(cluster) %>%
  mutate(average_ratio = mean(individual_ratio, na.rm=T))
all_pop$worldpop_adjusted <- all_pop$worldpop/all_pop$average_ratio

#fill in the blanks, keep the "known" values
all_pop$adjusted_pop <- all_pop$diresapop
all_pop$adjusted_pop[which(is.na(all_pop$adjusted_pop))] <- all_pop$worldpop_adjusted[which(is.na(all_pop$adjusted_pop))]

#################################### 
## fix edge cases
#################################### 

#clusters that dont have any worldpop/diresa overlap just use worldpop
all_pop_clusters_wo_diresapop_2000_2020 <- all_pop[which(all_pop$year %in% c(seq(as.Date("2000-01-01"), as.Date("2020-12-01"), by="years"))),] %>%
  group_by(cluster) %>%
  summarize(diresapop_sum = sum(diresapop, na.rm=T))
clusters_wo_diresa_2000_2020 <- all_pop_clusters_wo_diresapop_2000_2020$cluster[which(all_pop_clusters_wo_diresapop_2000_2020$diresapop_sum==0)]
all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2000_2020)] <- all_pop$worldpop[which(all_pop$cluster %in% clusters_wo_diresa_2000_2020)]

#cluster that dont have diresa (or worldpop) data for 2020-2022, linearly interpolate
all_pop_clusters_wo_diresapop_2021_2022 <- all_pop[which(all_pop$year %in% c(seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by="years"))),] %>%
  group_by(cluster) %>%
  summarize(diresapop_sum = sum(diresapop, na.rm=T))
clusters_wo_diresa_2021_2022 <- all_pop_clusters_wo_diresapop_2021_2022$cluster[which(all_pop_clusters_wo_diresapop_2021_2022$diresapop_sum==0)]

all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2021-01-01"))] <- 
  (all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2020-01-01"))] / 
  all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2019-01-01"))]) * 
  all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2020-01-01"))]

all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2022-01-01"))] <- 
  (all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2021-01-01"))] / 
     all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2020-01-01"))]) * 
  all_pop$adjusted_pop[which(all_pop$cluster %in% clusters_wo_diresa_2021_2022 & all_pop$year==as.Date("2021-01-01"))]

#reduce to final columns
adjusted_diresa_pop <- all_pop[,c(1,2,8)]
colnames(adjusted_diresa_pop) <- c('cluster', 'year', 'population')

#throw out 2023
adjusted_diresa_pop <- adjusted_diresa_pop[-which(adjusted_diresa_pop$year=="2023-01-01"),]

#export imputed population data
write.csv(adjusted_diresa_pop, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/adjusted_pop_all_years_clusters_final_7.5km.csv")

#################################### 
## SCRATCH
#################################### 
#individual ratios
#all_pop$individual_ratio <- NA
#for(i in 1:dim(all_pop)[1]){
#  all_pop$individual_ratio[i] <- all_pop$worldpop[i+1]/all_pop$worldpop[i]
#}


