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

population_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_0km/mdd_population_yearly.csv")
population_mdd <- population_mdd[,c(2:22,24)]
colnames(population_mdd) <- c(as.character(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="years")), 'key')
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

# e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_E_Salud_Coordinates_Key.csv")
# id_cluster_key <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_7.5km/idClusterKey_7.5km.csv")
# linked_ids_codes <- left_join(e_salud_codes, id_cluster_key, by = 'key')
# cleaned_diresa_pop <- full_join(cleaned_diresa_pop, linked_ids_codes[,c(1,2,6)], by = 'key')
# colnames(cleaned_diresa_pop)[c(19)] <- "cluster"
# cleaned_diresa_pop <- cleaned_diresa_pop[,c(1:5,19,6:18)]

#################################### 
## impute missing population values from 2000 to 2020 based on worldpop ratio
#################################### 

cleaned_diresa_pop <- cleaned_diresa_pop[,c(1,6:18)]

# change into long format and group into clusters 
cleaned_diresa_pop <- cleaned_diresa_pop %>%
  pivot_longer(cols = c(2:14), 
               names_to = "year", 
               values_to = "population")%>%
  group_by(key,year) %>%
  summarize(population=sum(population, na.rm=T),
            year=max(year))
cleaned_diresa_pop$year <- as.Date(cleaned_diresa_pop$year)

#full join with worldpop data
all_pop <- full_join(cleaned_diresa_pop, population_mdd_long, by=c('key','year'))
colnames(all_pop) <- c('key', 'year', 'diresapop', 'worldpop')

#change zeros to NAs
all_pop$diresapop[which(all_pop$diresapop==0)] <- NA
all_pop$diresapop[which(is.nan(all_pop$diresapop))] <- NA

#get average ratio between the diresa and worldpop for EACH KEY
all_pop$individual_ratio <- all_pop$worldpop/all_pop$diresapop
all_pop <- all_pop %>%
  group_by(key) %>%
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
  group_by(key) %>%
  summarize(diresapop_sum = sum(diresapop, na.rm=T))
clusters_wo_diresa_2000_2020 <- all_pop_clusters_wo_diresapop_2000_2020$key[which(all_pop_clusters_wo_diresapop_2000_2020$diresapop_sum==0)]
all_pop$adjusted_pop[which(all_pop$key %in% clusters_wo_diresa_2000_2020)] <- all_pop$worldpop[which(all_pop$key %in% clusters_wo_diresa_2000_2020)]

#keys that dont have diresa data for 2020-2022, linearly interpolate
clusters_wo_diresa_2021_2022 <- all_pop %>%
  filter(year %in% as.Date(c("2021-01-01", "2022-01-01"))) %>%
  group_by(key) %>%
  summarize(diresapop_sum = sum(diresapop, na.rm = TRUE), .groups = "drop") %>%
  filter(diresapop_sum == 0) %>%
  pull(key)

# Step 2: Reshape to wide format for reliable matching
pop_wide <- all_pop %>%
  filter(key %in% clusters_wo_diresa_2021_2022,
         year %in% as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01"))) %>%
  dplyr::select(key, year, adjusted_pop) %>%
  pivot_wider(names_from = year, values_from = adjusted_pop)

# Step 3: Compute interpolated values for missing years
pop_wide <- pop_wide %>%
  mutate(
    `2021-01-01` = if_else(
      is.na(`2021-01-01`),
      (`2020-01-01` / `2019-01-01`) * `2020-01-01`,
      `2021-01-01`
    ),
    `2022-01-01` = if_else(
      is.na(`2022-01-01`),
      (`2021-01-01` / `2020-01-01`) * `2021-01-01`,
      `2021-01-01`
    )
  )

# Step 4: Reshape back to long format and update original dataset
pop_long_updates <- pop_wide %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "adjusted_pop") %>%
  mutate(year = as.Date(year))

all_pop <- all_pop %>%
  left_join(
    pop_long_updates,
    by = c("key", "year"),
    suffix = c("", "_updated")
  ) %>%
  mutate(adjusted_pop = if_else(!is.na(adjusted_pop_updated), adjusted_pop_updated, adjusted_pop)) %>%
  dplyr::select(-adjusted_pop_updated)

#reduce to final columns
adjusted_diresa_pop <- all_pop[,c(1,2,8)]
colnames(adjusted_diresa_pop) <- c('key', 'year', 'population')

#throw out 2023
adjusted_diresa_pop <- adjusted_diresa_pop[-which(adjusted_diresa_pop$year=="2023-01-01"),]

#export imputed population data
write.csv(adjusted_diresa_pop, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/adjusted_pop_all_years_clusters_final_0km.csv")

#################################### 
## SCRATCH
#################################### 
#individual ratios
#all_pop$individual_ratio <- NA
#for(i in 1:dim(all_pop)[1]){
#  all_pop$individual_ratio[i] <- all_pop$worldpop[i+1]/all_pop$worldpop[i]
#}


#remove these in the end (few dengue cases and no pop data, all past/non stable 101 105 106)
