## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)
library(fixest)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

## load raw mdd case data (all diseases)
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
head(case_data)

#################################
### preprocess dengue data
#################################

## keep dengue data only (in this case do not include A97.1--alarm--or A97.2--severe)
dengue_data <- case_data[which(case_data$DIAGNOSTIC=="A97.0"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]

## group into months
dengue_data$FECHA_INI <- as.Date(dengue_data$FECHA_INI)
monthly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())

## merge e_salud codes and cluster ids
e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_ESalud_Coordinates_Key.csv")
id_cluster_key_7500 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/idClusterKey7500.csv")
linked_ids_codes <- left_join(e_salud_codes, id_cluster_key_7500, by = 'key')
#write.csv(linked_ids_codes,"~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid.csv")

## link to cluster ids and group into healthcare center clusters
dengue_data_linked <- left_join(linked_ids_codes, monthly_dengue_data, by = 'e_salud')
dengue_data_linked <- dengue_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))

## add zeroes to cluster-months with no recorded cases
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(full_months) <- 'month'
dengue_data_linked$cluster <- as.vector(dengue_data_linked$cluster)
dengue_data_complete_time_steps <- dengue_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

#################################
### create dataframe of road buffers
#################################

boundary_files <- list.files(path="~/Desktop/doctorate/ch2 mdd highway/data/boundary_dummy_vars", pattern="csv", all.files=FALSE, full.names=TRUE,recursive=TRUE)
boundary_dummy_vars <- as.data.frame(c(1:70)); colnames(boundary_dummy_vars) = c('cluster')
for (i in 1:length(boundary_files)){
  boundary_csv <- read.csv(boundary_files[i])
  boundary_csv <- boundary_csv[,c(2,4)]
  boundary_csv$isInsideBuffer[boundary_csv$isInsideBuffer == 'true'] <- 1
  boundary_csv$isInsideBuffer[boundary_csv$isInsideBuffer == 'false'] <- 0
  colnames(boundary_csv) <- c('cluster', strsplit(tools::file_path_sans_ext(boundary_files[i]), "_(?!.*_)", perl=TRUE)[[1]][2])
  boundary_dummy_vars <- left_join(boundary_dummy_vars, boundary_csv, by = "cluster")
}

# build all_cutoffs column to optionally create a buffer zone
for(i in 1:dim(boundary_dummy_vars)[1]){
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$onekm[i]==1, 1, 0)
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$fivekm[i]==1&&boundary_dummy_vars$onekm[i]==0, 2, 
                                               boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$tenkm[i]==1&&boundary_dummy_vars$fivekm[i]==0&&
                                                 boundary_dummy_vars$onekm[i]==0, 3, boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$twentykm[i]==1&&boundary_dummy_vars$tenkm[i]==0&&
                                                 boundary_dummy_vars$fivekm[i]==0&&boundary_dummy_vars$onekm[i]==0, 4, 
                                               boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$thirtykm[i]==1&&boundary_dummy_vars$twentykm[i]==0&&
                                                 boundary_dummy_vars$tenkm[i]==0&&boundary_dummy_vars$fivekm[i]==0&&
                                                 boundary_dummy_vars$onekm[i]==0, 5, boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$fortykm[i]==1&&boundary_dummy_vars$thirtykm[i]==0&&
                                                 boundary_dummy_vars$twentykm[i]==0&&boundary_dummy_vars$tenkm[i]==0&&
                                                 boundary_dummy_vars$fivekm[i]==0&&boundary_dummy_vars$onekm[i]==0, 6, 
                                               boundary_dummy_vars$all_cutoffs[i])
}

#link buffer data to case data
dengue_data_complete_time_steps$cluster <- as.numeric(dengue_data_complete_time_steps$cluster)
dengue_data_w_buffers <- full_join(dengue_data_complete_time_steps, boundary_dummy_vars, by='cluster')

#################################
### add population data
#################################

#build year column for linking to yearly population data
dengue_data_w_buffers$year <- format(as.Date(dengue_data_w_buffers$month, format="%Y-%m-%d"),"%Y")
dengue_data_w_buffers$year <- format(as.Date(dengue_data_w_buffers$year, format="%Y"),"%Y-01-01")
dengue_data_w_buffers$year <- as.Date(dengue_data_w_buffers$year)

#link population data
adjusted_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/adjusted_pop_all_years_clusters_final.csv")
adjusted_diresa_pop$year <- as.Date(adjusted_diresa_pop$year)
adjusted_diresa_pop <- adjusted_diresa_pop[which(adjusted_diresa_pop$year!=as.Date("2023-01-01")),]#remove 2023 because no case data
adjusted_diresa_pop <- adjusted_diresa_pop[,c(2:4)]
dengue_data_w_pop <- full_join(dengue_data_w_buffers, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))

#################################
### add covariate data
#################################

### precip data
precip_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data/mdd_precipitation_monthly_mean.csv")
precip_monthly <- precip_monthly[,c(2:278)]
precip_monthly <- precip_monthly[,c(277,1:276)]
colnames(precip_monthly)[2:277] <- as.character(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(precip_monthly)[1] <- "cluster"
precip_monthly_mdd_long <- precip_monthly %>%
  pivot_longer(cols = c(2:277), 
               names_to = "month", 
               values_to = "mean_precip")
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")

#look at precip to inform rainy season/biannual split
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
precip_monthly_all <- precip_monthly_mdd_long %>%
  group_by(month, year) %>%
  summarize(mean_precip = mean(mean_precip)) 
precip_monthly_all$month_wo_year <- format(as.Date(precip_monthly_all$month, format="%Y-%m-%d"),"%m")
ggplot(precip_monthly_all) +
  geom_line(aes(x=month_wo_year,y=mean_precip,group=year)) +
  geom_hline(yintercept=mean(precip_monthly_all$mean_precip), color='red', linetype="dashed") +
  geom_vline(xintercept=04, color='red', linetype="dashed") +
  geom_vline(xintercept=10, color='red', linetype="dashed")

### temp data
temp_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data/mdd_temperature_monthly_mean.csv")
temp_monthly <- temp_monthly[,c(2:278)]
temp_monthly <- temp_monthly[,c(277,1:276)]
colnames(temp_monthly)[2:277] <- as.character(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(temp_monthly)[1] <- "cluster"
temp_monthly_mdd_long <- temp_monthly %>%
  pivot_longer(cols = c(2:277), 
               names_to = "month", 
               values_to = "mean_temp")
temp_monthly_mdd_long$year <- format(as.Date(temp_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")

### urban area data
mapbiomas_areas <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data/mdd_mapbiomas.csv")
mapbiomas_areas <- mapbiomas_areas[,c(2:5)]
mapbiomas_areas <- mapbiomas_areas[which(mapbiomas_areas$class %in% c(15,18,21,24)),]
mapbiomas_areas$class <- ifelse(mapbiomas_areas$class %in% c(15,18,21),"ag","urban")
mapbiomas_areas <- mapbiomas_areas %>%
  group_by(layer, year, class) %>%
  summarize(area = sum(area)) %>%
  pivot_wider(names_from = class, values_from = area)
mapbiomas_areas[is.na(mapbiomas_areas)] <- 0
colnames(mapbiomas_areas) <- c("cluster", "year", "ag", "urban")

#link all covariates together
covariates <- left_join(precip_monthly_mdd_long,temp_monthly_mdd_long, by=c("cluster"="cluster", "month" = "month", "year" = "year"))
mapbiomas_areas$year <- as.character(mapbiomas_areas$year)
covariates <- full_join(covariates,mapbiomas_areas, by=c("cluster"="cluster", "year" = "year"))
covariates$month <- as.Date(covariates$month)
write.csv(covariates, "~/Desktop/doctorate/ch2 mdd highway/data/covariates_data/mdd_monthly_covariates.csv")

#link to monthly case data
covariates$month <- as.Date(covariates$month)
covariates <- covariates[,c(1:3,5:7)] #drop extra year column
dengue_data_w_covariates_monthly <- full_join(dengue_data_w_pop, covariates, by=c("cluster"="cluster", "month" = "month"))
dengue_data_w_covariates_monthly <- dengue_data_w_covariates_monthly[complete.cases(dengue_data_w_covariates_monthly),]
write.csv(dengue_data_w_covariates_monthly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/dengue_monthly_full_dataset.csv")

################################
### group yearly and biannually
################################

## group yearly
dengue_data_w_covariates_yearly <- dengue_data_w_covariates_monthly %>%
  group_by(year,cluster) %>%
  summarise(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            mean_precip = mean(mean_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(dengue_data_w_covariates_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/dengue_yearly_full_dataset.csv")

## group biannually
dengue_data_w_covariates_biannual <- dengue_data_w_covariates_monthly 
dengue_data_w_covariates_biannual$month_wo_year <- format(as.Date(dengue_data_w_covariates_biannual$month, format="%Y-%m-%d"),"%m")
dengue_data_w_covariates_biannual$biannual_index <- ifelse(dengue_data_w_covariates_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
dengue_data_w_covariates_biannual$biannual_date <- ifelse(dengue_data_w_covariates_biannual$month_wo_year==10 | dengue_data_w_covariates_biannual$month_wo_year=='04', 
                                                          as.character(dengue_data_w_covariates_biannual$month), NA)
dengue_data_w_covariates_biannual$biannual_date <- as.Date(dengue_data_w_covariates_biannual$biannual_date, format="%Y-%m-%d")
dengue_data_w_covariates_biannual <- dengue_data_w_covariates_biannual %>% 
  fill(biannual_date) %>%
  group_by(biannual_date,cluster) %>%
  summarize(biannual_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            mean_precip = mean(mean_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(dengue_data_w_covariates_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/dengue_biannual_full_dataset.csv")

################################
### preprocess leish data 
### (abbreviated from above, need to run lines above for this to work)
################################

## keep leish data only (in this case include both B55.1--CL--and--B55.2--ML)
leish_data <- case_data[which(case_data$DIAGNOSTIC=="B55.1" | case_data$DIAGNOSTIC=="B55.2"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(leish_data$UBIGEO)[1:11]
leish_data <- leish_data[which(leish_data$UBIGEO %in% mdd_districts),]

## group into months
leish_data$FECHA_INI <- as.Date(leish_data$FECHA_INI)
monthly_leish_data <- leish_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())

## link to cluster ids and group into healthcare center clusters (loaded from above)
leish_data_linked <- left_join(linked_ids_codes, monthly_leish_data, by = 'e_salud')
leish_data_linked <- leish_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))

## add zeroes to cluster-months with no recorded cases
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(full_months) <- 'month'
leish_data_linked$cluster <- as.vector(leish_data_linked$cluster)
leish_data_complete_time_steps <- leish_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

#link buffer data to case data (loaded from above)
leish_data_complete_time_steps$cluster <- as.numeric(leish_data_complete_time_steps$cluster)
leish_data_w_buffers <- full_join(leish_data_complete_time_steps, boundary_dummy_vars, by='cluster')

#build year column for linking to yearly population data
leish_data_w_buffers$year <- format(as.Date(leish_data_w_buffers$month, format="%Y-%m-%d"),"%Y")
leish_data_w_buffers$year <- format(as.Date(leish_data_w_buffers$year, format="%Y"),"%Y-01-01")
leish_data_w_buffers$year <- as.Date(leish_data_w_buffers$year)

#link population data (loaded from above)
leish_data_w_pop <- full_join(leish_data_w_buffers, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))

#link to covariate data (loaded from above)
leish_data_w_covariates_monthly <- full_join(leish_data_w_pop, covariates, by=c("cluster"="cluster", "month" = "month"))
leish_data_w_covariates_monthly <- leish_data_w_covariates_monthly[complete.cases(leish_data_w_covariates_monthly),]
write.csv(leish_data_w_covariates_monthly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/leish_monthly_full_dataset.csv")

## group yearly
leish_data_w_covariates_yearly <- leish_data_w_covariates_monthly %>%
  group_by(year,cluster) %>%
  summarise(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            mean_precip = mean(mean_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(leish_data_w_covariates_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/leish_yearly_full_dataset.csv")

## group biannually
leish_data_w_covariates_biannual <- leish_data_w_covariates_monthly 
leish_data_w_covariates_biannual$month_wo_year <- format(as.Date(leish_data_w_covariates_biannual$month, format="%Y-%m-%d"),"%m")
leish_data_w_covariates_biannual$biannual_index <- ifelse(leish_data_w_covariates_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
leish_data_w_covariates_biannual$biannual_date <- ifelse(leish_data_w_covariates_biannual$month_wo_year==10 | leish_data_w_covariates_biannual$month_wo_year=='04', 
                                                          as.character(leish_data_w_covariates_biannual$month), NA)
leish_data_w_covariates_biannual$biannual_date <- as.Date(leish_data_w_covariates_biannual$biannual_date, format="%Y-%m-%d")
leish_data_w_covariates_biannual <- leish_data_w_covariates_biannual %>% 
  fill(biannual_date) %>%
  group_by(biannual_date,cluster) %>%
  summarize(biannual_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            mean_precip = mean(mean_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(leish_data_w_covariates_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/leish_biannual_full_dataset.csv")

#################################
## SCRATCH
#################################

# just to build spatial inclusion/exclusion maps
#linked_ids_codes$cluster <- linked_ids_codes$clust
#linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes,onekm_tf, by='cluster')
#linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,fivekm_tf, by='cluster')
#linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,tenkm_tf, by='cluster')
#write.csv(linked_ids_codes_with_cutoffs, "~/Desktop/doctorate/ch2 mdd highway/data/mapping_cutoffs.csv")

