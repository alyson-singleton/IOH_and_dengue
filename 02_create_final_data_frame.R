# ID ----------------------------------------------------------------------
## Aly Singleton

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
#case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/Datos vigilancia MDD 2000-2022_SubsetStanford.csv")
head(case_data)

#################################
### preprocess dengue data
#################################
#dengue_data <- case_data
## keep dengue data only (in this case do not include A97.1--alarm--or A97.2--severe)
dengue_data <- case_data[which(case_data$DIAGNOSTIC=="A97.0"),]
table(dengue_data$TIPO_DX)

## remove "descartado" cases (options are D/C/P)
dengue_data <- dengue_data[which(dengue_data$TIPO_DX=="C" | dengue_data$TIPO_DX=="P"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]

## group into months
dengue_data$SEMANA <- ifelse(dengue_data$SEMANA==53,52,dengue_data$SEMANA)
dengue_data <- dengue_data %>%
  mutate(month = lubridate::month(as.Date(paste0(ANO, "-", SEMANA, "-", 1), format = "%Y-%U-%u")))
dengue_data$month_date <- as.Date(paste0(dengue_data$ANO, "-", dengue_data$month, "-", 01), format = "%Y-%m-%d")

monthly_dengue_data <- dengue_data %>%
  group_by(month = month_date, e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())

## load linked e_salud codes and cluster ids
linked_ids_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid_7.5km.csv")

## decide whether or not to include PM codes pre treatment
#linked_ids_codes <- linked_ids_codes[-c(104,105),]

## link to cluster ids and group into healthcare center clusters
dengue_data_linked <- full_join(linked_ids_codes, monthly_dengue_data, by = 'e_salud')
dengue_data_linked <- dengue_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))

## remove healthcare facilities that do not report at least one case (of any disease) pre and post treatment
mdd_districts <- unique(case_data$UBIGEO)[1:11]
case_data <- case_data[which(case_data$UBIGEO %in% mdd_districts),]
case_data$SEMANA <- ifelse(case_data$SEMANA==53,52,case_data$SEMANA)
case_data <- case_data %>%
  mutate(month = lubridate::month(as.Date(paste0(ANO, "-", SEMANA, "-", 1), format = "%Y-%U-%u")))
case_data$month_date <- as.Date(paste0(case_data$ANO, "-", case_data$month, "-", 01), format = "%Y-%m-%d")
monthly_case_data <- case_data %>%
  group_by(month = month_date, e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())
case_data_linked <- full_join(linked_ids_codes, monthly_case_data, by = 'e_salud')
case_data_linked <- case_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))

case_data_linked_time <- case_data_linked %>%
  group_by(cluster) %>%
  summarize(min_time = min(month, na.rm=T),
            max_time = max(month, na.rm=T))
clusters_wo_case_pre_treatment <- case_data_linked_time$cluster[which(case_data_linked_time$min_time>"2008-01-01")]
clusters_wo_case_post_treatment <- case_data_linked_time$cluster[which(case_data_linked_time$max_time<"2009-01-01")]
clusters_wo_case_either_pre_or_post_treatment <- unique(c(clusters_wo_case_pre_treatment,clusters_wo_case_post_treatment))
#dengue_data_linked <- dengue_data_linked[-which(dengue_data_linked$cluster %in% clusters_wo_case_either_pre_or_post_treatment),]

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

boundary_files <- list.files(path="~/Desktop/doctorate/ch2 mdd highway/data/boundary_dummy_vars_7.5km", pattern="csv", all.files=FALSE, full.names=TRUE,recursive=TRUE)
boundary_dummy_vars <- as.data.frame(c(1:length(unique(dengue_data_linked$cluster)))); colnames(boundary_dummy_vars) = c('cluster')
for (i in 2:length(boundary_files)){
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
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$fifteenkm[i]==1&&boundary_dummy_vars$tenkm[i]==0&&
                                                 boundary_dummy_vars$fivekm[i]==0&&boundary_dummy_vars$onekm[i]==0, 4, 
                                               boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$twentykm[i]==1&&boundary_dummy_vars$fifteenkm[i]==0&&
                                                 boundary_dummy_vars$tenkm[i]==0&&boundary_dummy_vars$fivekm[i]==0&&
                                                 boundary_dummy_vars$onekm[i]==0, 5, boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$thirtykm[i]==1&&boundary_dummy_vars$twentykm[i]==0&&
                                                 boundary_dummy_vars$fifteenkm[i]==0&&boundary_dummy_vars$tenkm[i]==0&&
                                                 boundary_dummy_vars$fivekm[i]==0&&boundary_dummy_vars$onekm[i]==0, 6, 
                                               boundary_dummy_vars$all_cutoffs[i])
  boundary_dummy_vars$all_cutoffs[i] <- ifelse(boundary_dummy_vars$fortykm[i]==1&&boundary_dummy_vars$thirtykm[i]==0&&
                                                 boundary_dummy_vars$twentykm[i]==0&&boundary_dummy_vars$fifteenkm[i]==0&&
                                                 boundary_dummy_vars$tenkm[i]==0&&boundary_dummy_vars$fivekm[i]==0&&
                                                 boundary_dummy_vars$onekm[i]==0, 7, 
                                               boundary_dummy_vars$all_cutoffs[i])
}

write.csv(boundary_dummy_vars,  "~/Desktop/doctorate/ch2 mdd highway/data/boundary_dummy_vars_7.5km/boundary_dummy_vars_7.5km.csv")

#link buffer data to case data
dengue_data_complete_time_steps$cluster <- as.numeric(dengue_data_complete_time_steps$cluster)
dengue_data_w_buffers <- left_join(dengue_data_complete_time_steps, boundary_dummy_vars, by='cluster')

#################################
### add population data
#################################

#build year column for linking to yearly population data
dengue_data_w_buffers$year <- format(as.Date(dengue_data_w_buffers$month, format="%Y-%m-%d"),"%Y")
dengue_data_w_buffers$year <- format(as.Date(dengue_data_w_buffers$year, format="%Y"),"%Y-01-01")
dengue_data_w_buffers$year <- as.Date(dengue_data_w_buffers$year)

#link population data
adjusted_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/adjusted_pop_all_years_clusters_final_7.5km.csv")
adjusted_diresa_pop$year <- as.Date(adjusted_diresa_pop$year)
adjusted_diresa_pop <- adjusted_diresa_pop[which(adjusted_diresa_pop$year!=as.Date("2023-01-01")),]#remove 2023 because no case data
adjusted_diresa_pop <- adjusted_diresa_pop[,c(2:4)]
dengue_data_w_pop <- full_join(dengue_data_w_buffers, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))

#################################
### add covariate data
#################################

### precip data
precip_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_7.5km/mdd_precipitation_monthly_sum.csv")
precip_monthly <- precip_monthly[,c(2:278)]
precip_monthly <- precip_monthly[,c(277,1:276)]
colnames(precip_monthly)[2:277] <- as.character(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(precip_monthly)[1] <- "cluster"
precip_monthly_mdd_long <- precip_monthly %>%
  pivot_longer(cols = c(2:277), 
               names_to = "month", 
               values_to = "sum_precip")
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")

#look at precip to inform rainy season/biannual split
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
precip_monthly_all <- precip_monthly_mdd_long %>%
  group_by(month, year) %>%
  summarize(sum_precip = sum(sum_precip)) 
precip_monthly_all$month_wo_year <- format(as.Date(precip_monthly_all$month, format="%Y-%m-%d"),"%m")
ggplot(precip_monthly_all) +
  geom_line(aes(x=month_wo_year,y=sum_precip,group=year)) +
  geom_hline(yintercept=mean(precip_monthly_all$sum_precip), color='red', linetype="dashed") +
  geom_vline(xintercept=04, color='red', linetype="dashed") +
  geom_vline(xintercept=10, color='red', linetype="dashed")

### temp data
temp_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_7.5km/mdd_temperature_monthly_mean.csv")
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
mapbiomas_areas <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_7.5km/mdd_mapbiomas.csv")
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
write.csv(covariates, "~/Desktop/doctorate/ch2 mdd highway/data/covariates_data_7.5km/mdd_monthly_covariates.csv")

#link to monthly case data
covariates$month <- as.Date(covariates$month)
covariates <- covariates[,c(1:3,5:7)] #drop extra year column
dengue_data_w_covariates_monthly <- full_join(dengue_data_w_pop, covariates, by=c("cluster"="cluster", "month" = "month"))
dengue_data_w_covariates_monthly <- dengue_data_w_covariates_monthly[complete.cases(dengue_data_w_covariates_monthly),]
write.csv(dengue_data_w_covariates_monthly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/dengue_monthly_full_dataset.csv")

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
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            sum_precip = sum(sum_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(dengue_data_w_covariates_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/dengue_yearly_full_dataset.csv")

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
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            sum_precip = sum(sum_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(dengue_data_w_covariates_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/dengue_biannual_full_dataset.csv")

################################
### preprocess leish data 
### (abbreviated from above, need to run lines above for this to work)
################################
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/Datos vigilancia MDD 2000-2022_SubsetStanford.csv")

## keep leish data only (in this case include both B55.1--CL--and--B55.2--ML)
leish_data <- case_data[which(case_data$DIAGNOSTIC=="B55.1" | case_data$DIAGNOSTIC=="B55.2"),]
table(leish_data$TIPO_DX)

## remove "descartado" cases (options are D/C/P)
leish_data <- leish_data[which(leish_data$TIPO_DX=="C" | leish_data$TIPO_DX=="P"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(leish_data$UBIGEO)[1:11]
leish_data <- leish_data[which(leish_data$UBIGEO %in% mdd_districts),]

## group into months
# leish_data$FECHA_INI <- as.Date(leish_data$FECHA_INI)
# monthly_leish_data <- leish_data %>% 
#   group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
#   summarize(monthly_cases = n())

leish_data$SEMANA <- ifelse(leish_data$SEMANA==53,52,leish_data$SEMANA)
leish_data <- leish_data %>%
  mutate(month = lubridate::month(as.Date(paste0(ANO, "-", SEMANA, "-", 1), format = "%Y-%U-%u")))
leish_data$month_date <- as.Date(paste0(leish_data$ANO, "-", leish_data$month, "-", 01), format = "%Y-%m-%d")
monthly_leish_data <- leish_data %>%
  group_by(month = month_date, e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())

## link to cluster ids and group into healthcare center clusters (loaded from above)
leish_data_linked <- left_join(linked_ids_codes, monthly_leish_data, by = 'e_salud')
leish_data_linked <- leish_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))
#leish_data_linked <- leish_data_linked[-which(leish_data_linked$cluster %in% clusters_wo_case_either_pre_or_post_treatment),]

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
write.csv(leish_data_w_covariates_monthly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/leish_monthly_full_dataset.csv")

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
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            sum_precip = sum(sum_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(leish_data_w_covariates_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/leish_yearly_full_dataset.csv")

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
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            sum_precip = sum(sum_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(leish_data_w_covariates_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/leish_biannual_full_dataset.csv")

################################
### preprocess malaria data 
### (abbreviated from above, need to run lines above for this to work)
################################

## keep malaria data only (in this case include both B55.1--CL--and--B55.2--ML)
malaria_data <- case_data[which(case_data$DIAGNOSTIC=="B51"),]
table(malaria_data$TIPO_DX)

## remove "descartado" cases (options are D/C/P)
malaria_data <- malaria_data[which(malaria_data$TIPO_DX=="C" | malaria_data$TIPO_DX=="P"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(malaria_data$UBIGEO)[1:11]
malaria_data <- malaria_data[which(malaria_data$UBIGEO %in% mdd_districts),]

## group into months
# malaria_data$FECHA_INI <- as.Date(malaria_data$FECHA_INI)
# monthly_malaria_data <- malaria_data %>% 
#   group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
#   summarize(monthly_cases = n())

malaria_data$SEMANA <- ifelse(malaria_data$SEMANA==53,52,malaria_data$SEMANA)
malaria_data <- malaria_data %>%
  mutate(month = lubridate::month(as.Date(paste0(ANO, "-", SEMANA, "-", 1), format = "%Y-%U-%u")))
malaria_data$month_date <- as.Date(paste0(malaria_data$ANO, "-", malaria_data$month, "-", 01), format = "%Y-%m-%d")
monthly_malaria_data <- malaria_data %>%
  group_by(month = month_date, e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())

## link to cluster ids and group into healthcare center clusters (loaded from above)
malaria_data_linked <- left_join(linked_ids_codes, monthly_malaria_data, by = 'e_salud')
malaria_data_linked <- malaria_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))

## add zeroes to cluster-months with no recorded cases
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(full_months) <- 'month'
malaria_data_linked$cluster <- as.vector(malaria_data_linked$cluster)
malaria_data_complete_time_steps <- malaria_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

#link buffer data to case data (loaded from above)
malaria_data_complete_time_steps$cluster <- as.numeric(malaria_data_complete_time_steps$cluster)
malaria_data_w_buffers <- full_join(malaria_data_complete_time_steps, boundary_dummy_vars, by='cluster')

#build year column for linking to yearly population data
malaria_data_w_buffers$year <- format(as.Date(malaria_data_w_buffers$month, format="%Y-%m-%d"),"%Y")
malaria_data_w_buffers$year <- format(as.Date(malaria_data_w_buffers$year, format="%Y"),"%Y-01-01")
malaria_data_w_buffers$year <- as.Date(malaria_data_w_buffers$year)

#link population data (loaded from above)
malaria_data_w_pop <- full_join(malaria_data_w_buffers, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))

#link to covariate data (loaded from above)
malaria_data_w_covariates_monthly <- full_join(malaria_data_w_pop, covariates, by=c("cluster"="cluster", "month" = "month"))
malaria_data_w_covariates_monthly <- malaria_data_w_covariates_monthly[complete.cases(malaria_data_w_covariates_monthly),]
write.csv(malaria_data_w_covariates_monthly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/malaria_monthly_full_dataset.csv")

## group yearly
malaria_data_w_covariates_yearly <- malaria_data_w_covariates_monthly %>%
  group_by(year,cluster) %>%
  summarise(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            mean_precip = mean(mean_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(malaria_data_w_covariates_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/malaria_yearly_full_dataset.csv")

## group biannually
malaria_data_w_covariates_biannual <- malaria_data_w_covariates_monthly 
malaria_data_w_covariates_biannual$month_wo_year <- format(as.Date(malaria_data_w_covariates_biannual$month, format="%Y-%m-%d"),"%m")
malaria_data_w_covariates_biannual$biannual_index <- ifelse(malaria_data_w_covariates_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
malaria_data_w_covariates_biannual$biannual_date <- ifelse(malaria_data_w_covariates_biannual$month_wo_year==10 | malaria_data_w_covariates_biannual$month_wo_year=='04', 
                                                         as.character(malaria_data_w_covariates_biannual$month), NA)
malaria_data_w_covariates_biannual$biannual_date <- as.Date(malaria_data_w_covariates_biannual$biannual_date, format="%Y-%m-%d")
malaria_data_w_covariates_biannual <- malaria_data_w_covariates_biannual %>% 
  fill(biannual_date) %>%
  group_by(biannual_date,cluster) %>%
  summarize(biannual_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            population = max(population),
            mean_temp = mean(mean_temp),
            mean_precip = mean(mean_precip),
            urban = max(urban),
            ag = max(ag),
            all_cutoffs = max(all_cutoffs))
write.csv(malaria_data_w_covariates_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/malaria_biannual_full_dataset.csv")

#################################
## SCRATCH
#################################

# just to build spatial inclusion/exclusion maps
#linked_ids_codes$cluster <- linked_ids_codes$clust
#linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes,onekm_tf, by='cluster')
#linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,fivekm_tf, by='cluster')
#linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,tenkm_tf, by='cluster')
#write.csv(linked_ids_codes_with_cutoffs, "~/Desktop/doctorate/ch2 mdd highway/data/mapping_cutoffs.csv")

# add cost mapping
#cost_mapping <- read.csv("~/Downloads/hcfc_dist2.csv")
#cost_mapping <- cost_mapping[,2:3]
#colnames(cost_mapping) <- c("cluster", "cost")
#cost_mapping <- left_join(cost_mapping, boundary_dummy_vars, by="cluster")
#cost_mapping$cost_adjusted <- cost_mapping$cost/30*60