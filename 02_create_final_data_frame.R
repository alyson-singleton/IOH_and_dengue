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

## load raw mdd case data
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
head(case_data)

#################################
### dengue
#################################

## keep dengue data only
dengue_data <- case_data[which(case_data$DIAGNOSTIC=="A97.0"),]
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]
dengue_data$FECHA_INI <- as.Date(dengue_data$FECHA_INI)

## monthly case data
monthly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(sum = n())

## merge e_salud codes and cluster ids
cluster_ids <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/cluster_centroids_70.csv")
e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_E_Salud_Coordinates_Key.csv")
id_cluster_key_7500 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/idClusterKey7500.csv")

linked_ids_codes <- left_join(e_salud_codes, id_cluster_key_7500, by = 'key')
#linked_ids_codes <- left_join(cluster_ids, id_cluster_key_7500, by = 'clust')

write.csv(linked_ids_codes,"~/Desktop/doctorate/ch2 mdd highway/data/linking_clusterid_esaludkey.csv")
dengue_data_linked <- left_join(linked_ids_codes, monthly_dengue_data, by = 'e_salud')
dengue_data_linked <- dengue_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(sum),
            name = first(name))

## add zeroes
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"))
colnames(full_months) <- 'month'
dengue_data_linked$cluster <- as.vector(dengue_data_linked$cluster)
dengue_data_linked <- dengue_data_linked[,c(1:3)]
dengue_data_complete_time_steps <- dengue_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()
incidence_data_yearly$year <- as.Date(incidence_data_yearly$year)
incidence_data_yearly <- incidence_data_yearly %>%
  complete(year = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"), 
           fill = list(yearly_cases = 0)) %>%
  as.data.frame()

## link to various road buffers
onekm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_1km_boolean.csv")
onekm_tf <- onekm_tf[,c(2,4)]
onekm_tf$isInsideBuffer[onekm_tf$isInsideBuffer == 'true'] <- 1
onekm_tf$isInsideBuffer[onekm_tf$isInsideBuffer == 'false'] <- 0
colnames(onekm_tf) <- c('cluster', 'onekm')
fivekm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_5km_boolean.csv")
fivekm_tf <- fivekm_tf[,c(2,4)]
fivekm_tf$isInsideBuffer[fivekm_tf$isInsideBuffer == 'true'] <- 1
fivekm_tf$isInsideBuffer[fivekm_tf$isInsideBuffer == 'false'] <- 0
colnames(fivekm_tf) <- c('cluster', 'fivekm')
tenkm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_10km_boolean.csv")
tenkm_tf <- tenkm_tf[,c(2,4)]
tenkm_tf$isInsideBuffer[tenkm_tf$isInsideBuffer == 'true'] <- 1
tenkm_tf$isInsideBuffer[tenkm_tf$isInsideBuffer == 'false'] <- 0
colnames(tenkm_tf) <- c('cluster', 'tenkm')

dengue_data_complete_time_steps$cluster <- as.numeric(dengue_data_complete_time_steps$cluster)
dengue_data_buffers <- full_join(dengue_data_complete_time_steps,onekm_tf, by='cluster')
dengue_data_buffers <- full_join(dengue_data_buffers,fivekm_tf, by='cluster')
dengue_data_buffers <- full_join(dengue_data_buffers,tenkm_tf, by='cluster')

# just to build spatial inclusion/exclusion maps
linked_ids_codes$cluster <- linked_ids_codes$clust
linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes,onekm_tf, by='cluster')
linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,fivekm_tf, by='cluster')
linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,tenkm_tf, by='cluster')
write.csv(linked_ids_codes_with_cutoffs, "~/Desktop/doctorate/ch2 mdd highway/data/mapping_cutoffs.csv")
  
#build year column for linking to yearly population data
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$month, format="%Y-%m-%d"),"%Y")
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$year, format="%Y"),"%Y-01-01")
dengue_data_buffers_21$year <- as.Date(dengue_data_buffers_21$year)

#link population data and create incidence
incidence_data <- full_join(dengue_data_buffers_21, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))
incidence_data$incidence <- incidence_data$monthly_cases/incidence_data$population
incidence_data$incidence[is.na(incidence_data$incidence)] <- 0
min(incidence_data$incidence)
max(incidence_data$incidence)
table(which(incidence_data$incidence=="Inf"))
incidence_data$incidence[which(incidence_data$incidence=="Inf")] <- 0
write.csv(incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data_pop_adjusted.csv")
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data_pop_adjusted.csv")

#quarterly, biannual and yearly incidence
#quarterly
incidence_data_quarterly <- incidence_data %>% 
  mutate(quarter = lubridate::quarter(month, type = "date_last"))
incidence_data_quarterly <- incidence_data_quarterly  %>%
  group_by(quarter,cluster) %>%
  summarize(quarterly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(incidence_data_quarterly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_incidence_data_pop_adjusted.csv")

#biannually
incidence_data_biannual <- incidence_data 
incidence_data_biannual$month_wo_year <- format(as.Date(incidence_data_biannual$month, format="%Y-%m-%d"),"%m")
incidence_data_biannual$biannual_index <- ifelse(incidence_data_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
incidence_data_biannual <- incidence_data_biannual[complete.cases(incidence_data_biannual),]
incidence_data_biannual$biannual_date <- ifelse(incidence_data_biannual$month_wo_year==10 | incidence_data_biannual$month_wo_year=='04', incidence_data_biannual$month, NA)
incidence_data_biannual <- incidence_data_biannual %>% 
  fill(biannual_date)
incidence_data_biannual <- incidence_data_biannual  %>%
  group_by(biannual_date,cluster) %>%
  summarize(biannual_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(incidence_data_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/biannual_incidence_data_pop_adjusted.csv")

#yearly
incidence_data_yearly <- incidence_data  %>%
  group_by(year,cluster) %>%
  summarize(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population))  %>%
  mutate(incidence = yearly_cases/population)
table(which(incidence_data_yearly$incidence=="Inf"))
incidence_data_yearly$incidence[which(incidence_data_yearly$incidence=="Inf")] <- 0
incidence_data_yearly$incidence[is.na(incidence_data_yearly$incidence)] <- 0
write.csv(incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data_pop_adjusted.csv")

## data coverage calculation
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")
incidence_data_yearly$case_yn <- ifelse(incidence_data_yearly$yearly_cases>0, 1, 0)
dengue_coverage_yearly <- incidence_data_yearly %>%
  group_by(year, tenkm) %>%
  summarize(coverage = sum(case_yn))

################################
#############Leish##############
################################

leish_data <- case_data[which(case_data$DIAGNOSTIC=="B55.1" | case_data$DIAGNOSTIC=="B55.2"),]
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
leish_data <- leish_data[which(leish_data$UBIGEO %in% mdd_districts),]
leish_data$FECHA_INI <- as.Date(leish_data$FECHA_INI)

## monthly case data
monthly_leish_data <- leish_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(sum = n())

leish_data_linked <- left_join(linked_ids_codes, monthly_leish_data, by = 'e_salud')
leish_data_linked <- leish_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(sum),
            name = first(name))

leish_data_linked$cluster <- as.vector(leish_data_linked$cluster)
leish_data_linked <- leish_data_linked[,c(1:3)]
leish_data_complete_time_steps <- leish_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

leish_data_complete_time_steps$cluster <- as.numeric(leish_data_complete_time_steps$cluster)
leish_data_buffers <- full_join(leish_data_complete_time_steps,onekm_tf, by='cluster')
leish_data_buffers <- full_join(leish_data_buffers,fivekm_tf, by='cluster')
leish_data_buffers <- full_join(leish_data_buffers,tenkm_tf, by='cluster')

leish_data_buffers_21 <- leish_data_buffers[!(leish_data_buffers$month %in% seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by="months")),]

leish_data_buffers_21$year <- format(as.Date(leish_data_buffers_21$month, format="%Y-%m-%d"),"%Y")
leish_data_buffers_21$year <- format(as.Date(leish_data_buffers_21$year, format="%Y"),"%Y-01-01")
leish_data_buffers_21$year <- as.Date(leish_data_buffers_21$year)

#link population data and create incidence
leish_incidence_data <- full_join(leish_data_buffers_21, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))
leish_incidence_data$incidence <- leish_incidence_data$monthly_cases/leish_incidence_data$population
leish_incidence_data$incidence[is.na(leish_incidence_data$incidence)] <- 0
min(leish_incidence_data$incidence)
max(leish_incidence_data$incidence)
table(which(leish_incidence_data$incidence=="Inf"))
leish_incidence_data$incidence[which(leish_incidence_data$incidence=="Inf")] <- 0
write.csv(leish_incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_leish_incidence_data_pop_adjusted.csv")
leish_incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_leish_incidence_data_pop_adjusted.csv")

#quarterly, biannual(rainy/dry), and yearly incidence
#quarterly
leish_incidence_data_quarterly <- leish_incidence_data %>% 
  mutate(quarter = lubridate::quarter(month, type = "date_last"))
leish_incidence_data_quarterly <- leish_incidence_data_quarterly  %>%
  group_by(quarter,cluster) %>%
  summarize(quarterly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(leish_incidence_data_quarterly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data_pop_adjusted.csv")

#biannually
leish_incidence_data_biannual <- leish_incidence_data 
leish_incidence_data_biannual$month_wo_year <- format(as.Date(leish_incidence_data_biannual$month, format="%Y-%m-%d"),"%m")
leish_incidence_data_biannual$biannual_index <- ifelse(leish_incidence_data_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
leish_incidence_data_biannual <- leish_incidence_data_biannual[complete.cases(leish_incidence_data_biannual),]
leish_incidence_data_biannual$biannual_date <- ifelse(leish_incidence_data_biannual$month_wo_year==10 | leish_incidence_data_biannual$month_wo_year=='04', leish_incidence_data_biannual$month, NA)
leish_incidence_data_biannual <- leish_incidence_data_biannual %>% 
  fill(biannual_date)
leish_incidence_data_biannual <- leish_incidence_data_biannual  %>%
  group_by(biannual_date,cluster) %>%
  summarize(biannual_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(leish_incidence_data_biannual, "~/Desktop/doctorate/ch2 mdd highway/data/biannual_leish_incidence_data_pop_adjusted.csv")

#yearly
leish_incidence_data_yearly <- leish_incidence_data  %>%
  group_by(year,cluster) %>%
  summarize(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(leish_incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data_pop_adjusted.csv")

##########################################
#############POTENTIALCONTROLS############
##########################################

#### Add precip, temp, and land-use data
precip_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_precipitation_monthly_mean.csv")
temp_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_temperature_monthly_mean.csv")
urban_area <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_urban_area_mapbiomas.csv")

#precip
precip_monthly <- precip_monthly[,c(2:254)]
precip_monthly <- precip_monthly[,c(253,1:252)]
colnames(precip_monthly)[2:253] <- as.character(seq(as.Date("2000-01-01"), as.Date("2020-12-01"), by="months"))
colnames(precip_monthly)[1] <- "cluster"
precip_monthly_mdd_long <- precip_monthly %>%
  pivot_longer(cols = c(2:253), 
               names_to = "month", 
               values_to = "mean_precip")

#look at precip for rainy season/biannual split
precip_monthly_all <- precip_monthly_mdd_long %>%
  group_by(month, year) %>%
  summarize(mean_precip = mean(mean_precip)) 
precip_monthly_all$month_wo_year <- format(as.Date(precip_monthly_all$month, format="%Y-%m-%d"),"%m")
ggplot(precip_monthly_all) +
  geom_line(aes(x=month_wo_year,y=mean_precip,group=year)) +
  geom_hline(yintercept=mean(precip_monthly_all$mean_precip), color='red', linetype="dashed") +
  geom_vline(xintercept=04, color='red', linetype="dashed") +
  geom_vline(xintercept=10, color='red', linetype="dashed")

#yearly
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
precip_yearly_mdd_long <- precip_monthly_mdd_long %>%
  group_by(year,cluster) %>%
  summarize(mean_precip = mean(mean_precip))
#biannually
precip_biannually_mdd_long <- precip_monthly_mdd_long 
precip_biannually_mdd_long$month_wo_year <- format(as.Date(precip_biannually_mdd_long$month, format="%Y-%m-%d"),"%m")
precip_biannually_mdd_long$biannual_index <- ifelse(precip_biannually_mdd_long$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
precip_biannually_mdd_long <- precip_biannually_mdd_long[complete.cases(precip_biannually_mdd_long),]
precip_biannually_mdd_long$biannual_date <- ifelse(precip_biannually_mdd_long$month_wo_year==10 | precip_biannually_mdd_long$month_wo_year=='04', precip_biannually_mdd_long$month, NA)
precip_biannually_mdd_long <- precip_biannually_mdd_long %>% 
  fill(biannual_date)
precip_biannually_mdd_long <- precip_biannually_mdd_long  %>%
  group_by(biannual_date,cluster) %>%
  summarize(mean_precip = mean(mean_precip)) 
precip_biannually_mdd_long$year <- format(as.Date(precip_biannually_mdd_long$biannual_date, format="%Y-%m-%d"),"%Y")
#quarterly
precip_quarterly_mdd_long <- precip_monthly_mdd_long %>%
  mutate(quarter = lubridate::quarter(month, type = "date_last")) %>%
  group_by(quarter,cluster) %>%
  summarize(mean_precip = mean(mean_precip))

#temp
temp_monthly <- temp_monthly[,c(2:254)]
temp_monthly <- temp_monthly[,c(253,1:252)]
colnames(temp_monthly)[2:253] <- as.character(seq(as.Date("2000-01-01"), as.Date("2020-12-01"), by="months"))
colnames(temp_monthly)[1] <- "cluster"
temp_monthly_mdd_long <- temp_monthly %>%
  pivot_longer(cols = c(2:253), 
               names_to = "month", 
               values_to = "mean_temp")
temp_monthly_mdd_long$year <- format(as.Date(temp_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
#yearly
temp_yearly_mdd_long <- temp_monthly_mdd_long %>%
  group_by(year,cluster) %>%
  summarize(mean_temp = mean(mean_temp))
#biannually
temp_biannually_mdd_long <- temp_monthly_mdd_long 
temp_biannually_mdd_long$month_wo_year <- format(as.Date(temp_biannually_mdd_long$month, format="%Y-%m-%d"),"%m")
temp_biannually_mdd_long$biannual_index <- ifelse(temp_biannually_mdd_long$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
temp_biannually_mdd_long <- temp_biannually_mdd_long[complete.cases(temp_biannually_mdd_long),]
temp_biannually_mdd_long$biannual_date <- ifelse(temp_biannually_mdd_long$month_wo_year==10 | temp_biannually_mdd_long$month_wo_year=='04', temp_biannually_mdd_long$month, NA)
temp_biannually_mdd_long <- temp_biannually_mdd_long %>% 
  fill(biannual_date)
temp_biannually_mdd_long <- temp_biannually_mdd_long  %>%
  group_by(biannual_date,cluster) %>%
  summarize(mean_temp = mean(mean_temp))
temp_biannually_mdd_long$year <- format(as.Date(temp_biannually_mdd_long$biannual_date, format="%Y-%m-%d"),"%Y")
#quarterly
temp_quarterly_mdd_long <- temp_monthly_mdd_long %>%
  mutate(quarter = lubridate::quarter(month, type = "date_last")) %>%
  group_by(quarter,cluster) %>%
  summarize(mean_temp = mean(mean_temp))

#urban_area
urban_area <- urban_area[,c(2:5)]
urban_area <- urban_area[which(urban_area$class==24),]
urban_area <- urban_area[,c(1,3,4)]
colnames(urban_area) <- c("urban_area", "cluster", "year")

#yearly all covariates
covariates <- left_join(precip_yearly_mdd_long,temp_yearly_mdd_long, by=c("cluster"="cluster", "year" = "year"))
urban_area$year <- as.character(urban_area$year)
covariates <- full_join(covariates,urban_area, by=c("cluster"="cluster", "year" = "year"))
covariates$urban_area[which(is.na(covariates$urban_area))] <- 0
write.csv(covariates, "~/Desktop/doctorate/ch2 mdd highway/data/mdd_yearly_covariates.csv")

#biannual all covariates
covariates <- left_join(precip_biannually_mdd_long,temp_biannually_mdd_long, by=c("cluster"="cluster", "biannual_date" = "biannual_date"))
urban_area$year <- as.character(urban_area$year)
covariates <- left_join(covariates,urban_area, by=c("cluster"="cluster", "year.x" = "year"))
covariates$urban_area[which(is.na(covariates$urban_area))] <- 0
write.csv(covariates, "~/Desktop/doctorate/ch2 mdd highway/data/mdd_biannual_covariates.csv")

#add more road buffer vars

#20km
twentykm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_20km_boolean.csv")
twentykm_tf <- twentykm_tf[,c(2,4)]
twentykm_tf$isInsideBuffer[twentykm_tf$isInsideBuffer == 'true'] <- 1
twentykm_tf$isInsideBuffer[twentykm_tf$isInsideBuffer == 'false'] <- 0
colnames(twentykm_tf) <- c('cluster', 'twentykm')

#30km
thirtykm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_30km_boolean.csv")
thirtykm_tf <- thirtykm_tf[,c(2,4)]
thirtykm_tf$isInsideBuffer[thirtykm_tf$isInsideBuffer == 'true'] <- 1
thirtykm_tf$isInsideBuffer[thirtykm_tf$isInsideBuffer == 'false'] <- 0
colnames(thirtykm_tf) <- c('cluster', 'thirtykm')

#40km
fortykm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_40km_boolean.csv")
fortykm_tf <- fortykm_tf[,c(2,4)]
fortykm_tf$isInsideBuffer[fortykm_tf$isInsideBuffer == 'true'] <- 1
fortykm_tf$isInsideBuffer[fortykm_tf$isInsideBuffer == 'false'] <- 0
colnames(fortykm_tf) <- c('cluster', 'fortykm')

#2km
twokm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_2km_boolean.csv")
twokm_tf <- twokm_tf[,c(2,4)]
twokm_tf$isInsideBuffer[twokm_tf$isInsideBuffer == 'true'] <- 1
twokm_tf$isInsideBuffer[twokm_tf$isInsideBuffer == 'false'] <- 0
colnames(twokm_tf) <- c('cluster', 'twokm')

#3km
threekm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_3km_boolean.csv")
threekm_tf <- threekm_tf[,c(2,4)]
threekm_tf$isInsideBuffer[threekm_tf$isInsideBuffer == 'true'] <- 1
threekm_tf$isInsideBuffer[threekm_tf$isInsideBuffer == 'false'] <- 0
colnames(threekm_tf) <- c('cluster', 'threekm')

#4km
fourkm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_4km_boolean.csv")
fourkm_tf <- fourkm_tf[,c(2,4)]
fourkm_tf$isInsideBuffer[fourkm_tf$isInsideBuffer == 'true'] <- 1
fourkm_tf$isInsideBuffer[fourkm_tf$isInsideBuffer == 'false'] <- 0
colnames(fourkm_tf) <- c('cluster', 'fourkm')

df_to_link <- left_join(twokm_tf, threekm_tf, by = "cluster")
df_to_link <- left_join(df_to_link, fourkm_tf, by = "cluster")

incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data_pop_adjusted.csv")
incidence_data_yearly <- left_join(incidence_data_yearly, df_to_link, by = "cluster")
write.csv(incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data_pop_adjusted.csv")


