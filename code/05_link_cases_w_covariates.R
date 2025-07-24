# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Link cases with environmental covariates and spatial information.
#
# Date created: 7/23/2025

## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)

#################################
### load diresa disease case data
#################################

dengue_case_data_processed <- read.csv("data/diresa_data/diresa_dengue_data_processed.csv")
leish_case_data_processed <- read.csv("data/diresa_data/diresa_leishmaniasis_data_processed.csv")

#################################
### create dataframe of road buffer variables
#################################

buffer_name_map <- c(
  "1km" = "onekm", "2km" = "twokm", "3km" = "threekm", "4km" = "fourkm", "5km" = "fivekm",
  "10km" = "tenkm", "15km" = "fifteenkm", "20km" = "twentykm",
  "30km" = "thirtykm", "40km" = "fortykm")

boundary_files <- list.files(path="data/environmental_data", pattern="buffered", all.files=FALSE, full.names=TRUE, recursive=TRUE)
boundary_dummy_vars <- data.frame(key = unique(dengue_data_linked$key))

for (file in boundary_files) {
  boundary_csv <- read.csv(file)
  
  # Detect column that starts with "isInside"
  isInside_col <- grep("^isInside", names(boundary_csv), value = TRUE)
  
  # Convert 'true'/'false' to 1/0
  boundary_csv[[isInside_col]] <- ifelse(boundary_csv[[isInside_col]] == "true", 1,
                                         ifelse(boundary_csv[[isInside_col]] == "false", 0,
                                                boundary_csv[[isInside_col]]))
  
  # Extract buffer (e.g. "10km") from column name or filename
  buffer_km <- gsub("isInside_|km", "", isInside_col)
  buffer_km <- paste0(buffer_km, "km")
  
  # Rename the isInside column
  buffer_name <- buffer_name_map[[buffer_km]]
  names(boundary_csv)[names(boundary_csv) == isInside_col] <- buffer_name
  
  boundary_csv$key <- as.numeric(boundary_csv$key)
  boundary_csv <- boundary_csv[, c("key", buffer_name)]
  boundary_dummy_vars <- left_join(boundary_dummy_vars, boundary_csv, by = "key")
}

# build all_cutoffs column to optionally create a buffer zone
boundary_dummy_vars <- boundary_dummy_vars %>%
  mutate(
    all_cutoffs = case_when(
      onekm == 1 ~ 1,
      fivekm == 1 & onekm == 0 ~ 2,
      tenkm == 1 & fivekm == 0 & onekm == 0 ~ 3,
      fifteenkm == 1 & tenkm == 0 & fivekm == 0 & onekm == 0 ~ 4,
      twentykm == 1 & fifteenkm == 0 & tenkm == 0 & fivekm == 0 & onekm == 0 ~ 5,
      thirtykm == 1 & twentykm == 0 & fifteenkm == 0 & tenkm == 0 & fivekm == 0 & onekm == 0 ~ 6,
      fortykm == 1 & thirtykm == 0 & twentykm == 0 & fifteenkm == 0 & tenkm == 0 & fivekm == 0 & onekm == 0 ~ 7,
      TRUE ~ 0
    )
  )

#################################
### load imputed population data
#################################

imputed_population_data <- read.csv("data/diresa_data/imputed_population_data.csv")
imputed_population_data <- imputed_population_data %>%
  dplyr::select(year, key, population) 
imputed_population_data$year <- format(as.Date(imputed_population_data$year, format="%Y-01-01"),"%Y")

#################################
### load environmental covariate data
#################################

### precip data
precip_monthly <- read.csv("data/environmental_data/mdd_precipitation_monthly_sum.csv")
precip_monthly <- precip_monthly[,c(2:279)]
precip_monthly <- precip_monthly[,c(278,1:276)]
colnames(precip_monthly)[2:277] <- as.character(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(precip_monthly)[1] <- "key"
precip_monthly_mdd_long <- precip_monthly %>%
  pivot_longer(cols = c(2:277), 
               names_to = "month", 
               values_to = "sum_precip")
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")

### temp data
temp_monthly <- read.csv("data/environmental_data/mdd_temperature_monthly_mean.csv")
temp_monthly <- temp_monthly[,c(2:279)]
temp_monthly <- temp_monthly[,c(278,1:276)]
colnames(temp_monthly)[2:277] <- as.character(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(temp_monthly)[1] <- "key"
temp_monthly_mdd_long <- temp_monthly %>%
  pivot_longer(cols = c(2:277), 
               names_to = "month", 
               values_to = "mean_temp")
temp_monthly_mdd_long$year <- format(as.Date(temp_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")

### land use data
mapbiomas_areas <- read.csv("data/environmental_data/mdd_mapbiomas_yearly_sum.csv")
mapbiomas_areas <- mapbiomas_areas[,c(2:5)]
mapbiomas_areas <- mapbiomas_areas[which(mapbiomas_areas$class %in% c(15,18,21,24)),]
mapbiomas_areas$class <- ifelse(mapbiomas_areas$class %in% c(15,18,21),"ag","urban")
mapbiomas_areas <- mapbiomas_areas %>%
  group_by(key, year, class) %>%
  summarize(area = sum(area)) %>%
  pivot_wider(names_from = class, values_from = area)
mapbiomas_areas[is.na(mapbiomas_areas)] <- 0
colnames(mapbiomas_areas) <- c("key", "year", "ag", "urban")
mapbiomas_areas$year <- as.character(mapbiomas_areas$year)

################################
### link all covariates
################################
covariates <- left_join(precip_monthly_mdd_long, temp_monthly_mdd_long, by=c("key" = "key", "month" = "month", "year" = "year"))
covariates <- covariates %>%
  full_join(mapbiomas_areas, by=c("key"="key", "year"="year")) %>%
  full_join(imputed_population_data, by=c("key"="key", "year"="year")) %>%
  full_join(boundary_dummy_vars, by=c("key"="key")) %>%
  mutate(month = as.Date(month)) %>%
  dplyr::select(-year)

################################
### link to monthly dengue data & aggregate
################################
dengue_case_data_processed$month <- as.Date(dengue_case_data_processed$month)
dengue_data_w_covariates_monthly <- full_join(dengue_case_data_processed, covariates, by=c("key"="key", "month"="month"))
dengue_data_w_covariates_monthly <- dengue_data_w_covariates_monthly[complete.cases(dengue_data_w_covariates_monthly),]
write.csv(dengue_data_w_covariates_monthly, "data/merged_data/dengue_monthly_merged_dataset.csv", row.names=F)

# pull "max" for these columns so they don't get dropped (they are the same across months)
max_columns <- c("onekm", "twokm", "threekm", "fourkm", "fivekm",
                 "tenkm", "fifteenkm", "twentykm", "thirtykm", "fortykm",
                 "population", "urban", "ag", "all_cutoffs", "clust")

## group yearly
dengue_data_w_covariates_yearly <- dengue_data_w_covariates_monthly %>%
  mutate(year = as.Date(paste0(year(month), "-01-01"))) %>%
  group_by(year, key) %>%
  summarise(
    yearly_cases_C = sum(monthly_cases_C),
    yearly_cases_CP = sum(monthly_cases_CP),
    across(all_of(max_columns), max),
    mean_temp = mean(mean_temp),
    sum_precip = sum(sum_precip),
    .groups = "drop"
  )

write.csv(dengue_data_w_covariates_yearly, "data/merged_data/dengue_yearly_merged_dataset.csv", row.names=F)

## group biannually
dengue_data_w_covariates_biannual <- dengue_data_w_covariates_monthly 
dengue_data_w_covariates_biannual$month_wo_year <- format(as.Date(dengue_data_w_covariates_biannual$month, format="%Y-%m-%d"),"%m")
dengue_data_w_covariates_biannual$biannual_index <- ifelse(dengue_data_w_covariates_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
dengue_data_w_covariates_biannual$biannual_date <- ifelse(dengue_data_w_covariates_biannual$month_wo_year==10 | dengue_data_w_covariates_biannual$month_wo_year=='04', 
                                                          as.character(dengue_data_w_covariates_biannual$month), NA)
dengue_data_w_covariates_biannual$biannual_date <- as.Date(dengue_data_w_covariates_biannual$biannual_date, format="%Y-%m-%d")
dengue_data_w_covariates_biannual <- dengue_data_w_covariates_biannual %>% 
  fill(biannual_date) %>%
  group_by(biannual_date,key) %>%
  summarise(
    biannual_cases_C = sum(monthly_cases_C),
    biannual_cases_CP = sum(monthly_cases_CP),
    across(all_of(max_columns), max),
    mean_temp = mean(mean_temp),
    sum_precip = sum(sum_precip),
    .groups = "drop"
  )

write.csv(dengue_data_w_covariates_biannual, "data/merged_data/dengue_biannual_merged_dataset.csv", row.names=F)

################################
### link to monthly leish data & aggregate
################################
leish_case_data_processed$month <- as.Date(leish_case_data_processed$month)
leish_data_w_covariates_monthly <- full_join(leish_case_data_processed, covariates, by=c("key"="key", "month"="month"))
leish_data_w_covariates_monthly <- leish_data_w_covariates_monthly[complete.cases(leish_data_w_covariates_monthly),]
write.csv(leish_data_w_covariates_monthly, "data/merged_data/leish_monthly_merged_dataset.csv", row.names=F)

# pull "max" for these columns so they don't get dropped (they are the same across months)
max_columns <- c("onekm", "twokm", "threekm", "fourkm", "fivekm",
                 "tenkm", "fifteenkm", "twentykm", "thirtykm", "fortykm",
                 "population", "urban", "ag", "all_cutoffs", "clust")

## group yearly
leish_data_w_covariates_yearly <- leish_data_w_covariates_monthly %>%
  mutate(year = as.Date(paste0(year(month), "-01-01"))) %>%
  group_by(year, key) %>%
  summarise(
    yearly_cases_C = sum(monthly_cases_C),
    yearly_cases_CP = sum(monthly_cases_CP),
    across(all_of(max_columns), max),
    mean_temp = mean(mean_temp),
    sum_precip = sum(sum_precip),
    .groups = "drop"
  )

write.csv(leish_data_w_covariates_yearly, "data/merged_data/leish_yearly_merged_dataset.csv", row.names=F)

## group biannually
leish_data_w_covariates_biannual <- leish_data_w_covariates_monthly 
leish_data_w_covariates_biannual$month_wo_year <- format(as.Date(leish_data_w_covariates_biannual$month, format="%Y-%m-%d"),"%m")
leish_data_w_covariates_biannual$biannual_index <- ifelse(leish_data_w_covariates_biannual$month_wo_year %in% c('01', '02', '03', 10, 11, 12), 1, 0)
leish_data_w_covariates_biannual$biannual_date <- ifelse((leish_data_w_covariates_biannual$month_wo_year==10 | 
                                                            leish_data_w_covariates_biannual$month_wo_year=='04'), 
                                                          as.character(leish_data_w_covariates_biannual$month), NA)
leish_data_w_covariates_biannual$biannual_date <- as.Date(leish_data_w_covariates_biannual$biannual_date, format="%Y-%m-%d")
leish_data_w_covariates_biannual <- leish_data_w_covariates_biannual %>% 
  fill(biannual_date) %>%
  group_by(biannual_date,key) %>%
  summarise(
    biannual_cases_C = sum(monthly_cases_C),
    biannual_cases_CP = sum(monthly_cases_CP),
    across(all_of(max_columns), max),
    mean_temp = mean(mean_temp),
    sum_precip = sum(sum_precip),
    .groups = "drop"
  )

write.csv(leish_data_w_covariates_biannual, "data/merged_data/leish_biannual_merged_dataset.csv", row.names=F)
