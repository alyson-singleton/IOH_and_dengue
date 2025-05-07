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
#dengue_data <- dengue_data[which(dengue_data$TIPO_DX=="C" | dengue_data$TIPO_DX=="P"),]
dengue_data <- dengue_data[which(dengue_data$TIPO_DX=="C"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]

## group into weeks
dengue_data$SEMANA <- ifelse(dengue_data$SEMANA==53,52,dengue_data$SEMANA)
weekly_dengue_data <- dengue_data %>%
  rename(week = SEMANA,
         e_salud = E_SALUD,
         year = ANO) %>%
  group_by(week, year, e_salud) %>%
  summarize(weekly_cases = n())


## load linked e_salud codes and cluster ids
linked_ids_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid_7.5km.csv")

## decide whether or not to include PM codes pre treatment
#linked_ids_codes <- linked_ids_codes[-c(104,105),]

## link to cluster ids and group into healthcare center clusters
dengue_data_linked <- full_join(linked_ids_codes, weekly_dengue_data, by = 'e_salud')
dengue_data_linked <- dengue_data_linked %>%
  group_by(cluster = clust, week, year) %>%
  summarize(weekly_cases = sum(weekly_cases))

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
dengue_data_linked <- dengue_data_linked[-which(dengue_data_linked$cluster %in% clusters_wo_case_either_pre_or_post_treatment),]

## add zeroes to cluster-months with no recorded cases
dengue_data_linked$cluster <- as.vector(dengue_data_linked$cluster)

full_index <- expand.grid(
  cluster = unique(dengue_data_linked$cluster),
  year = c(2000:2023),
  week = 1:52
)

dengue_data_complete_time_steps <- full_index %>%
  left_join(dengue_data_linked, by = c("cluster", "year", "week")) %>%
  mutate(weekly_cases = replace_na(weekly_cases, 0))

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
dengue_data_w_buffers$year <- as.Date(paste0(dengue_data_w_buffers$year, "-01-01"))

#link population data
adjusted_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/adjusted_pop_all_years_clusters_final_7.5km.csv")
adjusted_diresa_pop$year <- as.Date(adjusted_diresa_pop$year)
adjusted_diresa_pop <- adjusted_diresa_pop[which(adjusted_diresa_pop$year!=as.Date("2023-01-01")),]#remove 2023 because no case data
adjusted_diresa_pop <- adjusted_diresa_pop[,c(2:4)]
dengue_data_w_pop <- full_join(dengue_data_w_buffers, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))
dengue_data_w_pop$year <- format(dengue_data_w_pop$year, "%Y")

#reduce columns for harper

dengue_df_weekly <- dengue_data_w_pop %>%
  group_by(tenkm,week,year) %>%
  summarise(weekly_cases = sum(weekly_cases),
            population = sum(population)) %>%
  rename(group = tenkm) %>%
  filter(if_all(c(weekly_cases, year, population), ~ !is.na(.)))

write.csv(dengue_df_weekly,"~/Desktop/doctorate/ch2 mdd highway/data/harper_weekly_case_counts.csv")
