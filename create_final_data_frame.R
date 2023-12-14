## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

## load all mdd case data
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
head(case_data)

## dengue data only
dengue_data <- case_data[which(case_data$DIAGNOSTIC=="A97.0"),]
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]
dengue_data$FECHA_INI <- as.Date(dengue_data$FECHA_INI)

## monthly case data
monthly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(sum = n())
monthly_dengue_data$Disease <- c("Dengue")
monthly_dengue_data_district <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'),
           UBIGEO) %>%
  summarize(sum = n())
monthly_dengue_data_district$Disease <- c("Dengue")

## yearly case data by healthcare center
yearly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'year'), e_salud = E_SALUD) %>%
  summarize(yearly_cases = n())
yearly_dengue_data$Disease <- c("Dengue")

## merge e_salud codes and cluster ids
cluster_ids <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/cluster_cenetroids_7500.csv")
e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_E_Salud_Coordinates_Key.csv")
id_cluster_key_7500 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/idClusterKey7500.csv")

linked_ids_codes <- left_join(e_salud_codes, id_cluster_key_7500, by = 'key')

dengue_data_linked <- left_join(linked_ids_codes, monthly_dengue_data, by = 'e_salud')
dengue_data_linked <- dengue_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(sum),
            name = first(name))

## add zeroes
full_years <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="years"))
colnames(full_years) <- 'year'
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"))
colnames(full_months) <- 'month'
dengue_data_linked$cluster <- as.vector(dengue_data_linked$cluster)
dengue_data_linked <- dengue_data_linked[,c(1:3)]
dengue_data_complete_time_steps <- dengue_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
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

#remove 2021 & 2022
dengue_data_buffers_21 <- dengue_data_buffers[!(dengue_data_buffers$month %in% seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by="months")),]
dengue_data_buffers_22 <- dengue_data_buffers[!(dengue_data_buffers$month %in% as.Date('2022-01-01')),]

### add population data
population_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_population_yearly.csv")
population_mdd$cluster <- population_mdd$layer
population_mdd <- population_mdd[,c(2:22,25)]
colnames(population_mdd) <- c(as.character(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="years")), 'cluster')
population_mdd_long <- population_mdd %>%
  pivot_longer(cols = c(1:21), 
               names_to = "year", 
               values_to = "population")
population_mdd_long$year <- as.Date(population_mdd_long$year)

#build year column if needed for linking
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$month, format="%Y-%m-%d"),"%Y")
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$year, format="%Y"),"%Y-01-01")
dengue_data_buffers_21$year <- as.Date(dengue_data_buffers_21$year)
#link population data and create incidence
incidence_data <- full_join(dengue_data_buffers_21, population_mdd_long, by=c('cluster'='cluster', 'year'='year'))
incidence_data$incidence <- incidence_data$monthly_cases/incidence_data$population
incidence_data$incidence[is.na(incidence_data$incidence)] <- 0
min(incidence_data$incidence)
max(incidence_data$incidence)
write.csv(incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data.csv")
incidence_data_nopm <- incidence_data[!(incidence_data$cluster %in% 1),]

#plots
#onekm
onekm_plotting <- incidence_data_nopm %>%
  group_by(onekm, year) %>%
  summarize(new_incidence = sum(yearly_cases)/sum(population))

ggplot(onekm_plotting) +
  geom_line(aes(year, new_incidence, color=onekm)) +
  geom_vline(xintercept=as.Date('2009-12-01'))

#fivekm
fivekm_plotting <- incidence_data_nopm %>%
  group_by(fivekm, year) %>%
  summarize(new_incidence = sum(yearly_cases)/sum(population))

ggplot(fivekm_plotting) +
  geom_line(aes(year, new_incidence, color=fivekm)) +
  geom_vline(xintercept=as.Date('2009-12-01'))

#tenkm
tenkm_plotting <- incidence_data_nopm %>%
  group_by(tenkm, year) %>%
  summarize(new_incidence = sum(yearly_cases)/sum(population))

ggplot(tenkm_plotting) +
  geom_line(aes(year, new_incidence, color=tenkm)) +
  geom_vline(xintercept=as.Date('2009-12-01'))


## create year dummies
#incidence_data <- 
