## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)
library(fixest)

## load raw mdd case data (all diseases)
#case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
case_data <- read.csv("~/Desktop/Datos vigilancia MDD 2000-2022_SubsetStanford.csv")
head(case_data)

#################################
### preprocess dengue data
#################################

## keep lepto data only
lepto_data <- case_data[which(case_data$DIAGNOSTIC=="A27"),]
table(lepto_data$TIPO_DX)

## remove "descartado" cases (options are D/C/P)
lepto_data <- lepto_data[which(lepto_data$TIPO_DX=="C" | lepto_data$TIPO_DX=="P"),]

## remove cases listed as being from outside madre de dios
mdd_districts <- unique(lepto_data$UBIGEO)[1:11]
lepto_data <- lepto_data[which(lepto_data$UBIGEO %in% mdd_districts),]

## group into months
# lepto_data$FECHA_INI <- as.Date(lepto_data$FECHA_INI)
# lepto_data <- lepto_data %>%
#   mutate(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD)%>%
#   summarize(monthly_cases = n())

lepto_data$SEMANA <- ifelse(lepto_data$SEMANA==53,52,lepto_data$SEMANA)
lepto_data <- lepto_data %>%
  mutate(month = lubridate::month(as.Date(paste0(ANO, "-", SEMANA, "-", 1), format = "%Y-%U-%u")))
lepto_data$month_date <- as.Date(paste0(lepto_data$ANO, "-", lepto_data$month, "-", 01), format = "%Y-%m-%d")
monthly_lepto_data <- lepto_data %>%
  group_by(month = month_date, e_salud = E_SALUD) %>%
  summarize(monthly_cases = n())

## merge e_salud codes and cluster ids
e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_ESalud_Coordinates_Key.csv")
id_cluster_key_7500 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/idClusterKey7500.csv")
linked_ids_codes <- left_join(e_salud_codes, id_cluster_key_7500, by = 'key')
#write.csv(linked_ids_codes,"~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid.csv")

## link to cluster ids and group into healthcare center clusters
lepto_data_linked <- left_join(linked_ids_codes, monthly_lepto_data, by = 'e_salud')
lepto_data_linked <- lepto_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(monthly_cases))

## add zeroes to cluster-months with no recorded cases
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(full_months) <- 'month'
lepto_data_linked$cluster <- as.vector(lepto_data_linked$cluster)
lepto_data_complete_time_steps <- lepto_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

write.csv(lepto_data_complete_time_steps, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_lepto_data_confirmed.csv")
write.csv(lepto_data_complete_time_steps, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_lepto_data_confirmed_and_probable.csv")
