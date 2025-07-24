# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Process DIRESA dengue and leishmaniasis case data.
#
# Date created: 7/23/2025
## load diresa disease case data
dengue_case_data <- read.csv("data/diresa_data/diresa_dengue_data_raw.csv")
leish_case_data <- read.csv("data/diresa_data/diresa_leishmaniasis_data_raw.csv")

#################################
### preprocess dengue data
#################################

## remove cases listed as being from outside madre de dios
dengue_case_data <- dengue_case_data[which(substr(dengue_case_data$UBIGEO,1,2) == "17"),]

## keep standard dengue cases only
# (do not include A97.1--alarm--or A97.2--severe--because neither were reported before paving)
dengue_case_data <- dengue_case_data[which(dengue_case_data$dx_code=="A97.0"),]

## remove "descartado" cases (options are descartado/confirmado/probable)
table(dengue_case_data$dx_type)
dengue_case_data <- dengue_case_data[which(dengue_case_data$dx_type %in% c("C","P")),]

## make separate columns for confirmed and confirmed+probable
dengue_case_data <- dengue_case_data %>%
  pivot_wider(
    names_from = dx_type,
    values_from = dengue_cases,
    names_prefix = "cases_") %>%
  replace_na(list(cases_C = 0, cases_P = 0)) %>%
  mutate(cases_CP = cases_C + cases_P) %>%
  dplyr::select(-cases_P)

## load e_salud, key, and cluster information
diresa_esalud_coordinates_key <- read.csv("data/spatial_data/diresa_esalud_coordinates_key.csv")

## link to dengue data, only retain e_salud codes that are in MdD and have lat/lon info
dengue_data_linked <- left_join(linked_ids_codes, dengue_case_data, by = 'e_salud') %>%
  mutate(month = as.Date(month)) %>%
  replace_na(list(cases_C = 0, cases_CP = 0))

## add zeroes to unit-months with no recorded cases
dengue_data_complete_time_steps <- dengue_data_linked %>%
  group_by(key) %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by = "month"),
           fill = list(monthly_cases = 0)) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  distinct(key, month, .keep_all = TRUE) %>%
  filter(!is.na(key)) %>%
  dplyr::select(month, key, e_salud, clust, cases_C, cases_CP) %>%
  as.data.frame()

#################################
### preprocess leish data 
#################################

## remove cases listed as being from outside madre de dios
leish_case_data <- leish_case_data[which(substr(leish_case_data$UBIGEO,1,2) == "17"),]

## keep all leish cases (both B55.1--CL--and--B55.2--ML, run just to confirm nothing else)
leish_case_data <- leish_case_data[which(leish_case_data$dx_code %in% c("B55.1", "B55.2")),]

## remove "descartado" cases (options are descartado/confirmado/probable)
table(leish_case_data$dx_type)
leish_case_data <- leish_case_data[which(leish_case_data$dx_type %in% c("C","P")),]

## make separate columns for confirmed and confirmed+probable
leish_case_data <- leish_case_data %>%
  pivot_wider(
    names_from = dx_type,
    values_from = leish_cases,
    names_prefix = "cases_") %>%
  replace_na(list(cases_C = 0, cases_P = 0)) %>%
  mutate(cases_CP = cases_C + cases_P) %>%
  dplyr::select(-cases_P)

## load e_salud, key, and cluster information
diresa_esalud_coordinates_key <- read.csv("data/spatial_data/diresa_esalud_coordinates_key.csv")

## link to leish data, only retain e_salud codes that are in MdD and have lat/lon info
leish_data_linked <- left_join(linked_ids_codes, leish_case_data, by = 'e_salud') %>%
  mutate(month = as.Date(month)) %>%
  replace_na(list(cases_C = 0, cases_CP = 0))

## add zeroes to unit-months with no recorded cases
leish_data_complete_time_steps <- leish_data_linked %>%
  group_by(key) %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by = "month"),
           fill = list(monthly_cases = 0)) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  distinct(key, month, .keep_all = TRUE) %>%
  filter(!is.na(key)) %>%
  dplyr::select(month, key, e_salud, clust, cases_C, cases_CP) %>%
  as.data.frame()

#################################
### save processed datasets
#################################

write.csv(dengue_data_complete_time_steps, "data/diresa_data/diresa_dengue_data_processed.csv", row.names = FALSE)
write.csv(leish_data_complete_time_steps, "data/diresa_data/diresa_leishmaniasis_data_processed.csv", row.names = FALSE)
