# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Table S4.
#
# Date created: 8/4/2025

library(dplyr)
library(readr)

#####################
# Load data & results
#####################

## load from gee
cost_mapping <- read.csv("data/raw/environmental_data/cost_mapping.csv")

## load linked e_salud codes and cluster ids & link
linked_ids_codes <- read.csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")
cost_mapping_linked <- full_join(linked_ids_codes, cost_mapping, by = 'key')
cost_mapping_linked <- cost_mapping_linked %>%
  dplyr::select("key", "latitude", "longitude", "cumulative_cost", ".geo")

## load distance boundary vars and link
boundary_dummy_vars <- read.csv("data/intermediate/boundary_dummy_vars.csv")
cost_mapping_linked_buffers <- left_join(cost_mapping_linked, boundary_dummy_vars, by='key')
cost_mapping_linked_buffers <- cost_mapping_linked_buffers %>%
  mutate(all_cutoffs = if_else(all_cutoffs == 7, 6, all_cutoffs)) %>%
  group_by(all_cutoffs) %>%
  summarize(cumulative_cost_mean_minutes = median(cumulative_cost, na.rm=T)) %>%
  mutate(cumulative_cost_mean_hours = cumulative_cost_mean_minutes/60)

#####################
## STable 4
#####################
stable4 <- cost_mapping_linked_buffers %>%
  mutate(distance_label = case_when(
    all_cutoffs == 1 ~ '1km',
    all_cutoffs == 2 ~ '5km',
    all_cutoffs == 3 ~ '10km',
    all_cutoffs == 4 ~ '15km',
    all_cutoffs == 5 ~ '20km',
    all_cutoffs == 6 ~ '40km',
    all_cutoffs == 0 ~ '>40km')) %>%
  mutate(distance_label = factor(distance_label, levels = c('1km', '5km', '10km', '15km', '20km', '40km', '>40km'))) %>%
  arrange(distance_label) %>%
  mutate(cumulative_cost_mean_minutes = round(cumulative_cost_mean_minutes)) %>%
  rename(`Median Travel Time to Interoceanic Highway (min)` = cumulative_cost_mean_minutes,
         `Distance Boundary` = distance_label) %>%
  dplyr::select(`Distance Boundary`, `Median Travel Time to Interoceanic Highway (min)`)

stable4
write.csv(stable4, "figures/stable4.csv", row.names = F)
