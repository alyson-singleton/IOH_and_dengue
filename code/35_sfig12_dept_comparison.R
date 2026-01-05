# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure 12.
#
# Date created: 12/25/2025

library(sf)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggrepel)
library(stringr)
library(RColorBrewer)

###################
# Load spatial data
###################

# Departments
peru_depts <- read_sf("data/raw/spatial_data/peru_department_shapefiles.shp")
peru_depts <- st_as_sf(peru_depts) 
peru_depts$geometry <- st_transform(peru_depts$geometry, 4326)
peru_outline <- st_union(peru_depts$geometry)

mdd_peru <- peru_depts[which(peru_depts$NOMBDEP %in% c("MADRE DE DIOS")),]
cusco_peru <- peru_depts[which(peru_depts$NOMBDEP %in% c("CUSCO")),]
loreto_peru <- peru_depts[which(peru_depts$NOMBDEP %in% c("LORETO")),]

###################
# Districts
###################

peru_districts <- read_sf("data/raw/spatial_data/")

###################
# Highways
###################

highway <- read_sf("data/raw/spatial_data/peru_roads_important.shp") %>% 
  st_as_sf() %>%
  st_transform(4326)

highway_mdd <- st_covers(mdd_peru,highway$geometry, sparse = FALSE)
highway_mdd <- highway[highway_mdd[1,],]
mapview(highway_mdd)

highway_cusco <- st_covers(cusco_peru,highway$geometry, sparse = FALSE)
highway_cusco <- highway[highway_cusco[1,],]
mapview(highway_cusco)

highway_loreto <- st_covers(loreto_peru,highway$geometry, sparse = FALSE)
highway_loreto <- highway[highway_loreto[1,],]
mapview(highway_loreto)

###################
# Other roads (need for loreto)
###################

all_mdd_roads <- read_sf("data/raw/spatial_data/mdd_roads.shp") %>% 
  st_as_sf() %>% 
  st_transform(4326)
mdd_roads <- st_covers(mdd_peru, all_mdd_roads$geometry, sparse = FALSE)
mdd_roads <- all_mdd_roads[mdd_roads[1,],]

###################
# Dengue data (district level)
###################

dengue_district_data <- read_rds("./data/raw/cdc_district_dengue_data/solicitud_aly.rds")

mdd_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "MADRE DE DIOS") %>%
  filter(tipo_dx %in% c("C")) %>% #add prob?
  group_by(ano) %>%
  summarize(dengue_cases = n())

loreto_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "LORETO") %>% 
  filter(tipo_dx %in% c("C")) %>% #add prob?
  group_by(ano, distrito) %>%
  summarize(dengue_cases = n())

ggplot(loreto_dengue_district_df) +
  geom_line(aes(ano, dengue_cases, group = distrito))


cusco_dengue_district_df <- dengue_district_data %>% #fill this out for all dist/year combo
  filter(departamento == "CUSCO") %>%
  filter(tipo_dx %in% c("C", "P")) %>% #add prob?
  group_by(ano,distrito) %>%
  summarize(dengue_cases = n())

ggplot(cusco_dengue_district_df) +
  geom_line(aes(ano, dengue_cases, group = distrito))
