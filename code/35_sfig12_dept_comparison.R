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
library(mapview)
library(tidyverse)
library(patchwork)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Create standard figure theme
theme_stor <- theme(panel.grid.minor.x = element_line(linewidth = 0.3),
                    panel.grid.major.x = element_line(linewidth = 0.3),
                    panel.grid.major.y = element_line(linewidth = 0.3),
                    axis.line.x = element_line(color = "black", linewidth = 0.3),
                    axis.line.y = element_line(color = "black", linewidth = 0.3),
                    plot.title = element_text(size=14, hjust=0.5, face = "bold"),
                    plot.title.position = "plot",
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=12),
                    axis.title.y=element_text(size=11,angle=0, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=12),
                    axis.title.x=element_text(size=12),
                    axis.text.x=element_text(size=10),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=12),
                    legend.position = "none",
                    strip.text.x = element_text(size = 12))

###################
# Load spatial data
###################

# Departments
peru_depts <- read_sf("data/raw/spatial_data/peru_department_shapefiles.shp")
peru_depts <- st_as_sf(peru_depts) 
peru_depts$geometry <- st_transform(peru_depts$geometry, 4326)
peru_outline <- st_union(peru_depts$geometry)

mdd_peru <- peru_depts[which(peru_depts$NOMBDEP %in% c("MADRE DE DIOS")),]
loreto_peru <- peru_depts[which(peru_depts$NOMBDEP %in% c("LORETO")),]
cusco_peru <- peru_depts[which(peru_depts$NOMBDEP %in% c("CUSCO")),]

###################
# Districts
###################

peru_districts <- read_sf("./data/raw/spatial/peru_districts.shp") %>% #read_sf("data/raw/spatial_data/PER_adm/PER_adm3.shp") %>%
  st_as_sf() %>% 
  st_transform(4326)
mapview(peru_districts)

districts_mdd <- peru_districts %>% filter(DEPARTAMEN == "MADRE DE DIOS")
districts_loreto <- peru_districts %>% filter(DEPARTAMEN == "LORETO")
districts_cusco <- peru_districts %>% filter(DEPARTAMEN == "CUSCO")

###################
# Roads
###################

roads <- read_sf("data/raw/spatial_data/peru_osm_jan_2026/gis_osm_roads_free_1.shp") %>%
  st_as_sf() %>% 
  st_transform(4326)

roads_mdd <- st_covers(mdd_peru,roads$geometry, sparse = FALSE)
roads_mdd <- roads[roads_mdd[1,],]
mapview(roads_mdd %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_mdd)

roads_loreto <- st_covers(loreto_peru,roads$geometry, sparse = FALSE)
roads_loreto <- roads[roads_loreto[1,],]
mapview(roads_loreto %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_loreto)

roads_cusco <- st_covers(cusco_peru,roads$geometry, sparse = FALSE)
roads_cusco <- roads[roads_cusco[1,],]
mapview(roads_cusco %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_cusco)

###################
# Dengue & population data (district level)
###################

dengue_district_data <- read_rds("./data/raw/cdc_district_dengue_data/solicitud_aly.rds") #**add this into data folder
population_district_data <- read_csv("./data/raw/environmental_data/peru_population_districts_yearly.csv")
population_district_data <- population_district_data[,c(7,10:30)] %>%
  pivot_longer(
    cols = matches("^PER_\\d{4}_population$"),
    names_to = "year",
    values_to = "population") %>%
  mutate(year = as.integer(str_extract(year, "\\d{4}"))) %>%
  rename(district = "DISTRITO") %>%
  mutate(district = str_to_upper(district))

###################
# Madre de Dios
###################

mdd_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "MADRE DE DIOS",
         tipo_dx %in% c("C", "P")) %>%
  group_by(ano, distrito) %>%
  summarize(dengue_cases = n(), .groups = "drop") %>%
  complete(distrito, ano, fill = list(dengue_cases = 0)) %>%
  mutate(
    road_status = if_else(
      distrito %in% c("INAMBARI", "TAMBOPATA", "LABERINTO", "LAS PIEDRAS",
                      "TAHUAMANU", "IBERIA", "IÑAPARI"), 1L, 0L)) %>% #districts with major paved roads
  rename(district = distrito, year = ano) %>%
  left_join(population_district_data, by = c("district", "year")) %>%
  filter(year %in% 2000:2020) %>%
  mutate(incidence = dengue_cases / population * 1000)

# Raw trends
mdd_raw_trends <- mdd_dengue_district_df %>%
  group_by(year, road_status) %>%
  summarize(dengue_cases = sum(dengue_cases, na.rm = TRUE),
            population = sum(population,   na.rm = TRUE),
            incidence = dengue_cases / population * 1000,
            .groups = "drop")

sfig12a <- ggplot(mdd_raw_trends,
                  aes(x = year, y = incidence,
                      color = factor(road_status), group = road_status)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(name = "", 
                     breaks = c("1", "0"),
                     values=c("#E04490","#648FFF"), 
                     labels=c('Exposed\n(major road in district)', 'Unexposed\n(no major road in district)')) +
  geom_vline(aes(xintercept=2008), linetype='dashed', linewidth=0.5) +
  theme_minimal() +
  labs(title = "          Madre de Dios", x = NULL, y = "Dengue\nincidence\n per 1,000 ") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  theme_minimal() +
  theme_stor +
  theme(legend.position = "bottom")
sfig12_legend <- get_legend(sfig12a)
sfig12a <- sfig12a + theme(legend.position = "none")
sfig12a

# Year-demeaned (yd)
mdd_year_demeaned_model <- feols(
  incidence ~ 1 | year,
  vcov = ~ district,
  data = mdd_dengue_district_df)

mdd_yd_df <- mdd_dengue_district_df %>%
  mutate(yd_incidence = resid(mdd_year_demeaned_model))

mdd_yd_trends <- mdd_yd_df %>%
  group_by(year, road_status) %>%
  summarize(mean_yd = mean(yd_incidence, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(road_status, year)

mdd_yd_diff <- mdd_yd_trends %>%
  mutate(road_status = as.character(road_status),
         group = if_else(road_status == "1", "near", "far")) %>%
  select(year, group, mean_yd) %>%
  tidyr::pivot_wider(
    names_from  = group,
    values_from = mean_yd) %>%
  mutate(diff = near - far) %>%
  arrange(year)

sfig12b <- ggplot(mdd_yd_diff, aes(x = year, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept=2008), linetype='dashed', linewidth=0.5) +
  theme_minimal() +
  labs(title = "", x = "Year", y = "Difference\nin incidence\n(demeaned\nby year)")  +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  coord_cartesian(ylim = c(-20, 65)) +
  theme_minimal() +
  theme_stor +
  theme(axis.title.y=element_text(size=10,angle=0, vjust=.5, hjust=0.5))

sfig12b

# mdd panels
sfig12ab <- (sfig12a | sfig12b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
sfig12ab

###################
# Loreto
###################

loreto_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "LORETO",
         tipo_dx %in% c("C", "P")) %>%
  group_by(ano, distrito) %>%
  summarize(dengue_cases = n(), .groups = "drop") %>%
  complete(distrito, ano, fill = list(dengue_cases = 0)) %>%
  mutate(
    road_status = if_else(
      distrito %in% c("IQUITOS", "PUNCHANA", "NAUTA", "SAN JUAN BAUTISTA",
                      "YURIMAGUAS", "MANSERICHE"), 1L, 0L)) %>% #districts with major paved roads
  rename(district = distrito, year = ano) %>%
  left_join(population_district_data, by = c("district", "year")) %>%
  filter(year %in% 2000:2020) %>%
  mutate(incidence = dengue_cases / population * 1000)

# Raw trends
loreto_raw_trends <- loreto_dengue_district_df %>%
  group_by(year, road_status) %>%
  summarize(dengue_cases = sum(dengue_cases, na.rm = TRUE),
            population = sum(population,   na.rm = TRUE),
            incidence = dengue_cases / population * 1000,
            .groups = "drop")

sfig12c <- ggplot(loreto_raw_trends,
                  aes(x = year, y = incidence,
                      color = factor(road_status), group = road_status)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(name = "", 
                     breaks = c("1", "0"),
                     values=c("#E04490","#648FFF"), 
                     labels=c('Exposed\n(major road in district)', 'Unexposed\n(no major road in district)')) +
  geom_vline(aes(xintercept=2008), linetype='dashed', linewidth=0.5) +
  labs(title = "Loreto", x = NULL, y = "") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  theme_minimal() +
  theme_stor

sfig12c

# Year-demeaned (yd)
loreto_year_demeaned_model <- feols(
  incidence ~ 1 | year,
  vcov = ~ district,
  data = loreto_dengue_district_df)

loreto_yd_df <- loreto_dengue_district_df %>%
  mutate(yd_incidence = resid(loreto_year_demeaned_model))

loreto_yd_trends <- loreto_yd_df %>%
  group_by(year, road_status) %>%
  summarize(mean_yd = mean(yd_incidence, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(road_status, year)

loreto_yd_diff <- loreto_yd_trends %>%
  mutate(road_status = as.character(road_status),
    group = if_else(road_status == "1", "near", "far")) %>%
  select(year, group, mean_yd) %>%
  tidyr::pivot_wider(
    names_from  = group,
    values_from = mean_yd) %>%
  mutate(diff = near - far) %>%
  arrange(year)

sfig12d <- ggplot(loreto_yd_diff, aes(x = year, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.7) +
  geom_vline(aes(xintercept=2008), linetype='dashed', linewidth=0.5) +
  theme_minimal() +
  labs(title = "", x = "Year", y = "") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  coord_cartesian(ylim = c(-20, 65)) +
  theme_minimal() +
  theme_stor

sfig12d

# Loreto panels
sfig12cd <- (sfig12c | sfig12d) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
sfig12cd

###################
# SFig 12all
###################
sfig12combined <- grid.arrange(sfig12a, sfig12c, sfig12b, sfig12d, sfig12_legend,                     
                             ncol = 2, nrow = 3,
                             layout_matrix = rbind(c(1,2),c(3, 4), c(5, 5)), 
                             heights=c(4,4,1))

sfig12combined <- as_ggplot(sfig12combined) +                                
  draw_plot_label(label = c("A", "B", "C", "D"), size = 14,
                  x = c(0.12, 0.54, 0.12, 0.54), y = c(0.95, 0.95, 0.51, 0.51)) 

sfig12combined
ggsave("sfig12.pdf", plot=sfig12combined, path="figures/", width = 9.5, height = 6.5, units="in", device = "pdf")

###################
# Cusco?
###################

cusco_dengue_district_df <- dengue_district_data %>% #fill this out for all dist/year combo
  filter(departamento == "CUSCO") %>%
  filter(tipo_dx %in% c("C", "P")) %>% #add prob?
  group_by(ano,distrito) %>%
  summarize(dengue_cases = n())

ggplot(cusco_dengue_district_df) +
  geom_line(aes(ano, dengue_cases, group = distrito))
