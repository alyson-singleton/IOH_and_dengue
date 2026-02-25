# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Set up for MdD and Loreto comparison. Build SFig 6 plot.
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

# Districts
peru_districts <- read_sf("./data/raw/spatial_data/peru_districts.shp") %>%
  st_as_sf() %>% 
  st_transform(4326)
mapview(peru_districts)

districts_mdd <- peru_districts %>% filter(DEPARTAMEN == "MADRE DE DIOS")
districts_loreto <- peru_districts %>% filter(DEPARTAMEN == "LORETO")

# Roads (OSM)
roads_mdd <- read_sf("./data/raw/spatial_data/mdd_roads.shp") %>%
  st_as_sf() %>% 
  st_transform(4326)
mapview(roads_mdd %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_mdd)

roads_loreto <- read_sf("./data/raw/spatial_data/loreto_roads.shp") %>%
  st_as_sf() %>% 
  st_transform(4326)
mapview(roads_loreto %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_loreto)

###################
# Dengue & population data (district level)
###################

dengue_district_data <- read_rds("./data/raw/cdc_district_dengue_data/cdc_district_dengue_data.rds")
population_district_data <- read_csv("./data/raw/environmental_data/peru_population_districts_yearly.csv")
population_district_data <- population_district_data[,c(6,7,10:30)] %>%
  pivot_longer(
    cols = matches("^PER_\\d{4}_population$"),
    names_to = "year",
    values_to = "population") %>%
  mutate(year = as.integer(str_extract(year, "\\d{4}"))) %>%
  rename(district = "DISTRITO",
         department = "DEPARTAMEN") %>%
  mutate(district = str_to_upper(district),
         department = str_to_upper(department))

###################
# Madre de Dios
###################

mdd_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "MADRE DE DIOS",
         tipo_dx %in% c("C", "P")) %>%
  group_by(ano, distrito, departamento) %>%
  summarize(dengue_cases = n(), .groups = "drop") %>%
  complete(distrito, ano = 2000:2020, departamento, fill = list(dengue_cases = 0)) %>%
  mutate(road_status = if_else(
    distrito %in% c("INAMBARI", "TAMBOPATA", "LABERINTO", "LAS PIEDRAS",
                    "TAHUAMANU", "IBERIA", "IÑAPARI"), 1L, 0L)) %>% #districts with major paved roads
  rename(district = distrito, year = ano, department = departamento) %>%
  left_join(population_district_data, by = c("district", "year", "department")) %>%
  filter(year %in% 2000:2020) %>%
  mutate(incidence = dengue_cases / population * 1000)

###################
# Loreto
###################

loreto_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "LORETO",
         tipo_dx %in% c("C", "P")) %>%
  group_by(ano, distrito, departamento) %>%
  summarize(dengue_cases = n(), .groups = "drop") %>%
  complete(distrito, ano = 2000:2020, departamento, fill = list(dengue_cases = 0)) %>%
  mutate(road_status = if_else(
    distrito %in% c("IQUITOS", "PUNCHANA", "NAUTA", "SAN JUAN BAUTISTA"), 1L, 0L)) %>% #districts with major paved roads
  filter(!distrito %in% c("YURIMAGUAS", "MANSERICHE")) %>%
  rename(district = distrito, year = ano, department = departamento) %>%
  left_join(population_district_data, by = c("district", "year", "department")) %>%
  filter(year %in% 2000:2020) %>%
  mutate(incidence = dengue_cases / population * 1000)

###################
# SFig 6
###################

# SFig6a: MdD district map
mdd_exposure_lookup <- mdd_dengue_district_df %>%
  distinct(district, road_status) %>%
  mutate(district = str_to_upper(str_trim(district)),
         road_status = as.integer(road_status),
         exposure_group = if_else(road_status == 1L, "Exposed", "Unexposed"))

districts_mdd_sf <- districts_mdd %>%
  mutate(district = str_to_upper(str_trim(DISTRITO))) %>%
  left_join(mdd_exposure_lookup, by = "district") %>%
  mutate(road_status = replace_na(road_status, 0L),
         exposure_group = replace_na(exposure_group, "Unexposed"))

roads_mdd_major <- roads_mdd %>%
  filter(fclass %in% c("trunk"))

exposure_pal <- c("Exposed" = "#E04490", "Unexposed" = "#648FFF")

arrows_df <- data.frame(
  x = c(-69.56, -70.32),
  y = c(-10.88, -13.23),
  xend = c(-69.44, -70.50),
  yend = c(-10.70, -13.38),
  lab  = c("To Brazil", "To Cusco"),
  lab_x = c(-70.1, -70.32),  # text position (can differ from arrow start)
  lab_y = c(-10.8, -13.43))

sfig6a_map_mdd <- ggplot() +
  geom_sf(data = districts_mdd_sf, aes(fill = exposure_group), alpha = 0.5, color = "grey30", linewidth = 0.12) +
  geom_sf(data = roads_mdd_major, color = "black", linewidth = 0.8, alpha = 1) +
  scale_fill_manual(
    name = "", values = exposure_pal, breaks = c("Exposed", "Unexposed"),
    labels = c("Exposed", "Unexposed")) +
  coord_sf(datum = NA) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme_stor +
  theme(legend.position = "none",
        legend.box = "horizontal") +
  geom_segment(
    data = arrows_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 1.1,
    color = "black",
    inherit.aes = FALSE) +
  geom_text(
    data = arrows_df,
    aes(x = lab_x, y = lab_y, label = lab),
    size = 3.6,
    hjust = 0,
    vjust = 0,
    color = "black",
    fontface = "italic",
    inherit.aes = FALSE)
sfig6a_map_mdd

sfig6a_map_mdd_w_inset <- ggdraw() + 
  draw_plot(ggplot() +
              geom_sf(data = peru_outline, fill=NA, color='#3b3b3b', 
                      linewidth=0.03, show.legend = FALSE) +
              geom_sf(data = peru_depts, fill=NA, color='#3b3b3b', 
                      linewidth=0.03, show.legend = FALSE) +
              geom_sf(data = mdd_peru, fill='#cfcfcf', color='#3b3b3b', 
                      linewidth=0.03, show.legend = FALSE) +
              theme_minimal() +
              no_axis +
              theme(legend.text=element_text(size=12),
                    legend.title=element_text(size=14),
                    legend.position = "none"),
            0.05, 0.7, 0.27, 0.27) + 
  draw_plot(sfig11a_map_mdd,
            0.05, 0, 0.95, 1)
sfig6a_map_mdd_w_inset

# SFig6b: Loreto district map
removed_districts <- c("YURIMAGUAS", "MANSERICHE")

loreto_exposure_lookup <- loreto_dengue_district_df %>%
  distinct(district, road_status) %>%
  mutate(district = str_to_upper(str_trim(district)),
         road_status = as.integer(road_status),
         exposure_group = dplyr::case_when(
           district %in% removed_districts ~ "Removed",
           road_status == 1L ~ "Exposed",
           TRUE ~ "Unexposed"))

districts_loreto_sf <- districts_loreto %>%
  mutate(district = str_to_upper(str_trim(DISTRITO))) %>%
  left_join(loreto_exposure_lookup, by = "district") %>%
  mutate(road_status = if_else(district %in% removed_districts, NA_integer_, replace_na(road_status, 0L)),
         exposure_group = dplyr::case_when(
           district %in% removed_districts ~ "Removed",
           is.na(exposure_group) ~ "Unexposed",
           TRUE ~ exposure_group),
         exposure_group = factor(exposure_group, levels = c("Exposed", "Unexposed", "Removed")))

roads_loreto_major <- roads_loreto %>%
  filter(fclass %in% c("trunk", "primary")) %>%
  filter(ref != "LO-100")

exposure_pal <- c("Exposed" = "#E04490", "Unexposed" = "#648FFF", "Removed"   = "#FFFFFF")

sfig6b_map_loreto <- ggplot() +
  geom_sf(data = districts_loreto_sf, aes(fill = exposure_group), alpha = 0.5, color = "grey30", linewidth = 0.12) +
  geom_sf(data = roads_loreto_major, color = "black", linewidth = 0.8, alpha = 1) +
  scale_fill_manual(
    name = "", values = exposure_pal, breaks = c("Exposed", "Unexposed", "Removed"),
    labels = c("Exposed", "Unexposed", "Removed")) +
  coord_sf(datum = NA) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme_stor +
  theme(legend.position = "bottom",
        legend.box = "horizontal")
sfig6_legend <- get_legend(sfig6b_map_loreto)
sfig6b_map_loreto <- sfig6b_map_loreto + theme(legend.position = "none")
sfig6b_map_loreto

sfig6b_map_loreto_w_inset <- ggdraw() + 
  draw_plot(ggplot() +
              geom_sf(data = peru_outline, fill=NA, color='#3b3b3b', 
                      linewidth=0.03, show.legend = FALSE) +
              geom_sf(data = peru_depts, fill=NA, color='#3b3b3b', 
                      linewidth=0.03, show.legend = FALSE) +
              geom_sf(data = loreto_peru, fill='#cfcfcf', color='#3b3b3b', 
                      linewidth=0.03, show.legend = FALSE) +
              theme_minimal() +
              no_axis +
              theme(legend.text=element_text(size=12),
                    legend.title=element_text(size=14),
                    legend.position = "none"),
            0.0, 0.7, 0.27, 0.27) + 
  draw_plot(sfig11b_map_loreto,
            0.05, 0, 0.95, 1)
sfig6b_map_loreto_w_inset

###################
# SFig6all
###################
sfig6combined <- grid.arrange(sfig6a_map_mdd_w_inset, sfig6b_map_loreto_w_inset, sfig6_legend,                     
                               ncol = 2, nrow = 2,
                               layout_matrix = rbind(c(1,2), c(3, 3)), 
                               heights=c(7,1))

sfig6combined <- as_ggplot(sfig6combined) +
  draw_plot_label(label = c("Madre de Dios", "Loreto"), size = 14,
                  x = c(0.12, 0.7), y = c(1, 1)) +
  draw_plot_label(label = c("A", "B"), size = 14,
                  x = c(0, 0.5), y = c(1, 1)) +
  draw_plot_label(label = c("Interoceanic\n  Highway\n  (external)", "Iquitos-Nauta\n   Highway\n   (internal)"), size = 10,
                  x = c(0.26, 0.76), y = c(0.57, 0.75))

sfig6combined
ggsave("sfig6.pdf", plot=sfig6combined, path="figures/", width = 9.5, height = 6, units="in", device = "pdf")

