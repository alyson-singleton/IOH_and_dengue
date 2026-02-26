# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Conduct MdD and Loreto comparison. Build SFig 8 plot.
#
# Date created: 2/24/2026

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
library(fixest)

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
# Madre de Dios district data
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
# SFig 8a: MdD incidence trends
###################

mdd_raw_trends <- mdd_dengue_district_df %>%
  group_by(year, road_status) %>%
  summarize(dengue_cases = sum(dengue_cases, na.rm = TRUE),
            population = sum(population, na.rm = TRUE),
            incidence = dengue_cases / population * 1000,
            .groups = "drop")

sfig8a <- ggplot(mdd_raw_trends,
                  aes(x = year, y = incidence,
                      color = factor(road_status), group = road_status)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(name = "", 
                     breaks = c("1", "0"),
                     values=c("#E04490","#648FFF"), 
                     labels=c('Exposed', 'Unexposed')) +
  geom_vline(aes(xintercept=2008), linetype='dashed', linewidth=0.5) +
  theme_minimal() +
  labs(title = "          Madre de Dios", x = NULL, y = "dengue\nincidence\n per 1,000 ") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  theme_minimal() +
  theme_stor +
  theme(legend.position = "bottom")
sfig8_legend <- get_legend(sfig8a)
sfig8a <- sfig8a + theme(legend.position = "none")
sfig8a

###################
# SFig 8b: MdD DiD Model
###################

mdd_district_model <- feols(
  incidence ~ i(year, road_status, ref = 2008) | district + year,
  vcov = ~ district,
  data = mdd_dengue_district_df)
iplot(mdd_district_model)

mdd_district_results_df <- as.data.frame(mdd_district_model$coeftable)[1:20, ]
colnames(mdd_district_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
mdd_district_results_df$year <- c(2000:2007,2009:2020)

mdd_district_results_df <- mdd_district_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)
mdd_district_results_df <- bind_rows(
  mdd_district_results_df,
  tibble(year = 2008, estimate = 0, lower = 0,upper = 0))

y_lims <- c(-12,120)

sfig8b <- ggplot(mdd_district_results_df) +
  geom_hline(aes(yintercept=0), colour='grey30', linewidth=.4) +
  geom_ribbon(aes(x=year, ymax=upper, ymin=lower),fill = "grey60", alpha = 0.35) +
  geom_vline(aes(xintercept=2008), linetype='dashed', linewidth=0.4) +
  geom_line(aes(year, estimate), linewidth = 0.8, colour = "grey20") +
  geom_point(aes(year, estimate), size=2, fill='grey20') +
  geom_point(aes(x=2008, y=0), size=3, shape=21, fill='white') +
  xlab("") + ylab("change in\ndengue\nincidence\nper 1,000\nrelative\nto baseline") + 
  theme_minimal() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 6),
                     limits = y_lims,
                     breaks = c(-10, 0, 10, 20, 40, 80)) +
  theme_stor
sfig8b

# SFig 8ab: MdD panels
sfig8ab <- (sfig8a | sfig8b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
sfig8ab

###################
# Loreto district data
###################

loreto_dengue_district_df <- dengue_district_data %>%
  filter(departamento == "LORETO",
         tipo_dx %in% c("C", "P")) %>%
  group_by(ano, distrito, departamento) %>%
  summarize(dengue_cases = n(), .groups = "drop") %>%
  complete(distrito, ano = 2000:2020, departamento, fill = list(dengue_cases = 0)) %>%
  mutate(road_status = if_else(
    distrito %in% c("IQUITOS", "PUNCHANA", "NAUTA", "SAN JUAN BAUTISTA"), 1L, 0L)) %>% #districts with major paved roads ("YURIMAGUAS", "MANSERICHE")
  filter(!distrito %in% c("YURIMAGUAS", "MANSERICHE")) %>%
  rename(district = distrito, year = ano, department = departamento) %>%
  left_join(population_district_data, by = c("district", "year", "department")) %>%
  filter(year %in% 2000:2020) %>%
  mutate(incidence = dengue_cases / population * 1000)

###################
# SFig 8c: Loreto incidence trends
###################

loreto_raw_trends <- loreto_dengue_district_df %>%
  group_by(year, road_status) %>%
  summarize(dengue_cases = sum(dengue_cases, na.rm = TRUE),
            population = sum(population,   na.rm = TRUE),
            incidence = dengue_cases / population * 1000,
            .groups = "drop")

sfig8c <- ggplot(loreto_raw_trends,
                  aes(x = year, y = incidence,
                      color = factor(road_status), group = road_status)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(name = "", 
                     breaks = c("1", "0"),
                     values=c("#E04490","#648FFF"), 
                     labels = c("Exposed", "Unexposed")) +
  geom_vline(aes(xintercept=2004), linetype='dashed', linewidth=0.5) +
  labs(title = "Loreto", x = NULL, y = "") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  theme_minimal() +
  theme_stor
sfig8c

###################
# SFig 8d: Loreto DiD Model
###################

loreto_district_model <- feols(
  incidence ~ i(year, road_status, ref = 2004) | district + year,
  vcov = ~ district,
  data = loreto_dengue_district_df)
iplot(loreto_district_model)

loreto_district_results_df <- as.data.frame(loreto_district_model$coeftable)[1:20, ]
colnames(loreto_district_results_df) <- c('estimate', 'std_error', 't_value', 'p_value')
loreto_district_results_df$year <- c(2000:2003,2005:2020)

loreto_district_results_df <- loreto_district_results_df %>%
  mutate(estimate = estimate,
         upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)
loreto_district_results_df <- bind_rows(
  loreto_district_results_df,
  tibble(year = 2004, estimate = 0, lower = 0,upper = 0))

y_lims <- c(-12,120)

sfig8d <- ggplot(loreto_district_results_df) +
  geom_hline(aes(yintercept=0), colour='grey30', linewidth=.4) +
  geom_ribbon(aes(x=year, ymax=upper, ymin=lower),fill = "grey60", alpha = 0.35) +
  geom_vline(aes(xintercept=2004), linetype='dashed', linewidth=0.4) +
  geom_line(aes(year, estimate), linewidth = 0.8, colour = "grey20") +
  geom_point(aes(year, estimate), size=2, fill='grey20') +
  geom_point(aes(x=2004, y=0), size=3, shape=21, fill='white') +
  xlab("") + ylab("") + 
  theme_minimal() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 6),
                     limits = y_lims,
                     breaks = c(-10, 0, 10, 20, 40, 80)) +
  theme_stor
sfig8d

# SFig 8cd: Loreto panels
sfig8cd <- (sfig8c | sfig8d) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
sfig8cd

###################
# SFig 8all
###################

sfig8combined <- grid.arrange(sfig8a, sfig8c, sfig8b, sfig8d, sfig8_legend,                     
                               ncol = 2, nrow = 3,
                               layout_matrix = rbind(c(1,2),c(3, 4), c(5, 5)), 
                               heights=c(4,4,1))

sfig8combined <- as_ggplot(sfig8combined) +                                
  draw_plot_label(label = c("A", "B", "C", "D"), size = 14,
                  x = c(0.115, 0.54, 0.118, 0.545), y = c(0.95, 0.95, 0.54, 0.54)) 

sfig8combined
ggsave("sfig8.pdf", plot=sfig8combined, path="figures/", width = 9.5, height = 6.5, units="in", device = "pdf")