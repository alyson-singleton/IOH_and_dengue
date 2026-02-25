# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Conduct Madre de Dios and Loreto comparison and 
# build Supplementary Figures 11 and 12.
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

peru_districts <- read_sf("./data/raw/spatial_data/peru_districts.shp") %>% #read_sf("data/raw/spatial_data/PER_adm/PER_adm3.shp") %>%
  st_as_sf() %>% 
  st_transform(4326)
mapview(peru_districts)

districts_mdd <- peru_districts %>% filter(DEPARTAMEN == "MADRE DE DIOS")
districts_loreto <- peru_districts %>% filter(DEPARTAMEN == "LORETO")
districts_cusco <- peru_districts %>% filter(DEPARTAMEN == "CUSCO")

###################
# Roads
###################

roads <- read_sf("~/Desktop/gis_osm_roads/gis_osm_roads_free_1.shp") %>% #make this more usable and able to be put online
  st_as_sf() %>% 
  st_transform(4326)

roads_mdd <- st_covers(mdd_peru,roads$geometry, sparse = FALSE) #these take forever to run
roads_mdd <- roads[roads_mdd[1,],]
mapview(roads_mdd %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_mdd)

roads_loreto <- st_covers(loreto_peru,roads$geometry, sparse = FALSE)
roads_loreto <- roads[roads_loreto[1,],]
mapview(roads_loreto %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
  mapview(districts_loreto)

# roads_cusco <- st_covers(cusco_peru,roads$geometry, sparse = FALSE)
# roads_cusco <- roads[roads_cusco[1,],]
# mapview(roads_cusco %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red") +
#   mapview(districts_cusco)

###################
# Dengue & population data (district level)
###################

dengue_district_data <- read_rds("./data/raw/cdc_district_dengue_data/solicitud_aly.rds") #**add this into data folder
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

# sfig12a: MdD incidence trends
mdd_raw_trends <- mdd_dengue_district_df %>%
  group_by(year, road_status) %>%
  summarize(dengue_cases = sum(dengue_cases, na.rm = TRUE),
            population = sum(population, na.rm = TRUE),
            incidence = dengue_cases / population * 1000,
            .groups = "drop")

sfig12a <- ggplot(mdd_raw_trends,
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
sfig12_legend <- get_legend(sfig12a)
sfig12a <- sfig12a + theme(legend.position = "none")
sfig12a

# sfig12b: MdD DiD Model
mdd_district_model <- feols(
  incidence ~ i(year, road_status, ref = 2008) | district + year,
  vcov = ~ district,
  data = mdd_dengue_district_df)
iplot(mdd_district_model)

# mdd_district_df_agg <- mdd_dengue_district_df %>%
#   filter(year > 2007) %>%
#   mutate(year_binary = if_else(year > 2008, 1, 0))
# 
# mdd_district_agg_model <- feols(
#   incidence ~ year_binary * road_status | district + year,
#   vcov = ~ district,
#   data = mdd_district_df_agg)
# mdd_district_agg_model

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

sfig12b <- ggplot(mdd_district_results_df) +
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
sfig12b

# MdD panels
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

# sfig12c: Loreto incidence trends
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
                     labels = c("Exposed", "Unexposed")) +
  geom_vline(aes(xintercept=2004), linetype='dashed', linewidth=0.5) +
  labs(title = "Loreto", x = NULL, y = "") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4),
                     labels = seq(2000, 2020, by = 4)) +
  theme_minimal() +
  theme_stor

sfig12c

# sfig12d: Loreto DiD Model
loreto_district_model <- feols(
  incidence ~ i(year, road_status, ref = 2004) | district + year,
  vcov = ~ district,
  data = loreto_dengue_district_df)
iplot(loreto_district_model)

# loreto_district_df_agg <- loreto_dengue_district_df %>%
#   filter(year > 2004) %>%
#   mutate(year_binary = if_else(year > 2005, 1, 0))
# 
# loreto_district_agg_model <- feols(
#   incidence ~ year_binary * road_status | district + year,
#   vcov = ~ district,
#   data = loreto_district_df_agg)
# loreto_district_agg_model

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

sfig12d <- ggplot(loreto_district_results_df) +
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
sfig12d

# Loreto panels
sfig12cd <- (sfig12c | sfig12d) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
sfig12cd

###################
# SFig12all
###################
sfig12combined <- grid.arrange(sfig12a, sfig12c, sfig12b, sfig12d, sfig12_legend,                     
                             ncol = 2, nrow = 3,
                             layout_matrix = rbind(c(1,2),c(3, 4), c(5, 5)), 
                             heights=c(4,4,1))

sfig12combined <- as_ggplot(sfig12combined) +                                
  draw_plot_label(label = c("A", "B", "C", "D"), size = 14,
                  x = c(0.115, 0.54, 0.118, 0.545), y = c(0.95, 0.95, 0.54, 0.54)) 

sfig12combined
ggsave("sfig12.pdf", plot=sfig12combined, path="figures/", width = 9.5, height = 6.5, units="in", device = "pdf")

###################
# sfig11
###################

# sfig11a: MdD district map
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

sfig11a_map_mdd <- ggplot() +
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
    inherit.aes = FALSE
  ) +
  geom_text(
    data = arrows_df,
    aes(x = lab_x, y = lab_y, label = lab),
    size = 3.6,
    hjust = 0,
    vjust = 0,
    color = "black",
    fontface = "italic",
    inherit.aes = FALSE
  )

sfig11a_map_mdd

sfig11a_map_mdd_w_inset <- ggdraw() + 
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
sfig11a_map_mdd_w_inset

# sfig11b: Loreto district map
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

sfig11b_map_loreto <- ggplot() +
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
sfig11_legend <- get_legend(sfig11b_map_loreto)
sfig11b_map_loreto <- sfig11b_map_loreto + theme(legend.position = "none")
sfig11b_map_loreto

sfig11b_map_loreto_w_inset <- ggdraw() + 
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
sfig11b_map_loreto_w_inset

###################
# SFig11all
###################
sfig11combined <- grid.arrange(sfig11a_map_mdd_w_inset, sfig11b_map_loreto_w_inset, sfig11_legend,                     
                               ncol = 2, nrow = 2,
                               layout_matrix = rbind(c(1,2), c(3, 3)), 
                               heights=c(7,1))

sfig11combined <- as_ggplot(sfig11combined) +
  draw_plot_label(label = c("Madre de Dios", "Loreto"), size = 14,
                  x = c(0.12, 0.7), y = c(1, 1)) +
  draw_plot_label(label = c("A", "B"), size = 14,
                  x = c(0, 0.5), y = c(1, 1)) +
  draw_plot_label(label = c("Interoceanic\n  Highway\n  (external)", "Iquitos-Nauta\n   Highway\n   (internal)"), size = 10,
                  x = c(0.26, 0.76), y = c(0.57, 0.75))

sfig11combined
ggsave("sfig11.pdf", plot=sfig11combined, path="figures/", width = 9.5, height = 6, units="in", device = "pdf")

###################
# Cusco?
###################

# cusco_dengue_district_df <- dengue_district_data %>%
#   filter(departamento == "CUSCO",
#          tipo_dx %in% c("C", "P")) %>%
#   group_by(ano, distrito, departamento) %>%
#   summarize(dengue_cases = n(), .groups = "drop") %>%
#   complete(distrito, ano = 2000:2020, departamento, fill = list(dengue_cases = 0)) %>%
#   # mutate(
#   #   road_status = if_else(
#   #     distrito %in% c("IQUITOS", "PUNCHANA", "NAUTA", "SAN JUAN BAUTISTA",
#   #                     "YURIMAGUAS", "MANSERICHE"), 1L, 0L)) %>% #districts with major paved roads
#   rename(district = distrito, year = ano, department = departamento) %>%
#   left_join(population_district_data, by = c("district", "year", "department")) %>%
#   filter(year %in% 2000:2020) %>%
#   mutate(incidence = dengue_cases / population * 1000)
# 
# ggplot(cusco_dengue_district_df) +
#   geom_line(aes(year, incidence, group = district))
# 
# cusco_totals <- cusco_dengue_district_df %>%
#   filter(year %in% 2000:2020) %>%
#   group_by(district) %>%
#   summarise(total_cases = sum(dengue_cases, na.rm = TRUE), .groups = "drop") %>%
#   mutate(district = str_to_upper(str_trim(district)))
# 
# districts_cusco_sf <- districts_cusco %>%
#   mutate(district = str_to_upper(str_trim(DISTRITO)))  # change NAME_3 if your district-name field differs
# 
# cusco_map_df <- districts_cusco_sf %>%
#   left_join(cusco_totals, by = "district") %>%
#   mutate(total_cases = replace_na(total_cases, 0))
# 
# mapview(cusco_map_df, zcol = "total_cases", layer.name = "Cusco dengue cases (2000–2020)") + 
#   mapview(roads_cusco %>% filter(maxspeed >= 30 | fclass %in% c("trunk", "primary")), color = "red")
