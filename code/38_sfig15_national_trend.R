# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S15.
#
# Date created: 4/14/2026

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)
library(ggrepel)
library(tidyverse)

# Create standard figure theme
theme_stor <- theme(panel.grid.minor.x = element_line(linewidth = 0.3),
                    panel.grid.major.x = element_line(linewidth = 0.3),
                    panel.grid.major.y = element_line(linewidth = 0.3),
                    axis.line.x = element_line(color = "black", linewidth = 0.3),
                    axis.line.y = element_line(color = "black", linewidth = 0.3),
                    plot.title = element_text(size=20, hjust=0.1),
                    plot.title.position = "plot",
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=12),
                    axis.title.y=element_text(size=11,angle=0, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=12),
                    axis.title.x=element_text(size=12),
                    axis.text.x=element_text(size=12),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=12),
                    legend.position = "none",
                    strip.text.x = element_text(size = 12))

# Load data
# Madre de Dios dengue data
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_raw_plotting <- dengue_yearly$connected_buffered %>%
  mutate(year = as.integer(substr(as.character(year), 1, 4)),
         fivekm = as.character(fivekm)) %>%
  group_by(fivekm, year) %>%
  summarize(mean_incidence = mean(incidence, na.rm = TRUE),
            .groups = "drop")

# National dengue case data (publicly available: https://www.datosabiertos.gob.pe/dataset/vigilancia-epidemiol%C3%B3gica-de-dengue)
peru_national_dengue <- read_delim("data/raw/cdc_dengue_data/datos_abiertos_vigilancia_dengue_2000_2024.csv", delim=";")
peru_national_dengue_depts <- peru_national_dengue %>%
  filter(enfermedad == "DENGUE SIN SIGNOS DE ALARMA") %>%
  mutate(ano = as.integer(ano)) %>%
  group_by(departamento, ano) %>%
  dplyr::summarize(dengue_cases = n(), .groups = "drop") %>%
  ungroup()

all_departments <- unique(peru_population$department)
all_years <- seq(2000,2024)

peru_national_dengue_depts_complete <- peru_national_dengue_depts %>%
  complete(departamento = all_departments,
           ano = all_years,
           fill = list(dengue_cases = 0))

# Population data (WorldPop)
peru_population <- read.csv("data/raw/environmental_data/peru_population_departments_yearly.csv")
peru_population <- peru_population[,c(37:59)]
colnames(peru_population) <- c("department", "id", 2000:2020)
peru_population_long <- peru_population %>%
  pivot_longer(cols = c(3:23), 
               names_to = "year", 
               values_to = "population") %>%
  mutate(year = as.integer(year))

peru_dengue_inc_depts <- peru_population_long %>%
  left_join(peru_national_dengue_depts,
            by = c("department" = "departamento", "year" = "ano")) %>%
  mutate(dengue_cases = tidyr::replace_na(dengue_cases, 0),
         dengue_incidence = dengue_cases / population * 1000)

# Identify dengue endemic depts
# We define departments with sustained dengue transmission as those with 
# cases reported in more than five years and >500 cumulative cases, 
# excluding regions with only sporadic or imported transmission.
dengue_endemic_depts <- peru_dengue_inc_depts %>%
  group_by(department) %>%
  dplyr::summarize(
    total_cases = sum(dengue_cases, na.rm = TRUE),
    years_with_cases = sum(dengue_cases > 10, na.rm = TRUE),
    .groups = "drop") %>%
  filter(total_cases > 500 & years_with_cases > 5) %>%
  pull(department)

peru_dengue_restricted <- peru_dengue_inc_depts %>%
  filter(department %in% dengue_endemic_depts)

# Build median, 10th, and 90th percentile
peru_dept_summary <- peru_dengue_restricted %>%
  group_by(year) %>%
  dplyr::summarize(
    p10 = quantile(dengue_incidence, 0.10, na.rm = TRUE),
    median_incidence = median(dengue_incidence, na.rm = TRUE),
    p90 = quantile(dengue_incidence, 0.90, na.rm = TRUE),
    .groups = "drop")

# Prep facet plot data
focal_depts <- c("MADRE DE DIOS", "TUMBES", "PIURA", "LORETO", "UCAYALI")

dept_facet_data <- peru_dengue_inc_depts %>%
  filter(department %in% focal_depts) %>%
  mutate(
    department = factor(
      department,
      levels = c("MADRE DE DIOS", "TUMBES", "PIURA", "LORETO", "UCAYALI"),
      labels = c("Madre de Dios", "Tumbes", "Piura", "Loreto", "Ucayali")))

#####################
## SFig 15a
#####################

sfig15a <- ggplot() +
  geom_ribbon(
    data = peru_dept_summary,
    aes(x = year, ymin = p10, ymax = p90),
    fill = "grey70",
    alpha = 0.4) +
  geom_line(
    data = peru_dept_summary,
    aes(x = year, y = median_incidence),
    color = "grey40",
    linewidth = 0.9) +
  geom_line(
    data = peru_dengue_inc_depts %>% filter(department == "MADRE DE DIOS"),
    aes(x = year, y = dengue_incidence),
    color = "maroon",
    linewidth = 0.9) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  labs(x = NULL,
       y = "Dengue\nincidence\nper 1,000") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4)) +
  annotate("text", x = 2011, y = 23,
           label = "Madre de Dios", color = "maroon", hjust = 0, size = 5) +
  annotate("text", x = 2010, y = 4,
           label = "Median", color = "grey40", hjust = 0, size = 5) +
  theme_minimal() +
  theme_stor

#####################
## SFig 15b
#####################

sfig15b <- ggplot(dept_facet_data, aes(x = year, y = dengue_incidence)) +
  geom_line(color = "grey40", linewidth = 0.8) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black") +
  facet_wrap(~department, ncol = 3) +
  labs(x = "Year",
       y = "Dengue\nincidence\nper 1,000") +
  scale_x_continuous(breaks = c(2000,2008,2016)) +
  theme_minimal() +
  theme_stor +
  theme(legend.position = "none",
        strip.text = element_text(size = 11, face = "bold"))

#####################
## SFig 15all
#####################

sfig15all <- sfig15a / sfig15b +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 13))
sfig15all

ggsave("sfig15.pdf", plot=sfig15all, path="figures/", width = 8, height = 8, units="in", device = "pdf")
