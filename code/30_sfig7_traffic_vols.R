# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S7.
#
# Date created: 8/6/2025

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)
library(readxl)
library(tidyr)

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

# Function to load and reshape all datasets
reshape_traffic_data <- function(df, name) {
  df <- df %>%
    rename_with(~ as.character(.), -1)  # make sure all column names are characters
  
  # Force all year columns to numeric
  year_cols <- setdiff(colnames(df), "Department")
  df[year_cols] <- lapply(df[year_cols], function(x) as.numeric(as.character(x)))
  
  df %>%
    pivot_longer(-Department, names_to = "Year", values_to = "Value") %>%
    mutate(
      Year = as.integer(Year),
      Metric = name
    )
}

# Load and process data
# Interprovincial Passenger Traffic
interprovincial_passenger_traffic <- read_excel("data/raw/mtc_peru_traffic_data/TRÁFICO_DE_PASAJEROS_EN_EL_TRANSPORTE_INTERPROVINCIAL.xlsx") %>% 
  filter(`TRÁFICO DE PASAJEROS EN EL TRANSPORTE INTERPROVINCIAL, SEGÚN DEPARTAMENTO DESTINO: 2007-2018` %in% c("Madre de Dios", "Loreto"))
colnames(interprovincial_passenger_traffic) <- c("Department", as.character(2007:2018))
interprovincial_passenger_traffic_long <- reshape_traffic_data(interprovincial_passenger_traffic, "Passenger Traffic")

# Estimated Vehicle Fleet
estimated_vehicle_fleet <- read_excel("data/raw/mtc_peru_traffic_data/PARQUE_VEHICULAR_ESTIMADO.xlsx") %>% 
  filter(`PARQUE VEHICULAR ESTIMADO, SEGÚN DEPARTAMENTO: 2007-2018` %in% c("Madre de Dios", "Loreto"))
colnames(estimated_vehicle_fleet) <- c("Department", as.character(2007:2018))
estimated_vehicle_fleet_long <- reshape_traffic_data(estimated_vehicle_fleet, "Estimated Vehicle Fleet")

# National Cargo Vehicle Fleet
vehicle_fleet_for_national_cargo <- read_excel("data/raw/mtc_peru_traffic_data/PARQUE_VEHICULAR_AUTORIZADO_DEL_TRANSPORTE_DE_CARGA_GENERAL_NACIONAL.xlsx") %>% 
  filter(`PARQUE VEHICULAR AUTORIZADO DEL TRANSPORTE DE CARGA GENERAL NACIONAL, SEGÚN DEPARTAMENTO: 2007-2018` %in% c("Madre de Dios", "Loreto"))
colnames(vehicle_fleet_for_national_cargo) <- c("Department", as.character(2007:2018))
vehicle_fleet_national_cargo_long <- reshape_traffic_data(vehicle_fleet_for_national_cargo, "National Cargo Vehicle Fleet")

# Toll Unit Flow
vehicular_flow_at_toll_units <- read_excel("data/raw/mtc_peru_traffic_data/FLUJO_VEHICULAR_EN_LAS_UNIDADES_DE_PEAJE.xlsx") %>% 
  filter(`FLUJO VEHICULAR EN LAS UNIDADES DE PEAJE, SEGÚN DEPARTAMENTO: 2009-2018` %in% c("Madre de Dios", "Loreto"))
colnames(vehicular_flow_at_toll_units) <- c("Department", as.character(2009:2018))
vehicular_flow_toll_units_long <- reshape_traffic_data(vehicular_flow_at_toll_units, "Toll Unit Flow")

# Combine all long-form datasets
all_traffic_data <- bind_rows(
  interprovincial_passenger_traffic_long,
  estimated_vehicle_fleet_long,
  vehicle_fleet_national_cargo_long,
  vehicular_flow_toll_units_long
)

#write_csv(all_traffic_data, "./data/raw/mtc_peru_traffic_data/traffic_data_cleaned.csv")

desired_order <- c(
  "Passenger Traffic",
  "Estimated Vehicle Fleet",
  "National Cargo Vehicle Fleet",
  "Toll Unit Flow"
)

labeled_titles <- c(
  "Passenger Traffic" = "A. Passenger Traffic",
  "Estimated Vehicle Fleet" = "B. Estimated Vehicle Fleet (% change)",
  "National Cargo Vehicle Fleet" = "C. National Cargo Vehicle Fleet",
  "Toll Unit Flow" = "D. Toll Unit Flow"
)

percent_change_df <- all_traffic_data %>%
  filter(Metric == "Estimated Vehicle Fleet") %>%
  group_by(Department) %>%
  arrange(Year) %>%
  mutate(Value = 100 * (Value / first(Value) - 1)) %>%
  ungroup()

all_traffic_data <- all_traffic_data %>%
  filter(Metric != "Estimated Vehicle Fleet") %>%
  bind_rows(percent_change_df) %>%
  # mutate(Value = ifelse(Value == 0, NA, Value)) %>%
  mutate(Metric = factor(Metric, levels = desired_order))


#####################
## SFig 7
#####################

sfig7 <- ggplot(all_traffic_data, aes(x = Year, y = Value, color = Department)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "black", linewidth = 0.7) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2,
             labeller = as_labeller(labeled_titles)) +
  labs(x = "Year",
       y = "",
       color = "Department") +
  scale_color_manual(values = c("Madre de Dios" = "#E04490",
                                "Loreto" = "#648FFF")) +
  scale_x_continuous(breaks= seq(2007, 2017, by=2)) +
  theme_minimal(base_size = 14) +
  theme_stor +
  theme(strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "bottom")
sfig7

ggsave("sfig7.pdf", plot=sfig8, path="figures/", width = 9, height = 7, units="in", device = "pdf")
