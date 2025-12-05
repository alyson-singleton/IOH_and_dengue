# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure 11.
#
# Date created: 12/4/2025

library(sf)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(ggspatial)
library(cowplot)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Load dengue panel datasets
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_biannual <- readRDS("data/clean/dengue_biannual_panels.rds")

# Load leishmaniasis panel datasets
leish_yearly <- readRDS("data/clean/leish_yearly_panels.rds")
leish_biannual <- readRDS("data/clean/leish_biannual_panels.rds")

# Load spatial data
mdd_aedes_aegypti_chronology <- read.csv("data/raw/diresa_data/mdd_aedes_aegypti_chronology.csv")
linked_ids_codes <- read.csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")
linked_clean <- linked_ids_codes %>%
  mutate(name = name |> str_squish() |> str_to_lower()) %>%
  filter(!key %in% c(101, 105, 106))

hfs_lat_long_aedes <- mdd_aedes_aegypti_chronology %>%
  full_join(linked_clean, by = "name") %>%
  arrange(desc(is.na(year)), year)
hfs_lat_long_aedes <- hfs_lat_long_aedes %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

peru_depts <- read_sf("data/raw/spatial_data/peru_department_shapefiles.shp")
peru_depts <- st_as_sf(peru_depts) 
peru_depts$geometry <- st_transform(peru_depts$geometry, 4326)
peru_outline <- st_union(peru_depts$geometry)

districts <- read_sf("data/raw/spatial_data/mdd_districts.shp")
districts <- st_as_sf(districts) 
districts$geometry <- st_transform(districts$geometry, 4326)
mdd_region <- st_union(districts$geometry)

rivers <- read_sf("data/raw/spatial_data/mdd_rivers.shp")
rivers <- st_as_sf(rivers, crs=4326) 
rivers$geometry <- st_transform(rivers$geometry, "EPSG:4326")

roads <- read_sf("data/raw/spatial_data/mdd_roads.shp")
roads <- st_as_sf(roads) 
roads$geometry <- st_transform(roads$geometry, 4326)
roads_mdd <- st_covers(mdd_region,roads$geometry, sparse = FALSE)
roads_mdd <- roads[roads_mdd[1,],]

highway <- read_sf("data/raw/spatial_data/peru_roads_important.shp")
highway <- highway[which(highway$ref=="PE-30C"),]
highway <- st_as_sf(highway) 
highway$geometry <- st_transform(highway$geometry, 4326)

highway_mdd <- st_covers(mdd_region,highway$geometry, sparse = FALSE)
highway_mdd <- highway[highway_mdd[1,],]

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

#####################
## SFig 11
#####################

# Build columns for plotting
center_lat_long_fig1 <- center_lat_long %>%
  mutate(
    all_cutoffs = case_when(
      all_cutoffs %in% c('0', '4', '5', '6', '7') ~ '0',
      all_cutoffs %in% c('1', '2') ~ '1',
      TRUE ~ as.character(all_cutoffs))) %>%
  mutate(
    all_cutoffs = if_else(!key_connected, '2', all_cutoffs),
    all_cutoffs = factor(all_cutoffs, levels = c('1', '0', '3', '2')),
    pm_indicator = if_else(clust == 1, 24, 21),
    pm_indicator2 = if_else(clust == 1, 3.2, 2.5)) %>%
  arrange(if_else(clust == 1, 1, 0))

# Coordinates for labeling
cluster1_coords <- st_coordinates(center_lat_long_fig1 %>% filter(clust == 1))
arrow_x <- cluster1_coords[1, "X"] + 0.15
arrow_y <- cluster1_coords[1, "Y"] - 0.05
label_x <- arrow_x + 0.1
label_y <- arrow_y - 0.05

hfs_lat_long_aedes <- hfs_lat_long_aedes %>%
  mutate(year = factor(year))

year_colors <- c(
  "1999" = "#4D4D4D",   # dark gray for the unique year
  "2004" = "#bdd7e7",
  "2005" = "#6baed6",
  "2006" = "#3182bd",
  "2007" = "#08519c",
  "2008" = "#08306b",
  "2009" = "#041F3D"
)

# Plot MdD
mdd_map <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#3b3b3b', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='Rivers'), linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='Unpaved Roads'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='Highway'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = hfs_lat_long_aedes, aes(geometry = geometry, fill=year), color='black', shape = 21, size = 3) +
  geom_sf_text(
    data = hfs_lat_long_aedes,
    aes(geometry = geometry, label = year),
    nudge_y = -0.08,   # adjust vertically
    nudge_x = 0.15   # adjust horizontally
    #size = 3
  ) +
  scale_fill_manual(values = year_colors, na.value = "white") +
  # scale_fill_manual(name= "", values=c("1" = "#E04490", "0" = "#648FFF", "3" = "grey80", "2" = "#ffffff"),
  #                   labels=c("Exposed (<5km)", "Unexposed (>10km)", "Buffer (removed)", "Disconnected (removed)")) +
  scale_color_manual(name = "", values = c("Unpaved Roads" = '#8c8c8c', "Highway" = 'black',"Rivers" = 'lightblue')) +
  theme_minimal() +
  no_axis +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.spacing.x = unit(0.02, "cm"),
        legend.box.spacing = unit(0.01, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 10),
        legend.direction = "horizontal") +
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2)) +
  annotation_scale(location = "bl",height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "br",style = north_arrow_orienteering(text_size = 6), 
                         height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotate("segment", 
           x = label_x-0.02, y = label_y, 
           xend = arrow_x, yend = arrow_y,
           arrow = arrow(length = unit(0.17, "cm")), 
           color = "black") +
  annotate("text", 
           x = label_x, y = label_y, 
           label = "PM", 
           size = 3.4, fontface = "bold", hjust = 0, color = "black") + 
  geom_point(data = data.frame(lon = -69.19, lat = -12.5933), 
             aes(x = lon, y = lat), 
             color = "black", 
             size = 8, shape = 1, stroke = 1)
mdd_map
fig1_legend <- get_legend(mdd_map)
mdd_map <- mdd_map + theme(legend.position = "none")