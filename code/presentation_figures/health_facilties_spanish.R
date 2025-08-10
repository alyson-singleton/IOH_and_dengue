# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build simple health facilities map.
#
# Date created: 8/10/2025

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)
library(ggspatial)

# Load spatial data
linked_ids_codes <- read.csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")

center_lat_long <- dengue_yearly$full %>%
  dplyr::select(key, clust, all_cutoffs, key_connected, key_w_dengue) %>%
  distinct() %>%
  left_join(linked_ids_codes, by = "key")
center_lat_long <- center_lat_long %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

allFeatsCircleCentroid <- read_sf("data/intermediate/clustering_data/cluster_centroids.shp")

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



# Panel A: Full region
full_region_map <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='lightblue'), fill='lightblue', linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='#a6a6a6'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='black'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = center_lat_long, aes(geometry = geometry), fill = '#648FFF', color='black', pch=21, size = 2.5, alpha=0.8) +
  scale_color_manual(name = "", values = c('#a6a6a6','black','lightblue'),
                     labels = c('Caminos sin pavimentar','Carretera Interoceánica','Ríos')) +
  annotation_scale(location = "bl",height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "br",style = north_arrow_orienteering(text_size = 6), 
                         height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_minimal() +
  no_axis +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = 'bottom') +
  guides(fill = guide_legend(override.aes = list(shape=21)))+
  annotate("segment", 
           x = label_x-0.02, y = label_y, 
           xend = arrow_x, yend = arrow_y,
           arrow = arrow(length = unit(0.17, "cm")), 
           color = "black") +
  annotate("text", 
           x = label_x, y = label_y, 
           label = "PM", 
           size = 3.2, fontface = "bold", hjust = 0, color = "black")
full_region_map
ggsave("full_region_map.pdf", plot=full_region_map, "~/Desktop/doctorate/ch2 mdd highway/presentation_figures/spanish/", width = 7, height = 5, units = c("in"), device="pdf")

# Panel B: PM zoom
pm_zoom_map <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='lightblue'), fill='lightblue', linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='#a6a6a6'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='black'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = center_lat_long, aes(geometry = geometry), fill = '#648FFF', color='black', pch=21, size = 2.5, alpha=1) +
  scale_color_manual(name = "", values = c('#a6a6a6','black','lightblue'),
                     labels = c('Unpaved Roads','Highway','Rivers')) +
  theme_minimal() +
  no_axis +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = 'bottom') +
  guides(fill = guide_legend(override.aes = list(shape=21)))

xlim_pm <- c(-69.30, -69.07)
ylim_pm <- c(-12.83, -12.45)

pm_zoom_map_cropped <- pm_zoom_map +
  coord_sf(xlim = xlim_pm, ylim = ylim_pm, expand = FALSE) +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.position = 'none')+
  annotation_scale(location = "br",height = unit(0.1, "cm"))
pm_zoom_map_cropped

facilities_map_all <- full_region_map + plot_spacer() + pm_zoom_map_cropped +
  plot_layout(ncol = 3, widths = c(2.5, 0.05, 0.8)) + 
  plot_annotation(
    tag_levels = 'A',
    tag_prefix = '', tag_suffix = '',
    theme = theme(
      plot.tag = element_text(size = 18, face = "bold", hjust = -0.1, vjust = 1.2)
    )
  )

facilities_map_all

ggsave("facilities_map_all.pdf", plot=facilities_map_all, "~/Desktop/doctorate/ch2 mdd highway/presentation_figures/spanish/", width = 8, height = 5, units = c("in"), device="pdf")
