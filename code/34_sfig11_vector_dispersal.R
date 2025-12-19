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
library(ggrepel)
library(stringr)

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
# Add year paved to highway data
#####################

highway_mdd$year_paved <- NA_real_
section_2007_north <- c(94, 33, 96, 17, 32, 25, 43, 44, 26, 7, 45, 46, 13, 99, 100, 9, 42, 103, 104, 11)
section_2008_north <- c(41, 106, 107, 108, 40, 109, 110, 89, 90, 87, 39, 88, 38, 86)
section_2009_2010_north_pm <- c(37, 111, 113, 21, 36, 12, 23)

section_2007_middle <- c(34, 24, 30, 28, 19, 35, 19, 6, 5, 80, 81, 49, 4, 48, 58, 57, 76, 3)
unclear_middle_in_order <- c(91, 22, 118, 64, 65, 119, 120, 121, 122, 123, 124, 56, 125, 126, 127, 128, 130, 83, 131, 84, 134, 135, 55)

section_2008_south <- c(54, 53, 52, 137, 138, 143, 144, 51, 139)
section_2007_south <- c(140, 141, 142, 97, 98, 50, 68, 2, 77, 73, 74, 72, 69, 70, 71)

highway_mdd <- highway_mdd %>%
  mutate(mvFeatureId = row_number(),
         year_paved = case_when(
           mvFeatureId %in% section_2007_north            ~ 2007,
           mvFeatureId %in% section_2008_north            ~ 2008,
           mvFeatureId %in% section_2009_2010_north_pm    ~ NA_real_,  # or 2010 if you prefer
           mvFeatureId %in% section_2007_middle           ~ 2007,
           mvFeatureId %in% unclear_middle_in_order       ~ NA_real_,
           mvFeatureId %in% section_2008_south            ~ 2008,
           mvFeatureId %in% section_2007_south            ~ 2007,
           TRUE                                          ~ NA_real_)) %>%
  mutate(year_paved = factor(year_paved))

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
arrow_y <- cluster1_coords[1, "Y"] + 0
label_x <- arrow_x + 0.1
label_y <- arrow_y - 0.05

hfs_lat_long_aedes <- hfs_lat_long_aedes %>%
  mutate(year = factor(year)) %>%
  filter(!is.na(year))

year_colors <- c(
  "1999" = "#FDD0C2",
  "2004" = "#FCAE91",
  "2005" = "#FB8C6D",
  "2006" = "#F7684A",
  "2007" = "#E1412F",
  "2008" = "#C61B1D",
  "2009" = "#8A0812"
)

# Plot MdD
mdd_vector_dispersal <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#3b3b3b', size=.15, show.legend = FALSE) +
  #geom_sf(data = rivers, aes(geometry = geometry, color='Rivers'), linewidth=0.2, show.legend = F) +
  #geom_sf(data = roads_mdd, aes(geometry = geometry, color='Unpaved Roads'), linewidth=0.5, show.legend = F) +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color=year_paved), linewidth=2, show.legend = T) +
  geom_sf(data = hfs_lat_long_aedes, aes(geometry = geometry, fill=year), color='black', shape = 21, size = 5) +
  geom_label_repel(
    data = hfs_lat_long_aedes,
    stat = "sf_coordinates",            # convert sf → xy for repelling
    aes(geometry = geometry, label = year),
    size = 3,
    fill = "white",                     # white box behind text
    label.size = 0.2,                   # border thickness
    label.padding = unit(0.15, "lines"),
    box.padding = 0.5,                  # pushes labels farther away
    point.padding = 0.5,               # padding around each dot
    label.r = unit(0.05, "lines"),      # small corner rounding
    segment.color = "black",            # line connecting label and point
    segment.size = 0.5,
    min.segment.length = 0,
    max.overlaps = Inf) +
  scale_fill_manual(name = "", values = year_colors, na.value = "white") +
  # scale_fill_manual(name= "", values=c("1" = "#E04490", "0" = "#648FFF", "3" = "grey80", "2" = "#ffffff"),
  #                   labels=c("Exposed (<5km)", "Unexposed (>10km)", "Buffer (removed)", "Disconnected (removed)")) +
  #scale_color_manual(name = "", values = c("Unpaved Roads" = '#8c8c8c', "Highway" = 'black',"Rivers" = 'lightblue')) +
  theme_minimal() +
  no_axis +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.spacing.x = unit(0.02, "cm"),
        legend.box.spacing = unit(0.01, "cm"),
        legend.text = element_text(size = 10),
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
mdd_vector_dispersal

ggsave("sfig11.pdf", plot=mdd_vector_dispersal, path="figures/", width = 8, height = 6, units="in", device = "pdf")
