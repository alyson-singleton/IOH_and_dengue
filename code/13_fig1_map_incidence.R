# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Figure 1.
#
# Date created: 8/4/2025

library(sf)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(ggspatial)

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
linked_ids_codes <- read.csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")
center_lat_long <- dengue_yearly$full %>%
  dplyr::select(key, clust, all_cutoffs, key_connected, key_w_dengue) %>%
  distinct() %>%
  left_join(linked_ids_codes, by = "key")
center_lat_long <- center_lat_long %>%
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
## Fig 1a
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

# Plot MdD
mdd_map <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#3b3b3b', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='Rivers'), linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='Unpaved Roads'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='Highway'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = center_lat_long_fig1, aes(geometry = geometry, fill=all_cutoffs), color='black', shape = 21, size = 3) +
  scale_fill_manual(name= "", values=c("1" = "#E04490", "0" = "#648FFF", "3" = "grey80", "2" = "#ffffff"),
                    labels=c("Exposed (<5km)", "Unexposed (>10km)", "Buffer (removed)", "Disconnected (removed)")) +
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

fig1a <- ggdraw() + 
  draw_plot(ggplot() +
              geom_sf(data = peru_outline, fill=NA, color='#3b3b3b', 
                      size=.001, show.legend = FALSE) +
              geom_sf(data = peru_depts, fill=NA, color='#3b3b3b', 
                      size=.001, show.legend = FALSE) +
              geom_sf(data = mdd_region, fill='#cfcfcf', color='#3b3b3b', 
                      size=.001, show.legend = FALSE) +
              theme_minimal() +
              no_axis +
              theme(legend.text=element_text(size=12),
                    legend.title=element_text(size=14),
                    legend.position = "none"),
            0.65, 0.65, 0.3, 0.3) + 
  draw_plot(mdd_map,
            0, 0, 1, 1)
fig1a

#####################
## Fig 1b
#####################

dengue_raw_plotting <- dengue_yearly$connected_buffered %>%
  group_by(fivekm, year) %>%
  summarize(new_incidence = sum(yearly_cases_C)/sum(population)*1000)

dengue_raw_plotting$year <- as.Date(dengue_raw_plotting$year)
dengue_raw_plotting$fivekm <- as.character(dengue_raw_plotting$fivekm)

fig1b <- ggplot(dengue_raw_plotting) +
  geom_line(aes(x=year, y=new_incidence, colour=fivekm)) +
  geom_vline(xintercept=as.Date('2008-01-01'),linetype='dashed') +
  ggtitle("Dengue incidence per 1,000") +
  xlab("Year") + ylab("") + 
  scale_color_manual(name = "", values=c("#648FFF","#E04490"), labels=c('Unexposed (>10km)', 'Exposed (<5km)'),) +
  theme_bw()+
  theme(plot.title = element_text(size=12, face="bold"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=14),
        axis.title.y=element_text(size=12,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "none",
        strip.text.x = element_text(size = 10))
fig1b

#####################
## Fig 1c
#####################

leish_raw_plotting <- leish_yearly$connected_buffered %>%
  group_by(fivekm, year) %>%
  summarize(new_incidence = sum(yearly_cases_C)/sum(population)*1000)

leish_raw_plotting$year <- as.Date(leish_raw_plotting$year)
leish_raw_plotting$fivekm <- as.character(leish_raw_plotting$fivekm)
fig1c <- ggplot(leish_raw_plotting) +
  geom_line(aes(x=year, y=new_incidence, colour=fivekm)) +
  geom_vline(xintercept=as.Date('2008-01-01'),linetype='dashed') +
  scale_y_continuous(labels = function(x) round(x, 3)) +
  ggtitle("Leishmaniasis incidence per 1,000") +
  xlab("Year") + ylab("") + 
  scale_color_manual(name = "Exposure", values=c("#648FFF","#E04490"), labels=c('Far (>10km)', 'Near (<5km)'),) +
  theme_bw()+
  theme(plot.title = element_text(size=12, face="bold"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=14),
        axis.title.y=element_text(size=12,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "none",
        strip.text.x = element_text(size = 10))
fig1c

#####################
## Fig 1all
#####################

fig1all <- grid.arrange(fig1a, fig1b, fig1c, fig1_legend,
                        ncol = 4, nrow = 3,
                        layout_matrix = rbind(c(1,1,1,2,2,2),c(1,1,1,3,3,3),c(NA,4,4,4,4,NA)), 
                        heights=c(3,3,1))

fig1all <- as_ggplot(fig1all) +                                
  draw_plot_label(label = c("A", "B", "C"), size = 14,
                  x = c(0.01, 0.48, 0.48), y = c(1, 1, 0.57)) 

ggsave("fig1.pdf", plot=fig1all, path="figures/", width = 10, height = 6, units="in", device = "pdf")
