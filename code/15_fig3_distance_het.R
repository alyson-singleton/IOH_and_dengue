# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Figure 3.
#
# Date created: 8/4/2025

library(sf)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Load results
dengue_distance_het_df <- read_rds("results/main_text_results/fig3_dengue_distance_het_results_df.rds")

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

#####################
## Fig 3a
#####################
fig3a <- ggplot(dengue_distance_het_df) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=Cutoff, ymax=upper, ymin=lower, color=Cutoff), width=0, linewidth=0.7) +
  geom_point(aes(Cutoff, estimate, fill=Cutoff), color='black', size=3, shape=21) +
  scale_color_manual(name="Treatment", values=list('1km'="#DC267F",'5km'='#FF7900','10km'='#FFB000',
                                                   '15km'='#648FFF','20km'='#785EF0','30km'='#004600'))+ 
  scale_fill_manual(name="Treatment", values=list('1km'="#DC267F",'5km'='#FF7900','10km'='#FFB000',
                                                  '15km'='#648FFF','20km'='#785EF0','30km'='#004600'))+ 
  xlab("") + ylab("change in\ndengue\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal() +
  theme_stor

fig3a

#####################
## Fig 3b
#####################

linked_ids_codes <- read.csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")
center_lat_long <- dengue_yearly$full %>%
  dplyr::select(key, clust, all_cutoffs, key_connected, key_w_dengue) %>%
  distinct() %>%
  left_join(linked_ids_codes, by = "key")
center_lat_long <- center_lat_long %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

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

# Build columns for plotting
center_lat_long_fig4 <- center_lat_long %>%
  mutate(all_cutoffs = case_when(
    !key_connected ~ '-1',
    all_cutoffs %in% c('0', '6', '7') ~ '0',
    TRUE ~ as.character(all_cutoffs)),
    all_cutoffs = factor(all_cutoffs, levels = c('1','2','3','4','5','0','-1')),
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
fig3b <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='lightblue'), linewidth=0.2, show.legend = FALSE) +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='#a6a6a6'), linewidth=0.5, show.legend = FALSE) +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='black'), linewidth=0.7, show.legend = FALSE) +
  geom_sf(data = center_lat_long_fig4, 
          aes(geometry = geometry, fill=all_cutoffs), shape=21, color='black', size=2.7) +
  scale_fill_manual(name= "", values=list('1'="#DC267F",'2'='#FF7900','3'='#FFB000',
                                          '4'='#648FFF','5'='#785EF0','0'='lightgrey','-1'="#ffffff"), 
                    labels=c("1km", "5km", "10km", "15km", "20km", "Control", "Removed")) +
  scale_color_manual(name="", values=c('#8c8c8c','black','lightblue'),
                     labels=c('Unpaved Roads','Highway','Rivers')) +
  theme_minimal() +
  no_axis +
  guides(fill=guide_legend(nrow=2)) +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='bottom',
        legend.spacing.x = unit(0.05, 'cm'),
        legend.box.spacing = unit(0.05, 'cm'),
        legend.key.width = unit(0.4, "cm")) +
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
fig3b

#####################
## Fig 3all
#####################

fig3all <- grid.arrange(fig3a, fig3b,                     
                        ncol = 2, nrow = 1,
                        layout_matrix = rbind(c(1,1,1,2,2,2,2)))

fig3all <- as_ggplot(fig3all) +                                
  draw_plot_label(label = c("A", "B"), size = 17,
                  x = c(0.01, 0.44), y = c(0.99, 0.99)) 

fig3all

ggsave("fig3.pdf", plot=fig3all, path="figures/", width = 10, height = 6, units="in", device = "pdf")
