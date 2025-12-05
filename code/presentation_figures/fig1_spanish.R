# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Figure 1 w Spanish labels.
#
# Date created: 8/10/2025

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
  geom_sf(data = rivers, aes(geometry = geometry, color='Ríos'), linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='Caminos sin pavimentar'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='Carretera Interoceánica'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = center_lat_long_fig1, aes(geometry = geometry, fill=all_cutoffs), color='black', shape = 21, size = 3) +
  scale_fill_manual(name= "", values=c("1" = "#E04490", "0" = "#648FFF", "3" = "grey80", "2" = "#ffffff"),
                    labels=c("Expuesto (<5km)", "No expuesto (>10km)", "Buffer (eliminado)", "Desconectado (eliminado)")) +
  scale_color_manual(name = "", values = c("Caminos sin pavimentar" = '#8c8c8c', "Carretera Interoceánica" = 'black',"Ríos" = 'lightblue')) +
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
  annotation_scale(location = "bl", height = unit(0.15, "cm"), text_cex = 0.9) +
  annotation_north_arrow(location = "br",style = north_arrow_orienteering(text_size = 9), 
                         height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  annotate("segment", 
           x = label_x-0.02, y = label_y, 
           xend = arrow_x, yend = arrow_y,
           arrow = arrow(length = unit(0.17, "cm")), 
           color = "black") +
  annotate("text", 
           x = label_x, y = label_y, 
           label = "PM", 
           size = 4, hjust = 0, color = "black") + #fontface = "bold", 
  geom_point(data = data.frame(lon = -69.19, lat = -12.5933), 
             aes(x = lon, y = lat), 
             color = "black", 
             size = 8, shape = 1, stroke = 1)
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
              theme(legend.text=element_text(size=14),
                    legend.title=element_text(size=14),
                    legend.position = "none"),
            0.65, 0.65, 0.3, 0.3) + 
  draw_plot(mdd_map,
            0, 0, 1, 1)
fig1a
ggsave("fig1a_spanish.pdf", plot=fig1a, path="~/Desktop/doctorate/ch2 mdd highway/presentation_figures/spanish/", width = 6, height = 6, units="in", device = "pdf")

#####################
## Fig 1b
#####################

dengue_raw_plotting <- dengue_yearly$connected_buffered %>%
  group_by(fivekm, year) %>%
  summarize(mean_incidence = mean(incidence))

dengue_raw_plotting$year <- as.Date(dengue_raw_plotting$year)
dengue_raw_plotting$fivekm <- as.character(dengue_raw_plotting$fivekm)

# Recode fivekm so that "1" = "Expuesto", "0" = "No expuesto"
dengue_raw_plotting$fivekm <- factor(dengue_raw_plotting$fivekm, 
                                     levels = c("1", "0"), 
                                     labels = c("Expuesto (<5km)", "No expuesto (>10km)"))

fig1b <- ggplot(dengue_raw_plotting) +
  geom_line(aes(x=year, y=mean_incidence, colour=fivekm), linewidth = 0.7) +
  geom_vline(xintercept=as.Date('2008-01-01'),linetype='dashed') +
  ggtitle("Incidencia de dengue por cada 1,000 personas") +
  xlab("Año") + ylab("") + 
  scale_color_manual(values=c("#E04490","#648FFF"), labels=c('Expuesto (<5km)','No expuesto (>10km)')) +
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     breaks = c(0, 10, 20, 30)) +
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
        strip.text.x = element_text(size = 10),
        plot.background = element_blank())
fig1b
ggsave("fig1b_spanish.pdf", plot=fig1b, path="~/Desktop/doctorate/ch2 mdd highway/presentation_figures/spanish/", width = 6, height = 4, units="in", device = "pdf")
#12x4
#####################
## Fig 1c
#####################

leish_raw_plotting <- leish_yearly$connected_buffered %>%
  group_by(fivekm, year) %>%
  summarize(mean_incidence = mean(incidence))

leish_raw_plotting$year <- as.Date(leish_raw_plotting$year)
leish_raw_plotting$fivekm <- factor(leish_raw_plotting$fivekm, 
                                     levels = c("1", "0"), 
                                     labels = c("Expuesto (<5km)", "No expuesto (>10km)"))

fig1c <- ggplot(leish_raw_plotting) +
  geom_line(aes(x=year, y=mean_incidence, colour=fivekm), linewidth=0.7) +
  geom_vline(xintercept=as.Date('2008-01-01'),linetype='dashed') +
  scale_y_continuous(labels = function(x) round(x, 3)) +
  scale_x_date(
    date_breaks = "4 years",
    date_labels = "%Y") +
  ggtitle("Incidencia de leishmaniasis por cada 1,000 personas") +
  xlab("Año") + ylab("") + 
  scale_color_manual(name = "Exposure", values=c("#E04490", "#648FFF"), labels=c('Expuesto (<5km)','No expuesto (>10km)'),) +
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
fig1c
ggsave("fig1c_spanish.pdf", plot=fig1c, path="~/Desktop/doctorate/ch2 mdd highway/presentation_figures/spanish/", width = 7, height = 3, units="in", device = "pdf")

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

ggsave("fig1_spanish.pdf", plot=fig1all, path="~/Desktop/doctorate/ch2 mdd highway/presentation_figures/spanish/", width = 10, height = 6, units="in", device = "pdf")
