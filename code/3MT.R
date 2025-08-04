

############################
### simple plot wo insets ##
############################
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

sfig2a <- ggplot() +
  #geom_sf(data = peru_extra_depts, fill='#EEEEEE', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = bolivia_pando, fill=NA, color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = brazil_acre, fill=NA, color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = mdd_peru, fill='#DDDDDD', color='#a6a6a6', size=.5, show.legend = FALSE) +
  geom_sf(data = peru_outline, fill=NA, color='black', size=.3, show.legend = FALSE) +
  geom_sf(data = highway_final, aes(geometry = geometry), color='red', linewidth=0.6, show.legend = "line") +
  #geom_sf(data = brazil_norte_roads_primary_estrada, aes(geometry = geometry), color='red', linewidth=0.8, show.legend = "line") +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='none') +
  coord_sf(ylim = c(-19, 0),
           xlim = c(-83,-65),
           clip = "on",
           expand = F)

sfig2a <- sfig2a +                                
  draw_plot_label(label = c("Peru", "Brazil", "Bolivia"), size = 14, color = "darkred",
                  x = c(0.33, 0.56, 0.74), y = c(0.6, 0.62, 0.32)) +                                
  draw_plot_label(label = c("Interoceanic\nHighway"), size = 8, color = "red",
                  x = c(0.63), y = c(0.49)) 
sfig2a

ggsave("SFig2a.pdf", plot=sfig2a, path="~/Desktop/doctorate/ch2 mdd highway/supplementary_figures/", width = 9.25, height = 8.53, units="in", device = "pdf")
ggsave("SFig2a.png", plot=sfig2a, path="~/Desktop/", width = 9.25, height = 8.53, units="in", device = "png")

############################
### 3MT version ############
############################

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

sfig_3mt_small_map <- ggplot() +
  geom_sf(data = bolivia_union, fill=NA, color='black', size=.15, show.legend = FALSE) +
  geom_sf(data = mdd_peru, fill='#EEEEEE', color='slategrey', size=.5, show.legend = FALSE) +
  geom_sf(data = peru_outline, fill=NA, color='black', size=.3, show.legend = FALSE) +
  geom_sf(data = highway_final, aes(geometry = geometry), color='red', linewidth=0.5, show.legend = "line") +
  geom_sf(data = brazil_norte_roads_primary_estrada, aes(geometry = geometry), color='red', linewidth=0.5, show.legend = "line") +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='none') +
  coord_sf(ylim = c(-15, -8),
           xlim = c(-74,-66),
           clip = "on",
           expand = F)

sfig_3mt_small_map
ggsave("sfig_3mt_small_map.pdf", plot=sfig_3mt_small_map, path="~/Desktop/", width = 5, height = 5, units="in", device = "pdf")
ggsave("sfig_3mt_small_map.png", plot=sfig_3mt_small_map, path="~/Desktop/", width = 5, height = 5, units="in", device = "png")

library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
south_america <- world %>% 
  filter(continent == "South America") %>% 
  st_union()
south_america <- st_as_sf(south_america) 
south_america$geometry <- st_transform(south_america$geometry, 4326)

sfig_3mt_big_map <- ggplot() +
  geom_sf(data = south_america, fill=NA, color='#5d5a59', size=.15, show.legend = FALSE) +
  geom_sf(data = bolivia_union, fill=NA, color='black', size=.3, show.legend = FALSE) +
  geom_sf(data = brazil_country, fill='white', color='black', size=.3, show.legend = FALSE) +
  geom_sf(data = peru_outline, fill='white', color='black', size=.3, show.legend = FALSE) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='none')

sfig_3mt_big_map
ggsave("sfig_3mt_big_map.pdf", plot=sfig_3mt_big_map, path="~/Desktop/", width = 5, height = 5, units="in", device = "pdf")

sfig_3mt_cases_plot <- ggplot(dengue_df_yearly_summary) +
  geom_line(aes(x=year, y=incidence)) +
  geom_vline(xintercept=2008,linetype='dashed', color="red") +
  xlab("") + ylab("") + 
  theme_classic()+
  xlim(2000,2022)+
  theme(plot.title = element_text(size=12, face="bold"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=14),
        axis.title.y=element_text(size=10, vjust=.5, angle=0, hjust=0.5),
        axis.text.y=element_text(size=18),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=18),
        axis.text = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "none",
        strip.text.x = element_text(size = 10))

sfig_3mt_cases_plot
ggsave("sfig_3mt_cases_plot.pdf", plot=sfig_3mt_cases_plot, path="~/Desktop/", width = 8, height = 4, units="in", device = "pdf")

