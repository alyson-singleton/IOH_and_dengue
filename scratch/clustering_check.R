case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/Datos vigilancia MDD 2000-2022_SubsetStanford.csv")


case_data_la_pampa <- case_data[which(case_data$LOCALIDAD == "LA PAMPA"),]
table(case_data_la_pampa$E_SALUD)


tb_eess <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/random_resources/TB_EESS.csv")
tb_eess_mdd <- tb_eess[which(tb_eess$diresa == "MADRE DE DIOS"),]
table(tb_eess_mdd$categoria)
tb_eess_mdd <- tb_eess_mdd[tb_eess_mdd$categoria %in% c("I-1", "I-2", "I-3", "II-1", "III-2"),]

tb_eess_mdd <- tb_eess_mdd[complete.cases(tb_eess_mdd),]

tb_eess_mdd <- sf::st_as_sf(
  tb_eess_mdd,
  coords = c("longitud","latitud"),
  crs = 4326 # lat/long coordinate reference system
)

center_lat_long <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid.csv")
center_lat_long <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid_7.5km.csv")
center_lat_long <- data.frame(center_lat_long)
center_lat_long <- center_lat_long %>%
  mutate(latitude= as.numeric(latitude),longitude= as.numeric(longitude)) %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

# just take a look at it using mapview
mapview::mapview(
  tb_eess_mdd, col.regions="red"
) +
  mapview::mapview(
    center_lat_long, col.regions="blue"
  )

##############################################################################################################################
# old clustering figure 

#####################
## Load alternate clustering data and process
#####################
### 6km // 78 units
dengue_df_yearly_6km <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_6km/dengue_yearly_full_dataset_c.csv")
dengue_df_yearly_6km$year <- as.factor(dengue_df_yearly_6km$year)

# cases + 1 so 0's dont go to infinity when logged
dengue_df_yearly_6km$incidence <- (dengue_df_yearly_6km$yearly_cases+1)/dengue_df_yearly_6km$population 

# land use vars + 0.001 so 0's dont go to infinity when logged
dengue_df_yearly_6km$urban <- dengue_df_yearly_6km$urban + 0.001
dengue_df_yearly_6km$ag <- dengue_df_yearly_6km$ag + 0.001

# double check complete cases
dengue_df_yearly_6km <- dengue_df_yearly_6km[complete.cases(dengue_df_yearly_6km),]

# remove extreme outliers (all most likely due to inaccurate pop data)
dengue_df_yearly_6km <- dengue_df_yearly_6km[which(dengue_df_yearly_6km$incidence < 0.5),]

# remove PM
dengue_df_yearly_6km_no_pm <- dengue_df_yearly_6km[!(dengue_df_yearly_6km$cluster %in% 1),]

# build final, buffered dataset where below 5km is 1, between 5km and 10km are thrown out (buffer zone), and above 10km is 0
dengue_df_yearly_6km_buffered <- dengue_df_yearly_6km[which(dengue_df_yearly_6km$all_cutoffs %in% c(1,2,4,5,6,7,0)),]
dengue_df_yearly_6km_buffered_no_pm <- dengue_df_yearly_6km_buffered[!(dengue_df_yearly_6km_buffered$cluster %in% 1),]

### 5km // 81 units
dengue_df_yearly_5km <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_5km/dengue_yearly_full_dataset_c.csv")
dengue_df_yearly_5km$year <- as.factor(dengue_df_yearly_5km$year)

# cases + 1 so 0's dont go to infinity when logged
dengue_df_yearly_5km$incidence <- (dengue_df_yearly_5km$yearly_cases+1)/dengue_df_yearly_5km$population 

# land use vars + 0.001 so 0's dont go to infinity when logged
dengue_df_yearly_5km$urban <- dengue_df_yearly_5km$urban + 0.001
dengue_df_yearly_5km$ag <- dengue_df_yearly_5km$ag + 0.001

# double check complete cases
dengue_df_yearly_5km <- dengue_df_yearly_5km[complete.cases(dengue_df_yearly_5km),]

# remove extreme outliers (all most likely due to inaccurate pop data)
dengue_df_yearly_5km <- dengue_df_yearly_5km[which(dengue_df_yearly_5km$incidence < 0.5),]

# remove PM
dengue_df_yearly_5km_no_pm <- dengue_df_yearly_5km[!(dengue_df_yearly_5km$cluster %in% 1),]

# build final, buffered dataset where below 5km is 1, between 5km and 10km are thrown out (buffer zone), and above 10km is 0
dengue_df_yearly_5km_buffered <- dengue_df_yearly_5km[which(dengue_df_yearly_5km$all_cutoffs %in% c(1,2,4,5,6,7,0)),]
dengue_df_yearly_5km_buffered_no_pm <- dengue_df_yearly_5km_buffered[!(dengue_df_yearly_5km_buffered$cluster %in% 1),]

#####################
## SFig 8a
#####################

## main specification
dengue_yearly_model_main <- fepois(incidence ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, vcov = "cluster", data = dengue_df_yearly_buffered)

dengue_yearly_model_6km <- fepois(incidence ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, vcov = "cluster", data = dengue_df_yearly_6km_buffered)

dengue_yearly_model_5km <- fepois(incidence ~ i(year, fivekm, ref = '2008-01-01') + log(urban) + log(ag) + sum_precip + mean_temp | cluster + year, vcov = "cluster", data = dengue_df_yearly_5km_buffered)

df <- rbind(
  as.data.frame(cbind(dengue_yearly_model_5km$coeftable[1:22,],c(rep("5km clustering",22)))),
  as.data.frame(cbind(dengue_yearly_model_6km$coeftable[1:22,],c(rep("6km clustering",22)))),
  as.data.frame(cbind(dengue_yearly_model_main$coeftable[1:22,],c(rep("7.5km clustering",22)))))

colnames(df) <- c('estimate', 'std_error', 't_value', 'p_value', 'Model')
df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by="year"),
             seq(as.Date("2009-01-01"), as.Date("2022-01-01"), by="year"))
df$estimate <- as.numeric(df$estimate)
df$std_error <- as.numeric(df$std_error)
df$upper <- df$estimate+1.96*df$std_error
df$lower <- df$estimate-1.96*df$std_error
df$estimate <- exp(df$estimate)-1
df$upper <- exp(df$upper)-1
df$lower <- exp(df$lower)-1
df$Model <- factor(as.character(df$Model), levels=c('5km clustering','6km clustering','7.5km clustering'))

sfig8a <- ggplot(df, aes(group=Model)) +
  geom_hline(aes(yintercept=0), colour='red', linewidth=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower, color=Model), width=0.1, linewidth=0.5, alpha=0.8) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate, fill=Model), color='black', size=3, shape=21, alpha=0.7) +
  scale_color_manual(name = "Clustering specification", values=list('5km clustering'="#648FFF", '6km clustering'='#FFD218','7.5km clustering'='#DC267F'),
                     labels=list('5km clustering (n = 84)','6km clustering (n = 78)','7.5km clustering (n = 71)'))+
  scale_fill_manual(name = "Clustering specification", values=list('5km clustering'="#648FFF", '6km clustering'='#FFD218','7.5km clustering'='#DC267F'),
                    labels=list('5km clustering (n = 84)','6km clustering (n = 78)','7.5km clustering (n = 71)'))+
  xlab("Year") + ylab("Percent\nchange\ndengue\nincidence") + 
  theme_bw()+
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(size=20),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=18),
        axis.title.y=element_text(size=12,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text = element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "right",
        strip.text.x = element_text(size = 12))
sfig8a

#####################
## SFig 8b
#####################
cluster_lat_long_7.5km <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_7.5km/cluster_centroids_7.5km.csv")
cluster_lat_long_7.5km <- data.frame(cluster_lat_long_7.5km)
cluster_lat_long_7.5km <- cluster_lat_long_7.5km %>%
  mutate(latitude= as.numeric(latitude),longitude= as.numeric(longitude)) %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

districts <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/Madre_de_Dios.shp")
districts <- st_as_sf(districts) 
districts$geometry <- st_transform(districts$geometry, 4326)
mdd_region <- st_union(districts$geometry)

rivers <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/mdd_rivers2.shp")
rivers <- st_as_sf(rivers, crs=4326) 
rivers$geometry <- st_transform(rivers$geometry, "EPSG:4326")

roads <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/mdd_roads.shp")
roads <- st_as_sf(roads) 
roads$geometry <- st_transform(roads$geometry, 4326)
roads_mdd <- st_covers(mdd_region,roads$geometry, sparse = FALSE)
roads_mdd <- roads[roads_mdd[1,],]

highway <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/peru_roads_important.shp")
highway <- highway[which(highway$ref=="PE-30C"),]
highway <- st_as_sf(highway) 
highway$geometry <- st_transform(highway$geometry, 4326)

highway_mdd <- st_covers(mdd_region,highway$geometry, sparse = FALSE)
highway_mdd <- highway[highway_mdd[1,],]

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

sfig8b <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='lightblue'), linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='#a6a6a6'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='black'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = cluster_lat_long_7.5km, aes(geometry = geometry), fill='#DC267F', color='black', pch=21, size = 2, alpha=0.7) +
  scale_color_manual(name="", values=c('#a6a6a6','black','lightblue'),
                     labels=c('Unpaved Roads','Highway','Rivers')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='none') +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  annotation_scale(location = "bl",height = unit(0.08, "cm"))

sfig8b
#sfig8_legend <- get_legend(sfig8b)
#sfig8b <- sfig8b + theme(legend.position = "none")

#####################
## SFig 8c
#####################
cluster_lat_long_6km <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_6km/cluster_centroids_6km.csv")
cluster_lat_long_6km <- data.frame(cluster_lat_long_6km)
cluster_lat_long_6km <- cluster_lat_long_6km %>%
  mutate(latitude= as.numeric(latitude),longitude= as.numeric(longitude)) %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

sfig8c <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='lightblue'), linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='#a6a6a6'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='black'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = cluster_lat_long_6km, aes(geometry = geometry), fill='#FFD218', color='black', pch=21, size = 2, alpha=0.7) +
  scale_color_manual(name="", values=c('#a6a6a6','black','lightblue'),
                     labels=c('Unpaved Roads','Highway','Rivers')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='none') +
  guides(fill=guide_legend(override.aes=list(shape=21)))

sfig8c

#####################
## SFig 8d
#####################
cluster_lat_long_5km <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/cluster_centroids_5km.csv")
cluster_lat_long_5km <- data.frame(cluster_lat_long_5km)
cluster_lat_long_5km <- cluster_lat_long_5km %>%
  mutate(latitude= as.numeric(latitude),longitude= as.numeric(longitude)) %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

sfig8d <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#a6a6a6', size=.15, show.legend = FALSE) +
  geom_sf(data = rivers, aes(geometry = geometry, color='lightblue'), linewidth=0.2, show.legend = "line") +
  geom_sf(data = roads_mdd, aes(geometry = geometry, color='#a6a6a6'), linewidth=0.5, show.legend = "line") +
  geom_sf(data = highway_mdd, aes(geometry = geometry, color='black'), linewidth=0.7, show.legend = "line") +
  geom_sf(data = cluster_lat_long_5km, aes(geometry = geometry), fill='#648FFF', color='black', pch=21, size = 2, alpha=0.7) +
  scale_color_manual(name="", values=c('#a6a6a6','black','lightblue'),
                     labels=c('Unpaved Roads','Highway','Rivers')) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.position='none') +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  annotation_north_arrow(location = "br",style = north_arrow_orienteering(text_size = 6), 
                         height = unit(0.5, "cm"), width = unit(0.5, "cm"))

sfig8d

#####################
## SFig 8all
#####################
sfig8all <- grid.arrange(sfig8b, sfig8c, sfig8d, sfig8a,                     
                         ncol = 4, nrow = 2,
                         layout_matrix = rbind(c(1,2,3),c(1,2,3),c(1,2,3),c(1,2,3),c(5,5,5),c(5,5,5),c(5,5,5),c(5,5,5),c(5,5,5)))

sfig8all <- as_ggplot(sfig8all) +                                
  draw_plot_label(label = c("A", "B", "C", "D"), size = 17,
                  x = c(0.01, 0.33, 0.66, 0.01), y = c(0.95, 0.95, 0.95, 0.57)) 

sfig8all

ggsave("SFig8.pdf", plot=sfig8all, "~/Desktop/doctorate/ch2 mdd highway/supplementary_figures/", width = 9, height = 8, units = c("in"), device="pdf")
