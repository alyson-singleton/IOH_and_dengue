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
