# ID ----------------------------------------------------------------------
## Aly Singleton

## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)
library(fixest)
library(cowplot)
library(geojsonsf)
library(cowplot)
library(sf)
library(geobr)
library(mapview)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

#########################################
## load pop and dengue case data ########
#########################################

## load mdd case data
dengue_df_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data_7.5km/dengue_yearly_full_dataset_cp.csv")
head(dengue_df_yearly)

dengue_df_yearly_summary <- dengue_df_yearly %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(yearly_cases),
            population = sum(population))
dengue_df_yearly_summary$incidence <- (dengue_df_yearly_summary$dengue_cases)/dengue_df_yearly_summary$population*1000
dengue_df_yearly_summary$region <- 'A. Madre de Dios, Peru'
dengue_df_yearly_summary$year <- format(as.Date(dengue_df_yearly_summary$year, format="%Y-%m-%d"),"%Y")
dengue_df_yearly_summary$year <- as.numeric(dengue_df_yearly_summary$year)
  
## load global dataset
dengue_global_dataset <- read_rds("~/Desktop/doctorate/ch2 mdd highway/data/sfig_s2_data/dengue_temp_full.rds")

## peru population data (worldpop)
peru_population <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/sfig_s2_data/peru_population_yearly.csv")
peru_population <- peru_population[,c(37:59)]
colnames(peru_population) <- c("department", "id", 2000:2020)
peru_population <- peru_population %>%
  pivot_longer(cols = c(3:23), 
               names_to = "year", 
               values_to = "population")

## bolivia population data (worldpop)
bolivia_population <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/sfig_s2_data/bolivia_population_yearly.csv")
bolivia_population <- bolivia_population[,c(6,9:29)]
colnames(bolivia_population) <- c("department", 2000:2020)
bolivia_population <- bolivia_population %>%
  pivot_longer(cols = c(2:22), 
               names_to = "year", 
               values_to = "population")
  
## peru national
#population
peru_population_national <- peru_population %>%
  group_by(year) %>%
  summarize(population = sum(population))
peru_population_national <- as.data.frame(peru_population_national)
peru_population_national$year <- as.numeric(peru_population_national$year)
peru_population_national[nrow(peru_population_national) + 1,] <- c(2021,peru_population_national[21,2]*peru_population_national[21,2]/peru_population_national[20,2])
peru_population_national[nrow(peru_population_national) + 1,] <- c(2022,peru_population_national[22,2]*peru_population_national[22,2]/peru_population_national[21,2])
peru_population_national[nrow(peru_population_national) + 1,] <- c(2023,peru_population_national[23,2]*peru_population_national[23,2]/peru_population_national[22,2])

#dengue cases
peru_case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/sfig_s2_data/PER1_weekly2010-2023.csv")
peru_case_data$dengue_cases <- ifelse(is.na(peru_case_data$Casos), 0, peru_case_data$Casos)
peru_case_data$year <- peru_case_data$Ano
peru_case_data_summary <- peru_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))
to_link <- data.frame(c(2000:2009),
                      c(5557, 23526, 8085, 3349, 9547, 5637, 4022, 6344, 12824, 13326))
colnames(to_link) <- c("year", "dengue_cases")
peru_case_data_summary <- rbind(to_link,peru_case_data_summary)
peru_case_data_summary <- left_join(peru_case_data_summary, peru_population_national, by="year")
peru_case_data_summary$incidence <- peru_case_data_summary$dengue_cases/peru_case_data_summary$population*1000
peru_case_data_summary$region <- 'D. Peru (national)'

## cusco peru 
#population
peru_population_cusco <- peru_population[which(peru_population$department=="CUSCO"),]
peru_population_cusco$year <- as.numeric(peru_population_cusco$year)
peru_population_cusco <- peru_population_cusco[,c(3:4)]
peru_population_cusco[nrow(peru_population_cusco) + 1,] <- c(2021,peru_population_cusco[21,2]*peru_population_cusco[21,2]/peru_population_cusco[20,2])
peru_population_cusco[nrow(peru_population_cusco) + 1,] <- c(2022,peru_population_cusco[22,2]*peru_population_cusco[22,2]/peru_population_cusco[21,2])
peru_population_cusco[nrow(peru_population_cusco) + 1,] <- c(2023,peru_population_cusco[23,2]*peru_population_cusco[23,2]/peru_population_cusco[22,2])

#dengue cases
cusco_peru_case_data <- peru_case_data[which(peru_case_data$Departamento=="CUSCO"),]
cusco_peru_case_data_summary <- cusco_peru_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))
to_link <- data.frame(c(2000:2009),
                      c(0, 0, 2, 0, 0, 2, 0, 0, 0, 0))
colnames(to_link) <- c("year", "dengue_cases")
cusco_peru_case_data_summary <- rbind(to_link,cusco_peru_case_data_summary)
cusco_peru_case_data_summary <- left_join(cusco_peru_case_data_summary, peru_population_cusco, by="year")
cusco_peru_case_data_summary$incidence <- (cusco_peru_case_data_summary$dengue_cases)/cusco_peru_case_data_summary$population*1000
cusco_peru_case_data_summary$region <- 'G. Cusco, Peru'

## loreto peru
#population
peru_population_loreto <- peru_population[which(peru_population$department=="LORETO"),]
peru_population_loreto$year <- as.numeric(peru_population_loreto$year)
peru_population_loreto <- peru_population_loreto[,c(3:4)]
peru_population_loreto[nrow(peru_population_loreto) + 1,] <- c(2021,peru_population_loreto[21,2]*peru_population_loreto[21,2]/peru_population_loreto[20,2])
peru_population_loreto[nrow(peru_population_loreto) + 1,] <- c(2022,peru_population_loreto[22,2]*peru_population_loreto[22,2]/peru_population_loreto[21,2])
peru_population_loreto[nrow(peru_population_loreto) + 1,] <- c(2023,peru_population_loreto[23,2]*peru_population_loreto[23,2]/peru_population_loreto[22,2])

#dengue cases
loreto_peru_case_data <- peru_case_data[which(peru_case_data$Departamento=="LORETO"),]
loreto_peru_case_data_summary <- loreto_peru_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))
to_link <- data.frame(c(2000:2009),
                      c(518, 510, 2499, 784, 2580, 1772, 1995, 1720, 7232, 3723))
colnames(to_link) <- c("year", "dengue_cases")
loreto_peru_case_data_summary <- rbind(to_link,loreto_peru_case_data_summary)
loreto_peru_case_data_summary <- left_join(loreto_peru_case_data_summary, peru_population_cusco, by="year")
loreto_peru_case_data_summary$incidence <- (loreto_peru_case_data_summary$dengue_cases)/loreto_peru_case_data_summary$population*1000
loreto_peru_case_data_summary$region <- 'H. Loreto, Peru'

## brazil national
brazil_case_data <- read.csv("~/Desktop/doctorate/hfi_threshold/annual_dengue_case_inci.csv")
brazil_case_data_summary <- brazil_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases),
            population = sum(population),
            incidence = mean(incidence/100000)*1000)  
brazil_case_data_summary$region <- 'E. Brazil (national)'

## acre brazil
brazil_case_data$state <- substr(brazil_case_data$CD_MUN, 1, 2)
acre_brazil_case_data <- brazil_case_data[which(brazil_case_data$state == 12),]
acre_brazil_case_data_summary <- acre_brazil_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases),
            population = sum(population),
            incidence = mean(incidence/100000)*1000)  
acre_brazil_case_data_summary$region <- 'B. Acre, Brazil'

## bolivia national
#population
bolivia_population_national <- bolivia_population %>%
  group_by(year) %>%
  summarize(population = sum(population))
bolivia_population_national <- as.data.frame(bolivia_population_national)
bolivia_population_national$year <- as.numeric(bolivia_population_national$year)
bolivia_population_national[nrow(bolivia_population_national) + 1,] <- c(2021,bolivia_population_national[21,2]*bolivia_population_national[21,2]/bolivia_population_national[20,2])
bolivia_population_national[nrow(bolivia_population_national) + 1,] <- c(2022,bolivia_population_national[22,2]*bolivia_population_national[22,2]/bolivia_population_national[21,2])
bolivia_population_national[nrow(bolivia_population_national) + 1,] <- c(2023,bolivia_population_national[23,2]*bolivia_population_national[23,2]/bolivia_population_national[22,2])

#dengue cases
bol_case_data <- dengue_global_dataset[which(dengue_global_dataset$country=="BOL"),]
bol_case_data$dengue_cases <- ifelse(is.na(bol_case_data$dengue_cases), 0, bol_case_data$dengue_cases)
bol_case_data_summary <- bol_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))
to_link <- data.frame(c(2005:2014),
                      c(4443, 2555, 7332, 7807, 84047, 6620, 44804, 26587, 18206, 22846))
to_link2 <- data.frame(c(2015:2023),
                      c(27099, 32386, 9923, 7597, 19987, 111347, 8947, 16544, 156774))
colnames(to_link) <- c("year", "dengue_cases")
colnames(to_link2) <- c("year", "dengue_cases")
bol_case_data_summary <- rbind(to_link,to_link2)
bol_case_data_summary <- left_join(bol_case_data_summary, bolivia_population_national, by="year")
bol_case_data_summary$incidence <- bol_case_data_summary$dengue_cases/bol_case_data_summary$population*1000
bol_case_data_summary$region <- 'F. Bolivia (national)'

## pando, bolivia
#population
bolivia_population_pando <- bolivia_population[which(bolivia_population$department=="Pando"),]
bolivia_population_pando$year <- as.numeric(bolivia_population_pando$year)
bolivia_population_pando <- bolivia_population_pando[,c(2:3)]
bolivia_population_pando[nrow(bolivia_population_pando) + 1,] <- c(2021,bolivia_population_pando[21,2]*bolivia_population_pando[21,2]/bolivia_population_pando[20,2])
bolivia_population_pando[nrow(bolivia_population_pando) + 1,] <- c(2022,bolivia_population_pando[22,2]*bolivia_population_pando[22,2]/bolivia_population_pando[21,2])
bolivia_population_pando[nrow(bolivia_population_pando) + 1,] <- c(2023,bolivia_population_pando[23,2]*bolivia_population_pando[23,2]/bolivia_population_pando[22,2])

#dengue cases
to_link <- data.frame(c(2014:2023),
                       c(91, 496, 1831, 335, 265, 3099, 2333, 2413, 1633, 3362))
colnames(to_link) <- c("year", "dengue_cases")
pando_case_data_summary <- to_link
pando_case_data_summary <- left_join(pando_case_data_summary, bolivia_population_pando, by="year")
pando_case_data_summary$incidence <- pando_case_data_summary$dengue_cases/pando_case_data_summary$population*1000
pando_case_data_summary$region <- 'C. Pando, Bolivia'

## link all together in long format to build facet plot
regional_groupings_case_data <- rbind(peru_case_data_summary, dengue_df_yearly_summary,
                                      cusco_peru_case_data_summary, loreto_peru_case_data_summary, 
                                      brazil_case_data_summary, acre_brazil_case_data_summary,
                                      bol_case_data_summary, pando_case_data_summary)

################################
## comparing region plot #######
################################

region_comp_fig <- ggplot(regional_groupings_case_data) +
  geom_line(aes(x=year, y=incidence)) +
  facet_wrap(~region, scales = "free_y") +
  geom_vline(xintercept=2008,linetype='dashed', color="red") +
  ggtitle("Dengue incidence\nper 1,000") +
  xlab("Year") + ylab("") + 
  theme_bw()+
  theme(plot.title = element_text(size=14, face="bold"),
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
region_comp_fig

###################################
#### load region shapefiles #######
###################################

# peru department shapefiles
peru <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/per_admbnda_adm1_ign_20200714.shp")
peru <- st_as_sf(peru) 
peru$geometry <- st_transform(peru$geometry, 4326)
peru_three_dept <- peru[which(peru$ADM1_ES %in% c("Loreto", "Cusco", "Madre de Dios")),]
peru_three_dept <- st_union(peru_three_dept$geometry)
peru_extra_depts <- peru[which(peru$ADM1_ES %in% c("Loreto", "Cusco")),]
peru_extra_depts <- peru_extra_depts[,c(3,14)]
mdd_peru <- peru[which(peru$ADM1_ES %in% c("Madre de Dios")),]
peru_outline <- st_union(peru$geometry)

# bolivia pando shapefile
bolivia <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/bol_adm_gb2014_shp/bol_admbnda_adm1_gov_2020514.shp")
bolivia <- st_as_sf(bolivia) 
bolivia$geometry <- st_transform(bolivia$geometry, 4326)
bolivia_union <- st_union(bolivia)
bolivia_pando <- bolivia[which(bolivia$ADM1_ES %in% c("Pando")),]
bolivia_pando <- bolivia_pando[,c(1,11)]

# brazil acre shapefile
brazil <- st_read("~/Desktop/doctorate/hfi_threshold/brazil_municipality_yearly_hfi_max.csv", geometry_column = ".geo")
brazil <- st_as_sf(data.frame(brazil, geometry=geojson_sf(brazil$.geo)))
brazil$geometry <- st_transform(brazil$geometry, 4326)
brazil_acre <- brazil[brazil$SIGLA_UF=="AC",]
brazil_acre <- st_union(brazil_acre$geometry)
brazil_amazon <- st_read("~/Desktop/doctorate/ch3 amazon network/data/Limites_Amazonia_Legal_2022_shp/Limites_Amazonia_Legal_2022.shp")
brazil_amazon$geometry <- st_transform(brazil_amazon$geometry, 4326)
brazil_country <- read_country(year=2020)
  
###################################
#### load road shapefiles #########
###################################

# peru road shapefile
highway <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/peru_roads_important.shp")
mapview(highway)
highway1 <- highway[which(highway$ref %in% c("PE-30C","PE-30A","PE-30","PE-34B","PE-34A","PE-36B")),] #
highway2 <- highway[which(highway$osm_id %in% c(29102555,604763302,1049793277,
                                                1049793211,604763304,399003045)),] #PE-1S
highway3 <- highway[which(highway$osm_id %in% c(186707203,205371265,225770349,453990517,225770354,586138716,
                                                586138717,586138795,778806921,778791891,586138794,435395271,
                                                435395270,435395269,435395268,435395050,446383361,379231958,
                                                1029446417,198924459,86493671,465684096,462675294,188513541,
                                                8153377,316216142,82315367,446383347,446383345,86462741,
                                                890800597,890800598,520851257,311201234,698761268,698761269,
                                                586121507,435385594,586121509,586121510,40853459,46157510,
                                                787969069,40853464,610030580,610030579,40853462,610030578,
                                                40853467,40853470,40853476,462685333,462685332,462685329,
                                                80886877,40853472,40853479,80886879,328357042,559578215,
                                                80886878,40847782,610030573,80887116,80887120,221658347,
                                                256635558,304374768,186768667,462329344,186768679,186768637,
                                                29094033,186768631,186768650,186768629,186768652,186768685,
                                                29094029,186768691,186768632,186768693,186768628,1035240297,
                                                29094028,186768672,271201675,271201678,438353973,186768643,
                                                509105200,221656756,188513541,462675300,465684097,86493671,
                                                198924459,82315412,4819058,79095451,4819049,1089136611,
                                                86493673,198924482,4818990,4819000,792635133,4819825,
                                                378144776,1089136613,378144772,585246607,378143413,378143412,
                                                378143416,378143417,378143422,378143419,378143420,378143414,
                                                378143418,
                                                233687098,1011499450,1042410885,
                                                666016966,666016965,1084728138,
                                                374385515,1042410884,1042410883,
                                                892651044,890822752,892094307,
                                                256314666,606503179,770065001,
                                                770064999,890852296,23604416,
                                                770065007,407627084,1042480554,
                                                272198167,130682641,590329499,
                                                590329492,590329493,236839140)),] #PE-3S
highway4 <- highway[which(highway$osm_id %in% c(610023802,1005111393,
                                                32784764,1005109113,171592037,
                                                685010870,171592041,299663644,
                                                172478085,299663645,226540660,
                                                226540659,337421844,171125424,
                                                666016965,679342797,374385517)),] #PE-36A
highway_final <- rbind(highway1,highway2,highway3,highway4)
mapview(highway_final)
highway_final <- st_as_sf(highway_final) 
highway_final$geometry <- st_transform(highway_final$geometry, 4326)

# brazil roads
#peru_roads <- st_covers(peru,americas_roads$geometry, sparse = FALSE)
brazil_norte_roads <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/norte-latest-free.shp/gis_osm_roads_free_1.shp")
#brazil_norte_roads_primary <- brazil_norte_roads[which(brazil_norte_roads$fclass == 'primary'),]
brazil_norte_roads_primary_estrada <- brazil_norte_roads[which(brazil_norte_roads$name == 'Estrada do Pacífico'),]
brazil_norte_roads_primary_bool <- st_covers(brazil_acre,brazil_norte_roads_primary_estrada$geometry, sparse = FALSE)
brazil_norte_roads_primary_estrada <- brazil_norte_roads_primary_estrada[brazil_norte_roads_primary_bool[1,],]

# bolivia roads
bolivia_roads <- read_sf("~/Desktop/doctorate/ch2 mdd highway/data/shapefiles/bolivia-latest-free.shp/gis_osm_roads_free_1.shp")

###################################
#### plot full map w insets #######
###################################

sfig2 <- ggdraw() + 
  draw_plot(ggplot() +
              geom_sf(data = peru_extra_depts, fill='#EEEEEE', color='#a6a6a6', size=.15, show.legend = FALSE) +
              geom_sf(data = bolivia_pando, fill='#EEEEEE', color='#a6a6a6', size=.15, show.legend = FALSE) +
              geom_sf(data = brazil_acre, fill='#EEEEEE', color='#a6a6a6', size=.15, show.legend = FALSE) +
              geom_sf(data = mdd_peru, fill='#EEEEEE', color='#a6a6a6', size=.5, show.legend = FALSE) +
              geom_sf(data = peru_outline, fill=NA, color='black', size=.3, show.legend = FALSE) +
              geom_sf(data = highway_final, aes(geometry = geometry), color='red', linewidth=0.6, show.legend = "line") +
              geom_sf(data = brazil_norte_roads_primary_estrada, aes(geometry = geometry), color='red', linewidth=0.6, show.legend = "line") +
              theme_minimal() +
              no_axis +
              theme(legend.text=element_text(size=12),
                    legend.title=element_text(size=14),
                    legend.position='none'),
            0, 0, 1, 1) +
  draw_plot(ggplot(regional_groupings_case_data %>% filter(region == "A. Madre de Dios, Peru")) +
              geom_line(aes(x=year, y=incidence)) +
              geom_vline(xintercept=2008,linetype='dashed', color="red") +
              ggtitle("Madre de Dios, Peru") +
              xlab("Year") + ylab("Dengue\nincidence\nper 1,000") + 
              theme_classic()+
              xlim(2000,2022)+
              theme(plot.title = element_text(size=12, face="bold"),
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=14),
                    axis.title.y=element_text(size=10,angle=90, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=8),
                    axis.title.x=element_text(size=10),
                    axis.text.x=element_text(size=8),
                    axis.text = element_text(size=10),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    legend.position = "none",
                    strip.text.x = element_text(size = 10)),
            0.1, 0.07, 0.22, 0.22) +
  draw_plot(ggplot(regional_groupings_case_data %>% filter(region == "G. Cusco, Peru")) +
              geom_line(aes(x=year, y=incidence)) +
              geom_vline(xintercept=2008,linetype='dashed', color="red") +
              ggtitle("Cusco, Peru") +
              xlab("Year") + ylab("Dengue\nincidence\nper 1,000") + 
              theme_classic()+
              xlim(2000,2022)+
              theme(plot.title = element_text(size=12, face="bold"),
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=14),
                    axis.title.y=element_text(size=10,angle=90, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=8),
                    axis.title.x=element_text(size=10),
                    axis.text.x=element_text(size=8),
                    axis.text = element_text(size=10),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    legend.position = "none",
                    strip.text.x = element_text(size = 10)),
            0.05, 0.4, 0.22, 0.22) +
  draw_plot(ggplot(regional_groupings_case_data %>% filter(region == "H. Loreto, Peru")) +
              geom_line(aes(x=year, y=incidence)) +
              geom_vline(xintercept=2008,linetype='dashed', color="red") +
              ggtitle("Loreto, Peru") +
              xlab("Year") + ylab("Dengue\nincidence\nper 1,000") + 
              theme_classic()+
              xlim(2000,2022)+
              theme(plot.title = element_text(size=12, face="bold"),
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=14),
                    axis.title.y=element_text(size=10,angle=90, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=8),
                    axis.title.x=element_text(size=10),
                    axis.text.x=element_text(size=8),
                    axis.text = element_text(size=10),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    legend.position = "none",
                    strip.text.x = element_text(size = 10)),
            0.03, 0.75, 0.22, 0.22) +
  draw_plot(ggplot(regional_groupings_case_data %>% filter(region == "B. Acre, Brazil")) +
              geom_line(aes(x=year, y=incidence)) +
              geom_vline(xintercept=2008,linetype='dashed', color="red") +
              ggtitle("Acre, Brazil") +
              xlab("Year") + ylab("Dengue\nincidence\nper 1,000") + 
              theme_classic()+
              xlim(2000,2022)+
              theme(plot.title = element_text(size=12, face="bold"),
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=14),
                    axis.title.y=element_text(size=10,angle=90, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=8),
                    axis.title.x=element_text(size=10),
                    axis.text.x=element_text(size=8),
                    axis.text = element_text(size=10),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    legend.position = "none",
                    strip.text.x = element_text(size = 10)),
            0.69, 0.55, 0.22, 0.22) +
  draw_plot(ggplot(regional_groupings_case_data %>% filter(region == "C. Pando, Bolivia")) +
              geom_line(aes(x=year, y=incidence)) +
              geom_vline(xintercept=2008,linetype='dashed', color="red") +
              ggtitle("Pando, Bolivia") +
              xlab("Year") + ylab("Dengue\nincidence\nper 1,000") + 
              theme_classic()+
              xlim(2000,2022)+
              theme(plot.title = element_text(size=12, face="bold"),
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=14),
                    axis.title.y=element_text(size=10,angle=90, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=8),
                    axis.title.x=element_text(size=10),
                    axis.text.x=element_text(size=8),
                    axis.text = element_text(size=10),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    legend.position = "none",
                    strip.text.x = element_text(size = 10)),
            0.7, 0.1, 0.22, 0.22) +
  draw_line(x = c(0.68,0.76), y = c(0.4,0.3)) + #pando,bolivia 
  draw_line(x = c(0.68,0.6), y = c(0.66,0.52)) + #acre,brazil 
  draw_line(x = c(0.2,0.45), y = c(0.95,0.75)) + #loreto,peru 
  draw_line(x = c(0.31,0.585), y = c(0.27,0.35)) + #mdd,peru 
  draw_line(x = c(0.27,0.51), y = c(0.5,0.37)) #cusco,peru 

sfig2 <- sfig2 +                                
  draw_plot_label(label = c("Peru", "Brazil", "Bolivia"), size = 14, color = "darkred",
                  x = c(0.35, 0.53, 0.7), y = c(0.6, 0.62, 0.4)) +                                
  draw_plot_label(label = c("Interoceanic\nHighway"), size = 8, color = "red",
                  x = c(0.577), y = c(0.5)) 
sfig2

ggsave("SFig2.pdf", plot=sfig2, path="~/Desktop/doctorate/ch2 mdd highway/supplementary_figures/", width = 11.29, height = 7.29, units="in", device = "pdf")


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

