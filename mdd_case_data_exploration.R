# load data

#read in required packages
require(readxl)
require(tidyverse)
library(tidyverse)
library(sf)
library(mapview)
library(ggplot2)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()


#set the working directory from which the files will be read from
setwd("~/Desktop/doctorate/ch2 mdd highway/data/diresa_case_data/")
# Load combined case data
dataset <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")


## Combine all yearly datasets  
#create a list of the files from your target directory
file_list <- list.files(path="~/Desktop/doctorate/ch2 mdd highway/data/diresa_case_data/")
#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()
#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- read_excel(file_list[i], range = cell_cols("A:AQ")) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$Class <- sapply(strsplit(gsub(".xlsx", "", file_list[i]), "_"), function(x){x[2]}) #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
  dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
}

## Dengue
dengue_data <- dataset[which(dataset$DIAGNOSTIC=="A97.0"),]
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]
dengue_data$FECHA_INI <- as.Date(dengue_data$FECHA_INI)
monthly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month')) %>%
  summarize(sum = n())
monthly_dengue_data$Disease <- c("Dengue")
monthly_dengue_data_district <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'),
           UBIGEO) %>%
  summarize(sum = n())
monthly_dengue_data_district$Disease <- c("Dengue")
yearly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'year')) %>%
  summarize(sum = n())
yearly_dengue_data$Disease <- c("Dengue")

## Leish
leish_data <- dataset[which(dataset$DIAGNOSTIC=="B55.1"),]
leish_data <- leish_data[which(leish_data$UBIGEO %in% mdd_districts),]
leish_data$FECHA_INI <- as.Date(leish_data$FECHA_INI)
monthly_leish_data <- leish_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'),
           ) %>%
  summarize(sum = n())
monthly_leish_data$Disease <- c("Leish")
yearly_leish_data <- leish_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'year')) %>%
  summarize(sum = n())
yearly_leish_data$Disease <- c("Leish (CL)")

## Double Leish
leish_double_data <- dataset[which(dataset$DIAGNOSTIC=="B55.1" | dataset$DIAGNOSTIC=="B55.2"),]
leish_double_data <- leish_double_data[which(leish_double_data$UBIGEO %in% mdd_districts),]
leish_double_data$FECHA_INI <- as.Date(leish_double_data$FECHA_INI)
monthly_leish_double_data <- leish_double_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'),
  ) %>%
  summarize(sum = n())
monthly_leish_double_data$Disease <- c("Leish (all)")
yearly_leish_double_data <- leish_double_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'year')) %>%
  summarize(sum = n())
yearly_leish_double_data$Disease <- c("Leish (all)")

## Lepto
lepto_data <- dataset[which(dataset$DIAGNOSTIC=="A27"),]
lepto_data <- lepto_data[which(lepto_data$UBIGEO %in% mdd_districts),]
write_csv(lepto_data, "~/Desktop/doctorate/mordecai_lab/ch2 mdd highway/data/mdd_lepto_data.csv")
lepto_data$FECHA_INI <- as.Date(lepto_data$FECHA_INI)
monthly_lepto_data <- lepto_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month')) %>%
  summarize(sum = n())
monthly_lepto_data$Disease <- c("Lepto")
monthly_lepto_data_district <- lepto_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'),
           UBIGEO) %>%
  summarize(sum = n())
monthly_lepto_data$Disease <- c("Lepto")

## Malaria
malaria_data <- dataset[which(dataset$DIAGNOSTIC=="B51"),]
malaria_data <- malaria_data[which(malaria_data$UBIGEO %in% mdd_districts),]
malaria_data$FECHA_INI <- as.Date(malaria_data$FECHA_INI)
monthly_malaria_data <- malaria_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'),
  ) %>%
  summarize(sum = n())
monthly_malaria_data$Disease <- c("Malaria")
yearly_malaria_data <- malaria_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'year')) %>%
  summarize(sum = n())
yearly_malaria_data$Disease <- c("Malaria")

## ____________________________________________________________________________________
## ____________________________________________________________________________________
## ____________________________________________________________________________________

#what is going on with zip codes / health department codes

ubigeo_ccpp <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/ubigeo_ccpp_mdd.csv")
ubigeo_ccpp_mdd <- ubigeo_ccpp[which(ubigeo_ccpp$departamento=="MADRE DE DIOS"),]
write_csv(ubigeo_ccpp_mdd, "~/Desktop/doctorate/mordecai_lab/ch2 mdd highway/data/ubigeo_ccpp_mdd.csv")
ubigeo_ccpp_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/ubigeo_ccpp_mdd.csv")
mapview(ubigeo_ccpp_mdd, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

dataset_codes <- dataset[]


e_salud_key <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/e_salud_key.csv")
e_salud_list <- unique(e_salud_key$E_SALUD)

e_salud_included <- dataset[which(dataset$E_SALUD %in% e_salud_list),]
test <- dataset[!(dataset$E_SALUD %in% e_salud_list),]

library(dplyr)
dataset_17_all <- dataset %>% 
  filter(substr(E_SALUD, 1, 2) == "17")

dataset_17_not_included <- dataset %>% 
  filter(substr(E_SALUD, 1, 2) == "17") %>%
  filter(!(E_SALUD %in% e_salud_list))


#new healthcare center location information
center_locations <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/TB_EESS.csv")
center_locations <- center_locations %>%
  filter(diresa == "MADRE DE DIOS")
## ____________________________________________________________________________________
## ____________________________________________________________________________________
## ____________________________________________________________________________________

monthly_case_data <- rbind(monthly_dengue_data,monthly_leish_data)
ggplot(monthly_case_data) +
  geom_line(aes(month,sum, color=Disease)) +
  #labs(color="Model Type") +
  #ggtitle("B. straminea") +
  xlab("Month") + ylab("Incident\ncase\ncounts") + 
  scale_color_manual(values=c('#3ecbdd', '#FFBC42')) + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=22),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))

#leish only
ggplot(monthly_leish_data) +
  geom_line(aes(month,sum), color = "#FFBC42") +
  #labs(color="Model Type") +
  #ggtitle("B. straminea") +
  xlab("Month") + ylab("Leish\nincident\ncase\ncounts") + 
  #scale_color_manual(values=c('#FFBC42')) + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=22),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))

h1 <- hcl.colors(10, palette = "sunset")
h1_black <- c("black",h1)

# monthly by district w tambopata (puerto maldonado)
ggplot(monthly_dengue_data_district) +
  geom_line(aes(month,sum, color=UBIGEO)) +
  labs(color="District") +
  #ggtitle("B. straminea") +
  xlab("Month") + ylab("Dengue\nincident\ncase\ncounts") + 
  scale_color_manual(values=h1_black) + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=22),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))

# monthly by district without tambopata (puerto maldonado)
monthly_dengue_data_district_wopm <- monthly_dengue_data_district[which(monthly_dengue_data_district$UBIGEO!="170101"),]
ggplot(monthly_dengue_data_district_wopm) +
  geom_line(aes(month,sum, color=UBIGEO)) +
  #labs(color="Model Type") +
  #ggtitle("B. straminea") +
  xlab("Month") + ylab("Dengue\nincident\ncase\ncounts") + 
  scale_color_manual(values=h1) + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=22),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))


# yearly by district without tambopata (puerto maldonado)
yearly_dengue_data_district <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'year'),
           UBIGEO) %>%
  summarize(sum = n())
yearly_dengue_data_district_wopm <- yearly_dengue_data_district[which(yearly_dengue_data_district$UBIGEO!="170101"),]
ggplot(yearly_dengue_data_district_wopm) +
  geom_line(aes(month,sum, color=UBIGEO)) +
  labs(color="District") +
  #ggtitle("B. straminea") +
  xlab("Month") + ylab("Dengue\nincident\ncase\ncounts") + 
  scale_color_manual(values=h1) + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=22),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))

## ____________________________________________________________________________________
## ____________________________________________________________________________________
## ____________________________________________________________________________________
# test raster plots
library(raster)
test <- raster("~/Downloads/population_data.tif")
plot(test)
colors2 <- c(RColorBrewer::brewer.pal(9, 'BuPu'))[2:9]
fig2a <- plot(test, axes = FALSE, box = FALSE, legend = TRUE,
              breaks = c(0,0.001,0.01,0.1,1,10,100), col = colors2)
fig2a

## ____________________________________________________________________________________
## ____________________________________________________________________________________
## health center locations figure
center_lat_long <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/e_salud_key_with_lat_long.csv")
center_lat_long <- data.frame(center_lat_long)
center_lat_long <- center_lat_long %>%
  mutate(latitude= as.numeric(latitude),longitude= as.numeric(longitude) ) %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))
center_lat_long <- st_transform(center_lat_long$geometry, 4326)
mapview(center_lat_long, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

districts <- read_sf("~/Desktop/doctorate/ch2 mdd highway/peru_shapefiles/Madre_de_Dios.shp")
districts <- st_as_sf(districts) 
districts$geometry <- st_transform(districts$geometry, 4326)

highway <- read_sf("~/Desktop/doctorate/ch2 mdd highway/peru_shapefiles/peru_roads_important.shp")
highway <- highway[which(highway$ref=="PE-30C"),]
highway <- st_as_sf(highway) 
highway$geometry <- st_transform(highway$geometry, 4326)

mdd_region <- st_union(districts$geometry)
highway_mdd <- st_covers(mdd_region,highway$geometry, sparse = FALSE)
highway_mdd <- highway[highway_mdd[1,],]

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

# Plot all Brazilian states
fig1a <- ggplot() +
  geom_sf(data=districts, fill='#ffffff', color='#626262', size=.15, show.legend = FALSE) +
  geom_sf(data = highway_mdd, aes(geometry = geometry), colour='#2D936C', linewidth=1.5) +
  geom_sf(data = center_lat_long, aes(geometry = geometry), colour='#25CED1', size = 2.5, alpha=0.8) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.position='bottom') +
  guides(fill=guide_legend(override.aes=list(shape=21)))
fig1a
legend <- get_legend(fig1a)

## ____________________________________________________________________________________
## ____________________________________________________________________________________
## Temporal idea check

yearly_case_data <- rbind(yearly_dengue_data,yearly_leish_data, yearly_leish_double_data, yearly_malaria_data)
ggplot(yearly_case_data) +
  geom_line(aes(month,sum, color=Disease)) +
  #labs(color="Model Type") +
  #ggtitle("B. straminea") +
  xlab("Year") + ylab("Incident\ncase\ncounts") + 
  scale_color_manual(values=c('#3ecbdd', '#FFBC42', 'forestgreen', 'darkred')) + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=22),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))

