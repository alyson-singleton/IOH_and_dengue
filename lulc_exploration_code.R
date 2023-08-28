# load libraries
library(ggplot2)
library(dplyr)
library(sf)
library(astsa)
setwd("~/Desktop/doctorate/ch2 mdd highway/code/")
# load data
time_series_data <- read.csv("~/Desktop/doctorate/mordecai_lab/ch2 mdd highway/data/mdd_areas_lulc_reduced_classes.csv")
time_series_data <- as.data.frame(time_series_data)
# time_series_data$Feature <-c(substring(time_series_data$system.index[1:363], first = 22,last = 22),
#                              substring(time_series_data$system.index[364:1359], first = 23,last = 23))
# table(time_series_data$row_code)
time_series_data$log_area <- log(time_series_data$area)
time_series_data$class <- factor(time_series_data$class)

# ________________________________________________________________________________________________
# ________________________________________________________________________________________________

#build plot for areas over time for entire department
class_colors <- c("#D5D5E5", "#129912", "#BDB76B", "#AA0000", "#FF99FF", "#0000FF")
time_series_data_summarize <- time_series_data %>%
  group_by(class,year) %>%
  summarise(area_sum = sum(area))
time_series_data_summarize$log_area <- log(time_series_data_summarize$area_sum)

ggplot(data = time_series_data_summarize,aes(x = year, y = log_area, group=class)) + 
  geom_line(aes(colour=class)) + 
  theme_bw() + 
  ggtitle("Total LULC Areas MdD (1985 - 2020)") + ylab("log(area) (log(m^2))") +
  labs(x="Year") +
  scale_colour_manual(values=colors,
                      labels=c("Unclassified", "Forest", "Agriculture",
                               "Urban", "Mining", "Water"),
                      name="Land Use /\nLand Cover\nClass") +   
  theme(plot.title = element_text(hjust=0.5, size=10),
        plot.subtitle = element_text(hjust=0.5, size=10),
        strip.text.x = element_text(size = 10),
        axis.title=element_text(size=10),
        axis.text = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "right")

# graph areas over time for each district
library(gridExtra)
ggplot(data = time_series_data,aes(x = year, y = log_area, group=class)) + 
  geom_line(aes(colour=class)) + 
  theme_bw() + 
  facet_grid(. ~ row_code, scales="free_x", space="free_x") +
  ggtitle("LULC Areas MdD (1985 - 2020) by District") + ylab("log(area) (log(m^2))") +
  labs(x="Year") +
  scale_colour_manual(values=colors,
                      labels=c("Unclassified", "Forest", "Agriculture",
                               "Urban", "Mining", "Water"),
                      name="Land Use /\nLand Cover\nClass") +   
  theme(plot.title = element_text(hjust=0.5, size=10),
        plot.subtitle = element_text(hjust=0.5, size=16),
        strip.text.x = element_text(size = 10),
        axis.title=element_text(size=10),
        axis.text = element_text(size=5),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "right")

# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
#First forecasting attempt

#Build very simple example for overall forest
overall_forest <- time_series_data_summarize %>%
  filter(class==3)
ggplot(overall_forest, aes(x=year,y=log_area)) +
  geom_line(color="#129912") +
  ggtitle("Total Forest Area MdD (1985 - 2020)")

acf(overall_forest$log_area)
pacf(overall_forest$log_area)
overall_forest$difference1 <- c(NA,diff(overall_forest$log_area, difference=1))
acf(overall_forest$difference1[2:length(overall_forest$difference1)]) #sharp cutoff at lag(1) --> MA(1) term
pacf(overall_forest$difference1[2:length(overall_forest$difference1)]) #decays --> no AR term

forest.ma1.model <- sarima(overall_forest$log_area, p=0, d=1, q=1)
forest.ma1.model.for <- sarima.for(overall_forest$log_area, n.ahead=5, p=0, d=1, q=1)

#Build very simple example for overall agriculture
overall_agriculture <- time_series_data_summarize %>%
  filter(class==14)
ggplot(overall_agriculture, aes(x=year,y=log_area)) +
  geom_line(color="#BDB76B") +
  ggtitle("Total Agricultural Area MdD (1985 - 2020)")

acf(overall_agriculture$log_area)
pacf(overall_agriculture$log_area)
overall_agriculture$difference1 <- c(NA,diff(overall_agriculture$log_area, difference=1))
acf(overall_agriculture$difference1[2:length(overall_agriculture$difference1)]) #decays --> no MA term
pacf(overall_agriculture$difference1[2:length(overall_agriculture$difference1)]) #sharp cutoff at lag(1) --> AR(1) term

ag.ar1.model <- sarima(overall_agriculture$log_area, p=1, d=1, q=0)
ag.ar1.model.for <- sarima.for(overall_agriculture$log_area, n.ahead=5, p=1, d=1, q=0)

#Build very simple example for overall urban
overall_urban <- time_series_data_summarize %>%
  filter(class==24)
ggplot(overall_urban, aes(x=year,y=log_area)) +
  geom_line(color="#AA0000") +
  ggtitle("Total Urban Area MdD (1985 - 2020)")

acf(overall_urban$log_area)
pacf(overall_urban$log_area)
overall_urban$difference1 <- c(NA,diff(overall_urban$log_area, difference=1))
acf(overall_urban$difference1[2:length(overall_urban$difference1)]) #sharp cutoff at lag(1) --> MA(1) term
pacf(overall_urban$difference1[2:length(overall_urban$difference1)]) #decays --> no AR term

urban.ma1.model <- sarima(overall_urban$log_area, p=0, d=1, q=1)
urban.ma1.model.for <- sarima.for(overall_urban$log_area, n.ahead=5, p=0, d=1, q=1)

#Build very simple example for overall mining
overall_mining <- time_series_data_summarize %>%
  filter(class==30)
ggplot(overall_mining, aes(x=year,y=log_area)) +
  geom_line(color="#FF99FF") +
  ggtitle("Total Mining Area MdD (1985 - 2020)")

acf(overall_mining$log_area)
pacf(overall_mining$log_area)
overall_mining$difference1 <- c(NA,diff(overall_mining$log_area, difference=1))
acf(overall_mining$difference1[2:length(overall_mining$difference1)]) #sharp cutoff at lag(2) --> MA(2) term
pacf(overall_mining$difference1[2:length(overall_mining$difference1)]) #decays --> no AR term

mining.ma2.model <- sarima(overall_mining$log_area, p=0, d=1, q=2)
mining.ma2.model.for <- sarima.for(overall_mining$log_area, n.ahead=5, p=0, d=1, q=2)

#Build very simple example for overall water
overall_water <- time_series_data_summarize %>%
  filter(class==33)
ggplot(overall_water, aes(x=year,y=log_area)) +
  geom_line(color="#0000FF") +
  ggtitle("Total Mining Area MdD (1985 - 2020)")

acf(overall_water$log_area)
pacf(overall_water$log_area)
overall_water$difference1 <- c(NA,diff(overall_water$log_area, difference=1))
acf(overall_water$difference1[2:length(overall_water$difference1)]) #sharp cutoff at lag(1) --> MA(1) term
pacf(overall_water$difference1[2:length(overall_water$difference1)]) #decays --> no AR term

water.ma1.model <- sarima(overall_water$log_area, p=0, d=1, q=1)
water.ma1.model.for <- sarima.for(overall_water$log_area, n.ahead=5, p=0, d=1, q=1)

#build nice plot of basic forecasts
#build ag forecast df
time_series_data_summarize$predictions <- NA
ag.endpoint <- time_series_data_summarize %>% filter(year==2020,class==14)
ag.endpoint <- ag.endpoint$log_area
forest.endpoint <- time_series_data_summarize %>% filter(year==2020,class==3)
forest.endpoint <- forest.endpoint$log_area
urban.endpoint <- time_series_data_summarize %>% filter(year==2020,class==24)
urban.endpoint <- urban.endpoint$log_area
min.endpoint <- time_series_data_summarize %>% filter(year==2020,class==30)
min.endpoint <- min.endpoint$log_area
water.endpoint <- time_series_data_summarize %>% filter(year==2020,class==33)
water.endpoint <- water.endpoint$log_area
unknown.endpoint <- time_series_data_summarize %>% filter(year==2020,class==0)
unknown.endpoint <- unknown.endpoint$log_area

predictions_df <- data.frame(rbind(cbind(c(rep(14,6)),c(2020:2025),c(rep(NA,6)),c(rep(NA,6)),
                                         c(ag.endpoint,ag.ar1.model.for$pred[1:5])),
                                   cbind(c(rep(3,6)),c(2020:2025),c(rep(NA,6)),c(rep(NA,6)),
                                         c(forest.endpoint,forest.ma1.model.for$pred[1:5])),
                                   cbind(c(rep(24,6)),c(2020:2025),c(rep(NA,6)),c(rep(NA,6)),
                                         c(urban.endpoint,urban.ma1.model.for$pred[1:5])),
                                   cbind(c(rep(30,6)),c(2020:2025),c(rep(NA,6)),c(rep(NA,6)),
                                         c(min.endpoint,mining.ma2.model.for$pred[1:5])),
                                   cbind(c(rep(33,6)),c(2020:2025),c(rep(NA,6)),c(rep(NA,6)),
                                         c(water.endpoint,water.ma1.model.for$pred[1:5])),
                                   cbind(c(rep(0,6)),c(2020:2025),c(rep(NA,6)),c(rep(NA,6)),
                                         c(rep(unknown.endpoint,6)))))
colnames(predictions_df) <- colnames(time_series_data_summarize)
predictions_df$class <- factor(predictions_df$class)
predictions_df$upper_int <- c(c(predictions_df$predictions[1:6] + c(0,2*ag.ar1.model.for$se)),
                              c(predictions_df$predictions[7:12] + c(0,2*forest.ma1.model.for$se)),
                              c(predictions_df$predictions[13:18] + c(0,2*urban.ma1.model.for$se)),
                              c(predictions_df$predictions[19:24] + c(0,2*mining.ma2.model.for$se)),
                              c(predictions_df$predictions[25:30] + c(0,2*water.ma1.model.for$se)),
                              c(rep(8.392722,6)))
predictions_df$lower_int <- c(c(predictions_df$predictions[1:6] - c(0,2*ag.ar1.model.for$se)),
                              c(predictions_df$predictions[7:12] - c(0,2*forest.ma1.model.for$se)),
                              c(predictions_df$predictions[13:18] - c(0,2*urban.ma1.model.for$se)),
                              c(predictions_df$predictions[19:24] - c(0,2*mining.ma2.model.for$se)),
                              c(predictions_df$predictions[25:30] - c(0,2*water.ma1.model.for$se)),
                              c(rep(8.392722,6)))
time_series_data_summarize$upper_int <- NA
time_series_data_summarize$lower_int <- NA
time_series_data_summarize <- rbind(time_series_data_summarize,
                                    predictions_df)

ggplot(data = time_series_data_summarize, aes(x = year, y = log_area, group=class)) + 
  geom_line(aes(colour=class)) + 
  geom_line(aes(x=year,y=predictions,colour=class),linetype="longdash") +
  geom_ribbon(aes(x=year,ymin = lower_int, ymax = upper_int, fill=class, colour=class), alpha = 0.2, linetype="dotted") +
  theme_bw() + 
  ggtitle("Forecasting Total LULC Areas MdD (1985 - 2020)") + ylab("log(area) (log(m^2))") +
  labs(x="Year") +
  scale_colour_manual(values=class_colors,
                      labels=c("Unclassified", "Forest", "Agriculture",
                               "Urban", "Mining", "Water"),
                      name="Land Use /\nLand Cover\nClass") +  
  scale_fill_manual(values=class_colors,guide="none") +
  theme(plot.title = element_text(hjust=0.5, size=16),
        plot.subtitle = element_text(hjust=0.5, size=10),
        strip.text.x = element_text(size = 10),
        axis.title=element_text(size=10),
        axis.text = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "none")

class.labs <- c("Unclassified", "Forest", "Agriculture",
                "Urban", "Mining", "Water")
names(class.labs) <- c(0,3,14,24,30,33)

ggplot(data = time_series_data_summarize, aes(x = year, y = log_area, group=class)) + 
  geom_line(aes(colour=class)) + 
  geom_line(aes(x=year,y=predictions,colour=class),linetype="longdash") +
  geom_ribbon(aes(x=year,ymin = lower_int, ymax = upper_int, fill=class, colour=class), alpha = 0.2, linetype="dotted") +
  theme_bw() + 
  facet_wrap(~ class, scales="free", labeller = labeller(class=class.labs)) +
  ggtitle("Forecasting Total LULC Areas MdD by District (1985 - 2020)") + ylab("log(area) (log(m^2))") +
  labs(x="Year") +
  scale_colour_manual(values=class_colors,
                      labels=c("Unclassified", "Forest", "Agriculture",
                               "Urban", "Mining", "Water"),
                      name="Land Use /\nLand Cover\nClass") +  
  scale_fill_manual(values=class_colors,guide="none") +
  theme(plot.title = element_text(hjust=0.5, size=16),
        plot.subtitle = element_text(hjust=0.5, size=10),
        strip.text.x = element_text(size = 10),
        axis.title=element_text(size=10),
        axis.text = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        legend.position = "right")

# ________________________________________________________________________________________________
# ________________________________________________________________________________________________
#Map of MdD Districts
library(RColorBrewer)
district_names <- c("1. Iberia", "2. Tambopata", "3. Laberinto", "4. Inambari",     
                    "5. Manu", "6. Madre de Dios", "7. Huepetuhe", "8. Las Piedras",
                    "9. Tahuamanu", "10. Fitzcarrald", "11. I?apari") 
colors <- c("#D5D5E5",c(RColorBrewer::brewer.pal(9, "Blues"))[1:9],"#70A5AE")
mdd_shapefiles <- st_read("~/Desktop/doctorate/mordecai_lab/mdd_lulc/peru_shapefiles/Madre_de_Dios.shp")
mdd_shapefiles$row_code <- factor(mdd_shapefiles$row_code)
ggplot(mdd_shapefiles) +
  geom_sf(aes(geometry=geometry,fill=row_code),color='black') +
  geom_sf_label(aes(label=row_code)) +
  #ggtitle("Madre de Dios Districts") + ylab("Latitude") + xlab("Longitude") +
  theme_minimal() +
  scale_fill_manual(values=colors,
                    labels=district_names,
                    name="District Names") +
  theme(plot.title = element_text(hjust=0.5, size=16),
        plot.subtitle = element_text(hjust=0.5, size=10),
        strip.text.x = element_text(size = 10),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "right",
        line = element_blank(),
        #text = element_blank(),
        title = element_blank())
  



  