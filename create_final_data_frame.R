## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)
library(fixest)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

## load all mdd case data
case_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_case_data.csv")
head(case_data)

#################################
#############Dengue##############
## dengue data only
dengue_data <- case_data[which(case_data$DIAGNOSTIC=="A97.0"),]
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
dengue_data <- dengue_data[which(dengue_data$UBIGEO %in% mdd_districts),]
dengue_data$FECHA_INI <- as.Date(dengue_data$FECHA_INI)

## monthly case data
monthly_dengue_data <- dengue_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(sum = n())

## merge e_salud codes and cluster ids
cluster_ids <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/cluster_cenetroids_7500.csv")
e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_E_Salud_Coordinates_Key.csv")
id_cluster_key_7500 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/idClusterKey7500.csv")

linked_ids_codes <- left_join(e_salud_codes, id_cluster_key_7500, by = 'key')
#linked_ids_codes <- left_join(cluster_ids, id_cluster_key_7500, by = 'clust')

write.csv(linked_ids_codes,"~/Desktop/doctorate/ch2 mdd highway/data/linking_clusterid_esaludkey.csv")
dengue_data_linked <- left_join(linked_ids_codes, monthly_dengue_data, by = 'e_salud')
dengue_data_linked <- dengue_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(sum),
            name = first(name))

## add zeroes
full_months <- data.frame(seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"))
colnames(full_months) <- 'month'
dengue_data_linked$cluster <- as.vector(dengue_data_linked$cluster)
dengue_data_linked <- dengue_data_linked[,c(1:3)]
dengue_data_complete_time_steps <- dengue_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

## link to various road buffers
onekm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_1km_boolean.csv")
onekm_tf <- onekm_tf[,c(2,4)]
onekm_tf$isInsideBuffer[onekm_tf$isInsideBuffer == 'true'] <- 1
onekm_tf$isInsideBuffer[onekm_tf$isInsideBuffer == 'false'] <- 0
colnames(onekm_tf) <- c('cluster', 'onekm')
fivekm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_5km_boolean.csv")
fivekm_tf <- fivekm_tf[,c(2,4)]
fivekm_tf$isInsideBuffer[fivekm_tf$isInsideBuffer == 'true'] <- 1
fivekm_tf$isInsideBuffer[fivekm_tf$isInsideBuffer == 'false'] <- 0
colnames(fivekm_tf) <- c('cluster', 'fivekm')
tenkm_tf <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_IOH_buffered_10km_boolean.csv")
tenkm_tf <- tenkm_tf[,c(2,4)]
tenkm_tf$isInsideBuffer[tenkm_tf$isInsideBuffer == 'true'] <- 1
tenkm_tf$isInsideBuffer[tenkm_tf$isInsideBuffer == 'false'] <- 0
colnames(tenkm_tf) <- c('cluster', 'tenkm')

dengue_data_complete_time_steps$cluster <- as.numeric(dengue_data_complete_time_steps$cluster)
dengue_data_buffers <- full_join(dengue_data_complete_time_steps,onekm_tf, by='cluster')
dengue_data_buffers <- full_join(dengue_data_buffers,fivekm_tf, by='cluster')
dengue_data_buffers <- full_join(dengue_data_buffers,tenkm_tf, by='cluster')

# just to build spatial inclusion/exclusion maps
linked_ids_codes$cluster <- linked_ids_codes$clust
linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes,onekm_tf, by='cluster')
linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,fivekm_tf, by='cluster')
linked_ids_codes_with_cutoffs <- full_join(linked_ids_codes_with_cutoffs,tenkm_tf, by='cluster')
write.csv(linked_ids_codes_with_cutoffs, "~/Desktop/doctorate/ch2 mdd highway/data/mapping_cutoffs.csv")

#remove 2021 & 2022
dengue_data_buffers_21 <- dengue_data_buffers[!(dengue_data_buffers$month %in% seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by="months")),]

### add population data
population_mdd <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_population_yearly.csv")
population_mdd$cluster <- population_mdd$layer
population_mdd <- population_mdd[,c(2:22,25)]
colnames(population_mdd) <- c(as.character(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="years")), 'cluster')
population_mdd_long <- population_mdd %>%
  pivot_longer(cols = c(1:21), 
               names_to = "year", 
               values_to = "population")
population_mdd_long$year <- as.Date(population_mdd_long$year)

## load cleaned diresa pop data
cleaned_diresa_pop <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/cleaned_diresa_pop.csv")
cleaned_diresa_pop <- cleaned_diresa_pop[,c(4:18)]
colnames(cleaned_diresa_pop)[c(7:15)] <- c(as.character(seq(as.Date("2009-01-01"), as.Date("2017-01-01"), by="years")))

## impute population from 2000 to 2020 based on 2009 to 2017 values
# group into clusters and match format
cleaned_diresa_pop <- cleaned_diresa_pop[,c(6:15)]
cleaned_diresa_pop <- cleaned_diresa_pop %>%
  pivot_longer(cols = c(2:10), 
               names_to = "year", 
               values_to = "population") %>%
  group_by(clust,year) %>%
  summarize(population=sum(population, na.rm=T),
            year=max(year))
colnames(cleaned_diresa_pop) <- c('cluster','year','population')
cleaned_diresa_pop$year <- as.Date(cleaned_diresa_pop$year)
#left join
all_pop <- left_join(population_mdd_long, cleaned_diresa_pop, by=c('cluster','year'))
colnames(all_pop) <- c('cluster', 'year', 'worldpop', 'diresapop')
#get average ratio between the two *for EACH CLUSTER
all_pop$ratio <- all_pop$worldpop/all_pop$diresapop
all_pop <- all_pop %>%
  group_by(cluster) %>%
  mutate(average_ratio = mean(ratio, na.rm=T))
#fill in the blanks, keep the "known" values
all_pop$worldpop_ratio <- all_pop$worldpop/all_pop$average_ratio
adjusted_diresa_pop <- all_pop[,c(1,2,7)]
colnames(adjusted_diresa_pop) <- c('cluster', 'year', 'population')
  
#build year column for linking to yearly population data
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$month, format="%Y-%m-%d"),"%Y")
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$year, format="%Y"),"%Y-01-01")
dengue_data_buffers_21$year <- as.Date(dengue_data_buffers_21$year)

#link population data and create incidence
incidence_data <- full_join(dengue_data_buffers_21, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))
incidence_data$incidence <- incidence_data$monthly_cases/incidence_data$population
incidence_data$incidence[is.na(incidence_data$incidence)] <- 0
min(incidence_data$incidence)
max(incidence_data$incidence)
table(which(incidence_data$incidence=="Inf"))
incidence_data$incidence[which(incidence_data$incidence=="Inf")] <- 0
write.csv(incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data_pop_adjusted.csv")
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data_pop_adjusted.csv")

#quarterly and yearly incidence
incidence_data_quarterly <- incidence_data %>% 
  mutate(quarter = lubridate::quarter(month, type = "date_last"))
incidence_data_quarterly <- incidence_data_quarterly  %>%
  group_by(quarter,cluster) %>%
  summarize(quarterly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(incidence_data_quarterly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_incidence_data_pop_adjusted.csv")

incidence_data_yearly <- incidence_data  %>%
  group_by(year,cluster) %>%
  summarize(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population))  %>%
  mutate(incidence = yearly_cases/population)
table(which(incidence_data_yearly$incidence=="Inf"))
incidence_data_yearly$incidence[which(incidence_data_yearly$incidence=="Inf")] <- 0
incidence_data_yearly$incidence[is.na(incidence_data_yearly$incidence)] <- 0
write.csv(incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data_pop_adjusted.csv")

# load stored data
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data.csv")
incidence_data_quarterly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/quarterly_incidence_data.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data.csv")


### yearly model
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")
incidence_data_yearly$incidence <- incidence_data_yearly$yearly_cases/incidence_data_yearly$population
incidence_data_yearly$year <- as.factor(incidence_data_yearly$year)
incidence_data_yearly$onekm <- as.numeric(incidence_data_yearly$onekm)
incidence_data_yearly_no_pm <- incidence_data_yearly[!(incidence_data_yearly$cluster %in% 1),]

test <- feols(incidence ~ i(year, fivekm, ref = '2008-01-01') | cluster + year, vcov = "twoway", data = incidence_data_yearly)
#test <- feols(incidence ~ i(year, tenkm, ref = '2008-01-01') | cluster + year, vcov = "cluster", data = incidence_data_yearly)
#test <- feols(incidence ~ i(year, tenkm, ref = '2008-01-01') | cluster + year, data = incidence_data_yearly)
summary(test)
#class(test)

df <- as.data.frame(test$coeftable)
colnames(df) <- c('estimate', 'std_error', 't_value', 'p_value')
df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by="year"),
             seq(as.Date("2009-01-01"), as.Date("2020-01-01"), by="year"))
df$upper <- df$estimate+1.96*df$std_error
df$lower <- df$estimate-1.96*df$std_error
ggplot(df) +
  geom_hline(aes(yintercept=0), colour='red', size=.4) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', size=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("Yearly\nincidence") + 
  theme_bw()+
  ylim(c(-0.02,0.20))+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=18),
        axis.title.y=element_text(size=14,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.text.x=element_text(size=12),
        axis.text = element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "right",
        strip.text.x = element_text(size = 12))

# average yearly incidence pre treatment
incidence_data_yearly_popadj <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data_pop_adjusted.csv")
incidence_data_yearly_popadj_avg <- incidence_data_yearly_popadj %>%
  group_by(year) %>%
  summarize(mean = mean(incidence, na.rm=T))
print(incidence_data_yearly_popadj_avg, n=22)
#average pretreatment
mean(incidence_data_yearly_popadj_avg$mean[2:9])
mean(incidence_data_yearly_popadj_avg$mean[8:9])

incidence_data_yearly_avg <- cbind(incidence_data_yearly_avg,incidence_data_yearly_popadj_avg)
print(incidence_data_yearly_avg)

incidence_data_yearly_avg <- incidence_data_yearly_avg[1:21, c(1,2,4)]
colnames(incidence_data_yearly_avg) <- c('year', 'worldpop', 'pop_adj')
incidence_data_yearly_avg$worldpop <- round(incidence_data_yearly_avg$worldpop*100,3)
incidence_data_yearly_avg$pop_adj <- round(incidence_data_yearly_avg$pop_adj*100,3)

## data coverage calculation
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")
incidence_data_yearly$case_yn <- ifelse(incidence_data_yearly$yearly_cases>0, 1, 0)
dengue_coverage_yearly <- incidence_data_yearly %>%
  group_by(year, tenkm) %>%
  summarize(coverage = sum(case_yn))

#plot coverage
dengue_coverage_yearly$tenkm <- as.character(dengue_coverage_yearly$tenkm)
dengue_coverage_yearly <- dengue_coverage_yearly[0:42,]
dengue_coverage_yearly$year <- format(as.Date(dengue_coverage_yearly$year, format="%Y-%m-%d"),"%Y")
dengue_yearly_coverage_plot <- ggplot(dengue_coverage_yearly) +
  geom_col(aes(x= year, y=coverage, fill=tenkm), width=0.5, position = position_dodge(width = 0.5)) +
  scale_fill_manual(name = "Treatment", values=c('#3ecbdd', '#FFBC42'), labels=c('Far (>10km)', 'Near (<10km)'),) +
  geom_vline(aes(xintercept=("2008")), linetype='dashed', size=0.4) +
  xlab("Year") + ylab("No. spatial units\nw dengue case\n(n=70)") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title=element_text(size=20),
        axis.title.y=element_text(size=16,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=12),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        strip.text.x = element_text(size = 14))


### quarterly model

incidence_data_quarterly$incidence <- incidence_data_quarterly$quarterly_cases/incidence_data_quarterly$population
incidence_data_quarterly$quarter <- as.factor(incidence_data_quarterly$quarter)
incidence_data_quarterly$tenkm <- as.factor(incidence_data_quarterly$tenkm)
incidence_data_quarterly$cluster <- as.factor(incidence_data_quarterly$cluster)
incidence_data_quarterly$year <- format(as.Date(incidence_data_quarterly$quarter, format="%Y-%m-%d"),"%Y")
incidence_data_quarterly$year <- format(as.Date(incidence_data_quarterly$year, format="%Y"),"%Y-01-01")
incidence_data_quarterly_small <- incidence_data_quarterly[(incidence_data_quarterly$year %in% as.factor(c(seq(as.Date("2006-01-01"), as.Date("2015-01-01"), by="year")))),]

test <- feols(incidence ~ i(quarter, tenkm, ref = '2009-06-30') | cluster + quarter,  vcov = "twoway", data = incidence_data_quarterly_small)
summary(test)
df <- as.data.frame(test$coeftable)
df$quarter <- seq(as.Date("2006-03-31"), as.Date("2015-12-01"), by="quarter")
ggplot(df) +
  geom_point(aes(quarter, Estimate))

################################
#############Leish##############
leish_data <- case_data[which(case_data$DIAGNOSTIC=="B55.1" | case_data$DIAGNOSTIC=="B55.2"),]
mdd_districts <- unique(dengue_data$UBIGEO)[1:11]
leish_data <- leish_data[which(leish_data$UBIGEO %in% mdd_districts),]
leish_data$FECHA_INI <- as.Date(leish_data$FECHA_INI)

## monthly case data
monthly_leish_data <- leish_data %>% 
  group_by(month = lubridate::floor_date(FECHA_INI, 'month'), e_salud = E_SALUD) %>%
  summarize(sum = n())

leish_data_linked <- left_join(linked_ids_codes, monthly_leish_data, by = 'e_salud')
leish_data_linked <- leish_data_linked %>%
  group_by(cluster = clust, month = month) %>%
  summarize(monthly_cases = sum(sum),
            name = first(name))

leish_data_linked$cluster <- as.vector(leish_data_linked$cluster)
leish_data_linked <- leish_data_linked[,c(1:3)]
leish_data_complete_time_steps <- leish_data_linked %>%
  complete(month = seq(as.Date("2000-01-01"), as.Date("2022-01-01"), by="months"), 
           fill = list(monthly_cases = 0)) %>%
  as.data.frame()

leish_data_complete_time_steps$cluster <- as.numeric(leish_data_complete_time_steps$cluster)
leish_data_buffers <- full_join(leish_data_complete_time_steps,onekm_tf, by='cluster')
leish_data_buffers <- full_join(leish_data_buffers,fivekm_tf, by='cluster')
leish_data_buffers <- full_join(leish_data_buffers,tenkm_tf, by='cluster')

leish_data_buffers_21 <- leish_data_buffers[!(leish_data_buffers$month %in% seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by="months")),]

leish_data_buffers_21$year <- format(as.Date(leish_data_buffers_21$month, format="%Y-%m-%d"),"%Y")
leish_data_buffers_21$year <- format(as.Date(leish_data_buffers_21$year, format="%Y"),"%Y-01-01")
leish_data_buffers_21$year <- as.Date(leish_data_buffers_21$year)

#link population data and create incidence
leish_incidence_data <- full_join(leish_data_buffers_21, adjusted_diresa_pop, by=c('cluster'='cluster', 'year'='year'))
leish_incidence_data$incidence <- leish_incidence_data$monthly_cases/leish_incidence_data$population
leish_incidence_data$incidence[is.na(leish_incidence_data$incidence)] <- 0
min(leish_incidence_data$incidence)
max(leish_incidence_data$incidence)
table(which(leish_incidence_data$incidence=="Inf"))
leish_incidence_data$incidence[which(leish_incidence_data$incidence=="Inf")] <- 0
write.csv(leish_incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_leish_incidence_data_pop_adjusted.csv")
leish_incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_leish_incidence_data.csv")

#quarterly and yearly incidence
leish_incidence_data_quarterly <- leish_incidence_data %>% 
  mutate(quarter = lubridate::quarter(month, type = "date_last"))
leish_incidence_data_quarterly <- leish_incidence_data_quarterly  %>%
  group_by(quarter,cluster) %>%
  summarize(quarterly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(leish_incidence_data_quarterly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data_pop_adjusted.csv")

incidence_data_yearly <- leish_incidence_data  %>%
  group_by(year,cluster) %>%
  summarize(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data_pop_adjusted.csv")

# load stored data
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_leish_data.csv")
incidence_data_quarterly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")

##yearly model
incidence_data_yearly <- incidence_data_yearly[complete.cases(incidence_data_yearly), ]
incidence_data_yearly$cluster <- as.factor(incidence_data_yearly$cluster)

##########################################
#############POTENTIALCONTROLS############
##########################################

#### Add precip, temp, and land-use data
precip_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_precipitation_monthly_mean.csv")
temp_monthly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_temperature_monthly_mean.csv")
urban_area <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_urban_area_mapbiomas.csv")

#precip
precip_monthly <- precip_monthly[,c(2:254)]
precip_monthly <- precip_monthly[,c(253,1:252)]
colnames(precip_monthly)[2:253] <- as.character(seq(as.Date("2000-01-01"), as.Date("2020-12-01"), by="months"))
colnames(precip_monthly)[1] <- "cluster"
precip_monthly_mdd_long <- precip_monthly %>%
  pivot_longer(cols = c(2:253), 
               names_to = "month", 
               values_to = "mean_precip")
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
precip_yearly_mdd_long <- precip_monthly_mdd_long %>%
  group_by(year,cluster) %>%
  summarize(mean_precip = mean(mean_precip))
precip_quarterly_mdd_long <- precip_monthly_mdd_long %>%
  mutate(quarter = lubridate::quarter(month, type = "date_last")) %>%
  group_by(quarter,cluster) %>%
  summarize(mean_precip = mean(mean_precip))

#temp
temp_monthly <- temp_monthly[,c(2:254)]
temp_monthly <- temp_monthly[,c(253,1:252)]
colnames(temp_monthly)[2:253] <- as.character(seq(as.Date("2000-01-01"), as.Date("2020-12-01"), by="months"))
colnames(temp_monthly)[1] <- "cluster"
temp_monthly_mdd_long <- temp_monthly %>%
  pivot_longer(cols = c(2:253), 
               names_to = "month", 
               values_to = "mean_temp")
temp_monthly_mdd_long$year <- format(as.Date(temp_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
temp_yearly_mdd_long <- temp_monthly_mdd_long %>%
  group_by(year,cluster) %>%
  summarize(mean_temp = mean(mean_temp))
temp_quarterly_mdd_long <- temp_monthly_mdd_long %>%
  mutate(quarter = lubridate::quarter(month, type = "date_last")) %>%
  group_by(quarter,cluster) %>%
  summarize(mean_temp = mean(mean_temp))

#urban_area
urban_area <- urban_area[,c(2:5)]
urban_area <- urban_area[which(urban_area$class==24),]
urban_area <- urban_area[,c(1,3,4)]
colnames(urban_area) <- c("urban_area", "cluster", "year")

#yearly all covariates
covariates <- left_join(precip_yearly_mdd_long,temp_yearly_mdd_long, by=c("cluster"="cluster", "year" = "year"))
urban_area$year <- as.character(urban_area$year)
covariates <- left_join(covariates,urban_area, by=c("cluster"="cluster", "year" = "year"))
covariates$urban_area[which(is.na(covariates$urban_area))] <- 0
