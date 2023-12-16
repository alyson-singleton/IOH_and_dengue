## load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)

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
cluster_ids <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/cluster_cenetroids_7500.csv")
e_salud_codes <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/DIRESA_E_Salud_Coordinates_Key.csv")
id_cluster_key_7500 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/idClusterKey7500.csv")

linked_ids_codes <- left_join(e_salud_codes, id_cluster_key_7500, by = 'key')

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

#build year column for linking to yearly population data
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$month, format="%Y-%m-%d"),"%Y")
dengue_data_buffers_21$year <- format(as.Date(dengue_data_buffers_21$year, format="%Y"),"%Y-01-01")
dengue_data_buffers_21$year <- as.Date(dengue_data_buffers_21$year)

#link population data and create incidence
incidence_data <- full_join(dengue_data_buffers_21, population_mdd_long, by=c('cluster'='cluster', 'year'='year'))
incidence_data$incidence <- incidence_data$monthly_cases/incidence_data$population
incidence_data$incidence[is.na(incidence_data$incidence)] <- 0
min(incidence_data$incidence)
max(incidence_data$incidence)
write.csv(incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data.csv")
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data.csv")

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
write.csv(incidence_data_quarterly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_incidence_data.csv")

incidence_data_yearly <- incidence_data  %>%
  group_by(year,cluster) %>%
  summarize(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data.csv")

# load stored data
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data.csv")
incidence_data_quarterly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/quarterly_incidence_data.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data.csv")

##yearly model
## create yearly dummies
incidence_data_yearly$time_05 <- ifelse(incidence_data_yearly$year >= as.Date("2005-01-01"), 1, 0)
incidence_data_yearly$time_06 <- ifelse(incidence_data_yearly$year >= as.Date("2006-01-01"), 1, 0)
incidence_data_yearly$time_07 <- ifelse(incidence_data_yearly$year >= as.Date("2007-01-01"), 1, 0)
incidence_data_yearly$time_08 <- ifelse(incidence_data_yearly$year >= as.Date("2008-01-01"), 1, 0)
incidence_data_yearly$time_09 <- ifelse(incidence_data_yearly$year >= as.Date("2009-01-01"), 1, 0)
incidence_data_yearly$time_10 <- ifelse(incidence_data_yearly$year >= as.Date("2010-01-01"), 1, 0)
incidence_data_yearly$time_11 <- ifelse(incidence_data_yearly$year >= as.Date("2011-01-01"), 1, 0)
incidence_data_yearly$time_12 <- ifelse(incidence_data_yearly$year >= as.Date("2012-01-01"), 1, 0)
incidence_data_yearly$time_13 <- ifelse(incidence_data_yearly$year >= as.Date("2013-01-01"), 1, 0)
incidence_data_yearly$time_14 <- ifelse(incidence_data_yearly$year >= as.Date("2014-01-01"), 1, 0)
incidence_data_yearly$time_15 <- ifelse(incidence_data_yearly$year >= as.Date("2015-01-01"), 1, 0)
incidence_data_yearly$time_16 <- ifelse(incidence_data_yearly$year >= as.Date("2016-01-01"), 1, 0)
incidence_data_yearly$time_17 <- ifelse(incidence_data_yearly$year >= as.Date("2017-01-01"), 1, 0)
incidence_data_yearly$time_18 <- ifelse(incidence_data_yearly$year >= as.Date("2018-01-01"), 1, 0)
incidence_data_yearly$time_19 <- ifelse(incidence_data_yearly$year >= as.Date("2019-01-01"), 1, 0)

#baby model
incidence_data_yearly$incidence <- incidence_data_yearly$yearly_cases/incidence_data_yearly$population
baby <- lm(formula = incidence ~ time_10 + fivekm + time_10:fivekm, data = incidence_data_yearly)
summary(baby)

baby2 <- lm(formula = incidence ~ time_10*fivekm, data = incidence_data_yearly)
summary(baby2)

baby3 <- lm(formula = incidence ~ as.factor(cluster) + as.factor(year) + time_10 + fivekm + time_10:fivekm, data = incidence_data_yearly)
summary(baby3)

install.packages("plm")
library(plm)

incidence_data_yearly <- incidence_data_yearly[complete.cases(incidence_data_yearly), ]
incidence_data_yearly$cluster <- as.factor(incidence_data_yearly$cluster)
model <- plm(incidence ~ time_09 + fivekm + time_09:fivekm, 
                    data = incidence_data_yearly,
                    index = c("cluster", "year"), 
                    model = "within", 
                    effect = "twoways")
coeftest(model, vcovHC(model, type = 'HC0', cluster = 'group'))

### yearly model

incidence_data_yearly$year <- as.factor(incidence_data_yearly$year)
incidence_data_yearly_no_pm <- incidence_data_yearly[!(incidence_data_yearly$cluster %in% 1),]

test <- feols(incidence ~ i(year, tenkm, ref = '2008-01-01') | cluster + year, vcov = "cluster", data = incidence_data_yearly)
summary(test)
df <- as.data.frame(test$coeftable)

colnames(df) <- c('estimate', 'std_error', 't_value', 'p_value')
df$year <- c(seq(as.Date("2000-01-01"), as.Date("2007-01-01"), by="year"),
             seq(as.Date("2009-01-01"), as.Date("2020-01-01"), by="year"))
df$upper <- df$estimate+df$std_error
df$lower <- df$estimate-df$std_error
ggplot(df) +
  geom_hline(aes(yintercept=0), colour='red', size=.2) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', size=0.2) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("Yearly\nincidence") + 
  theme_bw()+
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

### quarterly model

incidence_data_quarterly$incidence <- incidence_data_quarterly$quarterly_cases/incidence_data_quarterly$population
incidence_data_quarterly$quarter <- as.factor(incidence_data_quarterly$quarter)
incidence_data_quarterly$tenkm <- as.factor(incidence_data_quarterly$tenkm)
incidence_data_quarterly$cluster <- as.factor(incidence_data_quarterly$cluster)
incidence_data_quarterly$year <- format(as.Date(incidence_data_quarterly$quarter, format="%Y-%m-%d"),"%Y")
incidence_data_quarterly$year <- format(as.Date(incidence_data_quarterly$year, format="%Y"),"%Y-01-01")
incidence_data_quarterly_small <- incidence_data_quarterly[(incidence_data_quarterly$year %in% as.factor(c(seq(as.Date("2006-01-01"), as.Date("2015-01-01"), by="year")))),]

test <- feols(incidence ~ i(quarter, tenkm, ref = '2009-06-30') | cluster + quarter, data = incidence_data_quarterly_small)
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
leish_incidence_data <- full_join(leish_data_buffers_21, population_mdd_long, by=c('cluster'='cluster', 'year'='year'))
leish_incidence_data$incidence <- leish_incidence_data$monthly_cases/leish_incidence_data$population
leish_incidence_data$incidence[is.na(leish_incidence_data$incidence)] <- 0
min(leish_incidence_data$incidence)
max(leish_incidence_data$incidence)
write.csv(leish_incidence_data, "~/Desktop/doctorate/ch2 mdd highway/data/monthly_leish_incidence_data.csv")
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
write.csv(leish_incidence_data_quarterly, "~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data.csv")

incidence_data_yearly <- leish_incidence_data  %>%
  group_by(year,cluster) %>%
  summarize(yearly_cases = sum(monthly_cases),
            onekm=max(onekm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            population=max(population)) 
write.csv(incidence_data_yearly, "~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")

# load stored data
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_leish_data.csv")
incidence_data_quarterly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/quarterly_leish_incidence_data.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")

##yearly model

#baby model
incidence_data_yearly$incidence <- incidence_data_yearly$yearly_cases/incidence_data_yearly$population
baby <- lm(formula = incidence ~ time_10 + fivekm + time_10:fivekm, data = incidence_data_yearly)
summary(baby)

baby2 <- lm(formula = incidence ~ time_10*fivekm, data = incidence_data_yearly)
summary(baby2)

baby3 <- lm(formula = incidence ~ as.factor(cluster) + as.factor(year) + time_10 + fivekm + time_10:fivekm, data = incidence_data_yearly)
summary(baby3)



incidence_data_yearly <- incidence_data_yearly[complete.cases(incidence_data_yearly), ]
incidence_data_yearly$cluster <- as.factor(incidence_data_yearly$cluster)

model <- plm(incidence ~ time_09 + fivekm + time_09:fivekm + time_09:fivekm + time_09:fivekm + time_09:fivekm + time_09:fivekm, 
             data = incidence_data_yearly,
             index = c("cluster", "year"), 
             model = "within", 
             effect = "twoways")
coeftest(model, vcov. = vcovHC, type = "HC0", cluster = 'group')
coeftest(model, vcovHC(model, type = 'HC0', cluster = 'group'))

fatal_fe_mod <- plm(incidence ~ time_09 + fivekm + time_09:fivekm, 
                    data = incidence_data_yearly,
                    index = 'year')
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")

