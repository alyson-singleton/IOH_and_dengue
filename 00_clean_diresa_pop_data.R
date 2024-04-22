
#read in required packages
require(readxl)
require(tidyverse)
library(sf)
library(mapview)
library(ggplot2)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

#################################### 
# load all diresa pop files (after some preprocessing done first in excel by hand)
#################################### 

#load 2023
pop_2023 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLAC_MDD_2023_FINAL.csv", header=T, check.names = F)
colnames(pop_2023) <- c('RENAES', 'COD_2000', 'microrred', 'health_center', 'category', 'department', 'province', 'district', 'city', 'QUNTIL_2019',
                        '28d_meta', '0_5m_meta', '6_11m_meta', '0y_meta', '1y_meta', '2y_meta', '3y_meta', '4y_meta', '5y_meta', 'total', 
                        '0_5y_total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80_84y', '85y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')
pop_2023 <- pop_2023[c(2:90),]

#load 2022
pop_2022 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLAC_MDD_2022_FINAL_A.csv", header=T, check.names = F)
pop_2022 <- pop_2022[c(2:92),c(1:62)]
colnames(pop_2022) <- c('RENAES', 'COD_2000', 'microrred', 'health_center', 'category', 'department', 'province', 'district', 'city', 'QUNTIL_2019',
                        '28d_meta', '0_5m_meta', '6_11m_meta', '0y_meta', '1y_meta', '2y_meta', '3y_meta', '4y_meta', '5y_meta', 'total', 
                        '0_5y_total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80_84y', '85y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2021
pop_2021 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLAC_MDD_2021_FINAL.csv", header=T, check.names = F)
pop_2021 <- pop_2021[c(2:92),]
colnames(pop_2021) <- c('RENAES', 'COD_2000', 'microrred', 'health_center', 'category', 'department', 'province', 'district', 'city', 'QUNTIL_2019',
                        '28d_meta', '0_5m_meta', '6_11m_meta', '0y_meta', '1y_meta', '2y_meta', '3y_meta', '4y_meta', '5y_meta', 'total', 
                        '0_5y_total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')##**80_84 v 80_plus

#load 2020
pop_2020 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLAC_MDD_2020_FINAL_01192020.csv", header=T, check.names = F)
pop_2020 <- pop_2020[c(2:92),]
colnames(pop_2020) <- c('RENAES', 'COD_2000', 'microrred', 'health_center', 'category', 'department', 'province', 'district', 'city', 'QUNTIL_2019',
                        '28d_meta', '0_5m_meta', '6_11m_meta', '0y_meta', '1y_meta', '2y_meta', '3y_meta', '4y_meta', '5y_meta', 'total', 
                        '0_5y_total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')##**80_84 v 80_plus

#load 2019
#missing

#load 2018
#missing

#load 2017
pop_2017 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2017.csv", header=T, check.names = F)
pop_2017 <- pop_2017[,c(5:dim(pop_2017)[2])]
pop_2017 <- pop_2017[c(61:dim(pop_2017)[1]),]
rownames(pop_2017) <- c(1:dim(pop_2017)[1])
pop_2017 <- pop_2017[c(1:26,28:41,43:55,57:65,67:72,74:77,79:87,89:96,98:101,103:109,111:117),]
colnames(pop_2017) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2016
pop_2016 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2016.csv", header=F, check.names = F)
colnames(pop_2016) <- c('renaes','health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2015
pop_2015 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2015.csv", header=F, check.names = F)
pop_2015 <- pop_2015[,c(2:dim(pop_2015)[2])]
colnames(pop_2015) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2014
pop_2014 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2014.csv", header=F, check.names = F)
pop_2014 <- pop_2014[,c(2:dim(pop_2014)[2])]
pop_2014 <- pop_2014[c(1:68, 70:dim(pop_2014)[1]),]
colnames(pop_2014) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2013
pop_2013 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2013.csv", header=F, check.names = F)
colnames(pop_2013) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2012
pop_2012 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2012.csv", header=F, check.names = F)
colnames(pop_2012) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'births', '28d', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem', 'pregnancies')

#load 2011
pop_2011 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2011.csv", header=F, check.names = F)
colnames(pop_2011) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'total_fem', '10_14y_fem', '15_19y_fem',
                        '20_49y_fem')

#load 2010
pop_2010 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2010.csv", header=F, check.names = F)
colnames(pop_2010) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5_9y', '10_11y', '12_17y',
                        '18_19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', 'pregnancies', '50301', 'private', 'did_not_receive_care',
                        'births', '28d', '10_19y_fem', '15_49y_fem')


#load 2009
pop_2009 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data/POBLACION_2009.csv", header=F, check.names = F)
colnames(pop_2009) <- c('health_center', 'total', '0y', '1y', '2y', '3y', '4y', '5y', '6y', '7y', '8y', '9y', '10y', '11y', '12y', '13y', '14y', '15y',
                        '16y', '17y', '18y', '19y', '20_24y', '25_29y', '30_34y', '35_39y', '40_44y', '45_49y', '50_54y', '55_59y', '60_64y',
                        '65_69y', '70_74y', '75_79y', '80y_older', '16_20y_fem', '15_49y_fem', 'pregnancies', 'births_adjusted')


#load 2008
#missing 

#################################### 
### build diresa combined total dataset for 2009--2017 (consistent format type)
#################################### 

pop_2009_2017 <- pop_2009[,c(1:2)] %>% 
  full_join(pop_2010[,c(1:2)], by='health_center') %>%
  full_join(pop_2011[,c(1:2)], by='health_center') %>%
  full_join(pop_2012[,c(1:2)], by='health_center') %>%
  full_join(pop_2013[,c(1:2)], by='health_center') %>%
  full_join(pop_2014[,c(1:2)], by='health_center') %>%
  full_join(pop_2015[,c(1:2)], by='health_center') %>%
  full_join(pop_2016[,c(2:3)], by='health_center') %>%
  full_join(pop_2017[,c(1:2)], by='health_center')
colnames(pop_2009_2017) <- c('pop_name' , as.character(seq(as.Date("2009-01-01"), as.Date("2017-01-01"), by="years")))
pop_2009_2017 <- pop_2009_2017[c(1:(dim(pop_2009_2017)[1]-3)),]
pop_2009_2017$`2014-01-01` <- as.numeric(gsub(",","",pop_2009_2017$`2014-01-01`))
pop_2009_2017$`2017-01-01` <- as.numeric(gsub(",","",pop_2009_2017$`2017-01-01`))

### link names with e_salud codes
key_esalud_clusterid <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid.csv")
pop_2009_2017_linked <- full_join(key_esalud_clusterid, pop_2009_2017, by=join_by('name'=='pop_name'))
## export to complete mis-matches by hand in excel
write.csv(pop_2009_2017_linked, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2009_2017_unmatched.csv")

## load matched pop_2009_2017 data for final editing
cleaned_diresa_pop_2009_2017 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2009_2017_matched.csv")
cleaned_diresa_pop_2009_2017 <- cleaned_diresa_pop_2009_2017[,c(2:16)]
colnames(cleaned_diresa_pop_2009_2017)[c(7:15)] <- c(as.character(seq(as.Date("2009-01-01"), as.Date("2017-01-01"), by="years")))
write.csv(cleaned_diresa_pop_2009_2017, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2009_2017_final.csv")

#################################### 
### build diresa combined total dataset for 2020--2023 (other consistent format type)
#################################### 

pop_2020_2023 <- pop_2020[,c(3,4,20)] %>% 
  full_join(pop_2021[,c(3,4,20)], by=c('health_center', 'microrred')) %>%
  full_join(pop_2022[,c(3,4,20)], by=c('health_center', 'microrred')) %>%
  full_join(pop_2023[,c(3,4,20)], by=c('health_center', 'microrred'))
colnames(pop_2020_2023) <- c('microrred', 'health_center' , as.character(seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by="years")))
pop_2020_2023$`2020-01-01` <- as.numeric(gsub(",","",pop_2020_2023$`2020-01-01`))
pop_2020_2023$`2021-01-01` <- as.numeric(gsub(",","",pop_2020_2023$`2021-01-01`))
pop_2020_2023$`2022-01-01` <- as.numeric(gsub(",","",pop_2020_2023$`2022-01-01`))
pop_2020_2023$`2023-01-01` <- as.numeric(gsub(",","",pop_2020_2023$`2023-01-01`))

### link names with e_salud codes
key_esalud_clusterid <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/key_esalud_clusterid.csv")
#remove "P.S." and "C.S." for matching"
key_esalud_clusterid$name_short <- substr(key_esalud_clusterid$name, 6, 100)
pop_2020_2023_linked <- full_join(key_esalud_clusterid, pop_2020_2023, by=join_by('name_short'=='health_center'))
## export to complete mis-matches by hand in excel
write.csv(pop_2020_2023_linked, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2020_2023_unmatched.csv")

## load matched pop_2020_2023 data for final editing
cleaned_diresa_pop_2020_2023 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2020_2023_matched.csv")
cleaned_diresa_pop_2020_2023 <- cleaned_diresa_pop_2020_2023[,c(2:6,9:12)]
colnames(cleaned_diresa_pop_2020_2023)[c(6:9)] <- c(as.character(seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by="years")))
write.csv(cleaned_diresa_pop_2020_2023, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2020_2023_final.csv")

####################################
### build diresa population dataset across all years
####################################
cleaned_diresa_pop_2009_2017 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2009_2017_matched.csv")
cleaned_diresa_pop_2020_2023 <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_2020_2023_matched.csv")
cleaned_diresa_pop_all_years <- full_join(cleaned_diresa_pop_2009_2017,cleaned_diresa_pop_2020_2023, by=c("key", "name", "e_salud", "latitude", "longitude", "clust"))
cleaned_diresa_pop_all_years <- cleaned_diresa_pop_all_years[,c(1:3, 5:16, 19:22)]
colnames(cleaned_diresa_pop_all_years)[c(7:19)] <- as.character(c(2009:2017,2020:2023))                                   
write.csv(cleaned_diresa_pop_all_years, "~/Desktop/doctorate/ch2 mdd highway/data/diresa_pop_data_processing/diresa_pop_all_years_final.csv")
