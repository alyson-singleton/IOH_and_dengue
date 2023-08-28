# load data

#read in required packages
require(readxl)
require(tidyverse)
library(sf)
library(mapview)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

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


#load 2016
#load 2015
#load 2014
#load 2013
#load 2012
#load 2011
#load 2010
#load 2009
#load 2008