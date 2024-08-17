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

## load raw mdd case data (all diseases)
dengue_df_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/processed_case_data/dengue_yearly_full_dataset.csv")
head(dengue_df_yearly)

dengue_df_yearly_summary <- dengue_df_yearly %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(yearly_cases))
#dengue_df_yearly_summary$incidence <- (dengue_df_yearly_summary$dengue_cases+1)/dengue_df_yearly_summary$population
dengue_df_yearly_summary$region <- 'Madre de Dios, Peru'
dengue_df_yearly_summary$year <- format(as.Date(dengue_df_yearly_summary$year, format="%Y-%m-%d"),"%Y")
dengue_df_yearly_summary$year <- as.numeric(dengue_df_yearly_summary$year)
  
## load global dataset
dengue_global_dataset <- read_rds("~/Desktop/dengue_temp_full.rds")

## peru national
#peru_case_data <- dengue_global_dataset[which(dengue_global_dataset$country=="PER"),]
peru_case_data <- read.csv("~/Desktop/PER1_weekly2010-2023.csv")
peru_case_data$dengue_cases <- ifelse(is.na(peru_case_data$Casos), 0, peru_case_data$Casos)
peru_case_data$year <- peru_case_data$Ano
#peru_case_data$incidence <- (peru_case_data$dengue_cases+1)/peru_case_data$pop
peru_case_data_summary <- peru_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))#,
            #incidence = mean(incidence)) 
to_link <- data.frame(c(2000:2009),
                      c(5557, 23526, 8085, 3349, 9547, 5637, 4022, 6344, 12824, 13326))
colnames(to_link) <- c("year", "dengue_cases")
peru_case_data_summary <- rbind(to_link,peru_case_data_summary)
peru_case_data_summary$region <- 'Peru (all)'

## cusco peru
cusco_peru_case_data <- peru_case_data[which(peru_case_data$Departamento=="CUSCO"),]
cusco_peru_case_data_summary <- cusco_peru_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))#,
#incidence = mean(incidence)) 
to_link <- data.frame(c(2000:2009),
                      c(0, 0, 2, 0, 0, 2, 0, 0, 0, 0))
colnames(to_link) <- c("year", "dengue_cases")
cusco_peru_case_data_summary <- rbind(to_link,cusco_peru_case_data_summary)
cusco_peru_case_data_summary$region <- 'Cusco, Peru'

## loreto peru
loreto_peru_case_data <- peru_case_data[which(peru_case_data$Departamento=="LORETO"),]
loreto_peru_case_data_summary <- loreto_peru_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))#,
            #incidence = mean(incidence)) 
to_link <- data.frame(c(2000:2009),
                      c(518, 510, 2499, 784, 2580, 1772, 1995, 1720, 7232, 3723))
colnames(to_link) <- c("year", "dengue_cases")
loreto_peru_case_data_summary <- rbind(to_link,loreto_peru_case_data_summary)
loreto_peru_case_data_summary$region <- 'Loreto, Peru'

## brazil national (change to global dataset?)
brazil_case_data <- read.csv("~/Desktop/doctorate/hfi_threshold/annual_dengue_case_inci.csv")
brazil_case_data_summary <- brazil_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))#,
            #incidence = mean(incidence/100000))  
brazil_case_data_summary$region <- 'Brazil (all)'

## acre brazil
brazil_case_data$state <- substr(brazil_case_data$CD_MUN, 1, 2)
acre_brazil_case_data <- brazil_case_data[which(brazil_case_data$state == 12),]

acre_brazil_case_data_summary <- acre_brazil_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))#,
            #incidence = mean(incidence/100000))  
acre_brazil_case_data_summary$region <- 'Acre, Brazil'

## bolivia national
bol_case_data <- dengue_global_dataset[which(dengue_global_dataset$country=="BOL"),]
bol_case_data$dengue_cases <- ifelse(is.na(bol_case_data$dengue_cases), 0, bol_case_data$dengue_cases)
#bol_case_data$incidence <- (bol_case_data$dengue_cases+1)/bol_case_data$pop
bol_case_data_summary <- bol_case_data %>%
  group_by(year) %>%
  summarize(dengue_cases = sum(dengue_cases))#,
            #incidence = mean(incidence)) 
to_link <- data.frame(c(2005:2014),
                      c(4443, 2555, 7332, 7807, 84047, 6620, 44804, 26587, 18206, 22846))
to_link2 <- data.frame(c(2015:2024),
                      c(27099, 32386, 9923, 7597, 19987, 111347, 8947, 16544, 156774, 41870))
colnames(to_link) <- c("year", "dengue_cases")
colnames(to_link2) <- c("year", "dengue_cases")
bol_case_data_summary <- rbind(to_link,to_link2)
bol_case_data_summary$region <- 'Bolivia (all)'

## pando
#to_link <- data.frame(c(2000:2013),
#                      c(4443, 2555, 7332, 7807, 84047, 6620, 44804, 26587, 18206, 22846))
to_link2 <- data.frame(c(2014:2024),
                       c(91, 496, 1831, 335, 265, 3099, 2333, 2413, 1633, 3362, 2106))
#colnames(to_link) <- c("year", "dengue_cases")
colnames(to_link2) <- c("year", "dengue_cases")
pando_case_data_summary <- to_link2#rbind(to_link,to_link2)
pando_case_data_summary$region <- 'Pando, Bolivia'

## link all together in long format to build facet plot
regional_groupings_case_data <- rbind(peru_case_data_summary, dengue_df_yearly_summary,
                                      cusco_peru_case_data_summary, loreto_peru_case_data_summary, 
                                      brazil_case_data_summary, acre_brazil_case_data_summary,
                                      bol_case_data_summary, pando_case_data_summary)

## comparing region plot
region_comp_fig <- ggplot(regional_groupings_case_data) +
  geom_line(aes(x=year, y=dengue_cases)) +
  facet_wrap(~region, scales = "free_y") +
  geom_vline(xintercept=2008,linetype='dashed', color="red") +
  ggtitle("Dengue cases") +
  xlab("Year") + ylab("") + 
  #scale_color_manual(name = "Treatment", values=c("#648FFF","#E04490"), labels=c('Far (>10km)', 'Near (<5km)'),) +
  theme_bw()+
  theme(plot.title = element_text(size=12, face="bold"),
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
