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

# load stored data
incidence_data <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/monthly_incidence_data.csv")
incidence_data_quarterly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/quarterly_incidence_data.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data.csv")

### yearly model
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_incidence_data_pop_adjusted.csv")
incidence_data_yearly <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/yearly_leish_incidence_data.csv")
incidence_data_yearly$incidence <- incidence_data_yearly$yearly_cases/incidence_data_yearly$population
incidence_data_yearly$year <- as.factor(incidence_data_yearly$year)
incidence_data_yearly$onekm <- as.numeric(incidence_data_yearly$onekm)
incidence_data_yearly_no_pm <- incidence_data_yearly[!(incidence_data_yearly$cluster %in% 1),]

covariates <- read.csv("~/Desktop/doctorate/ch2 mdd highway/data/mdd_yearly_covariates.csv")
covariates$year <- format(as.Date(as.character(covariates$year), c("%Y")),"%Y-01-01")
incidence_data_yearly <- left_join(incidence_data_yearly, covariates, by=c("cluster"= "cluster", "year" = "year"))

test <- feols(incidence ~ i(year, fivekm, ref = '2008-01-01') + mean_temp | cluster + year, vcov = "twoway", data = incidence_data_yearly)
#test <- feols(incidence ~ i(year, tenkm, ref = '2008-01-01') | cluster + year, vcov = "cluster", data = incidence_data_yearly)
#test <- feols(incidence ~ i(year, tenkm, ref = '2008-01-01') | cluster + year, data = incidence_data_yearly)
summary(test)
#class(test)
coefplot(test)

df <- as.data.frame(test$coeftable)
df <- df[1:20,]
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
