# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S9.
#
# Date created: 8/6/2025

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)
library(ggrepel)

#####################
## SFig 9a
#####################
### precip data
precip_monthly <- read.csv("data/raw/environmental_data/mdd_precipitation_monthly_sum.csv")
precip_monthly <- precip_monthly[,c(2:278)]
precip_monthly <- precip_monthly[,c(277,1:276)]
colnames(precip_monthly)[2:277] <- as.character(seq(as.Date("2000-01-01"), as.Date("2022-12-01"), by="months"))
colnames(precip_monthly)[1] <- "key"
precip_monthly_mdd_long <- precip_monthly %>%
  pivot_longer(cols = c(2:277), 
               names_to = "month", 
               values_to = "sum_precip")
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")

#look at precip to inform rainy season/biannual split
precip_monthly_mdd_long$year <- format(as.Date(precip_monthly_mdd_long$month, format="%Y-%m-%d"),"%Y")
precip_monthly_all <- precip_monthly_mdd_long %>%
  group_by(month, year) %>%
  summarize(sum_precip = sum(sum_precip)) 
precip_monthly_all$month_wo_year <- format(as.Date(precip_monthly_all$month, format="%Y-%m-%d"),"%m")
sfig9a <- ggplot(precip_monthly_all) +
  geom_line(aes(x=month_wo_year,y=sum_precip,group=year), color="black", alpha=0.7) +
  geom_hline(yintercept=mean(precip_monthly_all$sum_precip), color='red', linetype="dashed") +
  geom_vline(xintercept=04, color='red', linetype="dashed") +
  geom_vline(xintercept=10, color='red', linetype="dashed") +
  xlab("Month") + ylab("Acc.\nmonthly\nprecip") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=10,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=10, angle=45, vjust=0.45),
        axis.text = element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "none",
        strip.text.x = element_text(size = 14))
sfig9a

#####################
## SFig 9b
#####################
dengue_monthly <- read.csv("data/intermediate/dengue_monthly_covariate_df.csv")
dengue_monthly <- dengue_monthly %>%
  mutate(year = year(month)) %>%
  group_by(month, year) %>%
  summarize(sum_cases = sum(monthly_cases_C))
dengue_monthly$month_wo_year <- format(as.Date(dengue_monthly$month, format="%Y-%m-%d"),"%m")
dengue_monthly$year_wo_month <- format(as.Date(dengue_monthly$month, format="%Y-%m-%d"),"%Y")

sfig9b <- ggplot(dengue_monthly) +
  geom_line(aes(x=month_wo_year,y=sum_cases, group=year), color='black', alpha=0.7) +
  geom_vline(xintercept=04, color='red', linetype="dashed") +
  geom_vline(xintercept=10, color='red', linetype="dashed") +
  xlab("Month") + ylab("Monthly\ndengue\ncases") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=10,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=10, angle=45, vjust=0.45),
        axis.text = element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "none",
        strip.text.x = element_text(size = 14)) +
  geom_text_repel(
    data = subset(dengue_monthly, (month_wo_year == 12 & year_wo_month %in% c('2019','2022','2010','2020','2012','2009','2013','2015'))),
    aes(x = month_wo_year, y = sum_cases, label = year_wo_month), 
    color='slategrey',
    point.size = NA,
    min.segment.length = 0,
    size = 2.5,
    hjust = 'right',
    direction = "y",
    nudge_x = 0.8,
    segment.color = 'slategrey',
    segment.alpha = 0.7,
    point.padding = 0.1,
    show.legend = FALSE,
    angle = 0,
    arrow = arrow(length = unit(0.01, "npc"))
  )
sfig9b

#####################
## SFig 9all
#####################
sfig9all <- grid.arrange(sfig9a, sfig9b,
                         ncol = 1, nrow = 2,
                         layout_matrix = rbind(c(1),c(2)), 
                         heights=c(1,1))

sfig9all <- as_ggplot(sfig9all) +                                
  draw_plot_label(label = c("A", "B"), size = 14,
                  x = c(0.11, 0.135), y = c(0.99, 0.49)) 
sfig9all

ggsave("sfig9.pdf", plot=sfig9all, path="figures/", width = 8, height = 8, units="in", device = "pdf")
