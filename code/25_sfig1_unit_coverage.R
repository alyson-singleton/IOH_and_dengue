# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S1.
#
# Date created: 8/4/2025

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(cowplot)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Load data
dengue_yearly <- read_rds("data/clean/dengue_yearly_panels.rds")
leish_yearly <- read_rds("data/clean/leish_yearly_panels.rds")

#####################
## SFig 1a
#####################
dengue_yearly$full$case_yn <- ifelse(dengue_yearly$full$yearly_cases_C>0, 1, 0)
dengue_coverage_yearly <- dengue_yearly$full %>%
  group_by(year, fivekm) %>%
  summarize(coverage = sum(case_yn))

dengue_coverage_yearly$fivekm <- as.character(dengue_coverage_yearly$fivekm)
dengue_coverage_yearly$year <- format(as.Date(dengue_coverage_yearly$year, format="%Y-%m-%d"),"%Y")

sfig1a <- ggplot(dengue_coverage_yearly) +
  geom_col(aes(x= year, y=coverage, fill=fivekm), width=0.5, color="white", position = position_dodge(width = 0.5)) +
  scale_fill_manual(name = "", values=c('#648FFF', '#E04490'), labels=c('>5km', '<5km'),) +
  geom_vline(aes(xintercept=("2008")), linetype='dashed', size=0.4) +
  xlab("") + ylab("# units w/\ndengue\ncase") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=12,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=10, angle=45, vjust=0.45),
        axis.text = element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "bottom",
        strip.text.x = element_text(size = 14))
sfig1a
sfig1_legend <- get_legend(sfig1a)
sfig1a <- sfig1a + theme(legend.position = "none")

#####################
## SFig 1b
#####################
leish_yearly$full$case_yn <- ifelse(leish_yearly$full$yearly_cases_C>0, 1, 0)
leish_coverage_yearly <- leish_yearly$full %>%
  group_by(year, fivekm) %>%
  summarize(coverage = sum(case_yn))

leish_coverage_yearly$fivekm <- as.character(leish_coverage_yearly$fivekm)
leish_coverage_yearly$year <- format(as.Date(leish_coverage_yearly$year, format="%Y-%m-%d"),"%Y")

sfig1b <- ggplot(leish_coverage_yearly) +
  geom_col(aes(x= year, y=coverage, fill=fivekm), color="white", width=0.5, position = position_dodge(width = 0.5)) +
  scale_fill_manual(name = "Treatment", values=c('#648FFF', '#E04490'), labels=c('>5km', '<5km'),) +
  geom_vline(aes(xintercept=("2008")), linetype='dashed', size=0.4) +
  xlab("Year") + ylab("# units w/\nleish\ncase") + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=12, angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.text.x=element_text(size=10, angle=45, vjust=0.45),
        axis.text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "none",
        strip.text.x = element_text(size = 14))
sfig1b

#####################
## SFig 1all
#####################

sfig1all <- grid.arrange(sfig1a, sfig1b, sfig1_legend,
                         ncol = 1, nrow = 3,
                         layout_matrix = rbind(c(1),c(2),c(3)), 
                         heights=c(3,3,1))

sfig1all <- as_ggplot(sfig1all) +                                
  draw_plot_label(label = c("A", "B"), size = 14,
                  x = c(0.133, 0.133), y = c(0.99, 0.56)) 
sfig1all

ggsave("sfig1.pdf", plot=sfig1all, path="figures/", width = 8, height = 7, units="in", device = "pdf")
