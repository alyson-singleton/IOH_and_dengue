# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S15.
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

# Load results
dengue_biannual_results_df <- read_rds("results/supplementary_text_results/sfigure15_dengue_biannual_results.rds")
leish_biannual_results_df <- read_rds("results/supplementary_text_results/sfigure15_leish_biannual_results.rds")

# Create standard figure theme
theme_stor <- theme(panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_line(linewidth = 0.3),
                    panel.grid.major.y = element_line(linewidth = 0.3),
                    axis.line.x = element_line(color = "black", linewidth = 0.3),
                    axis.line.y = element_line(color = "black", linewidth = 0.3),
                    plot.title = element_text(size=20, hjust=0.1),
                    plot.title.position = "plot",
                    plot.subtitle = element_text(hjust=0.5, size=22),
                    axis.title=element_text(size=12),
                    axis.title.y=element_text(size=11,angle=0, vjust=.5, hjust=0.5),
                    axis.text.y=element_text(size=12),
                    axis.title.x=element_text(size=12),
                    axis.text.x=element_text(size=12),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=12),
                    legend.position = "none",
                    strip.text.x = element_text(size = 12))

# Set limits
y_lims_biannual <- c(-7,50)

#####################
## SFig 15a
#####################
sfig15a <- ggplot(dengue_biannual_results_df) +
  geom_hline(aes(yintercept=0), colour='black', size=.4) +
  geom_errorbar(aes(x=biannual_date, ymax=upper, ymin=lower, colour=rainy), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-04-01")), linetype='dashed', size=0.4) +
  geom_point(aes(x=as.Date("2008-04-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(biannual_date, estimate, fill=rainy), size=3, shape=21) +
  scale_fill_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  scale_colour_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  xlab("") + ylab("change in\ndengue\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal()+
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims_biannual,
                     breaks = c(-5, 0, 10, 20, 40)) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4)) +
  theme_stor +
  theme(legend.position = "bottom")
sfig15a
sfig15_legend <- get_legend(sfig15a)
sfig15a <- sfig15a + theme(legend.position = "none")

#####################
## SFig 15b
#####################
sfig15b <- ggplot(leish_biannual_results_df) +
  geom_hline(aes(yintercept=0), colour='black', size=.4) +
  geom_errorbar(aes(x=biannual_date, ymax=upper, ymin=lower, colour=rainy), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-04-01")), linetype='dashed', size=0.4) +
  geom_point(aes(x=as.Date("2008-04-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(biannual_date, estimate, fill=rainy), size=3, shape=21) +
  scale_fill_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  scale_colour_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  xlab("Biannual time period") + ylab("change in\nleish\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal()+
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "4 years")) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims_biannual,
                     breaks = c(-5, 0, 10, 20, 40)) +
  theme_stor
sfig15b

#####################
## SFig 15all
#####################

sfig15all <- grid.arrange(sfig15a, sfig15b, sfig15_legend,
                          ncol = 1, nrow = 3,
                          layout_matrix = rbind(c(1),c(2),c(3)), 
                          heights=c(5,5,1))

sfig15all <- as_ggplot(sfig15all) +                                
  draw_plot_label(label = c("A", "B"), size = 14,
                  x = c(0.14, 0.14), y = c(0.99, 0.535)) 
sfig15all

ggsave("sfig15.pdf", plot=sfig15all, path="figures/", width = 8, height = 8, units="in", device = "pdf")
