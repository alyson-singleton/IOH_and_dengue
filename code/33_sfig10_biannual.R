# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S10.
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
dengue_biannual_results_df <- read_rds("results/supplementary_text_results/sfigure10_dengue_biannual_results.rds")
leish_biannual_results_df <- read_rds("results/supplementary_text_results/sfigure10_leish_biannual_results.rds")

# Create standard figure theme
theme_stor <- theme(panel.grid.minor.x = element_line(linewidth = 0.3),
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
## SFig 10a
#####################
sfig10a <- ggplot(dengue_biannual_results_df) +
  geom_hline(aes(yintercept=0), colour='red', size=.4) +
  geom_errorbar(aes(x=biannual_date, ymax=upper, ymin=lower, colour=rainy), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-04-01")), linetype='dashed', size=0.4) +
  geom_point(aes(x=as.Date("2008-04-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(biannual_date, estimate, fill=rainy), size=3, shape=21) +
  scale_fill_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  scale_colour_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  xlab("Biannual time period") + ylab("change in\ndengue\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal()+
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims_biannual,
                     breaks = c(-5, 0, 10, 20, 40)) +
  theme_stor +
  theme(legend.position = "bottom")
sfig10a
sfig10_legend <- get_legend(sfig10a)
sfig10a <- sfig10a + theme(legend.position = "none")

#####################
## SFig 10b
#####################
sfig10b <- ggplot(leish_biannual_results_df) +
  geom_hline(aes(yintercept=0), colour='red', size=.4) +
  geom_errorbar(aes(x=biannual_date, ymax=upper, ymin=lower, colour=rainy), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-04-01")), linetype='dashed', size=0.4) +
  geom_point(aes(x=as.Date("2008-04-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(biannual_date, estimate, fill=rainy), size=3, shape=21) +
  scale_fill_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  scale_colour_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  xlab("Biannual time period") + ylab("change in\nleish\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal()+
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims_biannual,
                     breaks = c(-5, 0, 10, 20, 40)) +
  theme_stor

sfig10b

#####################
## SFig 10all
#####################

sfig10all <- grid.arrange(sfig10a, sfig10b, sfig10_legend,
                          ncol = 1, nrow = 3,
                          layout_matrix = rbind(c(1),c(2),c(3)), 
                          heights=c(5,5,1))

sfig10all <- as_ggplot(sfig10all) +                                
  draw_plot_label(label = c("A", "B"), size = 14,
                  x = c(0.14, 0.14), y = c(0.99, 0.535)) 
sfig10all

ggsave("sfig10.pdf", plot=sfig10all, path="figures/", width = 8, height = 11, units="in", device = "pdf")