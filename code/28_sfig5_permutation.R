# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S5.
#
# Date created: 8/6/2025

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)

# Load results
sfig5_df <- read_rds("results/supplementary_text_results/sfig5_permuted_effects_df.rds")
dengue_yearly_agg_results_df <- read_rds("results/main_text_results/fig2_dengue_yearly_ld_results.rds")
dengue_yearly_agg_effect_est <- dengue_yearly_agg_results_df$estimate

#####################
## SFig 5a
#####################

sfig5_df_full <- sfig5_df %>% filter(group == "full")
sfig5a <- ggplot() +
  geom_histogram(aes(sfig5_df_full$effect), binwidth = 0.05, fill="grey") +
  geom_vline(xintercept=dengue_yearly_agg_effect_est, color='red') +
  xlab("Bootstrapped effect estimate") + ylab("") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=10,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.text = element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "none",
        strip.text.x = element_text(size = 14))
sfig5a


#####################
## SFig 5b
#####################

sfig5_df_block <- sfig5_df %>% filter(group == "block")
sfig5b <- ggplot() +
  geom_histogram(aes(sfig5_df_block$effect), binwidth = 0.05, fill="grey") +
  geom_vline(xintercept=dengue_yearly_agg_effect_est, color='red') +
  xlab("Bootstrapped effect estimate") + ylab("") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=10,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.text = element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "none",
        strip.text.x = element_text(size = 14))
sfig5b


#####################
## SFig 5c
#####################

sfig5_df_within <- sfig5_df %>% filter(group == "within")
sfig5c <- ggplot() +
  geom_histogram(aes(sfig5_df_within$effect), binwidth = 0.05, fill="grey") +
  geom_vline(xintercept=dengue_yearly_agg_effect_est, color='red') +
  xlab("Bootstrapped effect estimate") + ylab("") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5, size=26, face="italic"),
        plot.subtitle = element_text(hjust=0.5, size=22),
        axis.title.y=element_text(size=10,angle=0, vjust=.5, hjust=0.5),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.text = element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position = "none",
        strip.text.x = element_text(size = 14))
sfig5c

#####################
## SFig 5all
#####################

sfig5all <- grid.arrange(sfig5a, sfig5b, sfig5c,
                         nrow = 1)

sfig5all <- as_ggplot(sfig5all) +                                
  draw_plot_label(label = c("A", "B","C"), size = 13,
                  x = c(0.038, 0.371, 0.705), y = c(0.98, 0.98, 0.98)) 
sfig5all

ggsave("sfig5.pdf", plot=sfig5all, path="figures/", width = 12, height = 5, units="in", device = "pdf")
