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

# Load data
dengue_yearly <- read_rds("data/clean/dengue_yearly_panels.rds")

#####################
## SFig 9a
#####################
#population
dengue_df_yearly_pop <- bind_rows(
  dengue_yearly$connected_buffered %>%
    group_by(year, fivekm) %>%
    summarise(pop_sum = sum(population), .groups = "drop"),
  
  dengue_yearly$connected_buffered_no_pm %>%
    group_by(year, fivekm) %>%
    summarise(pop_sum = sum(population), .groups = "drop") %>%
    mutate(fivekm = ifelse(fivekm == 1, 2, 0)) %>%
    filter(fivekm == 2)) %>%
  mutate(
    fivekm_group = case_when(
      fivekm == 1 ~ "Exposed (<5km)",
      fivekm == 2 ~ "Exposed wo PM (<5km)",
      fivekm == 0 ~ "Unexposed (>10km)"),
    fivekm_group = factor(fivekm_group, levels = c(
      "Exposed (<5km)", "Exposed wo PM (<5km)", "Unexposed (>10km)")),
    year = as.Date(year))

sfig9a <- ggplot(dengue_df_yearly_pop) +
  geom_line(aes(x=year, y=pop_sum, color=fivekm_group)) +
  geom_vline(xintercept=vert_line_date,linetype='dashed') +
  ggtitle("Population") +
  xlab("Year") + ylab("") + 
  scale_color_manual(values = c("#E04490", "#FFD218","#648FFF")) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
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
sfig9a

#####################
## SFig 9b
#####################
#urban area
dengue_df_yearly_urban <- bind_rows(
  dengue_yearly$connected_buffered %>%
    group_by(year, fivekm) %>%
    summarise(urban_sum = sum(urban), .groups = "drop"),
  
  dengue_yearly$connected_buffered_no_pm %>%
    group_by(year, fivekm) %>%
    summarise(urban_sum = sum(urban), .groups = "drop") %>%
    mutate(fivekm = ifelse(fivekm == 1, 2, 0)) %>%
    filter(fivekm == 2)) %>%
  mutate(
    fivekm_group = case_when(
      fivekm == 1 ~ "Exposed (<5km)",
      fivekm == 2 ~ "Exposed wo PM (<5km)",
      fivekm == 0 ~ "Unexposed (>10km)"),
    fivekm_group = factor(fivekm_group, levels = c(
      "Exposed (<5km)", "Exposed wo PM (<5km)", "Unexposed (>10km)")),
    year = as.Date(year))

sfig9b <- ggplot(dengue_df_yearly_urban) +
  geom_line(aes(x = year, y = urban_sum, color = fivekm_group)) +
  geom_vline(xintercept = vert_line_date, linetype = 'dashed') +
  ggtitle("Urban Area (m²)") +
  xlab("Year") + ylab("") + 
  scale_color_manual(values = c("#E04490", "#FFD218","#648FFF")) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 22),
    axis.title = element_text(size = 14),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5, hjust = 0.5),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    strip.text.x = element_text(size = 10))
sfig9b

#####################
## SFig 9c
#####################
#agricultural area
dengue_df_yearly_ag <- bind_rows(
  dengue_yearly$connected_buffered %>%
    group_by(year, fivekm) %>%
    summarise(ag_sum = sum(ag), .groups = "drop"),
  
  dengue_yearly$connected_buffered_no_pm %>%
    group_by(year, fivekm) %>%
    summarise(ag_sum = sum(ag), .groups = "drop") %>%
    mutate(fivekm = ifelse(fivekm == 1, 2, 0)) %>%
    filter(fivekm == 2)) %>%
  mutate(
    fivekm_group = case_when(
      fivekm == 1 ~ "Exposed (<5km)",
      fivekm == 2 ~ "Exposed wo PM (<5km)",
      fivekm == 0 ~ "Unexposed (>10km)"),
    fivekm_group = factor(fivekm_group, levels = c(
      "Exposed (<5km)", "Exposed wo PM (<5km)", "Unexposed (>10km)")),
    year = as.Date(year))

sfig9c <- ggplot(dengue_df_yearly_ag) +
  geom_line(aes(x = year, y = ag_sum, color = fivekm_group)) +
  geom_vline(xintercept = vert_line_date, linetype = 'dashed') +
  ggtitle("Agricultural Area (m²)") +
  xlab("Year") + ylab("") + 
  scale_color_manual(values = c("#E04490", "#FFD218","#648FFF"),
                     guide = guide_legend(nrow = 2)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 22),
    axis.title = element_text(size = 14),
    axis.title.y = element_text(size = 12, angle = 0, vjust = .5, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(0.75, "cm"),       # narrower color boxes
    legend.margin = margin(t = -5, b = -5),     # tighter top/bottom spacing
    legend.box.margin = margin(t = -5),         # tighter space between plot and legend
    legend.spacing.x = unit(0.4, "cm"),          # reduce spacing between items
    strip.text.x = element_text(size = 10))
sfig9c
sfig9_legend <- get_legend(sfig9c)
sfig9c <- sfig9c + theme(legend.position = "none")

#####################
## SFig 9all
#####################

sfig9all <- grid.arrange(sfig9a, sfig9b, sfig9c, sfig9_legend,
                         ncol = 1, nrow = 4,
                         layout_matrix = rbind(c(1),c(2),c(3),c(4)), 
                         heights=c(4,4,4,1))

sfig9all <- grid.arrange(
  sfig9a, sfig9b, sfig9c, sfig9_legend,
  ncol = 3,
  layout_matrix = rbind(c(1,1,1), c(2,2,2), c(3,3,3), c(NA,4,NA)), 
  heights = c(4, 4, 4, 1))

sfig9all <- as_ggplot(sfig9all) +                                
  draw_plot_label(label = c("A)", "B)", "C)"), size = 14,
                  x = c(0.02, 0.02, 0.02), y = c(1, 0.692, 0.385)) 
sfig9all

ggsave("sfig9.pdf", plot=sfig9all, path="figures/", width = 6, height = 10, units="in", device = "pdf")
