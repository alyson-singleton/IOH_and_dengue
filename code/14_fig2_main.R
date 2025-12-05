# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Figure 2.
#
# Date created: 8/4/2025

library(sf)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Load results
dengue_yearly_df <- read_rds("results/main_text_results/fig2_dengue_yearly_results.rds")
dengue_biannual_agg_df <- read_rds("results/main_text_results/fig2_dengue_biannual_ld_results.rds")
dengue_yearly_agg_df <- read_rds("results/main_text_results/fig2_dengue_yearly_ld_results.rds")
leish_yearly_df <- read_rds("results/main_text_results/fig2_leish_yearly_results.rds")
leish_biannual_agg_df <- read_rds("results/main_text_results/fig2_leish_biannual_ld_results.rds")
leish_yearly_agg_df <- read_rds("results/main_text_results/fig2_leish_yearly_ld_results.rds")

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

#####################
## Fig 2a
#####################

### yearly model
y_lims <- c(-9.5,57)

fig2a <- ggplot(dengue_yearly_df) +
  geom_hline(aes(yintercept=0), colour='grey30', linewidth=.4) +
  geom_rect(aes(xmin = as.Date("2008-01-01"), xmax = as.Date("2022-06-01"),
                ymin = dengue_yearly_agg_df$upper,
                ymax = dengue_yearly_agg_df$lower),
            fill = "gray60", alpha = 0.01, inherit.aes = FALSE) +
  geom_segment(aes(x = as.Date("2008-01-01"), xend = as.Date("2022-06-01"), 
                   y = dengue_yearly_agg_df$estimate, yend = dengue_yearly_agg_df$estimate),
               color = "red", linewidth = 0.4, inherit.aes = FALSE) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, linewidth=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("") + ylab("change in\ndengue\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims,
                     breaks = c(-5, 0, 10, 20, 40)) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y",
    breaks = seq(as.Date("2008-01-01"), as.Date("2022-01-01"), by = "2 years")
  ) +
  theme_stor

fig2a

#####################
## Fig 2b
#####################

fig2b <- ggplot(dengue_biannual_agg_df) +
  geom_hline(aes(yintercept=0), colour='black', size=.4) +
  geom_errorbar(aes(x=rainy, ymax=upper, ymin=lower, colour=rainy), width=0, size=0.5) +
  geom_point(aes(rainy, estimate, fill=rainy), size=3, shape=21) +
  scale_fill_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  scale_colour_manual(name="Season", values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  xlab("") + ylab("") + 
  theme_minimal() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims,
                     breaks = c(-5, 0, 10, 20, 40)) +
  theme_stor +
  theme(axis.text.y=element_blank())

fig2b

#####################
## Fig 2ab
#####################

fig2ab <- grid.arrange(fig2a, fig2b,                     
                       ncol = 2, nrow = 1,
                       layout_matrix = rbind(c(1,1,1,1,1,2)))

fig2ab <- as_ggplot(fig2ab) +                                
  draw_plot_label(label = c("A", "B"), size = 17,
                  x = c(0.122, 0.846), y = c(0.99, 0.99)) 

fig2ab

#####################
## Fig 2c
#####################

fig2c <- ggplot(leish_yearly_df) +
  geom_hline(aes(yintercept=0), colour='grey30', size=.4) +
  geom_rect(aes(xmin = as.Date("2008-01-01"), xmax = as.Date("2022-06-01"),
                ymin = leish_yearly_agg_df$upper,
                ymax = leish_yearly_agg_df$lower),
            fill = "gray60", alpha = 0.01, inherit.aes = FALSE) +
  geom_segment(aes(x = as.Date("2008-01-01"), xend = as.Date("2022-06-01"), 
                   y = leish_yearly_agg_df$estimate, yend = leish_yearly_agg_df$estimate),
               color = "red", linewidth = 0.4, inherit.aes = FALSE) +
  geom_errorbar(aes(x=year, ymax=upper, ymin=lower), width=0, size=0.5) +
  geom_vline(aes(xintercept=as.Date("2008-01-01")), linetype='dashed', size=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0), size=3, shape=21, fill='white') +
  geom_point(aes(year, estimate), size=3, shape=21, fill='white') +
  xlab("Year") + ylab("change in\nleish\nincidence\nper 1,000\nrelative\nto 2008") + 
  theme_minimal() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims,
                     breaks = c(-5, 0, 10, 20, 40)) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y",
    breaks = seq(as.Date("2008-01-01"), as.Date("2022-01-01"), by = "2 years")
  ) +
  theme_stor
fig2c

#####################
## Fig 2d
#####################

fig2d <- ggplot(leish_biannual_agg_df) +
  geom_hline(aes(yintercept=0), colour='black', size=.4) +
  geom_errorbar(aes(x=rainy, ymax=upper, ymin=lower, colour=rainy), width=0, size=0.5) +
  geom_point(aes(rainy, estimate, fill=rainy), size=3, shape=21) +
  scale_fill_manual(values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  scale_colour_manual(values=c('#DC267F', '#648FFF'), labels=c('Dry', 'Rainy')) + 
  xlab("") + ylab("") + 
  theme_minimal() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = y_lims,
                     breaks = c(-5, 0, 10, 20, 40)) +
  theme_stor +
  theme(axis.text.y=element_blank())
fig2d

#####################
## Fig 2cd
#####################
fig2cd <- grid.arrange(fig2c, fig2d,                     
                       ncol = 2, nrow = 1,
                       layout_matrix = rbind(c(1,1,1,1,1,2)))

fig2cd <- as_ggplot(fig2cd) +                                
  draw_plot_label(label = c("C", "D"), size = 17,
                  x = c(0.122, 0.846), y = c(0.99, 0.99)) 

fig2cd

#####################
## Fig 2combined
#####################
fig2combined <- grid.arrange(fig2ab, fig2cd,                     
                             ncol = 1, nrow = 2,
                             layout_matrix = rbind(c(1),c(2)), 
                             heights=c(1,1))

fig2combined
ggsave("fig2.pdf", plot=fig2combined, path="figures/", width = 9, height = 8, units="in", device = "pdf")
