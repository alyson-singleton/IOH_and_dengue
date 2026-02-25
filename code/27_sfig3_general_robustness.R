# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S3.
#
# Date created: 8/4/2025

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

# Load results
sfig3_df <- read_rds("results/supplementary_text_results/sfig3_general_robustness_df.rds")

#####################
## SFig 3
#####################

main_df <- sfig3_df %>% filter(Model == "Main")
other_models <- levels(sfig3_df$Model)[levels(sfig3_df$Model) != "Main"]
facet_df <- lapply(other_models, function(mod) {
  bind_rows(main_df, sfig3_df %>% 
              filter(Model == mod)) %>% 
    mutate(Facet = mod)
}) %>% 
  bind_rows()

facet_df$Facet <- factor(facet_df$Facet, levels = c('No PM', 'No LUC', 'Pop weight','1km','10km',
                                                    'No buffer', 'Big buffer', 'Conf & prob cases', 'Units w dengue'))

facet_labels <- setNames(paste0(LETTERS[1:9], ") ", c('No Puerto Maldonado', 'No land-use covariates', 'Population weighting','1km boundary','10km boundary',
                                                      'No buffer (<5km v >5km)', 'Big buffer (<5km v >20km)', 'Confirmed & probable cases', 'Units w at least one dengue case')),
                         c('No PM', 'No LUC', 'Pop weight','1km','10km',
                           'No buffer', 'Big buffer', 'Conf & prob cases', 'Units w dengue'))

model_colors_fill <- c('No PM'='darkred', 'No LUC' = '#DC267F', 'Pop weight'="#FE6100",
                       '1km'='#FFB000','10km'='#FFE518','No buffer'='#85CE76',
                       'Big buffer'='#126E29','Conf & prob cases'="#648FFF",
                       'Units w dengue'="#785EF0", 'Main' = "grey")

model_colors_line <- c('No PM'='darkred', 'No LUC' = '#DC267F', 'Pop weight'="#FE6100",
                       '1km'='#FFB000','10km'='#FFE518','No buffer'='#85CE76',
                       'Big buffer'='#126E29','Conf & prob cases'="#648FFF",
                       'Units w dengue'="#785EF0", 'Main' = "black")

# Plot
sfig3 <- ggplot(facet_df, aes(x = year, y = estimate, group = Model)) +
  geom_hline(yintercept=0, colour='black', linewidth=.4) +
  geom_vline(xintercept=as.Date("2008-01-01"), linetype='dashed', linewidth=0.4) +
  geom_point(aes(x=as.Date("2008-01-01"), y=0, fill=Model), size=3, shape=21, alpha=0.05) +
  geom_point(aes(fill=Model), shape=21, color="black", size=3, alpha=0.6) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=Model), width=0.1, linewidth=0.5, alpha=0.8) +
  facet_wrap(~Facet, ncol = 3, labeller = labeller(Facet = facet_labels)) +
  scale_color_manual(values = model_colors_line) +
  scale_fill_manual(values = model_colors_fill) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4), 
                     limits = c(-15, 70),
                     breaks = c(-10, 0, 10, 20, 40)) +
  xlab("Year") + ylab("change in\ndengue\nincidence\nper 1,000\nrelative\nto 2008") +
  theme_minimal() +
  theme_stor +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold", hjust = 0),
        axis.title.y = element_text(size = 12))
sfig3

ggsave("sfig3.pdf", plot = sfig3, path = "figures/", width = 13, height = 9, units = "in", device = "pdf")
