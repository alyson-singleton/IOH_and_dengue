# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure S4.
#
# Date created: 8/4/2025

library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)

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
dengue_yearly_main <- read_rds("results/main_text_results/fig2_dengue_yearly_results.rds")
dengue_yearly_2007 <- read_rds("results/supplementary_text_results/sfig4_dengue_yearly_2007_df.rds")
dengue_yearly_2008 <- read_rds("results/supplementary_text_results/sfig4_dengue_yearly_2008_df.rds")

#####################
## SFig 4
#####################
# Construct the dataframe
sfig4_df <- rbind(
  as.data.frame(cbind(dengue_yearly_2007, Model = rep("A) Paving in 2007", 22))),
  as.data.frame(cbind(dengue_yearly_2008, Model = rep("B) Paving in 2008", 22))),
  as.data.frame(cbind(dengue_yearly_main, Model = rep("C) Paving in 2009 (main model)", 22)))
)
sfig4_df$Model <- factor(sfig4_df$Model, levels = c("A) Paving in 2007", "B) Paving in 2008", "C) Paving in 2009 (main model)"))

facet_refs <- data.frame(
  Model = c("A) Paving in 2007", "B) Paving in 2008", "C) Paving in 2009 (main model)"),
  ref_date = as.Date(c("2006-01-01", "2007-01-01", "2008-01-01")),
  ref_y = 0  # y-location for optional highlight point
)

# Plot
sfig4 <- ggplot(sfig4_df, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, color = 'grey40', linewidth = 0.4) +
  
  # Add facet-specific vertical line
  geom_vline(data = facet_refs, aes(xintercept = ref_date), 
             linetype = 'dashed', linewidth = 0.4, inherit.aes = FALSE) +
  
  # Optional: highlight a reference point at y = 0
  geom_point(data = facet_refs, aes(x = ref_date, y = ref_y), 
             shape = 21, fill = "white", size = 3, inherit.aes = FALSE) +
  
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 0.5, alpha = 0.8) +
  geom_point(shape = 21, size = 3, fill = 'white') +
  
  facet_wrap(~Model, ncol = 1) +
  xlab("Year") +
  ylab("change in\ndengue\nincidence\nper 1,000\nrelative to\npaving year") +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 4),
                     limits = c(-5, 48)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  theme_minimal() +
  theme_stor +
  theme(strip.text = element_text(size = 12, face = "bold", hjust = 0),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
sfig4

ggsave("sfig4.pdf", plot=sfig4, "figures/", width = 6, height = 8, units = c("in"), device="pdf")
