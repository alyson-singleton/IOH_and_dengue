# Script written by:
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Build Supplementary Figure 11.
#
# Date created: 12/4/2025

library(sf)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(readr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggrepel)
library(stringr)
library(RColorBrewer)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Load dengue panel datasets
dengue_yearly <- readRDS("data/clean/dengue_yearly_panels.rds")
dengue_biannual <- readRDS("data/clean/dengue_biannual_panels.rds")

# Load leishmaniasis panel datasets
leish_yearly <- readRDS("data/clean/leish_yearly_panels.rds")
leish_biannual <- readRDS("data/clean/leish_biannual_panels.rds")

# Load spatial data
mdd_aedes_aegypti_chronology <- read.csv("data/raw/diresa_data/mdd_aedes_aegypti_chronology.csv")
linked_ids_codes <- read.csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")
linked_clean <- linked_ids_codes %>%
  mutate(name = name |> str_squish() |> str_to_lower()) %>%
  filter(!key %in% c(101, 105, 106))

hfs_lat_long_aedes <- mdd_aedes_aegypti_chronology %>%
  full_join(linked_clean, by = "name") %>%
  arrange(desc(is.na(year)), year)
hfs_lat_long_aedes <- hfs_lat_long_aedes %>%
  sf::st_as_sf(coords = c('longitude','latitude'), crs = st_crs(4326))

districts <- read_sf("data/raw/spatial_data/mdd_districts.shp")
districts <- st_as_sf(districts) 
districts$geometry <- st_transform(districts$geometry, 4326)
mdd_region <- st_union(districts$geometry)

roads <- read_sf("data/raw/spatial_data/mdd_roads.shp")
roads <- st_as_sf(roads) 
roads$geometry <- st_transform(roads$geometry, 4326)
roads_mdd <- st_covers(mdd_region,roads$geometry, sparse = FALSE)
roads_mdd <- roads[roads_mdd[1,],]

highway <- read_sf("data/raw/spatial_data/peru_roads_important.shp")
highway <- highway[which(highway$ref=="PE-30C"),]
highway <- st_as_sf(highway) 
highway$geometry <- st_transform(highway$geometry, 4326)

highway_mdd <- st_covers(mdd_region,highway$geometry, sparse = FALSE)
highway_mdd <- highway[highway_mdd[1,],]

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

#####################
# Add year paved to highway data
#####################

highway_mdd$year_paved <- NA_real_
section_2008_north <- c(94, 95, 33, 96, 17, 32, 25, 43, 44, 26, 7, 45, 46, 13, 99, 100, 101, 102, 8, 9, 10, 42, 103, 104, 105, 11)
section_2009_north <- c(41, 106, 107, 108, 40, 109, 110, 89, 90, 87, 39, 88, 38, 86)
section_2010_north <- c(37, 111, 113, 21, 36, 12, 23)

section_2008_middle <- c(34, 24, 30, 28, 19, 35, 19, 6, 5, 80, 81, 49, 4, 48, 58, 57, 76, 3)

section_2010_south <- c(91, 22, 118, 64, 65, 119, 120, 121, 122, 123, 124, 56, 125, 126, 127, 128, 129, 130, 83, 131, 
                        132, 133, 84, 82, 134, 135, 136, 55, 112, 114:117, 64, 62, 63, 66, 60, 61)
section_2009_south <- c(54, 53, 52, 137, 138, 143, 144, 51, 139)
section_2008_south <- c(140, 141, 142, 97, 98, 50, 68, 1, 2, 77, 73, 74, 72, 69, 70, 71)

highway_mdd <- highway_mdd %>%
  mutate(mvFeatureId = row_number(),
         year_paved = case_when(
           mvFeatureId %in% section_2008_north ~ 2008,
           mvFeatureId %in% section_2009_north ~ 2009,
           mvFeatureId %in% section_2010_north ~ 2010,
           mvFeatureId %in% section_2008_middle ~ 2008,
           mvFeatureId %in% section_2010_south ~ 2010,
           mvFeatureId %in% section_2009_south ~ 2009,
           mvFeatureId %in% section_2008_south ~ 2008,
           TRUE ~ NA_real_)) %>%
  mutate(year_paved = factor(year_paved))

#####################
## SFig 11a
#####################

# Coordinates for PM label
pm_x <- -69.185
pm_y <- -12.591
pm_dy <- 0.18
label_x <- pm_x
label_y <- pm_y - pm_dy
arrow_x <- pm_x
arrow_y <- pm_y - 0.1

# Vector detection formatting
hfs_lat_long_aedes <- hfs_lat_long_aedes %>%
  filter(!is.na(year))

hfs_lat_long_aedes_a <- hfs_lat_long_aedes %>%
  mutate(
    year_group = case_when(
      year %in% c(1999, 2004, 2005) ~ "1999–2005 (pre-paving)",
      TRUE ~ as.character(year)),
    year_group = factor(
      year_group,
      levels = c(
        "1999–2005 (pre-paving)",
        "2006", "2007", "2008", "2009"))) %>%
  st_set_crs(4326)

# Vector detection colors
blues <- brewer.pal(9, "Blues")
year_colors <- c(
  "1999–2005 (pre-paving)" = blues[2],
  "2006" = blues[3],
  "2007" = blues[5],
  "2008" = blues[7],
  "2009" = blues[9])

# Crop boundaries
xlim_crop <- c(-70.78, -68.55)
ylim_crop <- c(-13.40, -10.8)

# Vector label locations
dxA <- 0.12; dyA <- 0.06   # Group A: up-right
dxB <- 0.08; dyB <- -0.12  # Group B: down-right

groupA <- c("iberia","alerta","mavila","alegría", "planchon", "el triunfo")
groupB <- c("puerto maldonado","laberinto","alto libertad","santa rosa","mazuko")

# two bespoke points (relative offsets)
special <- tibble::tribble(
  ~city, ~dx, ~dy,
  "huepetuhe", -0.14, -0.15,
  "iñapari", 0.1, -0.1)

labs_sf <- hfs_lat_long_aedes_a %>%
  st_set_crs(4326) %>%
  mutate(dx = dplyr::case_when(city %in% groupA ~ dxA,
                               city %in% groupB ~ dxB,
                               TRUE ~ 0.03),
         dy = dplyr::case_when(city %in% groupA ~ dyA,
                               city %in% groupB ~ dyB,
                               TRUE ~ 0)) %>%
  left_join(special, by = "city", suffix = c("", "_sp")) %>%
  mutate(dx = dplyr::coalesce(dx_sp, dx),
         dy = dplyr::coalesce(dy_sp, dy))

xy <- sf::st_coordinates(labs_sf)

labs_sf$geom_lab <- sf::st_sfc(
  lapply(seq_len(nrow(labs_sf)), \(i) sf::st_point(c(xy[i,1] + labs_sf$dx[i], xy[i,2] + labs_sf$dy[i]))),
  crs = sf::st_crs(labs_sf))

# Build connector segments for labels
xy_pt  <- sf::st_coordinates(sf::st_geometry(labs_sf))
xy_lab <- sf::st_coordinates(labs_sf$geom_lab)
seg_df <- data.frame(x = xy_pt[,1], y = xy_pt[,2],
                     xend = xy_lab[,1], yend = xy_lab[,2])

# Plot
sfig11a <- ggplot() +
  geom_sf(data = mdd_region, fill='#ffffff', color='#3b3b3b', size=.15, show.legend = FALSE) +
  geom_sf(data = roads_mdd %>% filter(fclass %in% c("primary", "secondary", "tertiary", "trunk")), 
          aes(geometry = geometry, color='lightgrey'), linewidth=0.5, alpha=0.4, show.legend = F) +
  geom_sf(data = highway_mdd %>% filter(!is.na(year_paved)), aes(geometry = geometry, color=year_paved), linewidth=3, show.legend = T) +
  geom_segment(data = seg_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 0.4,
               color = "black") +
  geom_sf_label(data = labs_sf,
                aes(geometry = geom_lab, label = year),
                label.size = 0.2,
                fill = "white",
                size = 4,
                hjust = 0) +
  geom_sf(data = hfs_lat_long_aedes_a, aes(geometry = geometry, fill=year_group), color='black', shape = 21, size = 5) +
  scale_fill_manual(name = "Year vector detected", values = year_colors) +
  scale_color_manual(name = "Year highway paved", values = c("2008" = '#F4C2D7', "2009" = '#D86A9E', "2010" = '#9B2F64'),
                     labels = c("2008" = "2006-2008", "2009" = "2009", "2010" = "2010")) +
  theme_minimal() +
  no_axis +
  theme(legend.position = c(0, 0.38),   # x,y in [0,1] within the panel; tune these
        legend.justification = c(0, 0),     # anchor legend’s bottom-left at that point
        legend.background = element_rect(fill = scales::alpha("white", 0.7), color = NA),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(linewidth = 3, alpha = 1, shape = NA)),
         fill = "none") +
  annotate("label",
           x = -69.47, y = -10.91,
           label = "Year vector detected",
           label.size = 0.2,
           size = 4,
           hjust = 0,
           vjust = 0,
           color = "black",
           fill = "white",
           fontface = "bold") +
  annotation_scale(location = "bl", height = unit(0.17, "cm"), text_cex = 0.9) +
  annotation_north_arrow(location = "br",style = north_arrow_orienteering(text_size = 8), 
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  annotate("segment", 
           x = label_x, y = label_y + 0.01,
           xend = arrow_x, yend = arrow_y - 0.01,
           arrow = arrow(length = unit(0.17, "cm")),
           color = "black") +
  annotate("text", 
           x = label_x, y = label_y,
           label = "PM",
           size = 3.4, fontface = "bold",
           hjust = 0.5, vjust = 1,
           color = "black" ) +
  geom_point(data = data.frame(lon = -69.185, lat = -12.591), 
             aes(x = lon, y = lat), 
             color = "black", 
             size = 10, shape = 1, stroke = 1) +
  geom_segment(data = arrow_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.18, "cm")),
               linewidth = 1.1,
               color = "#D86A9E") +
  coord_sf(xlim = xlim_crop, ylim = ylim_crop, expand = FALSE)
sfig11a

arrow_df <- data.frame(
  x    = c(-70.47, -69.29, -69.66),
  y    = c(-12.89, -12.50, -11.00),
  xend = c(-70.36, -69.41, -69.67),
  yend = c(-12.80, -12.56, -11.12)
)

#####################
## SFig 11b
#####################

# Add proximate paving year
hfs_lat_long_aedes$year_paved <- c(NA, NA, NA, 2008, 2008, 2008, 2009, 2009, 2008, 2009, 2010, 2010, NA)
hfs_lat_long_aedes$year <- as.numeric(as.character(hfs_lat_long_aedes$year))

# Linear fit
lm_fit <- lm(year ~ year_paved, data = hfs_lat_long_aedes)
lm_sum <- summary(lm_fit)
r2_val <- lm_sum$r.squared
p_val  <- lm_sum$coefficients["year_paved", "Pr(>|t|)"]

set.seed(42)
sfig11b <- ggplot(hfs_lat_long_aedes, aes(x = year_paved, y = year)) +
  geom_point(size = 3, color=blues[7], alpha = 0.7,
             position = position_jitter(width  = 0, height = 0.15)) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color="#9B2F64", fill  = "#9B2F64", alpha = 0.1) +
  annotate(
    "text", x = 2008.2, y = 2009.6,
    hjust = 0,
    label = paste0(
      "r^2 = ", round(r2_val, 3),
      "\np = ", signif(p_val, 2)),
    size = 4) +
  labs(x = "Year proximate\nhighway section paved",
       y = "Year\nvector\ndetected") +
  scale_x_continuous(
    breaks = seq(
      floor(min(hfs_lat_long_aedes$year_paved, na.rm = TRUE)),
      ceiling(max(hfs_lat_long_aedes$year_paved, na.rm = TRUE)),
      by = 1)) +
  scale_y_continuous(breaks = 2005:2011) +
  coord_cartesian(ylim = c(2005, 2011)) +
  theme_minimal() +
  theme(axis.text.x  = element_text(size = 9),
        axis.text.y  = element_text(size = 9),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11, angle = 0, vjust = 0.5))
sfig11b

#####################
## SFig 11c
#####################

#cumulative vector df
vector_first_year <- hfs_lat_long_aedes %>%
  group_by(key) %>%
  summarise(first_year = min(year, na.rm = TRUE),.groups = "drop")

vector_yearly <- vector_first_year %>%
  count(first_year, name = "new_sites") %>%
  rename(year = first_year) %>%
  arrange(year) %>%
  mutate(cum_sites_vector = cumsum(new_sites)) %>%
  select(year, cum_sites_vector)

vector_yearly <- vector_yearly %>%
  tidyr::complete(
    year = seq(min(year), 2012, by = 1),
    fill = list(cum_sites_vector = 0)) %>%
  arrange(year) %>%
  mutate(cum_sites_vector = cummax(cum_sites_vector))

#passenger traffic only
all_traffic_data <- read_csv("./data/raw/mtc_peru_traffic_data/traffic_data_cleaned.csv")
mobility_yearly <- all_traffic_data %>%
  filter(Metric == "Passenger Traffic" & Department == "Madre de Dios") %>%
  rename(year = Year)

#build joint df
df_plot <- vector_yearly %>%
  full_join(mobility_yearly, by = "year") %>%
  arrange(year)

aedes_range <- range(df_plot$cum_sites_vector, na.rm = TRUE)
mob_range   <- range(df_plot$Value, na.rm = TRUE)

df_plot <- df_plot %>%
  mutate(mobility_scaled =
           (Value - mob_range[1]) / diff(mob_range) * diff(aedes_range) + aedes_range[1])

#build periods df
periods_df <- data.frame(phase = c("Pre-paving", "During construction", "Highway complete"),
                         xmin  = c(1999, 2006, 2010),
                         xmax  = c(2006, 2010, 2018))

# define transforms once (use the same ones everywhere)
to_left  <- function(y_right) (y_right - mob_range[1]) / diff(mob_range) * diff(aedes_range) + aedes_range[1]
to_right <- function(y_left)  (y_left - aedes_range[1]) / diff(aedes_range) * diff(mob_range) + mob_range[1]

# scale mobility using the transform
df_plot <- df_plot %>%
  mutate(mobility_scaled = to_left(Value))

# choose left-axis breaks (these will define gridlines)
left_breaks <- seq(0, 16, by = 4)     # adjust as you like
right_breaks <- to_right(left_breaks) # must be derived from left_breaks

sfig11c <- ggplot(df_plot, aes(x = year)) +
  geom_rect(
    data = periods_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = c("#f0f0f0", "#d9d9d9", "#bdbdbd"),
    alpha = 0.35) +
  annotate("text",
           x = c(2002.5, 2008, 2014), y = 16.3,
           label = c("Before", "During", "After"),
           vjust = 1.5, size = 4, color = "grey20", fontface = "italic") +
  geom_vline(xintercept = c(2006, 2010), linetype = "dotted", linewidth = 0.6, color = "grey40") +
  geom_step(aes(y = cum_sites_vector), linewidth = 1, color = "#1f78b4") +
  geom_line(aes(y = mobility_scaled), linewidth = 1, linetype = "dashed", color = "#9B2F64") +
  scale_y_continuous(
    name = "# sites vector detected",
    limits = c(0, 16.5),
    breaks = left_breaks,
    minor_breaks = NULL,
    sec.axis = sec_axis(
      trans = to_right,
      name = "passenger traffic",
      breaks = right_breaks,
      labels = function(x) paste0(signif(x, digits = 2), "k"))) +
  scale_x_continuous(breaks = seq(2000, 2018, by = 2)) +
  labs(x = "Year") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        axis.title.y.left  = element_text(size = 11, vjust = 0.5, color = "#1f78b4"),
        axis.title.y.right = element_text(size = 11, vjust = 0.5, color = "#9B2F64"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        axis.title.x = element_text(size = 11))

sfig11c

#####################
## SFig 11all
#####################

sfig11all <- grid.arrange(sfig11a, sfig11b, sfig11c,
                          ncol = 3, nrow = 2, 
                          layout_matrix = rbind(c(1,1,1,1,2,2,2),c(1,1,1,1,3,3,3)))

sfig11all <- as_ggplot(sfig11all) +
  draw_plot_label(label = c("A)", "B)", "C)"), size = 14,
                  x = c(0.02, 0.55, 0.55), y = c(0.99, 0.99, 0.5))
sfig11all

ggsave("sfig11.pdf", plot=sfig11all, path="figures/", width = 12, height = 7, units="in", device = "pdf") 
