# Script written by:
# TJ Sipin, tjsipin@ucsb.edu
# Alyson Singleton, asinglet@stanford.edu
#
# Script description: 
# Identify spatial groups for clustering standard errors.
#
# Date created: 7/23/2025

# Load Packages -----------------------------------------------------------

library(dismo)
library(tidyverse)
library(tmap)
library(sf)
library(geosphere)
library(terra)

tmap_mode("view")

# Cluster All IDs ---------------------------------------------------------
allID <- read_csv("data/raw/spatial_data/diresa_esalud_coordinates_key.csv")
allIDSpatialPoints <- SpatialPointsDataFrame(allID[,c("longitude", "latitude")], allID[,1])

# use the distm function to generate a geodesic distance matrix in meters
mdistAllID <- distm(allIDSpatialPoints)

# cluster all points using a hierarchical clustering approach
hcAllID <- hclust(as.dist(mdistAllID), method="complete")

# define the distance threshold
d = 7500

# define clusters based on a tree "height"
# cutoff "d" and add them to the SpDataFrame
allIDSpatialPoints$clust <- cutree(hcAllID, h = d)

addClustRaw <- allIDSpatialPoints %>%
  st_as_sf() %>%
  st_drop_geometry()

addClustRaw2 <- addClustRaw %>%
  right_join(allID, by = "key") %>%
  #Create centroids for groups
  group_by(clust) %>%
  mutate(
    x = longitude,
    y = latitude,
    .keep = "unused"
  )

# Transform to NAD83 albers
addCl <- addClustRaw2 %>%
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326")  %>%
  st_transform(crs = "EPSG:3310") %>%
  # Get number of points in cluster to appropriately handle center
  dplyr::group_by(clust) %>%
  mutate(
    numPoints = n()
  )

# Cluster with 1 location
points <- addCl %>%
  filter(numPoints == 1) %>%
  dplyr::summarise()

# Clusters with 2 locations
lines <- addCl %>%
  filter(numPoints == 2) %>%
  dplyr::summarise() %>%
  st_cast("LINESTRING")

# CLusters with 3 or more
polys <- addCl %>%
  filter(numPoints > 2) %>%
  dplyr::summarise() %>%
  # Need to cast to polygon and create convex hull around points
  st_cast("POLYGON") %>%
  st_convex_hull()

# Get lines and polys to make circumscribing circle
allFeats <- lines %>%
  bind_rows(polys)

# Create circumscribing circle for line and poly clusters
allFeatsCircle <- lwgeom::st_minimum_bounding_circle(allFeats) %>%
  # Find center of circle
  st_centroid() %>%
  # Add in point clusters
  bind_rows(points) %>%
  # Buffer by radius
  st_buffer((d/2))

allFeatsCircleCentroid <- lwgeom::st_minimum_bounding_circle(allFeats) %>%
  # Find center of circle
  st_centroid()

allFeatsCircleCentroid <- rbind(allFeatsCircleCentroid, points)
allFeatsCircleCentroid <- st_transform(allFeatsCircleCentroid, crs = "EPSG:4326") %>%
  mutate(longitude = sf::st_coordinates(.)[,1],
         latitude = sf::st_coordinates(.)[,2])

# Save clustering data -----------------------------------------------------

write_csv(addClustRaw, "data/intermediate/clustering_data/key_cluster_match.csv")
write_sf(allFeatsCircleCentroid,"data/intermediate/clustering_data/cluster_centroids.shp")
#write_sf(allFeatsCircle,"data/intermediate/clustering_data/cluster_polygons.shp")

# Map of Methods -----------------------------------------------------------
# Red shows the convex hull of polygon clusters, and blue is the
# final shape. Clusters are grouped by color points
tm_shape(allFeats) +
  tm_borders(col = "red") +
  tm_shape(allFeatsCircle) +
  tm_borders(col = "blue") +
  tm_shape(allIDSpatialPoints %>% st_as_sf()) +
  tm_dots(col = "clust", style = "pretty", palette = "Set3")

tm_shape(allFeats) +
  tm_borders(col = "red") +
  tm_shape(allIDSpatialPoints %>% st_as_sf() ) +
  tm_dots() +
  tm_shape(allFeatsCircle ) +
  tm_borders(col = "blue")
