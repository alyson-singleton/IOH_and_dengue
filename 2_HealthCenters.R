# ID ----------------------------------------------------------------------
## TJ Sipin and Aly Singleton

set.seed(90323)

## Methods Overview:
## 1) Take new points, create a 7.5km buffer zone around each point and get the population per 100m pixel
## 2) Fit a GAM and filter population that is greater than 1/0.9 of the fitted value 
##    (1/0.9 is a sensitivity parameter, and is relatively arbitrary – "choose what looks right")
## 3) Let the filtered raster be the only area to use in nearest neighbor operation
## 4) At each health center, make each value of that lat/long point an identifying key of the health center.
##    All other points in the filtered raster are NA. All points not in the filtered raster are set to ID 0 (to denote sparsely populated areas)
## 5) For all NA points, get nearest neighbor; note: the nearest neighbor for many points are ID 0, but others are health centers
## 6) Then do same nearest neighbor operation again and filter out NA points (previously set to ID 0 in step 4), 
##    but get nearest neighbor for each point that is currently set to ID 0. 
## 7) Create boundary polygon shapefiles.

# Packages ----------------------------------------------------------------

t0 <- Sys.time()

library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(foreach)
library(doParallel)
library(doSNOW)
library(tcltk)

print(detectCores())
registerDoParallel(detectCores()-1)

## read in set of lat/long points of each health center
diresa = read.csv("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/cluster_centroids_5km.csv")
diresa$key = diresa$clust

## set population to some impossible number, cannot set to NA
diresa_grid = diresa %>% 
    dplyr::select(long = longitude, lat = latitude, key) %>% 
    mutate(population = NA) 

diresa_spdf = SpatialPointsDataFrame(
  coords = cbind(diresa_grid$long, diresa_grid$lat), 
  data = diresa_grid$key %>% as.data.frame() %>% rename(key = '.'),
  proj4string=CRS("+proj=longlat")
) %>% 
  sf::st_as_sf()

# create raster using spdf
diresa_raster = raster(crs = "+proj=longlat", ext = extent(diresa_spdf)) %>% 
  rasterize(diresa_spdf, ., na.rm=F, background=NA) %>% 
  as.list()

plot(diresa_raster[[2]])

# Read in population raster 
pop=raster("~/Desktop/doctorate/ch2 mdd highway/data/rasters/mdd_world_pop_2020_5000_84_coarse.tif")
plot(pop)

# Take only pixels marked by the raster 
pop_df = pop %>% 
  as.data.frame() %>% 
  mutate(long = raster::coordinates(pop)[,1],
         lat = raster::coordinates(pop)[,2]) %>% 
  rename(population = population) %>% 
  filter(!is.na(population))

# set each pixel in the population raster to have missing key value
pop_grid = pop_df %>% 
  mutate(key = NA) 

# create data frame of pop_grid lat/long coords
xy = data.frame(
  x = pop_grid$long,
  y = pop_grid$lat
)

# bind health center and population grids together
diresa_pop_grid = rbind(diresa_grid, pop_grid) 

diresa_pop_spdf = SpatialPointsDataFrame(
  coords = diresa_pop_grid %>% select(long, lat), 
  data = diresa_pop_grid$key %>% as.data.frame() %>% rename(key = '.'),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
) %>% 
  sf::st_as_sf()

# make raster of health center and population data
diresa_pop_raster = raster(crs = "+proj=longlat", ext = extent(diresa_pop_spdf)) %>% 
  rasterize(diresa_pop_spdf, ., na.rm=F, background=0) %>% 
  as.list()

diresa_pop_raster = diresa_pop_raster[[2]]

# this is what we have to work with and fill in the pixels with missing values
plot(diresa_pop_raster)
dev.off()

# code to show progress bar for next step
cl <- makeSOCKcluster(3)
registerDoSNOW(cl)
ntasks <- nrow(xy)
pb <- tkProgressBar(max=ntasks)
progress <- function(n) setTkProgressBar(pb, n)
opts <- list(progress=progress)

#if this takes too long to run, use a raster with coarser resolution
t0 <- Sys.time()
xyz = foreach(j = 1:nrow(xy), .combine = rbind, .options.snow=opts) %dopar% { # nrow(xy) = 64800 #
  print(j)
  z = diresa_pop_raster@data@values[raster::which.min(replace(raster::distanceFromPoints(diresa_pop_raster, xy[j,]), is.na(diresa_pop_raster), NA))]
  data.frame(
    x = xy[j, 1],
    y = xy[j, 2],
    z = z
  )
}

#Sys.time() - t0
#print(summary(xyz))
#print(head(xyz))
saveRDS(xyz, "~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/nn_eess_NA_0.0_5km.rds")

xyz = xyz %>%
  filter(x != -1)
#print(head(xyz))
saveRDS(xyz, "~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/nn_diresa_0.0_5km.rds")

xyz_spdf = SpatialPointsDataFrame(
  coords = cbind(xyz$x, xyz$y), 
  data = xyz$z %>% as.data.frame() %>% rename(nn = '.'),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
) %>% 
  sf::st_as_sf()

# recursively use original nn_raster extent for eess one
nn_raster = raster(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84", ext = extent(diresa_pop_raster)) %>% 
  rasterize(xyz_spdf, ., na.rm=F, background=NA) %>% 
  as.list()

plot(nn_raster[[2]])
dev.off()

t0 <- Sys.time()
Sys.time() - t0

nnr2 <- nn_raster[[2]]

xy_v2 = data.frame(  
    x = rasterToPoints(nnr2, spatial=TRUE)@coords[,1],
    y = rasterToPoints(nnr2, spatial=TRUE)@coords[,2]
)

nnr2_spdf <- SpatialPointsDataFrame(
    coords = cbind(xy_v2$x, xy_v2$y),
    data = nnr2@data@values %>% as.data.frame() %>% filter(!is.na(.)),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
) %>% sf::st_as_sf()

plot(nnr2_spdf)

xyz_v2 = foreach(j = 1:nrow(xy_v2), .combine = rbind) %dopar% { 

  print(j)
  # calculate z (closest key)
  z = nnr2@data@values[raster::which.min(replace(raster::distanceFromPoints(nnr2, xy_v2[j,]), nnr2==0, NA))]

  # output data frame
  data.frame(
    x = xy_v2[j, 1],
    y = xy_v2[j, 2],
    z = z
  )
}

# mae spatial points data frame of xyz_v2
xyz_v2_spdf = SpatialPointsDataFrame(
  coords = cbind(xyz_v2$x, xyz_v2$y), 
  data = xyz_v2$z %>% as.data.frame() %>% rename(nn = '.'),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
) %>% 
  sf::st_as_sf()

plot(xyz_v2_spdf)

# use original nn_raster extent
nn_v2_raster = raster(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84", ext = extent(nn_raster[[2]])) %>% 
  rasterize(xyz_v2_spdf, ., na.rm=F, background=NA) %>% 
  as.list()

plot(nn_v2_raster[[2]])
dev.off()

# create polygons from the raster for each locality
for(i in values(nn_v2_raster[[2]]) %>% unique()){
  if(is.na(i)) next
  if(i == -1) next
  print(Sys.time() - t0)
  print(i)
  locality <- nn_v2_raster[[2]] == i
  locality[locality == 0] <- NA
  locality[locality == 1] <- i
  geom_bounds <- locality %>%
    rasterToPolygons(dissolve = T) %>%
    st_as_sf() %>% 
    st_set_crs(st_crs(pop)) 

  write_sf(geom_bounds, paste0("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/stor_5km/diresa_", i, ".shp"))
}


file_list <- list.files("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/stor_5km/", pattern = "*shp", full.names = TRUE)
shapefile_list <- lapply(file_list, read_sf)

all_bounds <- data.frame()

for(i in 1:length(shapefile_list)){
  print(i)
  all_bounds <- rbind(all_bounds, shapefile_list[[i]])
}

# take out empty layer (0)
all_bounds_v2 <- all_bounds %>%
  filter(layer != 0)

# write final shapefile
write_sf(all_bounds_v2, paste0("~/Desktop/doctorate/ch2 mdd highway/data/spatial_units/buffer_5km/diresa_nn_raster_5kmclus_5kmbuff_20kgam_00sens.shp"))
plot(all_bounds_v2)
