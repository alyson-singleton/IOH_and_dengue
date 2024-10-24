# ID ----------------------------------------------------------------------
## TJ Sipin

set.seed(91423)

# Packages ----------------------------------------------------------------
library(raster)
library(dplyr)
library(dbscan)

# Read in population raster (WORLD POP in GEE)
pop = raster("~/Downloads/mdd_world_pop_2020_clipped.tif")
plot(log(pop))
pop_df = pop %>% 
  as.data.frame() %>% 
  mutate(long = raster::coordinates(pop)[,1],
         lat = raster::coordinates(pop)[,2]) %>% 
  filter(!is.na(population)) %>% 
  # filter(population > 0)
  # filter(population > 0) %>% 
  mutate(population = log(population)) 

# Fit GAM
# higher k = tighter fit
pop_gam <- mgcv::gam(population ~ s(long, k = 20) + s(lat, k = 20), data = pop_df) #+ s(lat, long, k = 20)
summary(pop_gam)

saveRDS(pop_gam, "~/Desktop/doctorate/ch2 mdd highway/data/pop_gam_k20.rds")
pop_gam <- readRDS("~/Desktop/doctorate/ch2 mdd highway/data/pop_gam_k20.rds")


fitted_pop_df <- pop_df %>% 
  filter(!is.na(population)) %>% 
  mutate(fitted = pop_gam$fitted.values) %>% 
  mutate(
    sensitivity_coef = 0.8, # higher coef = more sensitive
    gt = sensitivity_coef*population >= fitted
  ) %>% 
  filter(gt)

fitted_pop_df %>% filter(gt == T) %>% dim()
fitted_pop_df %>% dim()


fitted_pop_spdf <- SpatialPointsDataFrame(
  coords = cbind(fitted_pop_df$long, fitted_pop_df$lat), 
  data = fitted_pop_df$fitted %>% as.data.frame() %>% rename(fitted = '.'),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
) %>% 
  sf::st_as_sf()

#plot(fitted_pop_spdf)

# create raster to use in nearest neighbor script
pop_raster = raster(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84", ext = extent(fitted_pop_spdf)) %>% 
  rasterize(fitted_pop_spdf, ., na.rm=F, background=NA) %>% 
  as.list()

plot(pop_raster[[2]])

writeRaster(pop_raster[[2]], "~/Desktop/doctorate/ch2 mdd highway/data/pop_raster_7500clus_10km_20k_0.0sens.tif", overwrite=T)

#look at raster
raster20 <- raster("~/Desktop/doctorate/ch2 mdd highway/data/building_spatial_units/pop_raster_7500clus_10km_20k.tif")
plot(raster20)


