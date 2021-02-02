######################################
### BFF Density Raster Calculation ###
### H. Wehner                      ###
### 01.02.2020                     ###
######################################

### PACKAGES ####
library(raster)
library(sf)
library(sp)
library(rgeos)
library(beepr)
library(rgdal)


### 1. Load Shape of Switzerland ####
pred <- readOGR("Model_Swiss/swiss.shp")
# change projection
crs <- crs(BFF5)
pred <- spTransform(pred, CRSobj = crs)
projection(pred)
plot(pred)

### Shape of BFF Area
aoi <- readOGR('Model_Swiss/canton1.shp')
aoi <- spTransform(aoi, CRSobj = crs)
plot(aoi)

### 2. Load Environmental Parameters ####

### BFF 30 with 0 and 1 Values Raster
BFF310 <- raster('raster/BFF3010.tif')
values(BFF310)[values(BFF310) > 0] = 2
values(BFF310)[values(BFF310) <= 0] = 1

### BFF 10 with 0 and 1 Values Raster
BFF310 <- raster('raster/BFF3010.tif')
values(BFF310)[values(BFF310) > 0] = 2
values(BFF310)[values(BFF310) <= 0] = 1

### BFF 5 with 0 and 1 Values Raster
BFF5 <- raster('raster/BFF5WGS2.tif')
projection(BFF5)
# Clip to Swiss Area
BFF5 <- mask(crop(BFF5, aoi),aoi)
writeRaster(BFF5, 'raster/BFF5WGSmask.tif', overwrite = T)
BFF52 <- raster('raster/BFF5WGSmask.tif')
BFF10 <- aggregate(BFF52, fact = 2)
writeRaster(BFF10, 'raster/BFF10WGSmask.tif')
BFF10 <- raster('raster/BFF10WGSmask.tif')
BFF10[BFF10>0] <- 2
BFF10[BFF10<=0] <- 1
plot(BFF10)
#values(BFF10)[values(BFF10) > 0] = 2
#values(BFF5)[values(BFF5) <= 0] = 1
plot(BFF5)

### Calculate Density of BFF Pixels
BFFdens30 <- raster::aggregate(BFF10 == 2, fact = 3, fun = mean)*100
beep(sound = 2)
plot(BFFdens30)

writeRaster(BFFdens30, 'raster/BFFdens30.tif', overwrite = T)










