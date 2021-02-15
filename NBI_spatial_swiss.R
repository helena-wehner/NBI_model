#############################
### Spatial Modelling     ###
### Northern Bald Ibis    ###
### Foraging Habitat Use  ###
### 15.01.2021            ###
#############################


library(rms) 
library(raster)
library(mgcv) # for gam
source("varImpBiomod.R")
# varImpBiomod Function: slightly changed, because of NA values 
library(randomForest)
library(dismo)
library(rgdal)
library(ellipse)
library(randomForest) # for random forest model
library(XML)
library(maptools)
library(dplyr)
library(ggplot2)
library(RStoolbox)
library(sf)
library(dplyr)
library(mapview)
library(rgeos)
library(beepr)
library(corrplot)

#######################
### Data Preparation ###
########################

### 1. load NBI Movement Data

# 'clipped NBI speed' Shapefile
nbi <- readOGR('Model_Swiss/GPS Daten Vögel/speed5.shp')

# NOTE: clip data to speed !!!!

plot(nbi)
# n = 48827 (points)

# if you want to use only a random split part of the data set:
#nbi <- dplyr::sample_frac(as.data.frame(nbi), 0.05)
#nbi <- SpatialPointsDataFrame(coords = data.frame(as.numeric(nbi$lon), as.numeric(nbi$lat)),nbi)

### load Training Area and Area for Prediction
pred <- readOGR("Model_Swiss/swiss.shp")
# change projection
pred <- spTransform(pred, CRSobj = crs(nbi))
projection(pred)
plot(pred)

# AOI of BFF Data
aoi <- readOGR('Model_Swiss/canton_dissolve.shp')
plot(aoi)
aoi <- spTransform(aoi, CRSobj = crs(nbi))
projection(aoi)
aoi <- raster::union(aoi)

### 2. load Environmenatl Parameters

### BFF DATA RASTER: Preparation for 10m Res
BFF <- raster('raster/BFFdens30.tif')

### SRTM/Elevation Data
srtm <- raster("raster/SRTM_Pred.tif")
names(srtm) <- c("SRTM")
# Clip to Area of Switzerland and cut parts of BaWü
#srtm <- raster::mask(crop(srtm_pred, pred),pred)
plot(srtm)

### Calculate Slope
# Training Area
#slope <- raster::terrain(srtm_pred, opt="slope", unit="degrees", neighbours=8) 
#writeRaster(slope, filename = 'raster/Slope.tif', overwrite = T)
slope <- raster('raster/Slope_Pred.tif')
names(slope) <- c('Slope')
#slope <- raster::mask(raster::crop(slope, pred),pred)
#beep(sound = 1)
plot(slope)

### Indices
#indices_pred <- raster::stack("Model_Swiss/Indices_pred.tif")
#names(indices_pred) <- c("NDVI","EVI","Brightness")
#n_pred <- indices_pred$NDVI
#e_pred <- indices_pred$EVI
#b_pred <- indices_pred$Brightness

### Grasscover
grasscover <- raster('raster/2019.tif')
names(grasscover) <- c('Grass')
grasscover <- projectRaster(grasscover, crs = projection(slope))
#extent(grasscover) <- extent(srtm)

### Wetness TCI
wetness <- raster('raster/Wetness.tif')
names(wetness) <- c('Wetness')
#extent(wetness) <- extent(srtm)

### Brightness TCI
bright <- raster('raster/Brightness.tif')
names(bright) <- c('Brightness')
#extent(bright) <- extent(srtm)

### RESCALE: NDVI, NDWI, EVI

### NDWI
ndwi <- raster('raster/NDWI.tif')

# 1. Try/Option
ndwicl <- clamp(ndwi, -1, 1)
ndwi2 <- setMinMax(ndwicl)
ndwi_rs <- RStoolbox::rescaleImage(x = ndwi2, ymin = -1, ymax = 1)
names(ndwi_rs) <- c('NDWI')

summary(ndwi_rs)
plot(ndwi_rs)

### NDVI
# 1. Methode
ndvi <- raster('raster/NDVI.tif')
ndviclamp <- clamp(ndvi, -1, 1)
ndvi2 <- setMinMax(ndviclamp)
ndvi_rs <- RStoolbox::rescaleImage( x = ndvi2, ymin = -1, ymax = 1)
names(ndvi_rs) <- c('NDVI')

summary(ndvi_rs)
plot(ndvi_rs)

#writeRaster(ndvi2, filename = 'NDVI2.tif', overwrite = T)

# 2. Methode
#ndvi.min = cellStats(ndvi, "min")
#ndvi.max = cellStats(ndvi, "max")

#ndvi.scale <- ((ndvi - ndvi.min) / (ndvi.max - ndvi.min) - 0.5 ) * 2
#plot(ndvi.scale)
#summary(values(ndvi.scale))

### EVI
evi <- raster('raster/EVI.tif')
names(evi) <- c('EVI')

plot(evi)
summary(evi)

evicl <- clamp(evi, -1, 1)
evi2 <- setMinMax(evicl)
evi_rs <- rescaleImage(evi2, ymin = -1, ymax = 1)
names(evi_rs) <- c('EVI')
summary(evi_rs)
plot(evi_rs)

### set Extent
bbox <- bbox(slope)
grasscover <- setExtent(grasscover, bbox)
#srtm <- setExtent(srtm, bbox)

### 3. Stack all Environmental Parameters into one Raster

# Prediction Area

# without srtm and grasscover
env <- stack(ndvi_rs, evi_rs, ndwi_rs, bright, wetness, slope, srtm)
plot(env)

### CLIP to AOI of BFF
#env_mask <- raster::mask(raster::crop(env, aoi), aoi)

# Stack BFF and env Data
# set extent of BFF to extent of env Data
BFF <- setExtent(BFF, raster::extent(env_mask))
# set Res. of BFF to Res. of env Data
BFF <- raster::resample(BFF, env_mask, method = 'bilinear')
env_bff <- stack(env_mask, BFF)

### Resolution 500
#env_train500 <- aggregate(env_train, fact = 50/3)
#env_pred500 <- aggregate(env_pred, fact = 50/3)
#beep(sound = 1)

### Change Resolution to 100m
#env100 <- raster::resample(env_bff, grasscover, method = 'bilinear')

### Change Resolution to 30m
grasscover <- raster::resample(grasscover, env, method = 'bilinear')

# Check if Resolution of env100 und grasscover is the same
#compareRaster(env100, grasscover)

# Add grasscover data to env100 Raster Stack
#env100 <- stack(env100, grasscover)
env30 <- stack(env, grasscover)
plot(env30)

# Clip Point Data to Area of Switzerland
nbi <- gIntersection(pred, nbi, byid = TRUE, drop_lower_td = TRUE)
beep(sound = 1)

#############################
### Setting upt the Model ###
#############################

### Splitting data into training and validation

# selecting 10000 random background samples
set.seed(2)
background <- randomPoints(env30, 2000, nbi)

# select only one presence record in each cell of environmental layer
presence <- gridSample(xy = nbi,r = env30, n=1)

# now we combine the presence and background points, adding a column "species" that contains
# the information about presence (1) and background (2)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                   data = data.frame("species" = rep(c(1,0),c(nrow(presence),nrow(background)))),
                                   match.ID = F,
                                   proj4string = CRS(projection(env30)))

# add information about environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(env30, fulldata))

# split data set into training and test/validation data
# not needed if I use training area and validation area
set.seed(2)
fold <- kfold(fulldata, k=5)
traindata <- fulldata[fold != 1,]
testdata <- fulldata[fold == 1, ]

varname <- names(env30)

####################
### Collinearity ###
####################
# test for collinearity between the environmental variables and decide which to take for the model 
# Visual inspection of collinearity
cm <- cor(getValues(env30), use = "complete.obs") # pearson coefficient (default)
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

### second Approach
jnk <- layerStats(env30, 'pearson', na.rm = T)
corrMatrix <- jnk$'pearson correlation coefficient'

rownames(corrMatrix) <- c('NDVI','EVI','NDWI','Brightness','Wetness','Slope','SRTM',
                  'Grass')
colnames(corrMatrix) <- c('NDVI','EVI','NDWI','Brightness','Wetness','Slope','SRTM',
                  'Grass')

### Plot Collinearity
plotcorr(corrMatrix, col=ifelse(abs(corrMatrix) > 0.7, "red", "grey"))

corrplot(corrMatrix, method = 'color',type = 'upper', order='alphabet',
         addCoef.col='black', tl.col='black', tl.srt=45, diag=F, outline = T)

# --> skip NDVI 

##############################
### General Additive Model ###
##############################

gammodel <- gam(species ~ s(EVI)+s(NDWI)+s(Brightness)+s(Wetness)+s(Slope)+s(SRTM),
                family = "binomial", data = traindata)

summary(gammodel)

# evaluate model on tes data
# a) predict to test data
gamtest <- predict(gammodel, newdata = testdata, type ="response")

# b) calculate performence indices
val.prob(gamtest, testdata[["species"]])

# Calculate Variable Importance
gamimp <- varImpBiomod(model = gammodel, varnames = varname,
                       data = traindata, n = 10)

# Plot the Variable Importance
gamimp3 <- 100*gamimp

gamimp2 <- as.data.frame(gamimp3)
rownames(gamimp2) <- varname
gamimp2 <- tibble::rownames_to_column(gamimp2, "Name")
colnames(gamimp2) <- c("Variable","Percent")

varimp_gam <- ggplot(gamimp2, aes(x=Variable,y=Percent, fill = Variable))+geom_col(show.legend = F)+
  labs(title="Variable Importance", subtitle = "Northern Bald Ibis (Switzerland)")+
  scale_fill_manual(values = c("#fe9929","#4d9221","palegreen4",
                               "palegreen3","royalblue1", "lightblue",'brown','grey','lightgreen'))+
  geom_text(aes(label=round(Percent, digits = 1), vjust=2.5))+
  ylim(0,100)
varimp_gam

# Response Function
plot(gammodel, pages =1)

### Finally: Prediction Map
gammap <- raster::predict(env100, gammodel, type = "response", "Pred_Swiss.tif", overwrite=T)
plot(gammap)

# save the Prediction Map on your harddrive
# writeRaster(gammap, filename = ".../Swiss_Pred_GAM.tif", overwrite = T)


###########################
### Random Forest Model ###
###########################

# random forest requires the dependent variable to be a factor
# if we want to do a classification
rftraindata <- as(traindata, "data.frame")
rftraindata$species <- factor(rftraindata$species)
names(rftraindata)

rfmodel <- randomForest(species ~ SRTM + Slope + EVI + Brightness + NDWI +
                          Grass + Wetness,
                        data = rftraindata, na.action=na.exclude)

### Prediction Map
rfmap <- raster::predict(env30, rfmodel, type = "prob", index = 2)
par(mfrow = c(1,1))
plot(rfmap)

writeRaster(rfmap, filename = "raster/rfmap30.tif", overwrite = T)
beep(sound = 3)

# Variable Importance for RfModel
Rfimp <- varImpBiomod(model = rfmodel, varnames = varname, data = rftraindata, n = 10)
barplot(100*Rfimp, ylab = "Variable Importance (%)")

Rfimp2 <- 100*Rfimp/sum(Rfimp)

Rfimp2 <- as.data.frame(Rfimp2)
rownames(Rfimp2) <- varname
Rfimp2 <- tibble::rownames_to_column(Rfimp2, "Name")
colnames(Rfimp2) <- c("Variable","Percent")

varimp_rf <- ggplot(Rfimp2, aes(x=Variable,y=Percent, fill = Variable))+
  geom_col(position = 'stack', show.legend = F)+
  labs(title="Variable Importance", subtitle = "Switzerland - Random Forest Model",
       x='', y='Percent (%)')+
  scale_fill_manual(values = c("#ffeda0","#993404","palegreen4","palegreen3",
                               "royalblue1", "#ec7014",'brown','lightblue','lightgreen'))+
  geom_text(aes(label=round(Percent, digits = 1), vjust=2.5), size=5)+
  ylim(0,50)
varimp_rf

# Plot both Results

par(mfrow=c(1,2))
plot(gammap, main = 'GAM')
plot(rfmap, main = 'RF')

plot(varimp_gam)
plot(varimp_rf)

# I decided to use Random Forest Model, because this model seems to work better, especially when
# having a look at lake areas.

gamimp2 <- as.data.frame(gamimp3)
rownames(gamimp2) <- c('Elevation','Slope','EVI','Brightness','NDWI')
gamimp2 <- tibble::rownames_to_column(gamimp2, "Name")
colnames(gamimp2) <- c("Variable","Percent")

varimp_gam <- ggplot(gamimp2, aes(x=Variable,y=Percent, fill = Variable))+geom_col()+
  labs(title="Variable Importance", subtitle = "Northern Bald Ibis (Switzerland and southern Baden-W?rttemberg (GER))")+
  scale_fill_manual(values = c("#fe9929","#4d9221","palegreen4","palegreen3","royalblue1", "lightblue"))+
  geom_text(aes(label=round(Percent, digits = 1), vjust=2.5))+
  ylim(0,100)

# Response Function
plot(gammodel, pages =1)

### Finally: Prediction Map
gammap <- raster::predict(env_pred500, gammodel, type = "response", "Pred_Swiss.tif", overwrite=T)
plot(gammap)

# save the Prediction Map on your harddrive
# writeRaster(gammap, filename = ".../Swiss_Pred_GAM.tif", overwrite = T)


###########################
### Random Forest Model ###
###########################

# random forest requires the dependent variable to be a factor
# if we want to do a classification
rftraindata <- as(traindata, "data.frame")
rftraindata$species <- factor(rftraindata$species)
names(rftraindata)

rfmodel <- randomForest(species ~ SRTM + Slope + EVI + Brightness + NDWI,
                        data = rftraindata, na.action=na.exclude)

### Prediction Map
rfmap <- raster::predict(env_pred500, rfmodel, type = "prob", index = 2)
plot(rfmap)

#writeRaster(rfmap, filename = ".../Swiss_Pred_RF.tif", overwrite = T)

# Variable Importance for RfModel
Rfimp <- varImpBiomod(model = rfmodel, varnames = varname, data = rftraindata, n = 10)
barplot(100*Rfimp, ylab = "Variable Importance (%)")

Rfimp2 <- 100*Rfimp/sum(Rfimp)

Rfimp2 <- as.data.frame(Rfimp2)
rownames(Rfimp2) <- c('Elevation','Slope','EVI','Brightness','NDWI')
Rfimp2 <- tibble::rownames_to_column(Rfimp2, "Name")
colnames(Rfimp2) <- c("Variable","Percent")

varimp_rf <- ggplot(Rfimp2, aes(x=Variable,y=Percent, fill = Variable))+
  geom_col(position = 'stack', show.legend = F)+
  labs(title="Variable Importance", subtitle = "Training Area: Switzerland and Baden-W?rttemberg (southern)",
       x='', y='Percent (%)')+
  scale_fill_manual(values = c("#ffeda0","#993404","palegreen4","palegreen3","royalblue1", "#ec7014"))+
  geom_text(aes(label=round(Percent, digits = 1), vjust=2.5), size=5)+
  ylim(0,50)

# Plot both Results

par(mfrow=c(1,2))
plot(gammap)
plot(rfmap)

plot(varimp_gam)
plot(varimp_rf)

# Export Final Map Raster

# Random Forest
writeRaster(rfmap, 'raster/rfmapBFF.tif')
# project Raster
utm <- c('+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs')
rfmap_utm <- projectRaster(rfmap, crs = utm)
writeRaster(rfmap_utm, 'raster/rfmapBFFutm.tif', overwrite = T)

# General Additive Model
writeRaster(gammap, 'raster/gammap.tif')

# I decided to use Random Forest Model, because this model seems to work better, especially when
# having a look at lake areas.
