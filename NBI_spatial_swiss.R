#############################
### Spatial Modelling     ###
### Northern Bald Ibis    ###
### Foraging Habitat Use  ###
### 15.01.2021            ###
#############################


setwd("C:/Users/Lenovo/Desktop/Master/SoSe_2020/Spatial Modelling and Prediction/")

library(rms) 
library(raster)
library(mgcv) # for gam
source("Spatial-Modelling-and-Prediction-MET-EAGLE-/varImpBiomod.R")
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

#######################
### Data Preparation ###
########################

### 1. load NBI Movement Data

# 'clipped NBI speed' Shapefile
nbi <- readOGR('GPS Daten Vögel/2020_feb-nov_16birds_exklLI.shp')
# NOTE: clip data to speed
plot(nbi)
# n = 48827 (points)

# if you want to use only a random split part of the data set:
#nbi <- dplyr::sample_frac(as.data.frame(nbi), 0.05)
#nbi <- SpatialPointsDataFrame(coords = data.frame(as.numeric(nbi$lon), as.numeric(nbi$lat)),nbi)

### load Training Area and Area for Prediction
pred <- readOGR("swiss.shp")
# change projection
pred <- spTransform(pred, CRSobj = crs(nbi))

### 2. load Environmenatl Parameters

### SRTM/Elevation Data
srtm_pred <- raster("SRTM_Pred.tif")
names(srtm_pred) <- c("SRTM")

### Calculate Slope
# Training Area
slope_pred <- terrain(srtm_pred, opt="slope", unit="degrees", neighbours=8) 
beep(sound = 1)
names(slope_pred) <- c("Slope")

### Indices
indices_pred <- raster::stack("Indices_pred.tif")
names(indices_pred) <- c("NDVI","EVI","Brightness")
n_pred <- indices_pred$NDVI
e_pred <- indices_pred$EVI
b_pred <- indices_pred$Brightness

### Normalized Difference Water Index (NDWI)
ndwi_pred <- raster("NDWI_pred.tif")
names(ndwi_pred) <- c('NDWI')


### 3. Stack all Environmental Parameters into one Raster

# Prediction Area
env_pred <- stack(srtm_pred, slope_pred, n_pred, e_pred, b_pred, ndwi_pred)

### Resolution 500
#env_train500 <- aggregate(env_train, fact = 50/3)
env_pred500 <- aggregate(env_pred, fact = 50/3)
beep(sound = 1)

# Clip to Area of Switzerland and cut parts of BaWü
env_pred500 <- raster::crop(raster::mask(env_pred500, pred),pred)

# Clip Point Data to Area of Switzerland
nbi <- gIntersection(pred, nbi, byid = TRUE, drop_lower_td = TRUE)
beep(sound = 1)

#############################
### Setting upt the Model ###
#############################

### Splitting data into training and validation

# selecting 10000 random background samples
set.seed(2)
background <- randomPoints(env_pred500, 2000, nbi)

# select only one presence record in each cell of environmental layer
presence <- gridSample(xy = nbi,r = env_pred500, n=1)

# now we combine the presence and background points, adding a column "species" that contains
# the information about presence (1) and background (2)
fulldata <- SpatialPointsDataFrame(rbind(presence, background),
                                   data = data.frame("species" = rep(c(1,0),c(nrow(presence),nrow(background)))),
                                   match.ID = F,
                                   proj4string = CRS(projection(env_pred500)))

# add information about environmental conditions at point locations
fulldata@data <- cbind(fulldata@data, extract(env_pred500, fulldata))

# split data set into training and test/validation data
# not needed if I use training area and validation area
set.seed(2)
fold <- kfold(fulldata, k=5)
traindata <- fulldata[fold != 1,]
testdata <- fulldata[fold == 1, ]

varname <- names(env_pred500)

####################
### Collinearity ###
####################
# test for collinearity between the environmental variables and decide which to take for the model 
# Visual inspection of collinearity
cm <- cor(getValues(env_pred500), use = "complete.obs") # pearson coefficient (default)
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

### second Approach
jnk <- layerStats(env_pred500, 'pearson', na.rm = T)
corrMatrix <- jnk$'pearson correlation coefficient'

rownames(cm) <- c('Elevation','Slope','NDVI', 'EVI','Brightness','NDWI')
colnames(cm) <- c('Elevation','Slope','NDVI','EVI','Brightness','NDWI')

### Plot Collinearity
corrplot(cm, method = 'color',type = 'upper', order='alphabet',
         addCoef.col='black', tl.col='black', tl.srt=45, diag=F, outline = T)

# skip NDVI data in final model

##############################
### General Additive Model ###
##############################

gammodel <- gam(species ~ s(SRTM)+(Slope) + s(EVI)+s(Brightness)+s(NDWI),
                family = "binomial", data = traindata)

summary(gammodel)

# evaluate model on tes data
# a) predict to test data
gamtest <- predict(gammodel, newdata = testdata, type ="response")

# b) calculate performence indices
val.prob(gamtest, testdata[["species"]])

# Calculate Variable Importance
gamimp <- varImpBiomod(model = gammodel, varnames = c("Elevation","Slope","EVI","Brightness","NDWI"),
                       data = traindata, n = 10)

# Plot the Variable Importance
gamimp3 <- 100*gamimp

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

# I decided to use Random Forest Model, because this model seems to work better, especially when
# having a look at lake areas.

