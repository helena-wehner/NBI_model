#############################
### FCover Values for FHS ###
###   Katharina Huchler   ###
###    Helena Wehner      ###
###     2020-12-09        ###
#############################

### Load and install (if not installed yet) needed Packages ----

if ( ! require('sp') ) { install.packages('sp');
  library('sp')}
if ( ! require("sf") ) { install.packages("sf");
  library("sf")      }
if ( ! require("raster") ) { install.packages("raster");
  library("raster")      }
if ( ! require("beepr") ) { install.packages("beepr");
  library("beepr")      }
if( ! require('graphics') ) { install.packages('graphics');
  library('graphics')}
if ( ! require('ggplot2') ) { install.packages('ggplot2')};
  library('ggplot2')

### STEP 1:  DATA PREPARATION ----

# buffer areas inwards (to make them smaller and avoid including LAI
# values of surroundings?)

# load fields shapefile
fields <- st_read('sampledareasshapefile/NBI_FHS_presabs_final_rework.shp')

# just have a short look/check at the shapefile
plot(fields$geometry)

# create an inner buffer of 5m to avoid side effects of the later used LAI data
fields_buffer_5 <- st_buffer(fields, unit = 'm', -5)
plot(fields_buffer_5$geometry) # plot again to check if inner buffer worked

# transform sf dataframe object fields_buffer_5 into shapefile/SpatialPolygonsObject
fields_buffer_shp <- as(fields_buffer_5, 'Spatial')
plot(fields_buffer_shp)

# inner buffer of 10m, because the resolution of LAI Images = 10m
#fields_buffer_10 <- st_buffer(fields, unit = 'm', -10)
#plot(fields_buffer_10$geometry) # some fields disappear this is why we deside to use an inner buffer
#of 5m instead of 10m

### STEP 2: Extract LAI values for every area ----

#### LAI DATA PREPARATION:----

# load LAI Raster files ----
# UUP File of Sentinel2 LAI Images
uup <- stack(raster('LAI_Images/UUP_right/August/IMAGE_2020-08-01_33UUP_22_FCOVER_10m_Id175305761_L2A.tiff'),
             raster('LAI_Images/UUP_right/August/IMAGE_2020-08-06_33UUP_22_FCOVER_10m_Id179115444_L2A.tiff'),
             raster('LAI_Images/UUP_right/August/IMAGE_2020-08-11_33UUP_22_FCOVER_10m_Id177061738_L2A.tiff'),
             raster('LAI_Images/UUP_right/July/IMAGE_2020-07-07_33UUP_22_FCOVER_10m_Id171035877_L2A.tiff'),
             raster('LAI_Images/UUP_right/July/IMAGE_2020-07-27_33UUP_22_FCOVER_10m_Id174981991_L2A.tiff'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-02_33UUP_22_FCOVER_10m_Id164933578_L2A.tiff'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-12_33UUP_22_FCOVER_10m_Id166696853_L2A.tiff'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-27_33UUP_22_FCOVER_10m_Id169280400_L2A.tiff'))
# UQU File of Sentinel2 LAI Images
uqu <- stack(raster('LAI_Images/UQU_left/August/IMAGE_2020-08-16_32UQU_22_FCOVER_10m_Id177898066_L2A.tiff'),
             raster('LAI_Images/UQU_left/July/IMAGE_2020-07-12_32UQU_22_FCOVER_10m_Id172389323_L2A.tiff'),
             raster('LAI_Images/UQU_left/July/IMAGE_2020-07-22_32UQU_22_FCOVER_10m_Id175160236_L2A.tiff'))

# Clip uup and uqu Files to Field Polygons ----
# --> makes it possible to stack them together

# need to change the projection of uup raster file 
uup <- projectRaster(uup, uqu)
beep(sound = 2) # notification 
# export UUP projected raster --> makes it is easier to redo the script later (saves time)
writeRaster(uup,'uup_stack_2', format = 'GTiff',options="INTERLEAVE=BAND")
beep(sound = 2)
# test: import Raster again
uup <- stack(raster('uup_stack_2.tif'))

uup_clip <-mask(crop(uup, fields_buffer_shp), fields_buffer_shp)
# plot for checking result
plot(uup_clip$IMAGE_2020.08.01_33UUP_22_FCOVER_10m_Id175305761_L2A)

uqu_clip <- mask(crop(uqu, fields_buffer_shp),fields_buffer_shp)
# plot for checking result
plot(uqu_clip$IMAGE_2020.08.16_32UQU_22_FCOVER_10m_Id177898066_L2A)

# stack UUP_clip and UQU_clip together
FCover <- stack(uup_clip, uqu_clip)

# export: FCover clipped Raster Stack (to be on the save side)
writeRaster(x = FCover, filename = 'FCover.tiff', options="INTERLEAVE=BAND", overwrite = T)

# not needed
# 2.1. convert polygon shapefile of sampled areas to raster resembling FCover data (10x10m resolution)
# just clip raster by polygon --> done above

### Masking out Cloud Pixel ----
### load SCL Images of Sentinel2
# UUP File of Sentinel2 Images
uup_scl <- stack(raster('LAI_Images/UUP_right/August/IMAGE_2020-08-01_33UUP_22_SCL_20m_Id175280181_L2A.jp2'),
                 raster('LAI_Images/UUP_right/August/IMAGE_2020-08-06_33UUP_22_SCL_20m_Id179084581_L2A.jp2'),
                 raster('LAI_Images/UUP_right/August/IMAGE_2020-08-11_33UUP_22_SCL_20m_Id177040167_L2A.jp2'),
                 raster('LAI_Images/UUP_right/July/IMAGE_2020-07-07_33UUP_22_SCL_20m_Id170977410_L2A.jp2'),
                 raster('LAI_Images/UUP_right/July/IMAGE_2020-07-27_33UUP_22_SCL_20m_Id174912026_L2A.jp2'),
                 raster('LAI_Images/UUP_right/June/IMAGE_2020-06-02_33UUP_22_SCL_20m_Id164881880_L2A.jp2'),
                 raster('LAI_Images/UUP_right/June/IMAGE_2020-06-12_33UUP_22_SCL_20m_Id166654867_L2A.jp2'),
                 raster('LAI_Images/UUP_right/June/IMAGE_2020-06-27_33UUP_22_SCL_20m_Id169238433_L2A.jp2'))
# UQU File of Sentinel2 Images
uqu_scl <- stack(raster('LAI_Images/UQU_left/August/IMAGE_2020-08-16_32UQU_22_SCL_20m_Id177871641_L2A.jp2'),
                 raster('LAI_Images/UQU_left/July/IMAGE_2020-07-12_32UQU_22_SCL_20m_Id172346359_L2A.jp2'),
                 raster('LAI_Images/UQU_left/July/IMAGE_2020-07-22_32UQU_22_SCL_20m_Id175100741_L2A.jp2'))

# Clip uup and uqu Files to Field Polygons ----
# --> makes it possible to stack them together

# need to change the projection of UUP_scl raster file 
# uup_scl <- projectRaster(uup_scl, uqu_scl)
# beep(sound = 2) # notification 
# export UUP_scl projected raster --> makes it is easier to redo the script later (saves time)
# f.e. for Fcover script (here)
#writeRaster(uup_scl,'uup_scl_stack', format = 'GTiff',options="INTERLEAVE=BAND")
#beep(sound = 2)
# test: import Raster again
uup_scl <- stack('uup_scl_stack.tif')

uup_scl_clip <-mask(crop(uup_scl, fields_buffer_shp), fields_buffer_shp)
# plot for checking result
plot(uup_scl_clip$IMAGE_2020.08.01_33UUP_22_SCL_20m_Id175280181_L2A)

uqu_scl_clip <- mask(crop(uqu_scl, fields_buffer_shp),fields_buffer_shp)
# plot for checking result
plot(uqu_scl_clip$IMAGE_2020.08.16_32UQU_22_SCL_20m_Id177871641_L2A)

# stack UUP_clip and UQU_clip together
SCL <- stack(uup_scl_clip, uqu_scl_clip, full.names = T)

# export: SCL clipped Raster Stack (to be on the save side)
writeRaster(x = SCL, filename = 'SCL.tiff', options="INTERLEAVE=BAND", overwrite = T)

######################################################################################################
### START FROM HERE NOW: LOAD THE SAVED FCover AND SCL DATA INSTEAD OF RUNNINGA ALL ABOVE AGAIN !!!!!----
######################################################################################################

# IMPORT: prepared Data - FCover and SCL Raster stacks

FCover <- stack('FCover.tif')
SCL <- stack('SCL.tif')

# rename Labels from LAI Raster Stack
names(FCover) <- c('FCover_2020.08.01','FCover_2020.08.06','FCover_2020.08.11',
                'FCover_2020.07.07','FCover_2020.07.27','FCover_2020.06.02','FCover_2020.06.12',
                'FCover_2020.06.27',
                'FCover_2020.08.16','FCover_2020.07.12','FCover_2020.07.22')

# rename Labels from SCL Raster Stack
names(SCL) <- c('SCL_2020.08.01','SCL_2020.08.06','SCL_2020.08.11',
                'SCL_2020.07.07','SCL_2020.07.27','SCL_2020.06.02','SCL_2020.06.12',
                'SCL_2020.06.27',
                'SCL_2020.08.16','SCL_2020.07.12','SCL_2020.07.22')

### STEP 2: CLOUD MASKING ----

# create Cloud Mask ----
# Values of SCL without cloud values = NA --> NA's = 75589
summary(getValues(SCL))
# set values of SCL stack (7:11 = related to clouds) to NA
SCL_cloud <- calc(SCL, fun = function(x)
{x[x >= 6.99] <- NA;
return(x)}
)
beep(sound = 1)
# check if there are still values above 7 in the SCL raster stack
summary(getValues(SCL_cloud))
plot(SCL_cloud$SCL_2020.07.22)
# Layers with NA's = 75589 --> no Pixels are covered with clouds

# Apply SCL Cloud Mask to FCover Raster Stack----

# change resolution of SCL to resolution of FCover: 20m to 10m
res(FCover)
res(SCL)

SCL_res <- disaggregate(SCL_cloud, fact = 2)
#beep(sound = 2)

# just for testing
#SCL_res1 <- disaggregate(x = SCL_cloud$SCL_2020.07.22, fact = 2)
#res(SCL_res1)
#res(SCL_res)

# remove from LAI raster stack all values that are NA in SCL raster stack
# extract all Values of Raster Stack LAI which are no clouds
# mask LAI Image with Cloud Mask
FCover2 <- mask(x = FCover, mask = SCL_res, maskvalue = NA)

# plot an image to see if it changed
par(mfrow = c(2,2))

plot(SCL$SCL_2020.07.22)
plot(FCover$FCover_2020.07.22)
plot(SCL_res$SCL_2020.07.22)
plot(FCover2$FCover_2020.07.22)
# best to see with image of: 2020.07.22


### STEP 3: FCover Values Extraction - Linear Regression

# EXTRACTION FCover Values and Calculation of MEAN per Area

FCover_v <- raster::extract(FCover2, fields_buffer_shp, fun = mean, na.rm = T, df = T)
beep(sound = 1)
FCover_v$CompID <- fields_buffer_shp$CompID

# reorder the columns of FCover_v table
FCover_v <- FCover_v[,order(names(FCover_v))]

# export FCover_v data table
write.csv2(FCover_v, file = 'FCover_v.csv')

par(mfrow = c(2,2))

# Density Plot
plot(density(as.numeric(FCover_v[1,2:12]), na.rm = T), main = FCover_v[1,1])
plot(density(as.numeric(FCover_v[2,2:12]), na.rm = T), main = FCover_v[2,1])
plot(density(as.numeric(FCover_v[3,2:12]), na.rm = T), main = FCover_v[3,1])
plot(density(as.numeric(FCover_v[4,2:12]), na.rm = T), main = FCover_v[4,1])
plot(density(as.numeric(FCover_v[5,2:12]), na.rm = T), main = FCover_v[5,1])
plot(density(as.numeric(FCover_v[6,2:12]), na.rm = T), main = FCover_v[6,1])
plot(density(as.numeric(FCover_v[7,2:12]), na.rm = T), main = FCover_v[7,1])
plot(density(as.numeric(FCover_v[8,2:12]), na.rm = T), main = FCover_v[8,1])
plot(density(as.numeric(FCover_v[9,2:12]), na.rm = T), main = FCover_v[9,1])
plot(density(as.numeric(FCover_v[10,2:12]), na.rm = T), main = FCover_v[10,1])
plot(density(as.numeric(FCover_v[11,2:12]), na.rm = T), main = FCover_v[11,1])
plot(density(as.numeric(FCover_v[12,2:12]), na.rm = T), main = FCover_v[12,1])
plot(density(as.numeric(FCover_v[13,2:12]), na.rm = T), main = FCover_v[13,1]) # absence
plot(density(as.numeric(FCover_v[14,2:12]), na.rm = T), main = FCover_v[14,1]) # absence
plot(density(as.numeric(FCover_v[15,2:12]), na.rm = T), main = FCover_v[15,1]) # presence
plot(density(as.numeric(FCover_v[16,2:12]), na.rm = T), main = FCover_v[16,1]) # presence

# Scatterplot
plot(as.numeric(FCover_v[13,2:12]), na.rm = T, main = FCover_v[13,1]) # absence
plot(as.numeric(FCover_v[14,2:12]), na.rm = T, main = FCover_v[14,1]) # absence
plot(as.numeric(FCover_v[15,2:12]), na.rm = T, main = FCover_v[15,1]) # presence
plot(as.numeric(FCover_v[16,2:12]), na.rm = T, main = FCover_v[16,1]) # presence

