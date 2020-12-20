###########################
###  LAI Values for FHS ###
###  Katharina Huchler  ###
###  Helena Wehner      ###
###     2020-12-09      ###
###########################

### Load and install (if not installed yet) needed Packages ----

if ( ! require('sp') ) { install.packages('sp');
  library('sp')}
if ( ! require("sf") ) { install.packages("sf");
  library("sf")      }
if ( ! require("raster") ) { install.packages("raster");
  library("raster")      }
if ( ! require("beepr") ) { install.packages("beepr");
  library("beepr")      }

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
uup <- stack(raster('LAI_Images/UUP_right/August/IMAGE_2020-08-01_33UUP_22_LAI_10m_Id175305793_L2A.tiff'),
             raster('LAI_Images/UUP_right/August/IMAGE_2020-08-06_33UUP_22_LAI_10m_Id179115446_L2A.tiff'),
             raster('LAI_Images/UUP_right/August/IMAGE_2020-08-11_33UUP_22_LAI_10m_Id177061733_L2A.tiff'),
             raster('LAI_Images/UUP_right/July/IMAGE_2020-07-07_33UUP_22_LAI_10m_Id171046190_L2A.tiff'),
             raster('LAI_Images/UUP_right/July/IMAGE_2020-07-27_33UUP_22_LAI_10m_Id174981989_L2A.tiff'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-02_33UUP_22_LAI_10m_Id164933579_L2A.tiff'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-12_33UUP_22_LAI_10m_Id166696852_L2A.tiff'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-27_33UUP_22_LAI_10m_Id169280403_L2A.tiff'))
# UQU File of Sentinel2 LAI Images
uqu <- stack(raster('LAI_Images/UQU_left/August/IMAGE_2020-08-16_32UQU_22_LAI_10m_Id177898068_L2A.tiff'),
             raster('LAI_Images/UQU_left/July/IMAGE_2020-07-12_32UQU_22_LAI_10m_Id172389242_L2A.tiff'),
             raster('LAI_Images/UQU_left/July/IMAGE_2020-07-22_32UQU_22_LAI_10m_Id175160268_L2A.tiff'))

# Clip uup and uqu Files to Field Polygons ----
# --> makes it possible to stack them together

# need to change the projection of uup raster file 
uup <- projectRaster(uup, uqu)
beep(sound = 2) # notification 
# export UUP projected raster --> makes it is easier to redo the script later (saves time)
#writeRaster(uup,'uup_stack', format = 'GTiff',options="INTERLEAVE=BAND")
#beep(sound = 2)
# test: import Raster again
uup <- stack(raster('uup_stack.tif'))

uup_clip <-mask(crop(uup, fields_buffer_shp), fields_buffer_shp)
# plot for checking result
plot(uup_clip$IMAGE_2020.08.01_33UUP_22_LAI_10m_Id175305793_L2A)

uqu_clip <- mask(crop(uqu, fields_buffer_shp),fields_buffer_shp)
# plot for checking result
plot(uqu_clip$IMAGE_2020.08.16_32UQU_22_LAI_10m_Id177898068_L2A)

# stack UUP_clip and UQU_clip together
LAI <- stack(uup_clip, uqu_clip)

# export: LAI clipped Raster Stack (to be on the save side)
writeRaster(x = LAI, filename = 'LAI.tiff', options="INTERLEAVE=BAND")

# not needed
# 2.1. convert polygon shapefile of sampled areas to raster resembling LAI data (10x10m resolution)
# just clip raster by polygon --> done above

### Masking out Cloud Pixel ----
### load SCL Images of Sentinel2
# UUP File of Sentinel2 LAI Images
uup_scl <- stack(raster('LAI_Images/UUP_right/August/IMAGE_2020-08-01_33UUP_22_SCL_20m_Id175280181_L2A.jp2'),
             raster('LAI_Images/UUP_right/August/IMAGE_2020-08-06_33UUP_22_SCL_20m_Id179084581_L2A.jp2'),
             raster('LAI_Images/UUP_right/August/IMAGE_2020-08-11_33UUP_22_SCL_20m_Id177040167_L2A.jp2'),
             raster('LAI_Images/UUP_right/July/IMAGE_2020-07-07_33UUP_22_SCL_20m_Id170977410_L2A.jp2'),
             raster('LAI_Images/UUP_right/July/IMAGE_2020-07-27_33UUP_22_SCL_20m_Id174912026_L2A.jp2'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-02_33UUP_22_SCL_20m_Id164881880_L2A.jp2'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-12_33UUP_22_SCL_20m_Id166654867_L2A.jp2'),
             raster('LAI_Images/UUP_right/June/IMAGE_2020-06-27_33UUP_22_SCL_20m_Id169238433_L2A.jp2'))
# UQU File of Sentinel2 LAI Images
uqu_scl <- stack(raster('LAI_Images/UQU_left/August/IMAGE_2020-08-16_32UQU_22_SCL_20m_Id177871641_L2A.jp2'),
             raster('LAI_Images/UQU_left/July/IMAGE_2020-07-12_32UQU_22_SCL_20m_Id172346359_L2A.jp2'),
             raster('LAI_Images/UQU_left/July/IMAGE_2020-07-22_32UQU_22_SCL_20m_Id175100741_L2A.jp2'))

# Clip uup and uqu Files to Field Polygons ----
# --> makes it possible to stack them together

# need to change the projection of UUP_scl raster file 
#uup_scl <- projectRaster(uup_scl, uqu_scl)
#beep(sound = 2) # notification 
# export UUP_scl projected raster --> makes it is easier to redo the script later (saves time)
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
### START FROM HERE NOW: LOAD THE SAVED LAI AND SCL DATA INSTEAD OF RUNNINGA ALL ABOVE AGAIN !!!!!----
######################################################################################################

# IMPORT: prepared Data - LAI and SCL Raster stacks

LAI <- stack('LAI.tif')
SCL <- stack('SCL.tif')

# rename Labels from LAI Raster Stack
names(LAI) <- c('LAI_2020.08.01','LAI_2020.08.06','LAI_2020.08.11',
                     'LAI_2020.07.07','LAI_2020.07.27','LAI_2020.06.02','LAI_2020.06.12',
                     'LAI_2020.06.27',
                     'LAI_2020.08.16','LAI_2020.07.12','LAI_2020.07.22')

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

# Apply SCL Cloud Mask to LAI Raster Stack----

# remove from LAI raster stack all values that are NA in SCL raster stack
LAI2 <- mask(x = LAI$LAI_2020.07.22,mask = SCL_cloud$SCL_2020.07.22, maskvalue = NA)




# extract all Values of Raster Stack LAI which are no clouds
# mask LAI Image with Cloud Mask



### STEP 3: LAI Values Extraction - Linear Regression

# 2.2. For-loop to extract LAI loop
## input:
### raster data file with spatial extent of all sampled areas
### rasterstack of LAI granules
### rasterstack of SCL granules

resultsdataframe <- c()

for(each area in areas) {
    for(each granule in granules) {
        results <- c()
        AreaSCL <- clip(SCL layer by area)
        AreaLAI <- clip(LAI layer by area)
        clouds <- # select raster cells from AreaSCL which are classified as clouds
        LAIcorrect <- mask(AreaLAI by clouds)
        LAIvalues <- extract(LAI from LAIcorrect)
        cells <- # n of raster cells in LAIcorrect
        # we might need that step later to exclude LAI values calculated from a few cells only
        results$area <- area ID
        results$granule <- granule date
        results$meanLAI <- mean(LAIvalues)
        results$validcells <- cells
        
        rbind(results, resultsdataframe)
    }
}

# then, add a column which gives an numeric ID to each granule

## the result of this step should be a dataframe with five columns:
### area ID
### granule date
### granule ID (numeric, i.e. 1-8)
### meanLAI
### n of used raster cells for calculating LAI


# 2.3. Data control
## Now we might set a threshold for "valid LAI calculations"
## i.e. if we have an area which was partly clouded and we calculated the
## LAI based on 3 raster cells, we might think about excluding those data rows

### STEP 3: Linear Interpolation ----
# 3.1 Breaking Points
## now we want to group our mowing bouts per area together

grouping <- c()
resultsdataframe <- cbind(resultsdataframe, grouping)

for(each area in resultsdataframe$area) {
  prevGroup <- 1
  
  resultsdataframe$grouping[which(resultsdataframe$area == each area AND resultsdataframe$granuleID == 1)] <- prevGroup
  
  prevLAI <- resultsdataframe$meanLAI[which(resultsdataframe$area == each area AND resultsdataframe$granuleID == 1)]
  
  for(each granule in 2:max(resultsdataframe$granuleID)) {
    newLAI <- resultsdataframe$meanLAI[which(resultsdataframe$area == each area AND resultsdataframe$granuleID == each granule)]
    if(newLAI > prevLAI) {
      resultsdataframe$grouping[which(resultsdataframe$area == each area AND resultsdataframe$granuleID == each granule)] <- prevGroup}
    else {
      prevGroup <- prevGroup + 1
      resultsdataframe$grouping[which(resultsdataframe$area == each area AND resultsdataframe$granuleID == each granule)] <- prevGroup
    }
    prevLAI <- newLAI
  }
}


## the result of this step should be a dataframe with six columns:
### area ID
### granule date
### granule ID (numeric, i.e. 1-8)
### meanLAI
### n of used raster cells for calculating LAI
### grouping variable for mowing bouts


# 3.2 Linear interpolation
## The next step is the linear interpolation for all LAI values
## per mowing bout and area. We want a linear function of
## x = day and y = LAI
## however, it is yet unclear how to handle x-values that are not
## covered by existing granules

## potential commands to use:
## approxfun()
## lm() and predict()

