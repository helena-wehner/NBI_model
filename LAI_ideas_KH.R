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
if( ! require('graphics') ) { install.packages('graphics');
  library('graphics')}
if ( ! require('ggplot2') ) { install.packages('ggplot2')};
  library('ggplot2')
if ( ! require('ellipse') ) { install.packages('ellipse')};
  library('ellipse')
if ( ! require('corrplot') ) { install.packages('corrplot')};
  library('corrplot')


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

# change resolution of SCL to resolution of LAI
res(LAI)
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
LAI2 <- mask(x = LAI,mask = SCL_res, maskvalue = NA)

# plot an image to see if it changed
x11()
par(mfrow = c(2,2))

plot(SCL$SCL_2020.07.22)
plot(LAI$LAI_2020.07.22)
plot(SCL_res$SCL_2020.07.22)
plot(LAI2$LAI_2020.07.22)
# best to see with image of: 2020.07.22

### STEP 3: LAI Value Range Correction ----

# actual value range from LAI values: 
range(LAI2)
# min values :       166,      2090 
# max values :      1711,     31218 

# given range by LAI formula has to be:
# Tolerance: 0.2
# Pmin: 0
# Pmax: 8.0

# 1. divide each LAI values by 1000
LAI.1000 <- LAI2/1000
# check again for min and max values:
range(LAI.1000)
# min values :  0.339000,  3.309017 
# max values :     1.711,    31.218 

# 2. all values > 10 need to be assigned to NA
# these valuea are consideres as invalid
LAI.NA <- LAI.1000
LAI.NA[LAI.NA > 10] <- NA
# check again for min and max values:
range(LAI.NA)
# min values :  0.339000,  3.309017 
# max values :  1.614760,  9.841179 

# 3. all values > 8 need to be changed to 8
# values between 8 and 10 are considered as valid, but need to be reclassified
LAI3 <- LAI.NA
LAI3[LAI3 > 8] <- 8
# check again for min and max values
range(LAI3)
# min values :  0.339000,  3.309017 
# max values :   1.61476,   8.00000

### STEP 4: LAI Values Extraction - Linear Regression ----

# EXTRACTION LAI Values and Calculation of MEAN per Area

LAI_v <- raster::extract(LAI2, fields_buffer_shp, fun = mean, na.rm = T, df = T)
beep(sound = 1)
LAI_v$CompID <- fields_buffer_shp$CompID

# reorder the columns of LAI_v table
LAI_v <- LAI_v[,order(names(LAI_v))]

# export LAI_v data table
write.csv2(LAI_v, file = 'LAI_v.csv')

par(mfrow = c(2,2))

# Density Plot
plot(density(as.numeric(LAI_v[1,2:12]), na.rm = T), main = LAI_v[1,1])
plot(density(as.numeric(LAI_v[2,2:12]), na.rm = T), main = LAI_v[2,1])
plot(density(as.numeric(LAI_v[3,2:12]), na.rm = T), main = LAI_v[3,1])
plot(density(as.numeric(LAI_v[4,2:12]), na.rm = T), main = LAI_v[4,1])
plot(density(as.numeric(LAI_v[5,2:12]), na.rm = T), main = LAI_v[5,1])
plot(density(as.numeric(LAI_v[6,2:12]), na.rm = T), main = LAI_v[6,1])
plot(density(as.numeric(LAI_v[7,2:12]), na.rm = T), main = LAI_v[7,1])
plot(density(as.numeric(LAI_v[8,2:12]), na.rm = T), main = LAI_v[8,1])
plot(density(as.numeric(LAI_v[9,2:12]), na.rm = T), main = LAI_v[9,1])
plot(density(as.numeric(LAI_v[10,2:12]), na.rm = T), main = LAI_v[10,1])
plot(density(as.numeric(LAI_v[11,2:12]), na.rm = T), main = LAI_v[11,1])
plot(density(as.numeric(LAI_v[12,2:12]), na.rm = T), main = LAI_v[12,1])
plot(density(as.numeric(LAI_v[13,2:12]), na.rm = T), main = LAI_v[13,1]) # absence
plot(density(as.numeric(LAI_v[14,2:12]), na.rm = T), main = LAI_v[14,1]) # absence
plot(density(as.numeric(LAI_v[15,2:12]), na.rm = T), main = LAI_v[15,1]) # presence
plot(density(as.numeric(LAI_v[16,2:12]), na.rm = T), main = LAI_v[16,1]) # presence

# Scatterplot
plot(as.numeric(LAI_v[13,2:12]), na.rm = T, main = LAI_v[13,1]) # absence
plot(as.numeric(LAI_v[14,2:12]), na.rm = T, main = LAI_v[14,1]) # absence
plot(as.numeric(LAI_v[15,2:12]), na.rm = T, main = LAI_v[15,1]) # presence
plot(as.numeric(LAI_v[16,2:12]), na.rm = T, main = LAI_v[16,1]) # presence

# hier habe ich einmal Plots gemacht, die von jeweils einem Feld die Verteilung der LAI
# Werte zeigt, die wir direkt aus den Sentinel Images ziehen.
# Ich finde es sehr spannend, dass zumindestens bis Feld No. 16 = 1P02 immer zwei Peaks
# in ähnlichen Regionen zu sehen sind. Manchmal ist nur der Peak der niederigeren Werte vorhanden


plot(density(as.numeric(LAI_v[2,2:ncol(LAI_v)]), na.rm = T))

  ## CORRELATION
    # Possible Correlation between LAI and FCover data?
    LAI_FC <- stack(LAI2, FCover2)
    cm <- cor(getValues(LAI_FC), use = "complete.obs") # pearson coefficient (default)
    par(mfrow = c(1,1))
    plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))
    corrplot(cm, method = 'color',type = 'upper', order='alphabet',
             addCoef.col='black', tl.col='black', tl.srt=45, diag=F, outline = T)
    # there is a strong correlation between LAI and FCover values 
    # always above 0.8, except for 07.07.2020


#########################################################

### TEST INTERPOLATION FOR ONE AREA

# extract all xP01 areas from fields_buffer_shp file



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

