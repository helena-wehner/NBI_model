###########################
###  LAI Loop Structure ###
###  Katharina Huchler  ###
###     2020-10-221     ###
###########################

### STEP 1: Buffer Areas ----

# buffer areas inwards (to make them smaller and avoid including LAI
# values of surroundings?)

### STEP 2: Extract LAI values for every area ----

# 2.1. convert polygon shapefile of sampled areas to raster resembling LAI data (10x10m resolution)
## or is there a way to do this with polygons as well?
## in my opinion, raster would be more easily to handle



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

