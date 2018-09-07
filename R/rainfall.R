
# README ------------------------------------------------------------------

# Aggregated MCFCD precipitation data downloaded from
# https://www.maricopa.gov/625/Rainfall-Data, data accessed through: "Download
# all (319) Rainfall History Documents (.xlsx) in one .zip file (86 MB, data
# through 09/30/2017)"


# libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(sqldf)
library(stringr)
library(rgdal)
library(lubridate)
library(sp)
library(gstat)
library(raster)

# options -----------------------------------------------------------------

options(scipen = 999)


# harvest and aggregate raw MCFCD data ------------------------------------

# function mergeYears pulls and format relevant metadata, and data for each
# sheet that contains precipitation data (where sheet name is a year)
mergeYears <- function(targetYear, targetFile) {
  
  merge(
    bind_cols(
      read_excel(targetFile,
                 sheet = "Meta_Stats",
                 range = "B4:B4",
                 col_names = "stationDesc"),
      read_excel(targetFile,
                 sheet = "Meta_Stats",
                 range = "C11:C12",
                 col_names = "latLongVal") %>% 
        mutate(valName = c("lat", "long")) %>% 
        spread(key = valName, value = latLongVal)
    ),
    read_excel(targetFile,
               sheet = as.character(targetYear),
               range = cell_cols("A:B"),
               col_names = c("date", "dailyTotal")) %>% 
      filter(!is.na(date)) %>% 
      mutate(dailyTotal = as.numeric(dailyTotal))
  )
  
}

# function mergeSites identifies all the years for which there are precipitation
# data, and calls the mergeYears function to extract metadata and precipitation
# data for those years
mergeSites <- function(targetFile) {
  
  targetYears <- grep("[0-9]", excel_sheets(targetFile), value = T)
  
  map2_df(targetYears, targetFile, mergeYears)
  
}

# list of files downloaded from MCFCD
fileList <- list.files('~/Dropbox/SNAZ.../Master_Rain_xlsx_2017/')

# iterate over all downloaded MCFCD files, and extract metadata and
# precipitation data into a single dataframe (need to set location of files as
# the working directory)
mcfcdDailyRainfall <- map_df(fileList, mergeSites)

# write these data to file so we do not have to run this long script every time
write.csv(mcfcdDailyRainfall,
          file = '~/Dropbox/SNAZ.../MCFCD_daily/mcfcdDailyRainfall.csv',
          row.names = FALSE)

# new tibble with more functional station name and proper units
dailyRainfall <- mcfcdDailyRainfall %>% 
  mutate(
    stationDesc = str_extract(stationDesc, "^[0-9]+"),
    dailyTotal = dailyTotal * 25.4 # convert inches to mm
  )


# AZMET Desert Ridge ------------------------------------------------------

desertRidge <- bind_rows(
  read_csv('https://cals.arizona.edu/azmet/data/2708rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2709rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2710rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2711rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2712rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2713rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2714rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2715rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2716rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2717rd.txt', col_names = FALSE),
  read_csv('https://cals.arizona.edu/azmet/data/2718rd.txt', col_names = FALSE)
)

colnames(desertRidge) <- c('Year',
                           'Day of Year (DOY)',
                           'Station Number',
                           'Air Temp - Max',
                           'Air Temp - Min',
                           'Air Temp - Mean',
                           'RH - Max',
                           'RH - Min',
                           'RH - Mean',
                           'VPD - Mean',
                           'Solar Rad. - Total',
                           'Precipitation - Total',
                           '4" Soil Temp - Max  ( = 2" prior to 1999 )',
                           '4" Soil Temp - Min  ( = 2" prior to 1999 )',
                           '4" Soil Temp - Mean ( = 2" prior to 1999 )',
                           '20" Soil Temp - Max  ( = 4" prior to 1999 )',
                           '20" Soil Temp - Min  ( = 4" prior to 1999 )',
                           '20" Soil Temp - Mean ( = 4" prior to 1999 )',
                           'Wind Speed - Mean',
                           'Wind Vector Magnitude for Day',
                           'Wind Vector Direction for Day',
                           'Wind Direction Standard Deviation for Day',
                           'Max Wind Speed',
                           'Heat Units (30/12.8 C) (86/55 F)',
                           'Reference Evapotranspiration (ETo)',
                           'Reference Evapotranspiration (ETos)',
                           'Actual Vapor Pressure - Daily Mean',
                           'Dewpoint, Daily Mean')

# format desert ridge as needed to join with MCFCD
desertRidge <- desertRidge %>% 
  mutate(
    stationDesc = "DesertRidge",
    date = as.Date(`Day of Year (DOY)`, origin = paste0(Year,"-01-01")),
    lat = 33.688202,
    long = -111.963449
  ) %>% 
  select(
    stationDesc,
    lat,
    long,
    date,
    dailyTotal = `Precipitation - Total`
  )

# add desert ridge to mcfcd data
dailyRainfall <- dailyRainfall %>% 
  mutate(date = as.Date(date)) %>% # posix > data to facilitate bind
  bind_rows(desertRidge)


# ibw hydro (from ibwQchem) -----------------------------------------------

# Need to relate IBW storm marks with subcatchment data. Here using ibwQminute
# but any of the IBW data would work so long as the stormMarks are
# accuracte/consistent.

# use read.csv to avoid tidyverse conversion to UTC
ibwQminute <- read.csv('https://www.dropbox.com/s/mhh6wd6fyq1ljxp/ibwQminute.csv?dl=1',
                       stringsAsFactors = FALSE) %>% 
  mutate(dateTime = as.POSIXct(dateTime, format = "%Y-%m-%d %H:%M:%S"))

# pare Q minute to relevant columns and bracket start and end of storms
ibwHydro <- ibwQminute %>% 
  mutate(dateTime = as.Date(dateTime)) %>% # posix > date to facilitate join w/ppt
  select(stormMark, dateTime, cumQ) %>% 
  group_by(stormMark) %>% 
  summarise(
    ibwBegin = min(dateTime),
    ibwEnd = max(dateTime),
    cumQibw = max(cumQ)
  )

# merge MCFCD & AZMET with ibwHydro data, here the inner join will pare ppt data
# to only those associated with a period of flow at IBW

# this workflow adjusted to incorporate rainfall in a 24-hour period before the
# start of flow at the gauge; reviewing storms suggests that 1-day ahead of the
# storm will capture most rainfall but careful of slop (e.g., storms that start
# even earlier than a day before the first flow at IBW, and isolated rain a day
# before flow at IBW but that may not have contributed to discharge).

# adjust start of storm to 24 h in advance of the start of flow at the gauge
ibwHydroAdj <- ibwHydro %>%
  mutate(
    ibwBeginS1 = ibwBegin - days(1)
  )

dailyRainfallStorms <- sqldf('
    SELECT
      ih.stormMark,
      ih.ibwBegin,
      ih.ibwEnd,
      mdr.stationDesc,
      mdr.lat, 
      mdr.long, 
      mdr.date, 
      mdr.dailyTotal
    FROM
      dailyRainfall mdr
    JOIN
      ibwHydroAdj ih ON ((mdr.date <= ih.ibwEnd) AND (mdr.date >= ih.ibwBeginS1));') %>% 
  filter(
    !is.na(dailyTotal),
    dailyTotal > 0
  ) %>% 
  group_by(stormMark, stationDesc) %>% 
  summarise(
    long = max(long), # be sure long ("X") precedes lat ("Y")
    lat = max(lat),
    stormMarkTotal = sum(dailyTotal)
  ) %>% 
  ungroup() %>% 
  arrange(
    stormMark,
    stationDesc
  )


# write these data to file for convenience to avoid the above steps (unless
# something changes)
write_csv(dailyRainfallStorms, '~/Dropbox/SNAZ meeting materials/indianBendWash/data/ibw_rainfall_GIS/dailyRainfallStorms.csv')


# interpolate rainfall ----------------------------------------------------

# import daily storm totals if not already loaded
dailyRainfallStorms <- read_csv('~/Dropbox/SNAZ meeting materials/indianBendWash/data/ibw_rainfall_GIS/dailyRainfallStorms.csv')


# lat long to coordinates
drsCoords <- SpatialPoints(dailyRainfallStorms[,c("long", "lat")],
                           proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 


# spatial points and other data to sp data frame
drsSPDF <- SpatialPointsDataFrame(drsCoords, dailyRainfallStorms)


# helper function to create empty grid to hold interpolated values
empty_grid <- function(spatialObject) {
  
  # create an empty grid where n is the total number of cells
  grd <- as.data.frame(spsample(spatialObject, "regular", n = 50000))
  names(grd) <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd) <- TRUE  # Create SpatialPixel object
  fullgrid(grd) <- TRUE  # Create SpatialGrid object
  
  # add projection information to the empty grid
  proj4string(grd) <- proj4string(spatialObject)
  
  return(grd) 
  
}


# helper function to calculate RMSE
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}


# helper function to optimize idp
f1 <- function(x, test, train) {
  idp <- x
  if (idp < .001) return(Inf)
  m <- gstat(formula=stormMarkTotal~1, locations=train, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$stormMarkTotal, p)
}


# ppt_interpolation function calculates mean precipitation in the Indian Bend
# Wash catchment using several variations of the inverse distance weighted
# approach. This function is highly environment specific and requires
# precipitation data (in a spatial format), a shapefile of the IBW catchment
# (with matching projection), and the above detailed helper functions. The
# function takes an an argument a stormID (i.e., stormMark). Output is a data
# frame of gstatIDW_IDP2, IDW_IDP2, nearest, IDW_IDP_opt, and idw for each
# stormMark provided (either individually or all if called through a loop).

# output:
# * gstatIDW_IDP2: idw with idp = 2 calling the idw function in gstat
# * IDW_IDP2: idw with idp = 2 calling interpolate on a gstat formula
# * nearest: same as IDW_IDP2 but with idp & nmax params presumably equivalent
#   to nearest neighbor
# * IDW_IDP_opt: idw with an idp optimized to minimize RMSE
# * idw: the optimized idp used for IDW_IDP_opt

ppt_interpolation <- function(stormID) {
  
  # filter to a single storm
  drsSPDFsub <- drsSPDF[drsSPDF$stormMark == stormID,]
  
  
  # IDW using workflow from Texas tutorial
  # from: https://mgimond.github.io/Spatial/interpolation-in-r.html
  
  # generate an empty grid to hold modeled data
  grd <- empty_grid(drsSPDFsub)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  # from: https://mgimond.github.io/Spatial/interpolation-in-r.html
  gstatIDW <- gstat::idw(stormMarkTotal ~ 1, drsSPDFsub, newdata=grd, idp=2.0)
  
  # convert to raster object then clip to ibw
  gstatIDWraster <- raster(gstatIDW)
  gstatIDWrasterMask <- mask(gstatIDWraster, ibwWatershedProj)
  gstatIDW <- cellStats(gstatIDWrasterMask, stat = 'mean', na.rm = TRUE)
  
  
  # IDW using workflow from California tutorial
  # from: http://rspatial.org/analysis/rst/4-interpolation.html
  
  # generate an empty grid to hold modeled data
  grd <- empty_grid(drsSPDFsub)
  
  # rasterize the sample grid 
  grdraster <- raster(grd)
  
  gstatFormula <- gstat(formula=stormMarkTotal~1, locations=drsSPDFsub, set=list(idp = 2.0))
  interpResult <- interpolate(grdraster, gstatFormula)
  interpMasked <- mask(interpResult, ibwWatershedProj)
  interpIDP2 <- cellStats(interpMasked, stat = 'mean', na.rm = TRUE)
  
  
  # NEAREST NEIGHBOR, California-style
  # from: http://rspatial.org/analysis/rst/4-interpolation.html
  
  # generate an empty grid to hold modeled data
  grd <- empty_grid(drsSPDFsub)
  
  # rasterize the sample grid 
  grdraster <- raster(grd)
  
  gstatFormula <- gstat(formula=stormMarkTotal~1, locations=drsSPDFsub, nmax=5, set=list(idp = 0))
  interpResult <- interpolate(grdraster, gstatFormula)
  interpMasked <- mask(interpResult, ibwWatershedProj)
  interpNN <- cellStats(interpMasked, stat = 'mean', na.rm = TRUE)
  
  
  # IDW, California-style optimizing for idp
  
  # generate an empty grid to hold modeled data
  grd <- empty_grid(drsSPDFsub)
  
  grdraster <- raster(grd) # rasterize the sample grid
  
  set.seed(42)
  i <- sample(nrow(drsSPDFsub), 0.2 * nrow(drsSPDFsub))
  tst <- drsSPDFsub[i,]
  trn <- drsSPDFsub[-i,]
  opt <- optim(2, f1, test=tst, train=trn, method = "Brent", lower = 0, upper = 10) # use Brent for 1-dimensional, which needs limits
  
  gstatFormula <- gstat(formula=stormMarkTotal~1, locations=drsSPDFsub, set=list(idp = opt$par))
  interpResult <- interpolate(grdraster, gstatFormula)
  interpMasked <- mask(interpResult, ibwWatershedProj)
  interpIDPopt  <- cellStats(interpMasked, stat = 'mean', na.rm = TRUE)
  
  
  # summary data to data frame
  interpolationSummary <- data.frame(stormMark = stormID,
                                     gstatIDW_IDP2 = gstatIDW,
                                     IDW_IDP2  = interpIDP2,
                                     nearest = interpNN,
                                     IDW_IDP_opt = interpIDPopt,
                                     idw = opt$par)
  
  return(interpolationSummary)
  
}

# e.g.: ppt_interpolation(80)

# storms 63, 70, 80, and 87 throwing errors. these are non-chem storms so we can
# remove these for the current effort but may have to delve into the issue(s) if
# there are any further, more hydrologically oriented efforts
map_df(unique(drsSPDF$stormMark)[61:70], ppt_interpolation, .id = "stormMark") # error: 63, 70
map_df(unique(drsSPDF$stormMark)[71:80], ppt_interpolation, .id = "stormMark") # error: 80
map_df(unique(drsSPDF$stormMark)[81:90], ppt_interpolation, .id = "stormMark") # error: 87

validStorms <- unique(drsSPDF$stormMark)
errorStorms <- c(63, 70, 80, 87) 
validStorms <- validStorms[!validStorms %in% errorStorms]

# .id is not behaving as expected (seems to be merely indexing (staring at 1)
# rather than transfering the stormMark data), map without this

# ibwPrecipitation <- map_df(validStorms, ppt_interpolation, .id = "stormMark")
ibwPrecipitation <- map_df(validStorms, ppt_interpolation)

# to fix .id issue noted above, should not be required in a possible future iteration
ibwPrecipitation <- ibwPrecipitation %>%
  as_tibble() %>% 
  dplyr::select(-stormMark) %>% 
  rename(stormMark = storm)

# add cumQ to interpolated precipitation data, note here we are using ibwHydro
# (load if not already in the env.), which features the actual beginning and end
# dates of flow (as opposed to ibwHydroAdj, where the storm begin date was moved
# 24 hours earlier in an attempt to capture rain in the 24-h period leading up
# to flow at the gauge)
ibwPrecipitation <- ibwPrecipitation %>% 
  inner_join(ibwHydro, by = c("stormMark"))

# write these data to file for convenience to avoid the above steps (unless
# something changes)
write_csv(ibwPrecipitation, "~/Dropbox/SNAZ meeting materials/indianBendWash/data/ibw_rainfall_GIS/ibwPrecipitation.csv")


# compare R and ArcGIS approaches -----------------------------------------

# import Katie's rainfall estimates
katiePrecip <- read_csv('https://www.dropbox.com/s/yz1iwkdljecruqs/ibwRainfall.csv?dl=1') %>% 
  mutate(arcGisNN = rainfall_inches * 25.4) %>% 
  dplyr::select(-rainfall_inches)

# compare R and ArcGis derived estimates (yuck!)
ibwPrecipitation %>% 
  inner_join(katiePrecip, by = c("stormMark")) %>% 
  mutate(
    absDiff = arcGisNN - IDW_IDP_opt,
    percentDiff = (absDiff / arcGisNN) * 100
  )

# view comparison
katieStorms <- katiePrecip %>% 
  pull(stormMark)

katiePrecip %>% 
  inner_join(ibwHydro, by = c("stormMark"))

ibwPrecipitation %>%
  filter(stormMark %in% katieStorms) %>% 
  ggplot(aes(x = IDW_IDP_opt, y = cumQibw)) +
  geom_point(aes(colour = 'R')) +
  geom_point(aes(x = arcGisNN, y = cumQibw, colour = 'arc'), data = katiePrecip %>% inner_join(ibwHydro, by = c("stormMark"))) +
  scale_x_log10() +
  scale_y_log10()


# precipitation versus discharge ------------------------------------------

ibwPrecipitation %>%
  mutate(
    monsoon = case_when(
      month(ibwBegin) %in% c(7,8,9) ~ 1,
      TRUE ~ 0)
  ) %>% 
  ggplot(aes(x = IDW_IDP_opt, y = cumQibw)) +
  geom_point(aes(colour = factor(monsoon))) +
  scale_x_log10() +
  scale_y_log10()


# interpolating rainfall scratch space ------------------------------------

# README

# The code in this section reflects the development and testing leading up to
# the ppt_interpolation function. I typically would not have kept this code but
# there are pieces related to variograms, kriging, and a few other topics that
# could be relevant but were not employed.

# try here with only a single storm, note be sure long precedes lat !!!
singleStorm <- mcfcdDailyRainfallStorms %>%
  filter(stormMark == 74)

# lat long to coordinates
dsp <- SpatialPoints(singleStorm[,c("long", "lat")], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 

# spatial points and other data to sp data frame
dsp2 <- SpatialPointsDataFrame(dsp, singleStorm)

# have a look
spplot(dsp2, "stormMarkTotal")
plot(dsp2)

# bring in IBW area shapefile and transform to matching coordinate system
ibwWatershed <- readOGR(dsn='Dropbox/SNAZ meeting materials/indianBendWash/data/ibw_rainfall_GIS/IBW_atCurry/', layer='usgs_at_curry')
proj4string(ibwWatershed)
dsp2proj <- CRS(proj4string(dsp2))
ibwWatershedProj <- spTransform(ibwWatershed, dsp2proj)
proj4string(ibwWatershedProj)

# Texas

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(dsp2, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

# Add dsp2 projection information to the empty grid
proj4string(grd) <- proj4string(dsp2)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(stormMarkTotal ~ 1, dsp2, newdata=grd, idp=2.0)

# Leave-one-out validation routine
IDW.out <- vector(length = length(dsp2))
for (i in 1:length(dsp2)) {
  IDW.out[i] <- idw(stormMarkTotal ~ 1, dsp2[-i,], dsp2[i,], idp=2.0)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ dsp2$stormMarkTotal, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ dsp2$stormMarkTotal), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Compute RMSE
sqrt(sum((IDW.out - dsp2$stormMarkTotal)^2) / length(dsp2))

# RMSE
# idp 0.0 = 6.224243 - should this be eq. to nearest neighbor? produces a nonsensical map
# idp 1.0 = 5.345751
# idp 2.0 = 4.339399
# idp 3.0 = 3.915036
# idp 4.0 = 3.845967

# Convert to raster object then clip to Texas
pidwraster <- raster(P.idw)
spplot(pidwraster)
pidwrasterMask <- mask(pidwraster, ibwWatershedProj)
cellStats(pidwrasterMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

# California

# nearest neighbor

grdraster <- raster(grd) # rasterize the sample grid
gs <- gstat(formula=stormMarkTotal~1, locations=dsp2, nmax=5, set=list(idp = 0))
nn <- interpolate(grdraster, gs)
spplot(nn)
nnMask <- mask(nn, ibwWatershedProj)
cellStats(nnMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

# idw - this appears similar to the Texas tutorial: P.idw <- gstat::idw(stormMarkTotal ~ 1, dsp2, newdata=grd, idp=2.0)
gs2 <- gstat(formula=stormMarkTotal~1, locations=dsp2)
nn2 <- interpolate(grdraster, gs2) # this is a raster
spplot(nn2)
nn2Mask <- mask(nn2, ibwWatershedProj)
cellStats(nn2Mask, stat = 'mean', na.rm = TRUE) # this is the ticket!


# null model for comparing RMSE
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(dsp2$stormMarkTotal), dsp2$stormMarkTotal)

rmsenn <- rep(NA, 5)
kf <- kfold(nrow(dsp2))
for (k in 1:5) {
  test <- dsp2[kf == k, ]
  train <- dsp2[kf != k, ]
  gscv <- gstat(formula=stormMarkTotal~1, locations=train, nmax=5, set=list(idp = 0))
  p <- predict(gscv, test)$var1.pred
  rmsenn[k] <- RMSE(test$stormMarkTotal, p)
}

rmsenn
mean(rmsenn)
1 - (mean(rmsenn) / null)

# VARIOGRAMS - these are relevant for kriging
vario <- gstat(formula=stormMarkTotal~1, locations=dsp2)
v <- variogram(vario, width=5)
head(v)
plot(v)

fve <- fit.variogram(v, vgm(psill = 85, model = "Sph", range = 75, nugget = 20))
plot(v, fve)

fve <- fit.variogram(v, vgm(psill = 85, model = "Exp", range = 75, nugget = 20))
plot(v, fve)

# Use variogram fve in a kriging interpolation
k <- gstat(formula=stormMarkTotal~1, locations=dsp2, model=fve)
# predicted values
kp <- predict(k, grd)
## [using ordinary kriging]
spplot(kp)

# ....which, is the same as below from the gstat vignette with the meuse data
# https://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf

meusetut <- krige(formula=stormMarkTotal~1, locations=dsp2, newdata=grd, model=fve)
spplot(meusetut)
spplot(meusetut["var1.pred"])

meusetutRaster <- raster(meusetut)
meusetutRasterMask <- mask(meusetutRaster, ibwWatershedProj)
spplot(meusetutRaster)
spplot(meusetutRasterMask)

cellStats(meusetutRasterMask, stat = 'sum', na.rm = TRUE)
cellStats(meusetutRasterMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

plot(meusetutRaster)
plot(ibwWatershedProj, add=TRUE)

plot(meusetut)
plot(ibwWatershedProj, add=TRUE)

bbox(meusetutRaster)
bbox(ibwWatershedProj)


# from Texas

# Define the 1st order polynomial equation
f.1 <- as.formula(stormMarkTotal ~ X + Y) 

# Add X and Y to P
dsp2$X <- coordinates(dsp2)[,1]
dsp2$Y <- coordinates(dsp2)[,2]

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, dsp2, cloud = FALSE, cutoff=1000000, width=5)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))


fit.variogram(var.smpl,
              fit.ranges = FALSE,
              fit.sills = FALSE,
              vgm(psill=85, model="Sph", range=75, nugget=20))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))


writeOGR(dsp2, '~/Desktop/rout/', driver="ESRI Shapefile", layer = "dsp2")
writeOGR(ibwWatershedProj, '~/Desktop/rout/', driver="ESRI Shapefile", layer = "ibw")


# functionalize spatial

# try here with only a single storm, note be sure long precedes lat !!!
singleStorm <- mcfcdDailyRainfallStorms %>%
  filter(stormMark == 11)

# lat long to coordinates
dsp <- SpatialPoints(singleStorm[,c("long", "lat")], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 

# spatial points and other data to sp data frame
dsp2 <- SpatialPointsDataFrame(dsp, singleStorm)

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(dsp2, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

# Add dsp2 projection information to the empty grid
proj4string(grd) <- proj4string(dsp2)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(stormMarkTotal ~ 1, dsp2, newdata=grd, idp=2.0)

# Leave-one-out validation routine
IDW.out <- vector(length = length(dsp2))
for (i in 1:length(dsp2)) {
  IDW.out[i] <- idw(stormMarkTotal ~ 1, dsp2[-i,], dsp2[i,], idp=2.0)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ dsp2$stormMarkTotal, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ dsp2$stormMarkTotal), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Compute RMSE
sqrt(sum((IDW.out - dsp2$stormMarkTotal)^2) / length(dsp2))

# RMSE
# idp 0.0 = 6.224243 - should this be eq. to nearest neighbor? produces a nonsensical map
# idp 1.0 = 5.345751
# idp 2.0 = 4.339399
# idp 3.0 = 3.915036
# idp 4.0 = 3.845967

# Convert to raster object then clip to Texas
pidwraster <- raster(P.idw)
spplot(pidwraster)
pidwrasterMask <- mask(pidwraster, ibwWatershedProj)
cellStats(pidwrasterMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

plot(pidwraster)
plot(ibwWatershedProj, add=TRUE)

# California

# nearest neighbor

grdraster <- raster(grd) # rasterize the sample grid
gs <- gstat(formula=stormMarkTotal~1, locations=dsp2, nmax=5, set=list(idp = 0))
nn <- interpolate(grdraster, gs)
spplot(nn)
nnMask <- mask(nn, ibwWatershedProj)
cellStats(nnMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

# idw - this appears similar to the Texas tutorial: P.idw <- gstat::idw(stormMarkTotal ~ 1, dsp2, newdata=grd, idp=2.0)
gs2 <- gstat(formula=stormMarkTotal~1, locations=dsp2)
nn2 <- interpolate(grdraster, gs2) # this is a raster
spplot(nn2)
nn2Mask <- mask(nn2, ibwWatershedProj)
cellStats(nn2Mask, stat = 'mean', na.rm = TRUE) # this is the ticket!


# null model for comparing RMSE
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(dsp2$stormMarkTotal), dsp2$stormMarkTotal)

rmsenn <- rep(NA, 5)
kf <- kfold(nrow(dsp2))
for (k in 1:5) {
  test <- dsp2[kf == k, ]
  train <- dsp2[kf != k, ]
  gscv <- gstat(formula=stormMarkTotal~1, locations=train, nmax=5, set=list(idp = 0))
  p <- predict(gscv, test)$var1.pred
  rmsenn[k] <- RMSE(test$stormMarkTotal, p)
}

rmsenn
mean(rmsenn)
1 - (mean(rmsenn) / null)

# VARIOGRAMS - these are relevant for kriging
vario <- gstat(formula=stormMarkTotal~1, locations=dsp2)
v <- variogram(vario, width=5)
head(v)
plot(v)

fve <- fit.variogram(v, vgm(psill = 85, model = "Sph", range = 75, nugget = 20))
plot(v, fve)

fve <- fit.variogram(v, vgm(psill = 85, model = "Exp", range = 75, nugget = 20))
plot(v, fve)

# Use variogram fve in a kriging interpolation
k <- gstat(formula=stormMarkTotal~1, locations=dsp2, model=fve)
# predicted values
kp <- predict(k, grd)
## [using ordinary kriging]
spplot(kp)

# ....which, is the same as below from the gstat vignette with the meuse data
# https://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf

meusetut <- krige(formula=stormMarkTotal~1, locations=dsp2, newdata=grd, model=fve)
spplot(meusetut)
spplot(meusetut["var1.pred"])

meusetutRaster <- raster(meusetut)
meusetutRasterMask <- mask(meusetutRaster, ibwWatershedProj)
spplot(meusetutRaster)
spplot(meusetutRasterMask)

cellStats(meusetutRasterMask, stat = 'sum', na.rm = TRUE)
cellStats(meusetutRasterMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

plot(meusetutRaster)
plot(ibwWatershedProj, add=TRUE)

plot(meusetut)
plot(ibwWatershedProj, add=TRUE)

bbox(meusetutRaster)
bbox(ibwWatershedProj)


# from Texas

# Define the 1st order polynomial equation
f.1 <- as.formula(stormMarkTotal ~ X + Y) 

# Add X and Y to P
dsp2$X <- coordinates(dsp2)[,1]
dsp2$Y <- coordinates(dsp2)[,2]

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, dsp2, cloud = FALSE, cutoff=1000000, width=5)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))


fit.variogram(var.smpl,
              fit.ranges = FALSE,
              fit.sills = FALSE,
                          vgm(psill=85, model="Sph", range=75, nugget=20))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))


# ibw
ibwWatershed <- readOGR(dsn='Dropbox/SNAZ meeting materials/indianBendWash/data/ibw_rainfall_GIS/IBW_atCurry/', layer='usgs_at_curry')
proj4string(ibwWatershed)
dsp2proj <- CRS(proj4string(dsp2))
ibwWatershedProj <- spTransform(ibwWatershed, dsp2proj)
proj4string(ibwWatershedProj)


writeOGR(dsp2, '~/Desktop/rout/', driver="ESRI Shapefile", layer = "dsp2")
writeOGR(ibwWatershedProj, '~/Desktop/rout/', driver="ESRI Shapefile", layer = "ibw")


# compare TX and CA approaches ----

# README 

# The code in this section is related to comparing approaches to interpolation
# between the tutorials focusing on the California and Texas data sets. Not
# require for the ppt_interpolation function but there is some insight to the
# development of the function that could be helpful if we need to revisit these
# calculations.

# Texas

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(dsp2, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

# Add dsp2 projection information to the empty grid
proj4string(grd) <- proj4string(dsp2)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(stormMarkTotal ~ 1, dsp2, newdata=grd, idp=2.0)
pidwraster <- raster(P.idw)
spplot(pidwraster)
pidwrasterMask <- mask(pidwraster, ibwWatershedProj)
cellStats(pidwrasterMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

# California

# nmax = 5, idp = 0

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(dsp2, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

# Add dsp2 projection information to the empty grid
proj4string(grd) <- proj4string(dsp2)

grdraster <- raster(grd) # rasterize the sample grid
gs <- gstat(formula=stormMarkTotal~1, locations=dsp2, nmax=5, set=list(idp = 0))
nn <- interpolate(grdraster, gs)
spplot(nn)
nnMask <- mask(nn, ibwWatershedProj)
cellStats(nnMask, stat = 'mean', na.rm = TRUE) # this is the ticket!

# nmax = 5, idp = 2

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(dsp2, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

# Add dsp2 projection information to the empty grid
proj4string(grd) <- proj4string(dsp2)

grdraster <- raster(grd) # rasterize the sample grid
gs2 <- gstat(formula=stormMarkTotal~1, locations=dsp2, nmax=5, set=list(idp = 2.0))
nn2 <- interpolate(grdraster, gs2)
spplot(nn2)
nnMask2 <- mask(nn2, ibwWatershedProj)
cellStats(nnMask2, stat = 'mean', na.rm = TRUE) # this is the ticket!

# this one below where we set idp = 2.0 (same as Texas) and did not specify nmax
# seems to yield similar (but not identical) results to the Texas approach.
# Would be nice if we could get the number of neighbors from the TX approach as
# that parameter could be the difference

# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(dsp2, "regular", n = 50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

# Add dsp2 projection information to the empty grid
proj4string(grd) <- proj4string(dsp2)

grdraster <- raster(grd) # rasterize the sample grid
gs3 <- gstat(formula=stormMarkTotal~1, locations=dsp2, set=list(idp = 2.0))
nn3 <- interpolate(grdraster, gs3)
spplot(nn3)
nnMask3 <- mask(nn3, ibwWatershedProj)
cellStats(nnMask3, stat = 'mean', na.rm = TRUE) # this is the ticket!

# so let's see if we can optimize for idp and ignore nmax using the California
# approach

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

# CA model: optimize for idp and nmax
f1 <- function(x, test, train) {
  nmx <- x[1]
  idp <- x[2]
  if (nmx < 1) return(Inf)
  if (idp < .001) return(Inf)
  m <- gstat(formula=OZDLYAV~1, locations=train, nmax=nmx, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$OZDLYAV, p)
}

set.seed(20150518)
i <- sample(nrow(aq), 0.2 * nrow(aq))
tst <- aq[i,]
trn <- aq[-i,]
opt <- optim(c(8, .5), f1, test=tst, train=trn)


# new model optimize for idp only
f1 <- function(x, test, train) {
  idp <- x
  if (idp < .001) return(Inf)
  m <- gstat(formula=stormMarkTotal~1, locations=train, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$stormMarkTotal, p)
}

set.seed(42)
i <- sample(nrow(dsp2), 0.2 * nrow(dsp2))
tst <- dsp2[i,]
trn <- dsp2[-i,]
# opt <- optim(2, f1, test=tst, train=trn) # default not appropriate for single-dimensional optimization
opt <- optim(2, f1, test=tst, train=trn, method = "Brent", lower = 0, upper = 10) # use Brent for 1-dimensional, which needs limits
opt$par

# okay, so now how does it look with our optimized idp of 7.2

grdraster <- raster(grd) # rasterize the sample grid
gs4 <- gstat(formula=stormMarkTotal~1, locations=dsp2, set=list(idp = 7.2))
nn4 <- interpolate(grdraster, gs4)
spplot(nn4)
nnMask4 <- mask(nn4, ibwWatershedProj)
cellStats(nnMask4, stat = 'mean', na.rm = TRUE)


# can we get closer to Katie's nearest neighbor value of 25.7 for storm 14?
# nope! but we are not sure of her start and end dates

# remove desert ridge
singleStorm <- singleStorm %>% 
  filter(stationDesc != "DesertRidge")

grdraster <- raster(grd) # rasterize the sample grid
gs5 <- gstat(formula=stormMarkTotal~1, locations=dsp2, nmax=Inf, set=list(idp = 0))
nn5 <- interpolate(grdraster, gs5)
spplot(nn5)
nnMask5 <- mask(nn5, ibwWatershedProj)
cellStats(nnMask5, stat = 'mean', na.rm = TRUE)
