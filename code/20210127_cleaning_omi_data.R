#### Cleaning NASA OMI Data ####
#### January 27, 2021 ####

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/omi/3_OMNO2d")
options(mc.cores=parallel::detectCores())

#### Load packages ####
library(tidyverse)
library(here)
library(stringr)
library(sf)
library(sp)
library(rgdal) 
library(glue)
library(raster)
library(stars)
library(gdalUtils)
library(data.table)
library(remotes)
library(fst)
library(gdalUtils)
library(lubridate)
library(ncdf4)

# Load all model functions (from Ian's helper functions)
paths <- here("code/sample/ian/helpers") %>%
  list.files(pattern = "*\\.R$", full.names = TRUE)
for (p in paths) {
  source(p)
}
rm(paths, p)


## Load a single OMI NC4 dataset
## Match it to the grid
## Output it as a points in csv format
## Merge the csv files



#### Opening a single file ####
## output a text file about the metadata
nc <- nc_open('OMI-Aura_L3-OMNO2d_2010m0424_v003-2019m1121t234332.he5.SUB.nc4')
{
  sink("OMI-Aura_L3-OMNO2d_2005m0101_v003-2019m1120t182907.he5.SUB_metadata.txt")
  print(nc)
  sink()
}

## Read in grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

## Read as raster
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/omi/3_OMNO2d")
nc <- raster("OMI-Aura_L3-OMNO2d_2010m0424_v003-2019m1121t234332.he5.SUB.nc4", 
             varname = "ColumnAmountNO2TropCloudScreened",
             crs = 4326)

## Project to the grid (via bilinear interpolation)
nc <- projectRaster(nc, grids)

## Match the grids
nc.new <- mask(nc, grids)

## Read as points
nc.points <- as.data.frame(rasterToPoints(nc.new))
names(nc.points)[3] <- "omi_no2"

## Add date information
name <- "OMI-Aura_L3-OMNO2d_2010m0424_v003-2019m1121t234332.he5.SUB.nc4"
date <- sub("m", "", str_split_fixed(name, "\\_",4)[3])
nc.points$date <- as.Date(date, format = "%Y%m%d")
nc.points <- nc.points[, c(1, 2, 4, 3)]

## save file in here
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/omi/3_OMNO2d/grid_matched")
write.csv(nc.points, "sample.csv", row.names = FALSE)


#### Looping the procedure ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/omi/3_OMNO2d/")
options(mc.cores=parallel::detectCores())

files <- dir(pattern=".nc4")
files.length <- length(files)

for (i in  1:files.length){
  nc.i <- raster(files[i],
  varname = "ColumnAmountNO2TropCloudScreened",
  crs = 4326)
  nc.i <- projectRaster(nc.i, grids)
  nc.new.i <- mask(nc.i, grids)
  nc.points.i <- as.data.frame(rasterToPoints(nc.new.i))
  names(nc.points.i)[3] <- "omi_no2"
  
  if(nrow(nc.points.i) != 0){
    nc.points.i$date <- sub("m", "", str_split_fixed(files[i], "\\_",4)[3])
    nc.points.i$date <- as.Date(nc.points.i$date, format = "%Y%m%d")
    nc.points.i <- nc.points.i[, c(1, 2, 4, 3)]
    nm1 <- paste(files[i], ".csv", sep="")
    write.csv(nc.points.i, file = nm1 ,row.names = FALSE)
  }
}


#### Processing ozone data ####
