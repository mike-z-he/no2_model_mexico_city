#### Cleaning ERA5 Boundary Layer/Cloud Data ####
#### February 5, 2021 ####

## this is largely similar in structure to the other ERA5 script
## but because this data was downloaded on a separate file
## i am keeping a separate script for it

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ecmwf/3_blh_cloud")
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



#### Processing Boundary Layer Height Data ####
## Read in grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ecmwf/3_blh_cloud")
## output a text file about the metadata (only need to do once)
#nc <- nc_open("0000_0900.nc")
#{
#  sink("0000_0900.nc.txt")
#  print(nc)
#  sink()
#}

## Output as a brick
blh_00_09 <- brick("0000_0900.nc", varname = "blh")
blh_10_19 <- brick("1000_1900.nc", varname = "blh")
blh_20_23 <- brick("2000_2300.nc", varname = "blh")

## generate indexes for subsetting
## 54780 total entries for 1st two files
## multiples of 5478, by 1
index_1 <- seq(1, 54780, 10)
index_2 <- seq(2, 54780, 10)
index_3 <- seq(3, 54780, 10)
index_4 <- seq(4, 54780, 10)
index_5 <- seq(5, 54780, 10)
index_6 <- seq(6, 54780, 10)
index_7 <- seq(7, 54780, 10)
index_8 <- seq(8, 54780, 10)
index_9 <- seq(9, 54780, 10)
index_10 <- seq(10, 54780, 10)
index_11 <- seq(1, 21912, 4)
index_12 <- seq(2, 21912, 4)
index_13 <- seq(3, 21912, 4)
index_14 <- seq(4, 21912, 4)

## Subset
blh_00 <- brick(subset(blh_00_09, index_1))
blh_01 <- brick(subset(blh_00_09, index_2))
blh_02 <- brick(subset(blh_00_09, index_3))
blh_03 <- brick(subset(blh_00_09, index_4))
blh_04 <- brick(subset(blh_00_09, index_5))
blh_05 <- brick(subset(blh_00_09, index_6))
blh_06 <- brick(subset(blh_00_09, index_7))
blh_07 <- brick(subset(blh_00_09, index_8))
blh_08 <- brick(subset(blh_00_09, index_9))
blh_09 <- brick(subset(blh_00_09, index_10))
blh_10 <- brick(subset(blh_10_19, index_1))
blh_11 <- brick(subset(blh_10_19, index_2))
blh_12 <- brick(subset(blh_10_19, index_3))
blh_13 <- brick(subset(blh_10_19, index_4))
blh_14 <- brick(subset(blh_10_19, index_5))
blh_15 <- brick(subset(blh_10_19, index_6))
blh_16 <- brick(subset(blh_10_19, index_7))
blh_17 <- brick(subset(blh_10_19, index_8))
blh_18 <- brick(subset(blh_10_19, index_9))
blh_19 <- brick(subset(blh_10_19, index_10))
blh_20 <- brick(subset(blh_20_23, index_11))
blh_21 <- brick(subset(blh_20_23, index_12))
blh_22 <- brick(subset(blh_20_23, index_13))
blh_23 <- brick(subset(blh_20_23, index_14))

## Average across the raster bricks (and preserve names and date/time)
blh_averaged <- mosaic(blh_00, blh_01, blh_02, blh_03, blh_04,
                          blh_05, blh_06, blh_07, blh_08, blh_09,
                          blh_10, blh_11, blh_12, blh_13, blh_14, 
                          blh_15, blh_16, blh_17, blh_18, blh_19, 
                          blh_20, blh_21, blh_22, blh_23, fun = mean)
names(blh_averaged) <- names(blh_00)

## Project to the grid (via bilinear interpolation)
blh_grid <- projectRaster(blh_averaged, grids)

## Match the grids
blh <- mask(blh_grid, grids)



#### Processing Cloud Data ####
## Output as a brick
cloud_00_09 <- brick("0000_0900.nc", varname = "tcc")
cloud_10_19 <- brick("1000_1900.nc", varname = "tcc")
cloud_20_23 <- brick("2000_2300.nc", varname = "tcc")

## Subset
cloud_00 <- brick(subset(cloud_00_09, index_1))
cloud_01 <- brick(subset(cloud_00_09, index_2))
cloud_02 <- brick(subset(cloud_00_09, index_3))
cloud_03 <- brick(subset(cloud_00_09, index_4))
cloud_04 <- brick(subset(cloud_00_09, index_5))
cloud_05 <- brick(subset(cloud_00_09, index_6))
cloud_06 <- brick(subset(cloud_00_09, index_7))
cloud_07 <- brick(subset(cloud_00_09, index_8))
cloud_08 <- brick(subset(cloud_00_09, index_9))
cloud_09 <- brick(subset(cloud_00_09, index_10))
cloud_10 <- brick(subset(cloud_10_19, index_1))
cloud_11 <- brick(subset(cloud_10_19, index_2))
cloud_12 <- brick(subset(cloud_10_19, index_3))
cloud_13 <- brick(subset(cloud_10_19, index_4))
cloud_14 <- brick(subset(cloud_10_19, index_5))
cloud_15 <- brick(subset(cloud_10_19, index_6))
cloud_16 <- brick(subset(cloud_10_19, index_7))
cloud_17 <- brick(subset(cloud_10_19, index_8))
cloud_18 <- brick(subset(cloud_10_19, index_9))
cloud_19 <- brick(subset(cloud_10_19, index_10))
cloud_20 <- brick(subset(cloud_20_23, index_11))
cloud_21 <- brick(subset(cloud_20_23, index_12))
cloud_22 <- brick(subset(cloud_20_23, index_13))
cloud_23 <- brick(subset(cloud_20_23, index_14))

## Average across the raster bricks (and preserve names and date/time)
cloud_averaged <- mosaic(cloud_00, cloud_01, cloud_02, cloud_03, cloud_04,
                         cloud_05, cloud_06, cloud_07, cloud_08, cloud_09,
                         cloud_10, cloud_11, cloud_12, cloud_13, cloud_14, 
                         cloud_15, cloud_16, cloud_17, cloud_18, cloud_19, 
                         cloud_20, cloud_21, cloud_22, cloud_23, fun = mean)
names(cloud_averaged) <- names(cloud_00)

## Project to the grid (via bilinear interpolation)
cloud_grid <- projectRaster(cloud_averaged, grids)

## Match the grids
cloud <- mask(cloud_grid, grids)



#### Loop and output both files####
## First, double check the two output files
blh
cloud

## Since files are same length, just pick one to use as "base" with x/y/time information
## cbind the rest

files.length <- dim(blh)[3]
dates <- as.data.frame(seq(as.Date("2005-01-01"), as.Date("2019-12-31"), by = 1))
names(dates)[1] <- "date"

for (i in  1:files.length){
  ## data frame all 6 variables
  blh.i <- as.data.frame(rasterToPoints(blh[[i]]))
  cloud.i <- as.data.frame(rasterToPoints(cloud[[i]]))
  
  ## date
  blh.i$date <- as.Date(substr(names(blh.i)[3], 2, 11), format = "%Y.%m.%d")
  
  ## change names
  names(blh.i)[3] <- "blh"
  names(cloud.i)[3] <- "cloud"
  
  ## use blh as "base", bind data together
  nc.i <- blh.i[, c(1, 2, 4, 3)]
  nc.i <- cbind(nc.i, cloud.i$cloud)
  
  ## change names (again)
  names(nc.i)[4] <- "blh"
  names(nc.i)[5] <- "cloud"
  
  ## output
  nm1 <- paste("era5_2_", dates$date[i], ".csv", sep="")
  write.csv(nc.i, file = nm1 ,row.names = FALSE)
}
