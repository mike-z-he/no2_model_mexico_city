#### Cleaning KNMI TROPOMI Data ####
#### March 18, 2021 ####

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/tropomi/KNMI")
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


#### Reading in single layer to explore data ####

## output a text file about the metadata (only needs to be done once)
#nc <- nc_open("s5p_l3_mexico_city.nc")
#{
#  sink("s5p_l3_mexico_city_metadata.txt")
#  print(nc)
#  sink()
#}

## Read in grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

## Read as raster
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/tropomi/KNMI")
no2_raw <- brick("s5p_l3_mexico_city.nc", varname = "no2_col")
weight_raw <- brick("s5p_l3_mexico_city.nc", varname = "weight")

## Project to the grid (via bilinear interpolation)
no2_grid <- projectRaster(no2_raw, grids)
no2_grid <- setZ(no2_grid, getZ(no2_raw))

weight_grid <- projectRaster(weight_raw, grids)
weight_grid <- setZ(weight_grid, getZ(weight_raw))

## Match the grids ()
no2 <- mask(no2_grid, grids)
weight <- mask(weight_grid, grids)

## Reformatting for a single CSV file
no2.1 <- as.data.frame(rasterToPoints(no2[[1]]))
no2.1$date <- as.Date(substr(names(no2.1)[3], 2, 11), format = "%Y.%m.%d")
names(no2.1)[3] <- "tropomi_no2"
no2.1 <-no2.1[, c(1, 2, 4, 3)]

weight.1 <- as.data.frame(rasterToPoints(weight[[1]]))
weight.1$date <- as.Date(substr(names(weight.1)[3], 2, 11), format = "%Y.%m.%d")
names(weight.1)[3] <- "tropomi_weight"
weight.1 <-weight.1[, c(1, 2, 4, 3)]

nc.1 <- left_join(no2.1, weight.1, by = c("x", "y", "date"))

## Looping procedure (note 2018-06-14 missing from TROPOMI dataset)
files.length <- dim(no2)[3] # should be 1030
dates <- as.data.frame(seq(as.Date("2018-05-01"), as.Date("2021-02-24"), by = 1))
names(dates)[1] <- "date"
dates <- dates %>%
  filter(date != as.Date("2018-06-14"))

for (i in 1:files.length){
  no2.i <- as.data.frame(rasterToPoints(no2[[i]]))
  
  if(nrow(no2.i) !=0){
    no2.i$date <- as.Date(substr(names(no2.i)[3], 2, 11), format = "%Y.%m.%d")
    names(no2.i)[3] <- "tropomi_no2"
    no2.i <-no2.i[, c(1, 2, 4, 3)]
    
    weight.i <- as.data.frame(rasterToPoints(weight[[i]]))
    weight.i$date <- as.Date(substr(names(weight.i)[3], 2, 11), format = "%Y.%m.%d")
    names(weight.i)[3] <- "tropomi_weight"
    weight.i <-weight.i[, c(1, 2, 4, 3)]
    
    nc.i <- left_join(no2.i, weight.i, by = c("x", "y", "date"))
    
    nm1 <- paste("tropomi_no2_", dates$date[i], ".csv", sep="")
    write.csv(nc.i, file = nm1 ,row.names = FALSE)
  }
}


