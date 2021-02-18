#### Cleaning Elevation Data ####
#### February 17, 2021 ####

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


## Load Grids
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/elevation")

## output a text file about the metadata (only need to do once)

#nc <- nc_open("N18W099.SRTMGL1_NC.nc")
#{
#  sink("N18W099.SRTMGL1_NC.nc.txt")
#  print(nc)
#  sink()
#}

## Read in nc files (6 total), merge into a single one
ele_a <- raster("N18W099.SRTMGL1_NC.nc", varname = "SRTMGL1_DEM")
ele_b <- raster("N18W100.SRTMGL1_NC.nc", varname = "SRTMGL1_DEM")
ele_c <- raster("N19W099.SRTMGL1_NC.nc", varname = "SRTMGL1_DEM")
ele_d <- raster("N19W100.SRTMGL1_NC.nc", varname = "SRTMGL1_DEM")
ele_e <- raster("N20W099.SRTMGL1_NC.nc", varname = "SRTMGL1_DEM")
ele_f <- raster("N20W100.SRTMGL1_NC.nc", varname = "SRTMGL1_DEM")
ele <- merge(ele_a, ele_b, ele_c, ele_d, ele_e, ele_f)

## Project to the grid (via bilinear interpolation)
ele_grid <- projectRaster(ele, grids)

## Match the grids ()
elevation <- mask(ele_grid, grids)

elevation <- as.data.frame(rasterToPoints(elevation[[1]]))
names(elevation)[3] <- "elevation"

#write_csv(elevation, "elevation_grid_matched.csv")
