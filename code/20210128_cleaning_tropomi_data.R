#### Cleaning NASA TROPOMI Data ####
#### January 28, 2021 ####

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/satellite_data/tropomi/1_S5P_L2__NO2___")
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

## output a text file about the metadata
nc <- nc_open("S5P_OFFL_L2__NO2____20181017T190205_20181017T204334_05241_01_010200_20181024T141528.SUB.nc4")
{
  sink("S5P_OFFL_L2__NO2____20181017T190205_20181017T204334_05241_01_010200_20181024T141528.SUB_metadata.txt")
  print(nc)
  sink()
}

## Read in grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

## Read as raster
nc <- raster("S5P_OFFL_L2__NO2____20181017T190205_20181017T204334_05241_01_010200_20181024T141528.SUB.nc4", 
             varname = "PRODUCT/nitrogendioxide_tropospheric_column",
             crs = 4326)

## Level 2 data appears to be much more difficult to process...so I will leave this one alone for now




