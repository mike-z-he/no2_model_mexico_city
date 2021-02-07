#### Cleaning ERA5 Meteorological Data ####
#### February 4, 2021 ####

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ecmwf/2_wind_temp_albedo_pressure_precip")
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


#### Processing U-Wind component data ####
## Read in grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ecmwf/2_wind_temp_albedo_pressure_precip")
## output a text file about the metadata (only need to do once)
#nc <- nc_open("0000_0100.nc")
#{
#  sink("0000_0100.nc.txt")
#  print(nc)
#  sink()
#}

## Output as a brick (in hourly format first)
wind_u_00_01 <- brick("0000_0100.nc", varname = "u10")
wind_u_02_03 <- brick("0200_0300.nc", varname = "u10")
wind_u_04_05 <- brick("0400_0500.nc", varname = "u10")
wind_u_06_07 <- brick("0600_0700.nc", varname = "u10")
wind_u_08_09 <- brick("0800_0900.nc", varname = "u10")
wind_u_10_11 <- brick("1000_1100.nc", varname = "u10")
wind_u_12_13 <- brick("1200_1300.nc", varname = "u10")
wind_u_14_15 <- brick("1400_1500.nc", varname = "u10")
wind_u_16_17 <- brick("1600_1700.nc", varname = "u10")
wind_u_18_19 <- brick("1800_1900.nc", varname = "u10")
wind_u_20_21 <- brick("2000_2100.nc", varname = "u10")
wind_u_22_23 <- brick("2200_2300.nc", varname = "u10")

## one hour of values stored as odd entries, the other hour of values stored as even entries
## generate odd and even indexes
odd_indexes <- seq(1, 10955, 2)
even_indexes <- seq(2, 10956, 2)

## Subset
wind_u_00 <- brick(subset(wind_u_00_01, odd_indexes))
wind_u_01 <- brick(subset(wind_u_00_01, even_indexes))
wind_u_02 <- brick(subset(wind_u_02_03, odd_indexes))
wind_u_03 <- brick(subset(wind_u_02_03, even_indexes))
wind_u_04 <- brick(subset(wind_u_04_05, odd_indexes))
wind_u_05 <- brick(subset(wind_u_04_05, even_indexes))
wind_u_06 <- brick(subset(wind_u_06_07, odd_indexes))
wind_u_07 <- brick(subset(wind_u_06_07, even_indexes))
wind_u_08 <- brick(subset(wind_u_08_09, odd_indexes))
wind_u_09 <- brick(subset(wind_u_08_09, even_indexes))
wind_u_10 <- brick(subset(wind_u_10_11, odd_indexes))
wind_u_11 <- brick(subset(wind_u_10_11, even_indexes))
wind_u_12 <- brick(subset(wind_u_12_13, odd_indexes))
wind_u_13 <- brick(subset(wind_u_12_13, even_indexes))
wind_u_14 <- brick(subset(wind_u_14_15, odd_indexes))
wind_u_15 <- brick(subset(wind_u_14_15, even_indexes))
wind_u_16 <- brick(subset(wind_u_16_17, odd_indexes))
wind_u_17 <- brick(subset(wind_u_16_17, even_indexes))
wind_u_18 <- brick(subset(wind_u_18_19, odd_indexes))
wind_u_19 <- brick(subset(wind_u_18_19, even_indexes))
wind_u_20 <- brick(subset(wind_u_20_21, odd_indexes))
wind_u_21 <- brick(subset(wind_u_20_21, even_indexes))
wind_u_22 <- brick(subset(wind_u_22_23, odd_indexes))
wind_u_23 <- brick(subset(wind_u_22_23, even_indexes))

## Average across the raster bricks (and preserve names and date/time)
wind_u_averaged <- mosaic(wind_u_00, wind_u_01, wind_u_02, wind_u_03, wind_u_04,
                          wind_u_05, wind_u_06, wind_u_07, wind_u_08, wind_u_09,
                          wind_u_10, wind_u_11, wind_u_12, wind_u_13, wind_u_14, 
                          wind_u_15, wind_u_16, wind_u_17, wind_u_18, wind_u_19, 
                          wind_u_20, wind_u_21, wind_u_22, wind_u_23, fun = mean)
names(wind_u_averaged) <- names(wind_u_00)

## Project to the grid (via bilinear interpolation)
wind_u_grid <- projectRaster(wind_u_averaged, grids)

## Match the grids
wind_u <- mask(wind_u_grid, grids)



#### Processing V-Wind component data ####
## Output as a brick (in hourly format first)
wind_v_00_01 <- brick("0000_0100.nc", varname = "v10")
wind_v_02_03 <- brick("0200_0300.nc", varname = "v10")
wind_v_04_05 <- brick("0400_0500.nc", varname = "v10")
wind_v_06_07 <- brick("0600_0700.nc", varname = "v10")
wind_v_08_09 <- brick("0800_0900.nc", varname = "v10")
wind_v_10_11 <- brick("1000_1100.nc", varname = "v10")
wind_v_12_13 <- brick("1200_1300.nc", varname = "v10")
wind_v_14_15 <- brick("1400_1500.nc", varname = "v10")
wind_v_16_17 <- brick("1600_1700.nc", varname = "v10")
wind_v_18_19 <- brick("1800_1900.nc", varname = "v10")
wind_v_20_21 <- brick("2000_2100.nc", varname = "v10")
wind_v_22_23 <- brick("2200_2300.nc", varname = "v10")

## Subset
wind_v_00 <- brick(subset(wind_v_00_01, odd_indexes))
wind_v_01 <- brick(subset(wind_v_00_01, even_indexes))
wind_v_02 <- brick(subset(wind_v_02_03, odd_indexes))
wind_v_03 <- brick(subset(wind_v_02_03, even_indexes))
wind_v_04 <- brick(subset(wind_v_04_05, odd_indexes))
wind_v_05 <- brick(subset(wind_v_04_05, even_indexes))
wind_v_06 <- brick(subset(wind_v_06_07, odd_indexes))
wind_v_07 <- brick(subset(wind_v_06_07, even_indexes))
wind_v_08 <- brick(subset(wind_v_08_09, odd_indexes))
wind_v_09 <- brick(subset(wind_v_08_09, even_indexes))
wind_v_10 <- brick(subset(wind_v_10_11, odd_indexes))
wind_v_11 <- brick(subset(wind_v_10_11, even_indexes))
wind_v_12 <- brick(subset(wind_v_12_13, odd_indexes))
wind_v_13 <- brick(subset(wind_v_12_13, even_indexes))
wind_v_14 <- brick(subset(wind_v_14_15, odd_indexes))
wind_v_15 <- brick(subset(wind_v_14_15, even_indexes))
wind_v_16 <- brick(subset(wind_v_16_17, odd_indexes))
wind_v_17 <- brick(subset(wind_v_16_17, even_indexes))
wind_v_18 <- brick(subset(wind_v_18_19, odd_indexes))
wind_v_19 <- brick(subset(wind_v_18_19, even_indexes))
wind_v_20 <- brick(subset(wind_v_20_21, odd_indexes))
wind_v_21 <- brick(subset(wind_v_20_21, even_indexes))
wind_v_22 <- brick(subset(wind_v_22_23, odd_indexes))
wind_v_23 <- brick(subset(wind_v_22_23, even_indexes))

## Average across the raster bricks (and preserve names and date/time)
wind_v_averaged <- mosaic(wind_v_00, wind_v_01, wind_v_02, wind_v_03, wind_v_04,
                          wind_v_05, wind_v_06, wind_v_07, wind_v_08, wind_v_09,
                          wind_v_10, wind_v_11, wind_v_12, wind_v_13, wind_v_14, 
                          wind_v_15, wind_v_16, wind_v_17, wind_v_18, wind_v_19, 
                          wind_v_20, wind_v_21, wind_v_22, wind_v_23, fun = mean)
names(wind_v_averaged) <- names(wind_v_00)

## Project to the grid (via bilinear interpolation)
wind_v_grid <- projectRaster(wind_v_averaged, grids)

## Match the grids
wind_v <- mask(wind_v_grid, grids)



#### Processing 2-m temperature data ####
## Output as a brick (in hourly format first)
temp_2m_00_01 <- brick("0000_0100.nc", varname = "t2m")
temp_2m_02_03 <- brick("0200_0300.nc", varname = "t2m")
temp_2m_04_05 <- brick("0400_0500.nc", varname = "t2m")
temp_2m_06_07 <- brick("0600_0700.nc", varname = "t2m")
temp_2m_08_09 <- brick("0800_0900.nc", varname = "t2m")
temp_2m_10_11 <- brick("1000_1100.nc", varname = "t2m")
temp_2m_12_13 <- brick("1200_1300.nc", varname = "t2m")
temp_2m_14_15 <- brick("1400_1500.nc", varname = "t2m")
temp_2m_16_17 <- brick("1600_1700.nc", varname = "t2m")
temp_2m_18_19 <- brick("1800_1900.nc", varname = "t2m")
temp_2m_20_21 <- brick("2000_2100.nc", varname = "t2m")
temp_2m_22_23 <- brick("2200_2300.nc", varname = "t2m")

## Subset
temp_2m_00 <- brick(subset(temp_2m_00_01, odd_indexes))
temp_2m_01 <- brick(subset(temp_2m_00_01, even_indexes))
temp_2m_02 <- brick(subset(temp_2m_02_03, odd_indexes))
temp_2m_03 <- brick(subset(temp_2m_02_03, even_indexes))
temp_2m_04 <- brick(subset(temp_2m_04_05, odd_indexes))
temp_2m_05 <- brick(subset(temp_2m_04_05, even_indexes))
temp_2m_06 <- brick(subset(temp_2m_06_07, odd_indexes))
temp_2m_07 <- brick(subset(temp_2m_06_07, even_indexes))
temp_2m_08 <- brick(subset(temp_2m_08_09, odd_indexes))
temp_2m_09 <- brick(subset(temp_2m_08_09, even_indexes))
temp_2m_10 <- brick(subset(temp_2m_10_11, odd_indexes))
temp_2m_11 <- brick(subset(temp_2m_10_11, even_indexes))
temp_2m_12 <- brick(subset(temp_2m_12_13, odd_indexes))
temp_2m_13 <- brick(subset(temp_2m_12_13, even_indexes))
temp_2m_14 <- brick(subset(temp_2m_14_15, odd_indexes))
temp_2m_15 <- brick(subset(temp_2m_14_15, even_indexes))
temp_2m_16 <- brick(subset(temp_2m_16_17, odd_indexes))
temp_2m_17 <- brick(subset(temp_2m_16_17, even_indexes))
temp_2m_18 <- brick(subset(temp_2m_18_19, odd_indexes))
temp_2m_19 <- brick(subset(temp_2m_18_19, even_indexes))
temp_2m_20 <- brick(subset(temp_2m_20_21, odd_indexes))
temp_2m_21 <- brick(subset(temp_2m_20_21, even_indexes))
temp_2m_22 <- brick(subset(temp_2m_22_23, odd_indexes))
temp_2m_23 <- brick(subset(temp_2m_22_23, even_indexes))

## Average across the raster bricks (and preserve names and date/time)
temp_2m_averaged <- mosaic(temp_2m_00, temp_2m_01, temp_2m_02, temp_2m_03, temp_2m_04,
                           temp_2m_05, temp_2m_06, temp_2m_07, temp_2m_08, temp_2m_09,
                           temp_2m_10, temp_2m_11, temp_2m_12, temp_2m_13, temp_2m_14, 
                           temp_2m_15, temp_2m_16, temp_2m_17, temp_2m_18, temp_2m_19, 
                           temp_2m_20, temp_2m_21, temp_2m_22, temp_2m_23, fun = mean)
names(temp_2m_averaged) <- names(temp_2m_00)

## Project to the grid (via bilinear interpolation)
temp_2m_grid <- projectRaster(temp_2m_averaged, grids)

## Match the grids
temp_2m <- mask(temp_2m_grid, grids)



#### Processing albedo data ####
## Output as a brick (in hourly format first)
albedo_00_01 <- brick("0000_0100.nc", varname = "asn")
albedo_02_03 <- brick("0200_0300.nc", varname = "asn")
albedo_04_05 <- brick("0400_0500.nc", varname = "asn")
albedo_06_07 <- brick("0600_0700.nc", varname = "asn")
albedo_08_09 <- brick("0800_0900.nc", varname = "asn")
albedo_10_11 <- brick("1000_1100.nc", varname = "asn")
albedo_12_13 <- brick("1200_1300.nc", varname = "asn")
albedo_14_15 <- brick("1400_1500.nc", varname = "asn")
albedo_16_17 <- brick("1600_1700.nc", varname = "asn")
albedo_18_19 <- brick("1800_1900.nc", varname = "asn")
albedo_20_21 <- brick("2000_2100.nc", varname = "asn")
albedo_22_23 <- brick("2200_2300.nc", varname = "asn")

## Subset
albedo_00 <- brick(subset(albedo_00_01, odd_indexes))
albedo_01 <- brick(subset(albedo_00_01, even_indexes))
albedo_02 <- brick(subset(albedo_02_03, odd_indexes))
albedo_03 <- brick(subset(albedo_02_03, even_indexes))
albedo_04 <- brick(subset(albedo_04_05, odd_indexes))
albedo_05 <- brick(subset(albedo_04_05, even_indexes))
albedo_06 <- brick(subset(albedo_06_07, odd_indexes))
albedo_07 <- brick(subset(albedo_06_07, even_indexes))
albedo_08 <- brick(subset(albedo_08_09, odd_indexes))
albedo_09 <- brick(subset(albedo_08_09, even_indexes))
albedo_10 <- brick(subset(albedo_10_11, odd_indexes))
albedo_11 <- brick(subset(albedo_10_11, even_indexes))
albedo_12 <- brick(subset(albedo_12_13, odd_indexes))
albedo_13 <- brick(subset(albedo_12_13, even_indexes))
albedo_14 <- brick(subset(albedo_14_15, odd_indexes))
albedo_15 <- brick(subset(albedo_14_15, even_indexes))
albedo_16 <- brick(subset(albedo_16_17, odd_indexes))
albedo_17 <- brick(subset(albedo_16_17, even_indexes))
albedo_18 <- brick(subset(albedo_18_19, odd_indexes))
albedo_19 <- brick(subset(albedo_18_19, even_indexes))
albedo_20 <- brick(subset(albedo_20_21, odd_indexes))
albedo_21 <- brick(subset(albedo_20_21, even_indexes))
albedo_22 <- brick(subset(albedo_22_23, odd_indexes))
albedo_23 <- brick(subset(albedo_22_23, even_indexes))

## Average across the raster bricks (and preserve names and date/time)
albedo_averaged <- mosaic(albedo_00, albedo_01, albedo_02, albedo_03, albedo_04,
                          albedo_05, albedo_06, albedo_07, albedo_08, albedo_09,
                          albedo_10, albedo_11, albedo_12, albedo_13, albedo_14, 
                          albedo_15, albedo_16, albedo_17, albedo_18, albedo_19, 
                          albedo_20, albedo_21, albedo_22, albedo_23, fun = mean)
names(albedo_averaged) <- names(albedo_00)

## Project to the grid (via bilinear interpolation)
albedo_grid <- projectRaster(albedo_averaged, grids)

## Match the grids
albedo <- mask(albedo_grid, grids)



#### Processing pressure data ####
## Output as a brick (in hourly format first)
pressure_00_01 <- brick("0000_0100.nc", varname = "sp")
pressure_02_03 <- brick("0200_0300.nc", varname = "sp")
pressure_04_05 <- brick("0400_0500.nc", varname = "sp")
pressure_06_07 <- brick("0600_0700.nc", varname = "sp")
pressure_08_09 <- brick("0800_0900.nc", varname = "sp")
pressure_10_11 <- brick("1000_1100.nc", varname = "sp")
pressure_12_13 <- brick("1200_1300.nc", varname = "sp")
pressure_14_15 <- brick("1400_1500.nc", varname = "sp")
pressure_16_17 <- brick("1600_1700.nc", varname = "sp")
pressure_18_19 <- brick("1800_1900.nc", varname = "sp")
pressure_20_21 <- brick("2000_2100.nc", varname = "sp")
pressure_22_23 <- brick("2200_2300.nc", varname = "sp")

## Subset
pressure_00 <- brick(subset(pressure_00_01, odd_indexes))
pressure_01 <- brick(subset(pressure_00_01, even_indexes))
pressure_02 <- brick(subset(pressure_02_03, odd_indexes))
pressure_03 <- brick(subset(pressure_02_03, even_indexes))
pressure_04 <- brick(subset(pressure_04_05, odd_indexes))
pressure_05 <- brick(subset(pressure_04_05, even_indexes))
pressure_06 <- brick(subset(pressure_06_07, odd_indexes))
pressure_07 <- brick(subset(pressure_06_07, even_indexes))
pressure_08 <- brick(subset(pressure_08_09, odd_indexes))
pressure_09 <- brick(subset(pressure_08_09, even_indexes))
pressure_10 <- brick(subset(pressure_10_11, odd_indexes))
pressure_11 <- brick(subset(pressure_10_11, even_indexes))
pressure_12 <- brick(subset(pressure_12_13, odd_indexes))
pressure_13 <- brick(subset(pressure_12_13, even_indexes))
pressure_14 <- brick(subset(pressure_14_15, odd_indexes))
pressure_15 <- brick(subset(pressure_14_15, even_indexes))
pressure_16 <- brick(subset(pressure_16_17, odd_indexes))
pressure_17 <- brick(subset(pressure_16_17, even_indexes))
pressure_18 <- brick(subset(pressure_18_19, odd_indexes))
pressure_19 <- brick(subset(pressure_18_19, even_indexes))
pressure_20 <- brick(subset(pressure_20_21, odd_indexes))
pressure_21 <- brick(subset(pressure_20_21, even_indexes))
pressure_22 <- brick(subset(pressure_22_23, odd_indexes))
pressure_23 <- brick(subset(pressure_22_23, even_indexes))

## Average across the raster bricks (and preserve names and date/time)
pressure_averaged <- mosaic(pressure_00, pressure_01, pressure_02, pressure_03, pressure_04,
                            pressure_05, pressure_06, pressure_07, pressure_08, pressure_09,
                            pressure_10, pressure_11, pressure_12, pressure_13, pressure_14, 
                            pressure_15, pressure_16, pressure_17, pressure_18, pressure_19, 
                            pressure_20, pressure_21, pressure_22, pressure_23, fun = mean)
names(pressure_averaged) <- names(pressure_00)

## Project to the grid (via bilinear interpolation)
pressure_grid <- projectRaster(pressure_averaged, grids)

## Match the grids
pressure <- mask(pressure_grid, grids)



#### Processing precipitation data ####
## Output as a brick (in hourly format first)
precip_00_01 <- brick("0000_0100.nc", varname = "tp")
precip_02_03 <- brick("0200_0300.nc", varname = "tp")
precip_04_05 <- brick("0400_0500.nc", varname = "tp")
precip_06_07 <- brick("0600_0700.nc", varname = "tp")
precip_08_09 <- brick("0800_0900.nc", varname = "tp")
precip_10_11 <- brick("1000_1100.nc", varname = "tp")
precip_12_13 <- brick("1200_1300.nc", varname = "tp")
precip_14_15 <- brick("1400_1500.nc", varname = "tp")
precip_16_17 <- brick("1600_1700.nc", varname = "tp")
precip_18_19 <- brick("1800_1900.nc", varname = "tp")
precip_20_21 <- brick("2000_2100.nc", varname = "tp")
precip_22_23 <- brick("2200_2300.nc", varname = "tp")

## Subset
precip_00 <- brick(subset(precip_00_01, odd_indexes))
precip_01 <- brick(subset(precip_00_01, even_indexes))
precip_02 <- brick(subset(precip_02_03, odd_indexes))
precip_03 <- brick(subset(precip_02_03, even_indexes))
precip_04 <- brick(subset(precip_04_05, odd_indexes))
precip_05 <- brick(subset(precip_04_05, even_indexes))
precip_06 <- brick(subset(precip_06_07, odd_indexes))
precip_07 <- brick(subset(precip_06_07, even_indexes))
precip_08 <- brick(subset(precip_08_09, odd_indexes))
precip_09 <- brick(subset(precip_08_09, even_indexes))
precip_10 <- brick(subset(precip_10_11, odd_indexes))
precip_11 <- brick(subset(precip_10_11, even_indexes))
precip_12 <- brick(subset(precip_12_13, odd_indexes))
precip_13 <- brick(subset(precip_12_13, even_indexes))
precip_14 <- brick(subset(precip_14_15, odd_indexes))
precip_15 <- brick(subset(precip_14_15, even_indexes))
precip_16 <- brick(subset(precip_16_17, odd_indexes))
precip_17 <- brick(subset(precip_16_17, even_indexes))
precip_18 <- brick(subset(precip_18_19, odd_indexes))
precip_19 <- brick(subset(precip_18_19, even_indexes))
precip_20 <- brick(subset(precip_20_21, odd_indexes))
precip_21 <- brick(subset(precip_20_21, even_indexes))
precip_22 <- brick(subset(precip_22_23, odd_indexes))
precip_23 <- brick(subset(precip_22_23, even_indexes))

## Average across the raster bricks (and preserve names and date/time)
precip_averaged <- mosaic(precip_00, precip_01, precip_02, precip_03, precip_04,
                          precip_05, precip_06, precip_07, precip_08, precip_09,
                          precip_10, precip_11, precip_12, precip_13, precip_14, 
                          precip_15, precip_16, precip_17, precip_18, precip_19, 
                          precip_20, precip_21, precip_22, precip_23, fun = mean)
names(precip_averaged) <- names(precip_00)

## Project to the grid (via bilinear interpolation)
precip_grid <- projectRaster(precip_averaged, grids)

## Match the grids
precip <- mask(precip_grid, grids)



#### Loop and output for all six files####
## First, double check the six output files
wind_u
wind_v
temp_2m
albedo
pressure
precip

## Since files are same length, just pick one to use as "base" with x/y/time information
## cbind the rest

files.length <- dim(wind_u)[3]
dates <- as.data.frame(seq(as.Date("2005-01-01"), as.Date("2019-12-31"), by = 1))
names(dates)[1] <- "date"

for (i in  1:files.length){
  ## data frame all 6 variables
  wind_u.i <- as.data.frame(rasterToPoints(wind_u[[i]]))
  wind_v.i <- as.data.frame(rasterToPoints(wind_v[[i]]))
  temp_2m.i <- as.data.frame(rasterToPoints(temp_2m[[i]]))
  albedo.i <- as.data.frame(rasterToPoints(albedo[[i]]))
  pressure.i <- as.data.frame(rasterToPoints(pressure[[i]]))
  precip.i <- as.data.frame(rasterToPoints(precip[[i]]))
  
  ## date
  wind_u.i$date <- as.Date(substr(names(wind_u.i)[3], 2, 11), format = "%Y.%m.%d")
  
  ## change names
  names(wind_u.i)[3] <- "wind_u"
  names(wind_v.i)[3] <- "wind_v"
  names(temp_2m.i)[3] <- "temp_2m"
  names(albedo.i)[3] <- "albedo"
  names(pressure.i)[3] <- "pressure"
  names(precip.i)[3] <- "precip"
  
  ## use wind_u as "base", bind data together
  nc.i <- wind_u.i[, c(1, 2, 4, 3)]
  nc.i <- cbind(nc.i, wind_v.i$wind_v)
  nc.i <- cbind(nc.i, temp_2m.i$temp_2m)
  nc.i <- cbind(nc.i, albedo.i$albedo)
  nc.i <- cbind(nc.i, pressure.i$pressure)
  nc.i <- cbind(nc.i, precip.i$precip)
  
  ## change names (again)
  names(nc.i)[5] <- "wind_v"
  names(nc.i)[6] <- "temp_2m"
  names(nc.i)[7] <- "albedo"
  names(nc.i)[8] <- "pressure"
  names(nc.i)[9] <- "precip"
  
  ## output
  nm1 <- paste("era5_", dates$date[i], ".csv", sep="")
  write.csv(nc.i, file = nm1 ,row.names = FALSE)
}
