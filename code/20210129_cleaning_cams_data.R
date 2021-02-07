#### Cleaning CAMS global reanalysis Data ####
#### January 28, 2021 ####

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ecmwf/1_no_no2_o3")
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


#### Processing NO2 data ####
## Read in grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ecmwf/1_no_no2_o3")
## output a text file about the metadata (only need to do once)
#nc <- nc_open("0000.nc")
#{
#  sink("0000.nc.txt")
#  print(nc)
#  sink()
#}

## Output as a brick
no2_0000 <- brick("0000.nc", varname = "tcno2")
no2_0300 <- brick("0300.nc", varname = "tcno2")
no2_0600 <- brick("0600.nc", varname = "tcno2")
no2_0900 <- brick("0900.nc", varname = "tcno2")
no2_1200 <- brick("1200.nc", varname = "tcno2")
no2_1500 <- brick("1500.nc", varname = "tcno2")
no2_1800 <- brick("1800.nc", varname = "tcno2")
no2_2100 <- brick("2100.nc", varname = "tcno2")

## Average across the raster bricks (and preserve names and date/time)
no2_averaged <- mosaic(no2_0000, no2_0300, no2_0600, no2_0900, no2_1200, no2_1500, no2_1800, no2_2100, fun = mean)
names(no2_averaged) <- names(no2_0000)

## Project to the grid (via bilinear interpolation, 2 mins)
no2_grid <- projectRaster(no2_averaged, grids)
no2_grid <- setZ(no2_grid, getZ(no2_0000))

## Match the grids ()
no2 <- mask(no2_grid, grids)

## Reformatting for a single CSV file
nc.1 <- as.data.frame(rasterToPoints(no2[[1]]))
nc.1$date <- as.Date(substr(names(nc.1)[3], 2, 11), format = "%Y.%m.%d")
names(nc.1)[3] <- "cams_no2"
nc.1 <-nc.1[, c(1, 2, 4, 3)]

## Looping procedure
files.length <- dim(no2)[3]
dates <- as.data.frame(seq(as.Date("2005-01-01"), as.Date("2019-12-31"), by = 1))
names(dates)[1] <- "date"

for (i in  1:files.length){
  nc.i <- as.data.frame(rasterToPoints(no2[[i]]))
  nc.i$date <- as.Date(substr(names(nc.i)[3], 2, 11), format = "%Y.%m.%d")
  names(nc.i)[3] <- "cams_no2"
  nc.i <-nc.i[, c(1, 2, 4, 3)]
  nm1 <- paste("cams_no2_", dates$date[i], ".csv", sep="")
  write.csv(nc.i, file = nm1 ,row.names = FALSE)
}

