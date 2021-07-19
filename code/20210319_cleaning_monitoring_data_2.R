#### Cleaning Monitoring Data, Part 2 ####
#### March 19, 2021 ####

## Coordinates of daily time series for monitoring data available
## Goal here is to match them onto our existing grids

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



#### Load data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grid <- read_stars("grd_1km_rst.tif")
grid <- st_as_sf(grid)
grid$id = 1:nrow(grid)

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
dta <- read_csv("monitor_time_series.csv")


#### Processing data ####
# Transform into grid coordinates
dta <- st_as_sf(dta, coords = c("long", "lat"), crs = 4326)
dta <- st_transform(dta, crs(grid))

# Intersect grid with polygon
result <- st_intersection(dta, grid)

# Clean results
output <- data.frame(result)

output <- output %>%
  rename(x = grd_1km_rst.tif.V2,
         y = grd_1km_rst.tif.V3,
         monitor_no2 = no2,
  ) %>%
  dplyr::select(x, y, date, monitor, monitor_no2)

write_csv(output, "monitor_grid_matched.csv")
