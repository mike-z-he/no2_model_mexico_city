#### Cleaning GTFS Data ####
#### February 26, 2021 ####

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
library(osmdata)
library(rgeos)
library(spatstat)
library(maptools)
library(gtfstools) ## Vignette available here: http://cran.ms.unimelb.edu.au/web/packages/gtfstools/vignettes/gtfstools.html



#### Experimenting with a single set of data ####
## Input CRS: note this has to be identical with grid CRS for st_transform later on
sinu_crs = structure(list(input = "unknown", wkt = 'PROJCRS["unknown",BASEGEOGCRS["unknown",DATUM["unknown",ELLIPSOID["unknown",6371007.181,0,LENGTHUNIT["metre",1,ID["EPSG",9001]]]],PRIMEM["Greenwich",0,ANGLEUNIT["degree",0.0174532925199433,ID["EPSG",9122]]]],CONVERSION["Sinusoidal",METHOD["Sinusoidal"],PARAMETER["Longitude of natural origin",0,ANGLEUNIT["degree",0.0174532925199433],ID["EPSG",8802]],PARAMETER["False easting",0,LENGTHUNIT["metre",1],ID["EPSG",8806]],PARAMETER["False northing",0,LENGTHUNIT["metre",1],ID["EPSG",8807]]],CS[Cartesian,2],AXIS["easting",east,ORDER[1],LENGTHUNIT["metre",1,ID["EPSG", 9001]]],AXIS["northing",north,ORDER[2],LENGTHUNIT["metre",1,ID["EPSG",9001]]]]'), class = "crs")

## Load grids
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- read_stars("grd_1km_rst.tif")
grids <- st_as_sf(grids)
grids$id = 1:nrow(grids)

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/gtfs")
gtfs <- read_gtfs("20131021.zip", files = c("shapes", "stops", "trips"))

## Get route data
trip_geom <- get_trip_geometry(gtfs, file = "shapes")
plot(trip_geom$geometry)
trip_geom <- st_transform(trip_geom, sinu_crs)

## Calculate route per pixel
roads <- st_intersection(trip_geom, grids)
roads$length_m <- st_length(roads)

# Assign to grid
grid_pol = left_join(grids %>% as.data.frame(), roads %>% as.data.frame(), by = "id")
grid_pol$geometry.y <- NULL
grid_pol <- st_sf(grid_pol, sf_column_name = "geometry.x")

## Get bus stop data
stops <- gtfs$stops
stops <- st_as_sf(gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops <- st_transform(stops, sinu_crs)
plot(stops$geometry)

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grid_ras <- raster("grd_1km_rst.tif")

bus_stops <- st_intersection(stops, grids)
bus_points <- as_Spatial(bus_stops$geometry)

## Do count
bus_counts <- rasterize(bus_points, grid_ras, fun='count')
bus_counts <- as.data.frame(rasterToPoints(bus_counts))

## Merge into a single dataframe
dta <- data.frame(grid_pol$grd_1km_rst.tif.V2.x, grid_pol$grd_1km_rst.tif.V3.x, grid_pol$length_m, grid_pol$id)
dta <- dta[!duplicated(dta$grid_pol.id),]
dta$grid_pol.id <- NULL
dta <- dta %>%
  rename(x = grid_pol.grd_1km_rst.tif.V2.x,
         y = grid_pol.grd_1km_rst.tif.V3.x,
         bus_length = grid_pol.length_m
         )
dta <- merge(dta, bus_counts, by = c("x", "y"), all.x = TRUE)

name <- "20131021.zip"
date <- as.Date(substr(name, 1, 8), format = "%Y%m%d")

dta$date <- date
dta <- dta[, c(1, 2, 4, 3)]