#### Cleaning OSM Data Alternative (Length) ####
#### February 20, 2021 ####

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

## Load data
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

## Build query
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/open_street_map")

# Extract all highways (354 Mb, ~5 mins)
osm_highway <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_xml(file = 'highway.osm')

osm_motorway <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  osmdata_xml(file = 'motorway.osm')

q <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
  add_osm_feature(key = "highway", value = "motorway")

dat_sp <- osmdata_sp(q, "motorway.osm")

#q <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
#  add_osm_feature(key = "highway")
#dat_sp <- osmdata_sp(q, "highway.osm") ## WARNING: this step takes ~4 hours

roads <- dat_sp$osm_lines
#writeOGR(roads, dsn = "tempdir", driver = "ESRI Shapefile")

roads <- spTransform(roads, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))

# Create blank raster
rs <- raster(extent(roads), crs = projection(roads))
rs[] <- 1:ncell(rs)

# Intersect lines with raster "polygons" and add length to new line segments
rsp <- rasterToPolygons(rs)
rp <- intersect(roads, rsp)
rp$length <- gLength(rp, byid = TRUE) / 1000
x <- tapply(rp$length, rp$layer.2, sum)
r <- raster(rs)
r[as.integer(names(x))] <- x

## Project to grids
rLength2 <- projectRaster(r, grids)

## Crop and mask
raster <- crop(rLength2, grids)
masked <- mask(rLength2, grids)
