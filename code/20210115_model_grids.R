#### Making model grids for the NO2 model ####
#### January 15, 2021 ####

### Much of the code used here is adapted from Ian's scripts, reorganized and
### combined into files that aligns with my work flow

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_modeling")
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


## Load Mexico City shapefile
roi <- here("data/mxcity_megalopolis/mxcity_megalopolis.shp") %>%
  st_read(quiet = TRUE) %>%
  st_transform(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")


## Load MODIS grids
pxs <- here("data/modis_grids") %>%
  list.files()

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_modeling/data/modis_grids")
dta <- gdal_subdatasets("MCD19A1.A2005002.h08v06.006.2018032190302.hdf") %>%
  dplyr::first() %>%
  raster::raster() %>%
  raster::raster()

dta2 <- gdal_subdatasets("MCD19A1.A2005002.h08v07.006.2018032190044.hdf") %>%
  dplyr::first() %>%
  raster::raster() %>%
  raster::raster()

dta3 <- merge(dta, dta2)
pxs <- dta3


## Crop the AOD pixels to the study area
pxs %<>% raster::crop(roi, snap = "out") %>%
  setValues(NA)


## Rasterizing
cells <- rasterize(roi, pxs)


## Get the coordinates of the centroid of each cell in the study area
pts <- raster::rasterToPoints(cells, spatial = TRUE) %>%
  st_as_sf()
xy <- st_coordinates(pts) %>%
  as.data.table() %>%
  setnames(c("sinu_x", "sinu_y"))
lon_lat <- st_transform(pts, crs = 4326) %>%
  st_coordinates() %>%
  as.data.table() %>%
  setnames(c("lon", "lat"))


## Assign a unique ID based on latitude and longitude
grd_1km <- cbind(xy, lon_lat)


## Save grid geometry
# Points (cell centroids)
pts <- st_as_sf(grd_1km, coords = c("sinu_x", "sinu_y"), crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs", remove = FALSE)

# Raster (cells)
# Must save as "FLT8S" to ensure ID maintains precision
rst <- as(pts, "Spatial") %>%
  sp::`gridded<-`(TRUE) %>%
  raster::stack()

# Polygons (cell borders)
pys <- raster::rasterToPolygons(rst) %>%
  st_as_sf() %>%
  dplyr::arrange(sinu_id) # sort by ID to match grd and pts

# Save grid
#save_safely(grd_1km, write_fst, grd_path, compress = 100)