#### Cleaning OSM Data ####
#### February 19, 2021 ####

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

## For Michael's code:
library(sf)
library(stars)
library(units)

#### Load data and build query ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

## Build query
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/open_street_map")

# Extract all highways (motorway is used as an example that actually runs)
osm_motorway <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
  add_osm_feature(key = "highway", value = "motorway") %>%
  osmdata_xml(file = 'motorway.osm')

# Additional roads (this is already 631 Mb)
osm_roads <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary", "unclassified",
                                             "residential", "motorway_link", "trunk_link", "primary_link",
                                             "secondary_link", "tertiary_link")) %>%
  osmdata_xml(file = 'roads.osm')

# All highways (this is 726 Mb, ~8Gb when extracted)
osm_highway <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_xml(file = 'highway.osm')

#q <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
#  add_osm_feature(key = "motorway")

#q <- opq(bbox = c(-100, 18.0, -97.0, 21.0)) %>%
#  add_osm_feature(key = "highway")



#### Road Length ####
## using https://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell/139316
dat_sp <- osmdata_sp(q, "motorway.osm")
#dat_sp <- osmdata_sp(q, "highway.osm") ## WARNING: this step takes ~5 hours

roads <- dat_sp$osm_lines
#roads <- spTransform(roads, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))

# Create blank raster
rs <- raster(extent(roads), crs = projection(roads))
rs[] <- 1:ncell(rs)

# Intersect lines with raster "polygons" and add length to new line segments
rsp <- rasterToPolygons(rs)
rp <- intersect(roads, rsp)
rp$length <- gLength(rp, byid = TRUE) / 1000 # this should convert to km?
x <- tapply(rp$length, rp$layer.2, sum)
r <- raster(rs)
r[as.integer(names(x))] <- x

## Project to grids
raster <- projectRaster(r, grids)

## Crop and mask
cropped <- crop(raster, grids)
masked <- mask(raster, grids)



#### Alternative Method ####
dat_sp <- osmdata_sp(q, "motorway.osm")
#bnd_wkt <- st_bbox(dat_sp) %>%
#  st_as_sfc() %>%
#  st_as_text()

## Extract lines data
lines <- dat_sp$osm_lines
lines <- spTransform(lines, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"))

## Estimate density
pspS1 <- as.psp(lines)
px <- pixellate(pspS1, eps = 850) # I did this to best match the number of grids we have

## Convert to raster
rLength <- raster(px, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")

## Project to grids
rLength2 <- projectRaster(rLength, grids)

## Crop and mask
cropped_2 <- crop(rLength2, grids)
masked_2 <- mask(rLength2, grids)



#### U


#### Using Michael's Code (Modified to skip PostGIS) ####
# Settings
sinu_crs = structure(list(input = "unknown", wkt = 'PROJCRS["unknown",BASEGEOGCRS["unknown",DATUM["unknown",ELLIPSOID["unknown",6371007.181,0,LENGTHUNIT["metre",1,ID["EPSG",9001]]]],PRIMEM["Greenwich",0,ANGLEUNIT["degree",0.0174532925199433,ID["EPSG",9122]]]],CONVERSION["Sinusoidal",METHOD["Sinusoidal"],PARAMETER["Longitude of natural origin",0,ANGLEUNIT["degree",0.0174532925199433],ID["EPSG",8802]],PARAMETER["False easting",0,LENGTHUNIT["metre",1],ID["EPSG",8806]],PARAMETER["False northing",0,LENGTHUNIT["metre",1],ID["EPSG",8807]]],CS[Cartesian,2],AXIS["easting",east,ORDER[1],LENGTHUNIT["metre",1,ID["EPSG", 9001]]],AXIS["northing",north,ORDER[2],LENGTHUNIT["metre",1,ID["EPSG",9001]]]]'), class = "crs")

# Reading data
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/open_street_map")
dat <- st_read("highway.osm", layer = "lines", quiet = TRUE)

# Area of interest
p1 = st_point(c(-100, 18.0))
p2 = st_point(c(-97.0, 21.0))
p = c(p1, p2)
aoi = st_bbox(p)
aoi = st_as_sfc(aoi)
st_crs(aoi) = 4326

# Transform OSM data
dat = st_transform(dat, sinu_crs)
aoi = st_transform(aoi, sinu_crs)

# Create grid
#aoi = st_transform(aoi, sinu_crs)
#grid = st_as_stars(aoi, dx = 1000, dy = 1000)
#grid[[1]][] = 1
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grid <- read_stars("grd_1km_rst.tif")
grid <- st_as_sf(grid)
grid$id = 1:nrow(grid)

# Grid to polygons
grid_pol = st_as_sf(grid)
grid_pol = grid_pol[aoi, ]
grid_pol$id = 1:nrow(grid_pol)
grid_pol$values = NULL

# Calculate road length per pixel
result <- st_intersection(dat, grid_pol)
result$length_m <- st_length(result)

# Assign to grid
#grid_pol2 = left_join(grid_pol %>% as.data.frame(), result %>% as.data.frame(), by = "id") ## why are there slightly more observations than grids after the join? (n = 111999 vs 105516)

grid_pol = left_join(grid_pol %>% as.data.frame(), result %>% as.data.frame(), by = "id")
grid_pol$geometry.y <- NULL
grid_pol <- st_sf(grid_pol, sf_column_name = "geometry.x") 

dta <- data.frame(grid_pol$grd_1km_rst.tif.V2.x, grid_pol$grd_1km_rst.tif.V3.x, grid_pol$length_m, grid_pol$id)

dta2 <- aggregate(x = dta$grid_pol.length_m, by = list(dta$grid_pol.id), FUN = sum) ## this aggregates by ID

dta <- dta[!duplicated(dta$grid_pol.id),] ## remove to get the right number of grids

dta <- dta %>%
  rename(x = grid_pol.grd_1km_rst.tif.V2.x,
         y = grid_pol.grd_1km_rst.tif.V3.x,
         road_length = grid_pol.length_m,
         id = grid_pol.id
  )

dta2 <- dta2 %>%
  rename(id = Group.1,
         road_length = x)

dta$road_length <- NULL ## remove the row first to avoid double merge
dta <- merge(dta, dta2, by = "id") ## merge the aggregated results

dta$road_length = set_units(dta$road_length, "km")

#dta3 <- st_as_sf(dta, coords = c("x", "y"))

# Calculate density (this is likely unnecessary)
#grid_pol$length_m[is.na(grid_pol$length_m)] = 0
#grid_pol$area_m2 = st_area(grid_pol)
#grid_pol$area_km2 = set_units(grid_pol$area_m2, "km^2")
#grid_pol$area_m2 = NULL
#grid_pol$area_km2 = as.numeric(grid_pol$area_km2)
#grid_pol$density_m_km2 = grid_pol$length_m / grid_pol$area_km2

# Rasterize
#grid[[1]][] = NA
#r_dens = st_rasterize(grid_pol["density_m_km2"], grid)

# Write result
#write_stars(r_dens, "result_01_density_m_km2.tif")

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/open_street_map")
write_csv(dta, "osm_grid_matched.csv")


