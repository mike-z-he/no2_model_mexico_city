#### Making model grids for the NO2 model ####
#### January 15, 2021 ####

### Much of the code used here is adapted from Ian's scripts, reorganized and
### combined into files that aligns with my work flow

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city")
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

# Load all model functions (from Ian's helper functions)
paths <- here("code/sample/ian/helpers") %>%
  list.files(pattern = "*\\.R$", full.names = TRUE)
for (p in paths) {
  source(p)
}
rm(paths, p)



#### Making 1km grids covering the entire study area ####
## Define paths
grd_path <- here("data/geo/grd_1km.fst")
pts_path <- str_replace(grd_path, "\\.fst$", "_pts.shp")
pys_path <- str_replace(grd_path, "\\.fst$", "_pys.shp")
rst_path <- str_replace(grd_path, "\\.fst$", "_rst.tif")
out_paths <- c(grd = grd_path, pts = pts_path, pys = pys_path, rst = rst_path)

## Load Mexico City shapefile
roi <- here("data/mxcity_megalopolis/mxcity_megalopolis.shp") %>%
  st_read(quiet = TRUE) %>%
  st_transform(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs")


## Load MODIS grids
pxs <- here("data/modis_grids") %>%
  list.files()

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/modis_grids")
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

# Assign a unique ID based on latitude and longitude
grd_1km <- cbind(xy, lon_lat) %>%
  .[, sinu_id := hash_xy(sinu_x, sinu_y)] %>%
  setcolorder(c("sinu_id", "sinu_x", "sinu_y", "lat", "lon")) %>%
  setkey("sinu_id")
names(grd_1km) %>% paste(collapse = " ")


# Points (cell centroids)
pts <- st_as_sf(grd_1km, coords = c("sinu_x", "sinu_y"), crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs", remove = FALSE)
save_safely(pts, st_write, pts_path, quiet = TRUE)

# Raster (cells)
# Must save as "FLT8S" to ensure ID maintains precision
rst <- as(pts, "Spatial") %>%
  sp::`gridded<-`(TRUE) %>%
  raster::stack()
save_safely(rst, save_geotiff, rst_path, datatype = "FLT8S")

# Polygons (cell borders)
pys <- raster::rasterToPolygons(rst) %>%
  st_as_sf() %>%
  dplyr::arrange(sinu_id) # sort by ID to match grd and pts
save_safely(pys, st_write, pys_path, quiet = TRUE)

# Save grid
save_safely(grd_1km, write_fst, grd_path, compress = 100)




#### Cluster 1 km grid cells for spatial cross-validation ####

cluster_grd_1km <- function(n = 50, force = FALSE, verbose = FALSE) {
  
  # Skip if exists
  clusters_path <- here(glue("data/geo/grd_1km_clusters_{n}.fst"))
  if (all_exist(clusters_path, force = force)) return(clusters_path)
  
  report(glue("[cluster_grd_1km] clustering 1 km grid cells with n = {n}"))
  
  # Load 1 km grid cells
  cells <- read_fst(here("data/geo/grd_1km.fst"), as.data.table = TRUE,
                    columns = c("sinu_id", "sinu_x", "sinu_y"))
  
  # Cluster by spatial coordinates (round to 5 km to facilitate convergence)
  set.seed(100)
  km <- cells[, .(sinu_x, sinu_y)] %>%
    magrittr::divide_by(5000) %>%
    round() %>%
    kmeans(centers = n, iter.max = 20)
  if (km$ifault != 0) {
    stop("Kmeans clustering of grid cells did not converge")
  }
  cells[, cluster := km$cluster]
  set.seed(Sys.time())
  
  # Drop coordinates
  cells[, c("sinu_x", "sinu_y") := NULL]
  names(cells) %>% paste(collapse = " ")
  # sinu_id cluster
  
  # Save
  save_safely(cells, write_fst, clusters_path, compress = 100)
  report(glue("[cluster_grd_1km] -> {clusters_path}"), verbose)
  
  return(clusters_path)
  
}

cluster_grd_1km(n=50, force = FALSE, verbose = TRUE)



#### Creating a CSV grid of all dates/points ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
options(mc.cores=parallel::detectCores())

grid <- read.csv("grid.csv", stringsAsFactors = FALSE)

dates <- as.data.frame(seq(as.Date("2005-01-01"), as.Date("2019-12-31"), by = 1))
names(dates)[1] <- "date"

grid_date <- merge(grid, dates)
write.csv(grid_date, "grids_complete.csv", row.names = FALSE)
