#### Sandbox for playing with existing functions ####
#### January 14, 2021 ####

### Much of the code used here is adapted from Ian's scripts, reorganized and
### combined into files that aligns with my work flow

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city")
options(mc.cores=parallel::detectCores())



#### Load Packages ####
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


#### Load Functions ####
make_grd_1km <- function(data_dir, force = FALSE, verbose = FALSE) {
  
  # Skip if exists
  grd_path <- here("data/geo/grd_1km.fst")
  pts_path <- str_replace(grd_path, "\\.fst$", "_pts.shp")
  pys_path <- str_replace(grd_path, "\\.fst$", "_pys.shp")
  rst_path <- str_replace(grd_path, "\\.fst$", "_rst.tif")
  out_paths <- c(grd = grd_path, pts = pts_path, pys = pys_path, rst = rst_path)
  # if (all_exist(out_paths, force = force)) return(out_paths) couldn't get this to work
  
  # report("[make_grd_1km] making 1 km model grid")
  
  # Load the study area and transform to MODIS Sinusoidal
  roi <- here("data/mxcity_megalopolis/mxcity_megalopolis.shp") %>%
    st_read(quiet = TRUE) %>%
    st_transform(crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") ## this is based on something I found online: https://spatialreference.org/ref/sr-org/6974/
    # st_transform(crs = "MODIS_SINU") this didn't work
  
  # Load and merge the pixels of one day of MAIAC AOD data
  #pxs <- glue("{data_dir}/data/modis_grids") %>%
  #  list.files(pattern = "20[01][0-9]", full.names = TRUE) %>%
  #  last() %>%
  #  list.files(pattern = "MCD19A2\\.A20[01][0-9]001.+\\.hdf", full.names = TRUE)
  #if (length(pxs) != 3) {
  #  stop(glue("Expected 3 MAIAC tiles; got {length(pxs)}:\n{paste(pxs, collapse = '\n')}"))
  #}
  
  pxs <- here("data/modis_grids") %>%
    list.files()
  
  #pxs %<>% purrr::map(function(x) {
  #  withCallingHandlers(
  #    gdal_subdatasets(x) %>%
  #      dplyr::first() %>%
  #      raster::raster() %>%
  #      raster::raster()  #load only geometry, no values
  #     # warning = muffle_crs_warnings # muffle warnings due to deprecated proj4strings
  #  )
  #}) %>%
  #  do.call(raster::merge, .)
  
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
  
  # Paranoia: confirm ROI is same CRS as MAIAC AOD
  # * use proj4strings b/c MAIAC data does not have WKT CRS
  stopifnot(identical(st_crs(roi)$proj4string, st_crs(pxs)$proj4string))
  
  # Crop the AOD pixels to the study area
  pxs %<>% raster::crop(roi, snap = "out") %>%
    setValues(NA)
  
  # Rasterize the study area using the AOD pixels as a template
  # * Rasterize both polygons and lines to include all pixels that intersect border
  # * Don't use rasterize: slow and includes pixels in Spanish enclave near Andorra
  # * Use fasterize: fast and correct, but only works with polygons
  # * Use velox$rasterize: fast for rasterizing lines
  # report("[make_grd_1km] rasterizing study area at MAIAC AOD resolution", verbose = verbose)
  #inside <- fasterize::fasterize(roi, pxs)
  #b <- st_cast(roi, "MULTILINESTRING") %>%
  #  dplyr::mutate(tmp = 1)
  # v <- velox::velox(pxs) velox is deprecated
  #v <- pxs
  #v$rasterize(b, field = "tmp")
  
  ## because above doesn't work, I will try using just rasterize
  
  #border <- withCallingHandlers(
  #  v$as.RasterLayer(),
  #  warning = muffle_crs_warnings
  #)
  # cells <- (inside | border)
  
  cells <- rasterize(roi, pxs)
  
  # Get the coordinates of the centroid of each cell in the study area
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
  #grd_1km <- cbind(xy, lon_lat) %>%
   # .[, sinu_id := hash_xy(sinu_x, sinu_y)] %>%
  #  setcolorder(c("sinu_id", "sinu_x", "sinu_y", "lat", "lon")) %>%
  #  setkey("sinu_id")
  #names(grd_1km) %>% paste(collapse = " ")
  # sinu_id sinu_x sinu_y lat lon
  
  grd_1km <- cbind(xy, lon_lat)
  ## couldn't get the rest of this to work either, so I just binded the columns
  
  
  # Save grid geometry
  report("[make_grd_1km] generating polygons and centroids", verbose = verbose)
  
  # Points (cell centroids)
  pts <- st_as_sf(grd_1km, coords = c("sinu_x", "sinu_y"), crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs", remove = FALSE)
  save_safely(pts, st_write, pts_path, quiet = TRUE)
  report(glue("[make_grd_1km] -> {pts_path}"))
  
  # Raster (cells)
  # Must save as "FLT8S" to ensure ID maintains precision
  rst <- as(pts, "Spatial") %>%
    sp::`gridded<-`(TRUE) %>%
    raster::stack()
  save_safely(rst, save_geotiff, rst_path, datatype = "FLT8S")
  report(glue("[make_grd_1km] -> {rst_path}"))
  
  # Polygons (cell borders)
  pys <- raster::rasterToPolygons(rst) %>%
    st_as_sf() %>%
    dplyr::arrange(sinu_id) # sort by ID to match grd and pts
  save_safely(pys, st_write, pys_path, quiet = TRUE)
  report(glue("[make_grd_1km] -> {pys_path}"))
  
  # Save grid
  save_safely(grd_1km, write_fst, grd_path, compress = 100)
  report(glue("[make_grd_1km] -> {grd_path}"))
  
  return(out_paths)
  
}



#### 
make_grd_1km(data_dir = here(), force = FORCE, verbose = VERBOSE)
