#### Cleaning NASA NDVI Data ####
#### January 20, 2021 ####

### Much of the code used here is adapted from Ian's scripts, reorganized and
### combined into files that aligns with my work flow

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km")
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

# Load all model functions (from Ian's helper functions)
paths <- here("code/sample/ian/helpers") %>%
  list.files(pattern = "*\\.R$", full.names = TRUE)
for (p in paths) {
  source(p)
}
rm(paths, p)


## Run for each year (update origin for date for each year; ~8 mins per year for 15 years)

#2005
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2005")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2004-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2006
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2006")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2005-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2007
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2007")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2006-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2008
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2008")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2007-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2009
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2009")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2008-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2010
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2010")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2009-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2011
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2011")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2010-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2012
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2012")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2011-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2013
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2013")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2012-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2014
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2014")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2013-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2015
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2015")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2014-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2016
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2016")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2015-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2017
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2017")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2016-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2018
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2018")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2017-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}

#2019
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/2019")
files <- dir(pattern = ".hdf")
files.length <- length(files)
for (i in  1:files.length){
  dta.i <- gdal_subdatasets(files[i]) %>%
    dplyr::first() %>%
    raster::raster()
  dta.i <- dta.i * (1/10000^2)
  dta.i <- rasterToPoints(dta.i)
  dta.i <- as.data.frame(dta.i)
  names(dta.i)[3] <- "NDVI"
  dta.i$jday <- as.numeric(substr(str_split_fixed(files[i], "\\.",6)[2], 6, 8)) ## this subsets the part of the name file that contains the date info
  dta.i$date<- as.Date(dta.i$jday, origin = "2018-12-31") ## since it is in Julian date form, convert to calendar day (use last day of the previous year)
  dta.i$jday <- NULL
  nm1 <- paste(files[i], ".csv", sep="")
  write.csv(dta.i, file = nm1 ,row.names = FALSE)
}



## To convert back to a raster (specify CRS)
## crs: +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs 
dta.i$date <- NULL
dfr <- rasterFromXYZ(dta.i)
plot(dfr)



#### Read and merge all NDVI data ####
## Too big so broke data down into three and working with it that way
options(mc.cores=parallel::detectCores())
library(tidyverse)

## 2005-2009
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/csv/2005_2009")
csv_files <- dir(pattern=".csv")
df <- csv_files %>%
  map_dfr(read.csv, header = TRUE, fill = TRUE)
write.csv(df, "ndvi_2005_2009.csv", row.names = FALSE)

## 2010-2014
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/csv/2010_2014")
csv_files <- dir(pattern=".csv")
df2 <- csv_files %>%
  map_dfr(read.csv, header = TRUE, fill = TRUE)
write.csv(df2, "ndvi_2010_2014.csv", row.names = FALSE)

## 2015-2019
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/csv/2015_2019")
csv_files <- dir(pattern=".csv")
df3 <- csv_files %>%
  map_dfr(read.csv, header = TRUE, fill = TRUE)
write.csv(df3, "ndvi_2015_2019.csv", row.names = FALSE)




#### Sample matching (using January 2005) ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grid <- raster("grd_1km_rst.tif", band = 1)
points <- rasterToPoints(grid)
points <- as.data.frame(points)
points$grd_1km_rst <- NULL
#write.csv(points, "grid.csv", row.names = FALSE)

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/csv/sample")
csv_files <- dir(pattern=".csv")
df <- csv_files %>%
  map_dfr(read.csv, header = TRUE, fill = TRUE)

## Sample both = same month
df$month <- 1
df$date <- NULL
points$month <- 1

df$x <- round(df$x, digits = 2)
df$y <- round(df$y, digits = 2)

points$x <- round(points$x, digits = 2)
points$y <- round(points$y, digits = 2)

joined <- left_join(points, df, by = c("x", "y", "month")) ## this works!

## Create a "date file" to merge data with, merge with grid
## Create x_noLoss and y_noLoss variables in the grid
## left_join grid and dataset, get 21639*12*5 variables


#### Actual matching (by 5-year chunks) ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/csv")
options(mc.cores=parallel::detectCores())

library(tidyverse)

## Merge date and grid files
dates <- read.csv("dates.csv", stringsAsFactors = FALSE)
grid <- read.csv("grid.csv", stringsAsFactors = FALSE)
joined <- merge(dates, grid)
#write.csv(joined, "dates_grid.csv", row.names = FALSE)

dates_05_09 <- read.csv("dates_2005_2009.csv", stringsAsFactors = FALSE)
grid <- read.csv("grid.csv", stringsAsFactors = FALSE)
joined <- merge(dates_05_09, grid)
#write.csv(joined, "dates_grid_2005_2009.csv", row.names = FALSE)

dates_10_14 <- read.csv("dates_2010_2014.csv", stringsAsFactors = FALSE)
grid <- read.csv("grid.csv", stringsAsFactors = FALSE)
joined <- merge(dates_10_14, grid)
#write.csv(joined, "dates_grid_2010_2014.csv", row.names = FALSE)

dates_15_19 <- read.csv("dates_2015_2019.csv", stringsAsFactors = FALSE)
grid <- read.csv("grid.csv", stringsAsFactors = FALSE)
joined <- merge(dates_15_19, grid)
#write.csv(joined, "dates_grid_2015_2019.csv", row.names = FALSE)


## Create matched NDVI grids
# Read in grid file
dates_05_09 <- read.csv("dates_grid_2005_2009.csv", stringsAsFactors = FALSE)
dates_05_09$date <- as.Date(dates_05_09$date, format = "%m/%d/%Y")

# Read in NDVI file (all grids for 2005-2009)
ndvi_05_09 <- read.csv("ndvi_2005_2009.csv", stringsAsFactors = FALSE)
ndvi_05_09$date <- as.Date(ndvi_05_09$date, format = "%Y-%m-%d")

## Creating unrounded and rounded X and Y values
dates_05_09$x_noLoss <- dates_05_09$x
dates_05_09$y_noLoss <- dates_05_09$y

## Round x and y to create match
## Tried this 3 times: digits = 2 (NA = 65223), digits = 1 (NA = 64429), digits = 0 (NA = 6049)
## So will go with digits = 0 for matching purposes
dates_05_09$x <- round(dates_05_09$x, digits = 0)
dates_05_09$y <- round(dates_05_09$y, digits = 0)
ndvi_05_09$x <- round(ndvi_05_09$x, digits = 0)
ndvi_05_09$y <- round(ndvi_05_09$y, digits = 0)

# Subset to only the Mexico City grids
ndvi_grid_05_09 <- left_join(dates_05_09, ndvi_05_09, by = c("x", "y", "date"))

# Replace x and y with unrounded values, then delete x_noLoss and y_noLoss
ndvi_grid_05_09$x <- ndvi_grid_05_09$x_noLoss
ndvi_grid_05_09$y <- ndvi_grid_05_09$y_noLoss
ndvi_grid_05_09$x_noLoss <- NULL
ndvi_grid_05_09$y_noLoss <- NULL

# Save file
#write.csv(ndvi_grid_05_09, "ndvi_grid_2005_2009.csv", row.names = FALSE)


## Repeat for 2010-2014
# Read in grid file
dates_10_14 <- read.csv("dates_grid_2010_2014.csv", stringsAsFactors = FALSE)
dates_10_14$date <- as.Date(dates_10_14$date, format = "%m/%d/%Y")

# Read in NDVI file (all grids for 2005-2009)
ndvi_10_14 <- read.csv("ndvi_2010_2014.csv", stringsAsFactors = FALSE)
ndvi_10_14$date <- as.Date(ndvi_10_14$date, format = "%Y-%m-%d")

## Creating unrounded and rounded X and Y values
dates_10_14$x_noLoss <- dates_10_14$x
dates_10_14$y_noLoss <- dates_10_14$y

## Round x and y to create match
dates_10_14$x <- round(dates_10_14$x, digits = 0)
dates_10_14$y <- round(dates_10_14$y, digits = 0)
ndvi_10_14$x <- round(ndvi_10_14$x, digits = 0)
ndvi_10_14$y <- round(ndvi_10_14$y, digits = 0)

# Subset to only the Mexico City grids
ndvi_grid_10_14 <- left_join(dates_10_14, ndvi_10_14, by = c("x", "y", "date"))

# Replace x and y with unrounded values, then delete x_noLoss and y_noLoss
ndvi_grid_10_14$x <- ndvi_grid_10_14$x_noLoss
ndvi_grid_10_14$y <- ndvi_grid_10_14$y_noLoss
ndvi_grid_10_14$x_noLoss <- NULL
ndvi_grid_10_14$y_noLoss <- NULL

# Save file
#write.csv(ndvi_grid_10_14, "ndvi_grid_2010_2014.csv", row.names = FALSE)


## Repeat for 2015-2019
# Read in grid file
dates_15_19 <- read.csv("dates_grid_2015_2019.csv", stringsAsFactors = FALSE)
dates_15_19$date <- as.Date(dates_15_19$date, format = "%m/%d/%Y")

# Read in NDVI file (all grids for 2005-2009)
ndvi_15_19 <- read.csv("ndvi_2015_2019.csv", stringsAsFactors = FALSE)
ndvi_15_19$date <- as.Date(ndvi_15_19$date, format = "%Y-%m-%d")

## Creating unrounded and rounded X and Y values
dates_15_19$x_noLoss <- dates_15_19$x
dates_15_19$y_noLoss <- dates_15_19$y

## Round x and y to create match
dates_15_19$x <- round(dates_15_19$x, digits = 0)
dates_15_19$y <- round(dates_15_19$y, digits = 0)
ndvi_15_19$x <- round(ndvi_15_19$x, digits = 0)
ndvi_15_19$y <- round(ndvi_15_19$y, digits = 0)

# Subset to only the Mexico City grids
ndvi_grid_15_19 <- left_join(dates_15_19, ndvi_15_19, by = c("x", "y", "date"))

# Replace x and y with unrounded values, then delete x_noLoss and y_noLoss
ndvi_grid_15_19$x <- ndvi_grid_15_19$x_noLoss
ndvi_grid_15_19$y <- ndvi_grid_15_19$y_noLoss
ndvi_grid_15_19$x_noLoss <- NULL
ndvi_grid_15_19$y_noLoss <- NULL

# Save file
#write.csv(ndvi_grid_15_19, "ndvi_grid_2015_2019.csv", row.names = FALSE)


#### Merge the three into one file ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/ndvi_1km/csv")
options(mc.cores=parallel::detectCores())

library(tidyverse)

ndvi_grid_1 <- read.csv("ndvi_grid_2005_2009.csv", stringsAsFactors = FALSE)
ndvi_grid_2 <- read.csv("ndvi_grid_2010_2014.csv", stringsAsFactors = FALSE)
ndvi_grid_3 <- read.csv("ndvi_grid_2015_2019.csv", stringsAsFactors = FALSE)

ndvi_grid_05_19 <- rbind(ndvi_grid_1, ndvi_grid_2)
ndvi_grid_05_19 <- rbind(ndvi_grid_05_19, ndvi_grid_3)

## Double check coordinates look okay
unique(ndvi_grid_05_19$x)
unique(ndvi_grid_05_19$y)

#write.csv(ndvi_grid_05_19, "ndvi_grid_2005_2019.csv", row.names = FALSE) #Final dataset
