#### Building Subset Dataset for Stage One NO2 Modeling ####
####  April 28, 2021 ####

## This is currently being done for 2019 only, but will be expanded to full dataset
## 5/10: added code for incorporating imputed NO2 (rf_no2)

options(mc.cores=parallel::detectCores())

#### Load packages ####
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(readxl)


#### Build grids by date ####
## Grid
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grid <- read_stars("grd_1km_rst.tif")
grid <- as.data.frame(st_as_sf(grid))
grid <- grid %>%
  dplyr::rename(x = grd_1km_rst.tif.V2,
                y = grd_1km_rst.tif.V3,
                lat = grd_1km_rst.tif.V4,
                long = grd_1km_rst.tif.V5
  )
grid$grd_1km_rst.tif.V1 <- NULL
grid$geometry <- NULL
grid$grid_id <- 1:nrow(grid)


## Dates (one year for now)
dates <- as.data.frame(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = 1))
names(dates)[1] <- "date"
grid_date <- merge(grid, dates)
grid <- grid_date
grid$year <- year(grid$date)
grid$month <- month(grid$date)


## Create unrounded x and y (convert back in the very end)
grid$x_hd <- grid$x
grid$y_hd <- grid$y

grid$x <- round(grid$x, digits = 0)
grid$y <- round(grid$y, digits = 0)



#### ERA5 Data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data/era_5_grid_matched/2019")
file.list <- list.files(pattern='*.csv')
file.list <- file.list[1:365]
df.list <- lapply(file.list, read_csv)
df <- bind_rows(df.list, .id = "id")
df$id <- NULL
era5_1 <- df
era5_1$x <- round(era5_1$x, digits = 0)
era5_1$y <- round(era5_1$y, digits = 0)

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data/era_5_2_grid_matched/2019")
file.list <- list.files(pattern='*.csv')
file.list <- file.list[1:365]
df.list <- lapply(file.list, read_csv)
df <- bind_rows(df.list, .id = "id")
df$id <- NULL
era5_2 <- df
era5_2$x <- round(era5_2$x, digits = 0)
era5_2$y <- round(era5_2$y, digits = 0)

# note that there is some minor joining error: round before join
#a <- round(era5_1$y, digits = 0) == round(era5_2$y, digits = 0)

era5 <- left_join(era5_1, era5_2, by = c("x", "y", "date"))



#### OMI Data ####
## This will include the imputed data, once it is finalized
## In here as a filler for now
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data/omi_grid_matched/2019")
file.list <- list.files(pattern='*.csv')
file.list <- file.list[1:274]
df.list <- lapply(file.list, read_csv)
df <- bind_rows(df.list, .id = "id")
df$id <- NULL
omi <- df
omi$x <- round(omi$x, digits = 0)
omi$y <- round(omi$y, digits = 0)



#### TROPOMI Data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data/tropomi_grid_matched/2019")
file.list <- list.files(pattern='*.csv')
file.list <- file.list[1:358]
df.list <- lapply(file.list, read_csv)
df <- bind_rows(df.list, .id = "id")
df$id <- NULL
tropomi <- df
tropomi$x <- round(tropomi$x, digits = 0)
tropomi$y <- round(tropomi$y, digits = 0)
tropomi$tropomi_weight <- NULL # remove the weight for now



#### Time-Fixed Data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
ele <- read_csv("elevation_grid_matched.csv")
ele$x <- round(ele$x, digits = 0)
ele$y <- round(ele$y, digits = 0)

osm <- read_csv("osm_grid_matched.csv")
osm$x <- round(osm$x, digits = 0)
osm$y <- round(osm$y, digits = 0)



#### Population Data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
pop <- read_csv("population_grid_matched.csv")
pop$x <- round(pop$x, digits = 0)
pop$y <- round(pop$y, digits = 0)



#### NDVI Data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
ndvi <- read_csv("ndvi_grid_2005_2019.csv")
ndvi$x <- round(ndvi$x, digits = 0)
ndvi$y <- round(ndvi$y, digits = 0)
ndvi <- ndvi %>%
  rename(
    ndvi = NDVI
  )
ndvi$year <- year(ndvi$date)
ndvi$month <- month(ndvi$date)
ndvi$date <- NULL


#### Monitor NO2 ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
monitor <- read_csv("monitor_grid_matched.csv", col_types = list(col_double(), col_double(), col_date(format = "%m/%d/%Y"), col_character(), col_double()))
monitor$x <- round(monitor$x, digits = 0)
monitor$y <- round(monitor$y, digits = 0)
monitor$monitor <- NULL



#### Imputed OMI Data (rf_no2) ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
rf <- read_csv("rf_no2_2019.csv")
rf$x <- round(rf$x, digits = 0)
rf$y <- round(rf$y, digits = 0)



#### Merge Dataset Together####
dta <- left_join(grid, omi, by = c("x", "y", "date"))
dta <- left_join(dta, tropomi, by = c("x", "y", "date"))
dta <- left_join(dta, era5, by = c("x", "y", "date"))
dta <- left_join(dta, ele, by = c("x", "y"))
dta <- left_join(dta, osm, by = c("x", "y"))
dta <- left_join(dta, pop, by = c("x", "y", "year"))
dta <- left_join(dta, ndvi, by = c("x", "y", "year", "month"))
dta <- left_join(dta, monitor, by = c("x", "y", "date"))
dta <- left_join(dta, rf, by = c("x", "y", "date"))



#### Replace with hd X and Y ####
dta$x <- dta$x_hd
dta$y <- dta$y_hd
dta$x_hd <- NULL
dta$y_hd <- NULL



#### Subset of data with monitors only ####
dta2 <- dta %>%
  drop_na(monitor_no2) %>%
  filter(date != "2019-12-31")

## this is a subset of n = 8888

## Write full and subsets
write_csv(dta, "second_stage_full_2019.csv")
write_csv(dta2, "second_stage_monitors_2019.csv")