#### Cleaning Gridded Population of the World (GPW) Data ####
#### February 5, 2021 ####

## 5 separate population datasets available
## 2000, 2005, 2010, 2015, 2020
## For purposes of this dataset, using 2005, 2010, and 2015 for the years 2005-2019

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/demographics/2_population_count")
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


## Load Grids
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/geo")
grids <- raster("grd_1km_rst.tif")

## Load GeoTIFF
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/demographics/2_population_count")
pop2005 <- raster("gpw_v4_population_count_rev11_2005_30_sec.tif")
pop2010 <- raster("gpw_v4_population_count_rev11_2010_30_sec.tif")
pop2015 <- raster("gpw_v4_population_count_rev11_2015_30_sec.tif")

## Project to the grid (via bilinear interpolation)
pop_05_grid <- projectRaster(pop2005, grids)
pop_10_grid <- projectRaster(pop2010, grids)
pop_15_grid <- projectRaster(pop2015, grids)

## Match the grids
pop_05 <- mask(pop_05_grid, grids)
pop_10 <- mask(pop_10_grid, grids)
pop_15 <- mask(pop_15_grid, grids)

## Flatten into data frame
pop.05 <- as.data.frame(rasterToPoints(pop_05))
pop.10 <- as.data.frame(rasterToPoints(pop_10))
pop.15 <- as.data.frame(rasterToPoints(pop_15))

## Rename
names(pop.05)[3] <- "population"
pop.05$year <- 2005

names(pop.10)[3] <- "population"
pop.10$year <- 2010

names(pop.15)[3] <- "population"
pop.15$year <- 2015

## Assigning next four years to earlier population
## e.g 2006, 2007, 2008, 2009 to 2005, and so on
pop.06 <- pop.05
pop.06$year <- 2006

pop.07 <- pop.05
pop.07$year <- 2007

pop.08 <- pop.05
pop.08$year <- 2008

pop.09 <- pop.05
pop.09$year <- 2009

pop.11 <- pop.10
pop.11$year <- 2011

pop.12 <- pop.10
pop.12$year <- 2012

pop.13 <- pop.10
pop.13$year <- 2013

pop.14 <- pop.10
pop.14$year <- 2014

pop.16 <- pop.15
pop.16$year <- 2016

pop.17 <- pop.15
pop.17$year <- 2017

pop.18 <- pop.15
pop.18$year <- 2018

pop.19 <- pop.15
pop.19$year <- 2019

pop <- 
  do.call(rbind, list(pop.05, pop.06, pop.07, pop.08, pop.09, pop.10,
                      pop.11, pop.12, pop.13, pop.14, pop.15, pop.16,
                      pop.17, pop.18, pop.19))

## Export file
#write_csv(pop, "population_grid_matched.csv")
