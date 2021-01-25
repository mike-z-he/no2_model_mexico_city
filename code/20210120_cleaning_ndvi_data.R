#### Cleaning NASA NDVI Data ####
#### January 20, 2021 ####

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



