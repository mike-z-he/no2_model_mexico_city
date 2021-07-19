#### Second Stage NO2 Modeling with 2019 data ####
#### May 10, 2021 ####


## 5/14: updated to change outcome to non z-scores

options(mc.cores=parallel::detectCores())

#### Load packages ####
library(tidyverse)
library(lubridate)
library(ranger)
library(lme4)
library(reshape)
library(foreign)
library(ggplot2)
library(plyr)
library(data.table)
library(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
library(DataCombine)
library(readr)
library(bit64)
library(FNN)
library(splitstackshape)
library(foreign)
library(lattice)
library(datasets)
library(foreach)
library(doParallel)
library(ModelMetrics)


#### Load Dataset ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
dta <- read_csv("second_stage_monitors_2019.csv")

## Adding Julian date
dta$jd <- julian(dta$date)
dta$yday <- yday(dta$date)



#### Rescaling Variables ####
names <- c("lat", "long", "omi_no2", "tropomi_no2", "wind_u", "wind_v", "temp_2m", "albedo", "pressure", "precip", 
           "blh", "cloud", "elevation", "road_length", "population", "ndvi", "monitor_no2", "rf_no2")

scaled <- dta[, names] %>%
  mutate_all(funs(scale))

colnames(scaled) = paste0(colnames(scaled), "_z")
dta <- cbind(dta, scaled)
names(dta)



#### Preliminary Mixed Effect Modeling
# Simplest model
try <- lm(monitor_no2 ~ rf_no2_z, data=dta)
summary(try)
#Multiple R-squared:  0.1216,	Adjusted R-squared:  0.1215 

# Include lat/long
try <- lm(monitor_no2 ~ rf_no2_z + lat_z + long_z, data=dta)
summary(try)
#Multiple R-squared:  0.1753,	Adjusted R-squared:  0.175

# Random intercept by day: mixed effects model
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.5309184

# Add lat/long
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.5606047

# Add wind
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.5818571

# Add temperature
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7320502 quite a large jump...does this have to do with the fact that temperature was used in stage 1 modeling?

# Add albedo
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + albedo_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7320561 #doesn't really help, won't add

# Add pressure
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + pressure_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7290074 actually decreases R2? won't add

# Add precipitation
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + precip_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7320782 doesn't really help, won't add

# Add blh
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7325641

# Add cloud
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7332009

# Add elevation
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + elevation_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7351303

# Add road_length (final model for now)
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + elevation_z + road_length_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7366251

# Add population
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + elevation_z + road_length_z + population_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta)
summary(mod_0)
dta$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta))$r.squared)
# [1] 0.7362246 doesn't really help, won't add

# Add NDVI (NA = 93)
dta2 <- dta[!is.na(dta$ndvi), ]
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + elevation_z + road_length_z + population_z + ndvi_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
summary(mod_0)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.7508423 does seem to help

# Add TROPOMI (NA = 3970)
dta3 <- dta2[!is.na(dta2$tropomi_no2), ]
formula_1 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + elevation_z + road_length_z + population_z + ndvi_z + tropomi_no2_z + (1 + rf_no2_z | date))
mod_0 <- lmer(formula_1, data = dta3)
summary(mod_0)
dta3$pred.m0 <- predict(mod_0)
print(summary(lm(monitor_no2_z~pred.m0,data=dta3))$r.squared)
# [1] 0.7843405 helps slightly


#### Preliminary Random Forest Modeling ####
## Default model (for now)
formula <- as.formula(monitor_no2 ~ rf_no2 + lat + long + wind_u + wind_v + temp_2m + blh + cloud + elevation + road_length)
formula2 <- as.formula(monitor_no2 ~ rf_no2_z + lat_z + long_z + wind_u_z + wind_v_z + temp_2m_z + blh_z + cloud_z + elevation_z + road_length_z)

mod1 <- ranger(formula, data=dta, num.trees=50, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 17.49538 OOB.R2= 0.762499 RUN in ~ 1 minute.

mod1 <- ranger(formula2, data=dta, num.trees=50, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 17.65446 OOB.R2= 0.7603395 RUN in ~ 1 minute.

## Seems similar...I'll just use non z-scored version then

mod1 <- ranger(formula, data=dta, num.trees=100, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 17.11238 OOB.R2= 0.7676983 RUN in ~ 1 minute.

mod1 <- ranger(formula, data=dta, num.trees=150, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 16.85277 OOB.R2= 0.7712226 RUN in ~ 1 minute.

mod1 <- ranger(formula, data=dta, num.trees=500, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 16.51723 OOB.R2= 0.7757775 RUN in ~ 1 minute.
## Will use 500 for now, since dataset is small

## Adding Julian date
formula <- as.formula(monitor_no2 ~ rf_no2 + lat + long + wind_u + wind_v + temp_2m + blh + cloud + elevation + road_length + jd)

mod1 <- ranger(formula, data=dta, num.trees=500, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 12.29542 OOB.R2= 0.8330888 RUN in ~ 1 minute.

## Adding yday
formula <- as.formula(monitor_no2 ~ rf_no2 + lat + long + wind_u + wind_v + temp_2m + blh + cloud + elevation + road_length + yday)

mod1 <- ranger(formula, data=dta, num.trees=500, mtry=9)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 12.28647 OOB.R2= 0.8332104 RUN in ~ 1 minute.
## Seems to be fine this time...and both perform similarly

## Adding TROPOMI (NA = 3970)
formula <- as.formula(monitor_no2 ~ rf_no2 + lat + long + wind_u + wind_v + temp_2m + blh + cloud + elevation + road_length + yday + tropomi_no2)

mod1 <- ranger(formula, data=dta3, num.trees=500, mtry=10)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE= 13.35159 OOB.R2= 0.8315512 RUN in ~ 1 minute.
## Doesn't add much here