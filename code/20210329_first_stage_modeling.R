#### First Stage NO2 Modeling: Imputing missing OMI values ####
#### March 29, 2021 ####

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



#### Load Dataset ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/processed_data")
dta <- read_csv("first_stage.csv")



#### Some Descriptive Statistics ####
summary(dta)

## ~10% of OMI data missing; 16% of OSM data missing
## Replace NAs with 0s in OSM data?

## omi_no2 units: molecules / cm^2
## cams no2 units: kg / m^2
## temperature units: Kelvins
## blh units: meters
## elevation units: meters
## road length units: km



#### Unit Conversions ####
dta$temp_2m <- dta$temp_2m - 273.15
#dta$blh <- dta$blh / 1000

## Stoichiometry to convert from molecules/cm^2 to kg/m^2 (is this right?)
#dta$omi_no2 <- dta$omi_no2*((10000*46.0055)/(6.02e23*1000))



#### Create a z-score for OMI (unnecessary with scaling) ####
dta2 <- dta[!is.na(dta$omi_no2),]

omi <- dta2$omi_no2
hist(omi)
omi_sd <- sd(omi)*sqrt((length(omi)-1)/(length(omi)))
omi_mean <- mean(omi)

dta2$no2_omi_z <- ((dta2$omi_no2 - omi_mean ) / omi_sd)



#### Create a z-score for CAMS (unnecessary with scaling) ####
dta2 <- dta2[!is.na(dta2$cams_no2), ]

cams <- dta2$cams_no2
hist(cams)
cams_sd <- sd(cams)*sqrt((length(cams)-1)/(length(cams)))
cams_mean <- mean(cams)

dta$no2_cams_z <- ((dta$cams_no2 - cams_mean) / cams_sd)
dta2$no2_cams_z <- ((dta2$cams_no2 - cams_mean) / cams_sd)



#### Rescaling Variables ####
names <- c("lat", "long", "omi_no2", "cams_no2", "temp_2m", "blh", "elevation", "road_length")

scaled <- dta[, names] %>%
  dplyr::mutate_all(funs(scale))

colnames(scaled) = paste0(colnames(scaled), "_z")
dta <- cbind(dta, scaled)
names(dta)

## Dataset without missing OMI/CAMS data
dta2 <- dta[!is.na(dta$omi_no2), ]
dta2 <- dta2[!is.na(dta2$cams_no2), ]



#### Trying Regression Models ####
# Simplest model
try <- lm(omi_no2_z ~ cams_no2_z, data=dta2)
summary(try)
#Multiple R-squared:  0.201,	Adjusted R-squared:  0.201

# Include x and y
try <- lm(omi_no2_z ~ cams_no2_z + lat_z + long_z, data=dta2)
summary(try)
#Multiple R-squared:  0.2584,	Adjusted R-squared:  0.2584

# Random intercept by day: mixed effects model
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
summary(mod_0)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.4940695

# Random intercept + x and y
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.5318488

# Random intercept, x and y, blh
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.5383007

# Random intercept, x and y, blh, temp
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.5415122

# Random intercept, x and y, blh, temp, elevation (this has NA = 7792, based on dta2)
dta3 <- dta2[!is.na(dta2$elevation), ]
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta3)
dta3$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta3))$r.squared)
# [1] 0.5443969

# Random intercept, x and y, blh, temp, elevation, road (this has NA = 98656, based on dta2)
dta4 <- dta2[!is.na(dta2$road_length), ]
dta4 <- dta4[!is.na(dta4$elevation), ]
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta4)
dta4$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta4))$r.squared)
# [1] 0.597839



#### Testing Random Forest Models ####
# Random forest of x and y, blh, temp
tryrf <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z)
mod1rf <- ranger(tryrf, data=dta2, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9965957
mod1rf$prediction.error #[1] 0.003404323
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)

# Random forest of x and y, blh, temp, elevation (this has NA = 7792, based on dta2)
tryrf <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z)
mod1rf <- ranger(tryrf, data=dta3, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9966219
mod1rf$prediction.error #[1] 0.003391104
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)

# Random forest of x and y, blh, temp, elevation, road (this has NA = 98656, based on dta2)
tryrf <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z)
mod1rf <- ranger(tryrf, data=dta4, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9947245
mod1rf$prediction.error #[1] 0.005664584
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)



#### Impute omi_no2_z where there is no omi_no2_z ####
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + (1 + cams_no2_z | date))
mod_1 <- lmer(formula_1, data = dta2)

#now use formula of mixed effect model to estimate omi_no2_z where there is no omi_no2_z
#easiest would be to make a new field pred.m0 with the prediction and then copy the values
#to omi_no2_z when omi_no2_z = NA
dta <- as.data.table(dta)
dta[, pred.m0 := predict(object= mod_1, newdata= dta , allow.new.levels=TRUE, re.form=NULL)]
print(summary(lm(omi_no2_z~pred.m0,data=dta))$r.squared)


#Create a new variable "omi_no2_zi" where omi_no2_z and pred.m1 are combined
#, so where omi_no2_z > 0, omi_no2_zi = omi_no2_z
#. where omi_no2_z = NA, omi_no2_zi = pred.m0

#to do this split the table up and merge afterwards "subset" and ....

xdb_set1 <- subset(dta, omi_no2_z != "NA")
xdb_set2 <- dta[is.na(dta$omi_no2_z),]
xdb_set1$omi_no2_zi <- xdb_set1$omi_no2_z
xdb_set2$omi_no2_zi <- xdb_set2$pred.m0
xdb_set1$omi_no2_ind <- 1
xdb_set2$omi_no2_ind <- 0
dta_merged <- rbind(xdb_set1,xdb_set2)



#### Mixed effect models with imputed datasets ####
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta_merged)
dta_merged$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_zi~pred.m0,data=dta_merged))$r.squared)
# [1] 0.5594855

# Random intercept, x and y, blh, temp, elevation (this has NA = 7192, based on dta2)
dta3 <- dta_merged[!is.na(dta_merged$elevation), ]
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta3)
dta3$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_zi~pred.m0,data=dta3))$r.squared)
# [1] 0.5618029

# Random intercept, x and y, blh, temp, elevation, road (this has NA = 98656, based on dta2)
dta4 <- dta_merged[!is.na(dta_merged$road_length), ]
dta4 <- dta4[!is.na(dta4$elevation), ]
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta4)
dta4$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_zi~pred.m0,data=dta4))$r.squared)
# [1] 0.6092317



#### Testing Random Forest Models of Imputed OMI ####
# Imputed OMI: Random forest of x and y, blh, temp
tryrf <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z)
mod1rf <- ranger(tryrf, data=dta_merged, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9958037
mod1rf$prediction.error #[1] 0.003911836
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)

# Imputed OMI: Random forest of x and y, blh, temp, elevation (this has NA = 7688, based on dta_merged)
tryrf <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z)
mod1rf <- ranger(tryrf, data=dta3, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9959141
mod1rf$prediction.error #[1] 0.003819105
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)

# Imputed OMI: Random forest of x and y, blh, temp, elevation, road (this has NA = 108996, based on dta_merged)
tryrf <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z)
mod1rf <- ranger(tryrf, data=dta4, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9932061
mod1rf$prediction.error #[1] 0.006773456
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)

