#### First Stage NO2 Modeling with 2019 data ####
#### April 12, 2021 ####
#### Updated May 6, 2021 ####

## 5/10: Added analyses for jday/yday, code for predicting full dataset
## 5/14: Updated portions of code to change outcome to non Z-score

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
dta <- read_csv("first_stage_2019.csv")



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

## Adding Julian date
dta$jd <- julian(dta$date)
dta$yday <- yday(dta$date)
dta <- filter(dta, date != "2019-12-31") ## to prevent issues later on



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
names <- c("lat", "long", "omi_no2", "cams_no2", "tropomi_no2", "temp_2m", "blh", "elevation", "road_length", "jd", "year", "yday")

scaled <- dta[, names] %>%
  mutate_all(funs(scale))

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
#Multiple R-squared:  0.04428,	Adjusted R-squared:  0.04428

# Include x and y
try <- lm(omi_no2_z ~ cams_no2_z + lat_z + long_z, data=dta2)
summary(try)
#Multiple R-squared:  0.1357,	Adjusted R-squared:  0.1357

# Random intercept by day: mixed effects model
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
summary(mod_0)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.5541898

# Random intercept + x and y
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.6161429

# Random intercept, x and y, blh
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.6257868

# Random intercept, x and y, blh, temp
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta2)
dta2$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta2))$r.squared)
# [1] 0.6257944

# Random intercept, x and y, blh, temp, elevation (this has NA = 49924, based on dta2)
dta3 <- dta2[!is.na(dta2$elevation), ]
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta3)
dta3$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta3))$r.squared)
# [1] 0.6274506

# Random intercept, x and y, blh, temp, elevation, road (this has NA = 716708, based on dta2)
dta4 <- dta2[!is.na(dta2$road_length), ]
dta4 <- dta4[!is.na(dta4$elevation), ]
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta4)
dta4$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta4))$r.squared)
# [1] 0.6511759

# Random intercept, x and y, blh, temp, elevation, road, and TROPOMI data (this has NA = 1443893, based on dta2)
dta5 <- dta2[!is.na(dta2$road_length), ]
dta5 <- dta5[!is.na(dta5$elevation), ]
dta5 <- dta5[!is.na(dta5$tropomi_no2), ]
formula_1 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z + tropomi_no2_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta5)
dta5$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta5))$r.squared)
# [1] 0.726535



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

## Remove 12/31 to prevent further errors
#dta_merged <- filter(dta_merged, date != "2019-12-31")


#### Mixed effect models with imputed datasets ####
## Note that because CAMS data is missing 12/31; there are missings that should be removed prior to running the code
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta_merged)
dta_merged$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_zi~pred.m0,data=dta_merged))$r.squared)
# [1] 0.7350369

# Random intercept, x and y, blh, temp, elevation (this has NA = 49924, based on dta2)
dta3 <- dta_merged[!is.na(dta_merged$elevation), ]
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta3)
dta3$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_zi~pred.m0,data=dta3))$r.squared)
# [1] 0.734671

# Random intercept, x and y, blh, temp, elevation, road (this has NA = 716708, based on dta2)
dta4 <- dta_merged[!is.na(dta_merged$road_length), ]
dta4 <- dta4[!is.na(dta4$elevation), ]
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta4)
dta4$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_zi~pred.m0,data=dta4))$r.squared)
# [1] 0.7436294

# Random intercept, x and y, blh, temp, elevation, road, and TROPOMI data (this has NA = 1443893, based on dta2)
dta5 <- dta_merged[!is.na(dta_merged$road_length), ]
dta5 <- dta5[!is.na(dta5$elevation), ]
dta5 <- dta5[!is.na(dta5$tropomi_no2), ]
formula_1 <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + elevation_z + road_length_z + tropomi_no2_z + (1 + cams_no2_z | date))
mod_0 <- lmer(formula_1, data = dta5)
dta5$pred.m0 <- predict(mod_0)
print(summary(lm(omi_no2_z~pred.m0,data=dta5))$r.squared)
# [1] 0.6419816



#### Testing Random Forest Models of Imputed OMI ####
## This is not possible to run at the moment with the full model
# Imputed OMI: Random forest of x and y, blh, temp
tryrf <- as.formula(omi_no2_zi ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z)
mod1rf <- ranger(tryrf, data=dta_merged, num.trees=100, mtry=5, importance="impurity", keep.inbag=TRUE)
mod1rf$r.squared #[1] 0.9958037
mod1rf$prediction.error #[1] 0.003911836
plot(importance(mod1rf))
mod1rf.importance <- sort(importance(mod1rf), decreasing = TRUE)
cbind(mod1rf.importance)



#### Testing Random Forest Models with a subset (following Massimo's code)####
## Subset of 500,000
set.seed(824)
s1 <- dta2[sample(nrow(dta2), 500000),] ## Note: I updated this with dta2 instead of dta_merged, which had the already imputed omi from mixed effect models

## Default model
formula <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z)

mod1 <- ranger(formula, data=s1, num.trees=50, mtry=4)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.1175358 OOB.R2=0.8816701 RUN in ~ 1 minute.

mod1 <- ranger(formula, data=s1, num.trees=100, mtry=4)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.1101278 OOB.R2=0.8891282 RUN in ~ 2 minutes.

mod1 <- ranger(formula, data=s1, num.trees=150, mtry=4)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.1073423 OOB.R2=0.8919325 RUN in ~ 3 minutes.

mod1 <- ranger(formula, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.1167746 OOB.R2=0.8824365 RUN in ~ 1 minute.

## Doesn't look too different, choosing num.trees=100 and mtry=4 as the "final" model here

mod1 <- ranger(formula, data=s1, num.trees=100, mtry=4, importance="impurity")
mod1.importance <- sort(importance(mod1), decreasing = TRUE)
cbind(mod1.importance)

#mod1.importance
#cams_no2_z       109146.86
#blh_z            106203.55
#long_z           105895.70
#lat_z             95313.10
#temp_2m_z         75222.78

mod1 <- ranger(formula, data=s1, num.trees=100, mtry=4, importance="permutation")
mod1.importance <- sort(importance(mod1), decreasing = TRUE)
cbind(mod1.importance)

#mod1.importance
#long_z           1.1663868
#lat_z            1.1340105
#blh_z            0.9587731
#temp_2m_z        0.8954283
#cams_no2_z       0.8555927


## Adding Julian date
formula <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + jd_z)
formula2 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + jd)

# When I try to as.numeric(jd), I get the following error:
# Error in parse.formula(formula, data, env = parent.frame()) : 
# Error: Illegal column names in formula interface. Fix column names or use alternative interface in ranger.
# But using a z-scored jd should get it to read as numeric right?
 
mod1 <- ranger(formula, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.003877914 OOB.R2=0.9960959 RUN in ~ 3 minutes.

mod1 <- ranger(formula2, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.003840703 OOB.R2=0.9961334 RUN in ~ 3 minutes.

mod1 <- ranger(formula2, data=s1, num.trees=50, mtry=5, respect.unordered.factors = TRUE)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.003760565 OOB.R2=0.996214 RUN in ~ 3 minutes.

## jd or z-score jd doesn't seem to make a difference

mod1 <- ranger(formula, data=s1, num.trees=100, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.003190769 OOB.R2=0.9967877 RUN in ~ 3 minutes.

mod1 <- ranger(formula, data=s1, num.trees=100, mtry=5, importance="impurity")
mod1.importance <- sort(importance(mod1), decreasing = TRUE)
cbind(mod1.importance)

#mod1.importance
#jd               214433.61
#long_z            86576.77
#lat_z             72312.03
#cams_no2_z        58629.43
#blh_z             43200.68
#temp_2m_z         21036.65

mod1 <- ranger(formula, data=s1, num.trees=100, mtry=5, importance="permutation")
mod1.importance <- sort(importance(mod1), decreasing = TRUE)
cbind(mod1.importance)

#mod1.importance
#jd               1.5224702
#lat_z            0.7876117
#long_z           0.7534276
#cams_no2_z       0.5783542
#blh_z            0.4906858
#temp_2m_z        0.2399435

## Using yday (1-365)
formula <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + yday_z)
formula2 <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + yday)

mod1 <- ranger(formula, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.003772623 OOB.R2=0.9962019 RUN in ~ 3 minutes.

mod1 <- ranger(formula2, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.003776882 OOB.R2=0.9961976 RUN in ~ 3 minutes.



#### Cross Validation of Sample (500,000) Dataset ####
set.seed(824)
size <- dim(s1)[1]
random                <- sample(size)
s1[,random:= random]
s1[random>=0*round(size/10)+1 & random<=1*round(size/10), split:= 1]
s1[random>=1*round(size/10)+1 & random<=2*round(size/10), split:= 2]
s1[random>=2*round(size/10)+1 & random<=3*round(size/10), split:= 3]
s1[random>=3*round(size/10)+1 & random<=4*round(size/10), split:= 4]
s1[random>=4*round(size/10)+1 & random<=5*round(size/10), split:= 5]
s1[random>=5*round(size/10)+1 & random<=6*round(size/10), split:= 6]
s1[random>=6*round(size/10)+1 & random<=7*round(size/10), split:= 7]
s1[random>=7*round(size/10)+1 & random<=8*round(size/10), split:= 8]
s1[random>=8*round(size/10)+1 & random<=9*round(size/10), split:= 9]
s1[random>=9*round(size/10)+1 & random<=size,             split:= 10]
gc()

formula <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z + jd_z)
s1 <- as.data.table(s1)

# Takes about 25 minutes to run
final =  foreach (i=1:10, .packages = c("data.table", "ranger") ) %dopar% {
   print(i)
   test               <- subset(s1, split==i)
   train              <- subset(s1, split!=i)
   mod                <- ranger(formula, data=train, num.trees=50, mtry=5)
   pred               <- mod$predictions
   test$pred.no2.cv   <- predict(mod, test)$predictions
   test$itercv        <- i
   test[, list(jd, x, y, grid_id, omi_no2_z, pred.no2.cv, itercv)]
 }
mod.all = do.call(rbind, final)

# Overall
m1.fit.all  <- lm(omi_no2_z~pred.no2.cv, data=mod.all)
r2.all      <- summary(m1.fit.all)$r.squared #0.9906183 (0.9898922 for non CV)
rmspe       <- ModelMetrics::rmse(modelObject = m1.fit.all) #0.08481805
inter       <- summary(m1.fit.all)$coef[1,1] #0.002352379
slope       <- summary(m1.fit.all)$coef[2,1] #1.023173

# Spatial
spatial_all <- mod.all %>%
   group_by(grid_id) %>%
   dplyr::summarize(
      bar_omi = mean(omi_no2_z),
      bar_pred = mean(pred.no2.cv))

m1.fit.all.s    <- lm(bar_omi ~ bar_pred, data=spatial_all)
r2.spatial      <- summary(m1.fit.all.s)$r.squared
rmse.spatial    <- ModelMetrics::rmse(modelObject = m1.fit.all.s)
spatial.inter   <- summary(m1.fit.all.s)$coef[1,1]
spatial.slope   <- summary(m1.fit.all.s)$coef[2,1]

# Temporal
temporal_all    <- left_join(mod.all,spatial_all)
del_no2         <- temporal_all$omi_no2_z-temporal_all$bar_omi
del_pred        <- temporal_all$pred.no2.cv-temporal_all$bar_pred
mod_temporal    <- lm(del_no2 ~ del_pred)
r2.temporal     <- summary(mod_temporal)$r.squared
rmse.temporal   <- ModelMetrics::rmse(modelObject = mod_temporal)
temporal.inter  <- summary(mod_temporal)$coef[1,1]
temporal.slope  <- summary(mod_temporal)$coef[2,1]
 
# Results
options(scipen=999)
cv.results <<- rbind(r2.all,      rmspe,         inter,          slope,
                      r2.spatial,  rmse.spatial,  spatial.inter,  spatial.slope, 
                      r2.temporal, rmse.temporal, temporal.inter, temporal.slope)
cv.results <- round(cv.results,digits=3)
print(cv.results)
# r2.all         0.991
# rmspe          0.085
# inter          0.002
# slope          1.023
# r2.spatial     0.998
# rmse.spatial   0.019
# spatial.inter  0.001
# spatial.slope  1.008
# r2.temporal    0.988
# rmse.temporal  0.083
# temporal.inter 0.000
# temporal.slope 1.028



#### Scaled vs. Unscaled Models? ####
formula <- as.formula(omi_no2_z ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z)
formula2 <- as.formula(omi_no2 ~ cams_no2 + long + lat + blh + temp_2m)

mod1 <- ranger(formula, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=0.1165895 OOB.R2=0.8833758 RUN in ~ 2 minutes.

mod1 <- ranger(formula2, data=s1, num.trees=50, mtry=5)
mod1$prediction.error
mod1$r.squared
#OOB.RMSE=7.709241e+29 OOB.R2=0.8822025 RUN in ~ 2 minutes.


## Check scales
head((dta$lat - mean(dta$lat)) / sd(dta$lat))
head(scale(dta$lat))

head(((dta$lat_z)*sd(dta$lat)) + mean(dta$lat))
head(dta$lat)



#### Predicting for Full Dataset ####
## Subset of 500,000
set.seed(824)
s1 <- dta2[sample(nrow(dta2), 500000),]

## Change this to full dataset, finalized model once we're ready
formula <- as.formula(omi_no2 ~ cams_no2_z + long_z + lat_z + blh_z + temp_2m_z)
formula2 <- as.formula(omi_no2 ~ cams_no2 + long + lat + blh + temp_2m)
mod1 <- ranger(formula, data=s1, num.trees=50, mtry=4)
dta$pred_no2  <- predict(mod1, dta)$predictions

# Replace observed NO2 to predicted, when observed are available
dta <- as.data.table(dta)
dta[,rf_no2 := pred_no2]
dta[!is.na(omi_no2), rf_no2 := omi_no2]

cor(dta$pred_no2, dta$rf_no2) #0.9689745
cor(dta$omi_no2, dta$pred_no2, use = "complete.obs") #0.9541778
cor(dta$omi_no2, dta$rf_no2, use = "complete.obs") #1


## Extract relevant information (update with final model)
output <- dta %>%
   select(x, y, date, rf_no2)

write_csv(output, "rf_no2_2019.csv")
