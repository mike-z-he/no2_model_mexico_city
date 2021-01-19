#### Cleaning monitoring station data ####
#### January 16, 2021 ####

setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city")
options(mc.cores=parallel::detectCores())


#### Load packages ####
library(tidyverse)
library(readxl)
library(here)



#### Load data ####

## Data was previous extracted from zip files manually and loaded into individual
## folders based on pollutant type
## Set working directory depending on what data is loaded

#setwd(here("data/monitor_data/rama/no"))
#setwd(here("data/monitor_data/rama/no2"))
#setwd(here("data/monitor_data/rama/ozone"))

file.list <- list.files(pattern = "*.xls")
df.list <- lapply(file.list, read_excel)



#### Data merging ####

## Run dta portion 3 times separately for each data, to create no.dta, no2.dta, ozone.dta

## Single dataframe with the data
dta <- bind_rows(df.list)

## Replace -99s with NA
dta[dta == -99] <- NA

## Replace 0s with NA
dta[dta == 0] <- NA

## Create daily averages based on existing data
no.dta <- dta %>%
  group_by(FECHA) %>%
  summarize(
    no_tac = mean(TAC, na.rm = TRUE),
    no_fac = mean(FAC, na.rm = TRUE),
    no_sag = mean(SAG, na.rm = TRUE),
    no_tla = mean(TLA, na.rm = TRUE),
    no_xal = mean(XAL, na.rm = TRUE),
    no_mer = mean(MER, na.rm = TRUE),
    no_ped = mean(PED, na.rm = TRUE),
    no_ces = mean(CES, na.rm = TRUE),
    no_han = mean(HAN, na.rm = TRUE),
    no_tli = mean(TLI, na.rm = TRUE),
    no_ati = mean(ATI, na.rm = TRUE),
    no_vif = mean(VIF, na.rm = TRUE),
    no_pla = mean(PLA, na.rm = TRUE),
    no_lag = mean(LAG, na.rm = TRUE),
    no_azc = mean(AZC, na.rm = TRUE),
    no_uiz = mean(UIZ, na.rm = TRUE),
    no_tax = mean(TAX, na.rm = TRUE),
    no_sur = mean(SUR, na.rm = TRUE),
    no_izt = mean(IZT, na.rm = TRUE),
    no_aco = mean(ACO, na.rm = TRUE),
    no_cam = mean(CAM, na.rm = TRUE),
    no_cho = mean(CHO, na.rm = TRUE),
    no_coy = mean(COY, na.rm = TRUE),
    no_cua = mean(CUA, na.rm = TRUE),
    no_lla = mean(LLA, na.rm = TRUE),
    no_lpr = mean(LPR, na.rm = TRUE),
    no_mon = mean(MON, na.rm = TRUE),
    no_nez = mean(NEZ, na.rm = TRUE),
    no_sja = mean(SJA, na.rm = TRUE),
    no_tah = mean(TAH, na.rm = TRUE),
    no_tpn = mean(TPN, na.rm = TRUE),
    no_cut = mean(CUT, na.rm = TRUE),
    no_hgm = mean(HGM, na.rm = TRUE),
    no_sfe = mean(SFE, na.rm = TRUE),
    no_uax = mean(UAX, na.rm = TRUE),
    no_cca = mean(CCA, na.rm = TRUE),
    no_ajm = mean(AJM, na.rm = TRUE),
    no_bju = mean(BJU, na.rm = TRUE),
    no_inn = mean(INN, na.rm = TRUE),
    no_mgh = mean(MGH, na.rm = TRUE),
    no_mpa = mean(MPA, na.rm = TRUE),
    no_far = mean(FAR, na.rm = TRUE),
    no_sac = mean(SAC, na.rm = TRUE)
  )

no2.dta <- dta %>%
  group_by(FECHA) %>%
  summarize(
    no2_tac = mean(TAC, na.rm = TRUE),
    no2_fac = mean(FAC, na.rm = TRUE),
    no2_sag = mean(SAG, na.rm = TRUE),
    no2_tla = mean(TLA, na.rm = TRUE),
    no2_xal = mean(XAL, na.rm = TRUE),
    no2_mer = mean(MER, na.rm = TRUE),
    no2_ped = mean(PED, na.rm = TRUE),
    no2_ces = mean(CES, na.rm = TRUE),
    no2_han = mean(HAN, na.rm = TRUE),
    no2_tli = mean(TLI, na.rm = TRUE),
    no2_ati = mean(ATI, na.rm = TRUE),
    no2_vif = mean(VIF, na.rm = TRUE),
    no2_pla = mean(PLA, na.rm = TRUE),
    no2_lag = mean(LAG, na.rm = TRUE),
    no2_azc = mean(AZC, na.rm = TRUE),
    no2_uiz = mean(UIZ, na.rm = TRUE),
    no2_tax = mean(TAX, na.rm = TRUE),
    no2_sur = mean(SUR, na.rm = TRUE),
    no2_izt = mean(IZT, na.rm = TRUE),
    no2_aco = mean(ACO, na.rm = TRUE),
    no2_cam = mean(CAM, na.rm = TRUE),
    no2_cho = mean(CHO, na.rm = TRUE),
    no2_coy = mean(COY, na.rm = TRUE),
    no2_cua = mean(CUA, na.rm = TRUE),
    no2_lla = mean(LLA, na.rm = TRUE),
    no2_lpr = mean(LPR, na.rm = TRUE),
    no2_mon = mean(MON, na.rm = TRUE),
    no2_nez = mean(NEZ, na.rm = TRUE),
    no2_sja = mean(SJA, na.rm = TRUE),
    no2_tah = mean(TAH, na.rm = TRUE),
    no2_tpn = mean(TPN, na.rm = TRUE),
    no2_cut = mean(CUT, na.rm = TRUE),
    no2_hgm = mean(HGM, na.rm = TRUE),
    no2_sfe = mean(SFE, na.rm = TRUE),
    no2_uax = mean(UAX, na.rm = TRUE),
    no2_cca = mean(CCA, na.rm = TRUE),
    no2_ajm = mean(AJM, na.rm = TRUE),
    no2_bju = mean(BJU, na.rm = TRUE),
    no2_inn = mean(INN, na.rm = TRUE),
    no2_mgh = mean(MGH, na.rm = TRUE),
    no2_mpa = mean(MPA, na.rm = TRUE),
    no2_far = mean(FAR, na.rm = TRUE),
    no2_sac = mean(SAC, na.rm = TRUE)
  )

ozone.dta <- dta %>%
  group_by(FECHA) %>%
  summarize(
    ozone_tac = mean(TAC, na.rm = TRUE),
    ozone_fac = mean(FAC, na.rm = TRUE),
    ozone_sag = mean(SAG, na.rm = TRUE),
    ozone_tla = mean(TLA, na.rm = TRUE),
    ozone_xal = mean(XAL, na.rm = TRUE),
    ozone_mer = mean(MER, na.rm = TRUE),
    ozone_ped = mean(PED, na.rm = TRUE),
    ozone_ces = mean(CES, na.rm = TRUE),
    ozone_han = mean(HAN, na.rm = TRUE),
    ozone_tli = mean(TLI, na.rm = TRUE),
    ozone_ati = mean(ATI, na.rm = TRUE),
    ozone_vif = mean(VIF, na.rm = TRUE),
    ozone_pla = mean(PLA, na.rm = TRUE),
    ozone_lag = mean(LAG, na.rm = TRUE),
    ozone_azc = mean(AZC, na.rm = TRUE),
    ozone_uiz = mean(UIZ, na.rm = TRUE),
    ozone_tax = mean(TAX, na.rm = TRUE),
    ozone_sur = mean(SUR, na.rm = TRUE),
    ozone_izt = mean(IZT, na.rm = TRUE),
    ozone_aco = mean(ACO, na.rm = TRUE),
    ozone_cam = mean(CAM, na.rm = TRUE),
    ozone_cho = mean(CHO, na.rm = TRUE),
    ozone_coy = mean(COY, na.rm = TRUE),
    ozone_cua = mean(CUA, na.rm = TRUE),
    ozone_lla = mean(LLA, na.rm = TRUE),
    ozone_lpr = mean(LPR, na.rm = TRUE),
    ozone_mon = mean(MON, na.rm = TRUE),
    ozone_nez = mean(NEZ, na.rm = TRUE),
    ozone_sja = mean(SJA, na.rm = TRUE),
    ozone_tah = mean(TAH, na.rm = TRUE),
    ozone_tpn = mean(TPN, na.rm = TRUE),
    ozone_cut = mean(CUT, na.rm = TRUE),
    ozone_hgm = mean(HGM, na.rm = TRUE),
    ozone_sfe = mean(SFE, na.rm = TRUE),
    ozone_uax = mean(UAX, na.rm = TRUE),
    ozone_cca = mean(CCA, na.rm = TRUE),
    ozone_ajm = mean(AJM, na.rm = TRUE),
    ozone_bju = mean(BJU, na.rm = TRUE),
    ozone_inn = mean(INN, na.rm = TRUE),
    ozone_mgh = mean(MGH, na.rm = TRUE),
    ozone_mpa = mean(MPA, na.rm = TRUE),
    ozone_far = mean(FAR, na.rm = TRUE),
    ozone_sac = mean(SAC, na.rm = TRUE)
  )

## Merge three datasets together
exposure.dta <- merge(no.dta, no2.dta, by = "FECHA")
exposure.dta <- merge(exposure.dta, ozone.dta, by = "FECHA")

## Flatten dataset to contain all rows in one column
long <- exposure.dta %>%
  pivot_longer(
    -FECHA,
    names_to = c(".value", "monitor"),
    names_sep = "_"
  )

## rename FECHA to date
long <- rename(long, date = FECHA)

## Export dataset
#write.csv(long, "monitor_data_20210118.csv")



#### Descriptive statistics of monitoring data ####
setwd("D:/Users/profu/Documents/Schoolwork/Postdoc/Research Projects/no2_model_mexico_city/data/monitor_data")
library(patchwork)

dta <- read.csv("monitor_data_20210118.csv", header=TRUE, stringsAsFactors = FALSE)
dta$date <- as.Date(dta$date, format="%m/%d/%Y")

summary(dta)

a <- ggplot(dta, aes(x=date, y=no)) +
  geom_line() +
  theme_bw() +
  labs(y = expression("NO (ppb)"), x = "Date")

b <- ggplot(dta, aes(x=date, y=no2)) +
  geom_line() +
  theme_bw() +
  labs(y = expression("NO"[2]*" (ppb)"), x = "Date")

c <- ggplot(dta, aes(x=date, y=ozone)) +
  geom_line() +
  theme_bw() +
  labs(y = expression("O"[3]*" (ppb)"), x = "Date")


a / b / c 
