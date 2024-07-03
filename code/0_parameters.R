#### Load required R packages ####
rm(list = ls())    # clear workspace
library(tidyverse) # for data manipulation
library(haven)     # to read Stata files
library(labelled)  # to remove stata variable names
library(forcats)   # for factor manipulation
library(readxl)    # to read Excel files


#### Parameters ####
# Official exchange rate https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=FJ
xrate2010       <- 1.92 # USD to FJD in 2010
xrate2013       <- 1.84 # USD to FJD in 2013
xrate2020       <- 2.17 # USD to FJD in 2020

# GDP deflator date from https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=FJ
defl2020.2013   <- 109.6 / 88.9 # GDP deflator 2013/2020
defl2020.2010   <- 109.6 / 75.7 # GDP deflator 2010/2020
hhsize          <- 4.2 # average household size in Fiji

# Unit costs of health services from WHO-CHOICE and a Fiji costing study (Irava)
cVisit    <- c("base" = (17.39  * defl2020.2010 / xrate2020),              # NAU PHC
               "low"  = (4.58   * xrate2010 * defl2020.2010 / xrate2020),  # WHO-CHOICE
               "high" = (43.57  * defl2020.2010 / xrate2020))              # LTK hospital
cWard     <- c("base" = (84.35  * defl2020.2010 / xrate2020),              # CWM hospital
               "low"  = (33.83  * xrate2010 * defl2020.2010 / xrate2020),  # WHO-CHOICE
               "high" = (91.77  * defl2020.2010 / xrate2020))              # LTK hospital
cICU      <- c("base" = ((178.93+577.80)/2 * defl2020.2010 / xrate2020),   # LTK hospital
               "low"  = (178.93 * defl2020.2010 / xrate2020),              # CWM hospital
               "high" = (577.80 * defl2020.2010 / xrate2020))              # CWM hospital
cLabtest  <- c("base" = ((16.97+22.14)/2  * defl2020.2010 / xrate2020),    # LTK hospital
               "low"  = (16.97  * defl2020.2010 / xrate2020),              # LTK hospital
               "high" = (22.14  * defl2020.2010 / xrate2020))              # LTK hospital

# Unit costs of medicines. Use mean if unknown. Prices from 2013 FJD to 2020 USD
cCloxacIV <- c("base" = 0.480 / xrate2020)    # intravenous cloxacillin
cGentamIV <- c("base" = 0.074 / xrate2020)    # intravenous gentamicin
cPeniciIV <- c("base" = 1.414 / xrate2020)    # intravenous penicillin procaine
cMetronIV <- c("base" = 1.940 / xrate2020)    # intravenous metronidazole
cErythrIV <- c("base" = 10.17 / xrate2020)    # intravenous erythromycin
cCeftriIV <- c("base" = 0.899 / xrate2020)    # intravenous ceftriaxone
cCiprofIV <- c("base" = 6.300 / xrate2020)    # intravenous ciprofloxacin
cMeropeIV <- c("base" = 29.00 / xrate2020)    # intravenous meropenem
cOthersIV <- c("base" = mean(cCloxacIV, cGentamIV, cPeniciIV, cMetronIV, 
                             cErythrIV, cCeftriIV, cCiprofIV, cMeropeIV))
cPeniciIM <- c("base" = 0.950 / xrate2020)    # intramuscular penicillin
cFlucloTb <- c("base" = 0.080 / xrate2020)    # tablet flucloxacillin
cPeniciTb <- c("base" = 0.028 / xrate2020)    # tablet penicillin
cAmoxycTb <- c("base" = 0.040 / xrate2020)    # tablet amoxicillin
cMetronTb <- c("base" = 0.010 / xrate2020)    # tablet metronidazole
cErythrTb <- c("base" = 0.060 / xrate2020)    # tablet erythromycin
cDoxycyTb <- c("base" = 0.027 / xrate2020)    # tablet flucloxacillin
cCephalTb <- c("base" = 0.034)                # tablet cephalexin
cSeptriTb <- c("base" = 0.016 / xrate2020)    # tablet septrin
cOthersTb <- c("base" = mean(cFlucloTb, cPeniciTb, cAmoxycTb, cMetronTb,
                             cErythrTb, cDoxycyTb, cCephalTb, cSeptriTb))
cFlucloSn <- c("base" = 0.238 / xrate2020)    # suspension flucloxacillin
cPeniciSn <- c("base" = 2.990000 / xrate2020) # suspension penicillin
cAmoxycSn <- c("base" = 0.850 / xrate2020)    # suspension amoxicillin
cMetronSn <- c("base" = 0.560)                # suspension metronidazole
cErythrSn <- c("base" = 2.460 / xrate2020)    # suspension erythromycin
cCephalSn <- c("base" = 0.560)                # suspension cephalexin
cSeptriSn <- c("base" = 0.630 / xrate2020)    # suspension septrin
cOthersSn <- c("base" = mean(cFlucloSn, cPeniciSn, cAmoxycSn, cMetronSn, 
                             cErythrSn, cDoxycySn, cCephalSn, cSeptriSn))
cPermetCr <- c("base" = 1.165)                # cream permethrin


#### Clean medication data ####

# Read medication data from Excel file
medsCost        <- read_excel("data/Drugs.xlsx", sheet = "Costs", range = cell_cols("A:G"))
medsDose        <- as.data.frame(read_excel("data/Drugs.xlsx", sheet = "Doses"))

# Create list called medList for daily doses. Use mean by age and form if unknown. 
medList                       <- as.list(setNames(medsDose$DoseDaily, 
                                                  medsDose$NameAge))
medList["qInjnOthers_00.04"]  <- mean(unlist(medList[c("qInjnCloxac_00.04", 
                                                       "qInjnGentam_00.04", 
                                                       "qInjnPenici_00.04", 
                                                       "qInjnMetron_00.04", 
                                                       "qInjnErythr_00.04", 
                                                       "qInjnCeftri_00.04", 
                                                       "qInjnCiprof_00.04", 
                                                       "qInjnMerope_00.04")]))
medList["qInjnOthers_05.09"]  <- mean(unlist(medList[c("qInjnCloxac_05.09", 
                                                       "qInjnGentam_05.09", 
                                                       "qInjnPenici_05.09", 
                                                       "qInjnMetron_05.09", 
                                                       "qInjnErythr_05.09", 
                                                       "qInjnCeftri_05.09", 
                                                       "qInjnCiprof_05.09", 
                                                       "qInjnMerope_05.09")]))
medList["qInjnOthers_10.14"]  <- mean(unlist(medList[c("qInjnCloxac_10.14", 
                                                       "qInjnGentam_10.14", 
                                                       "qInjnPenici_10.14", 
                                                       "qInjnMetron_10.14", 
                                                       "qInjnErythr_10.14", 
                                                       "qInjnCeftri_10.14", 
                                                       "qInjnCiprof_10.14", 
                                                       "qInjnMerope_10.14")]))
medList["qInjnOthers_15plus"] <- mean(unlist(medList[c("qInjnCloxac_15plus", 
                                                       "qInjnGentam_15plus", 
                                                       "qInjnPenici_15plus", 
                                                       "qInjnMetron_15plus", 
                                                       "qInjnErythr_15plus", 
                                                       "qInjnCeftri_15plus", 
                                                       "qInjnCiprof_15plus", 
                                                       "qInjnMerope_15plus")]))
medList["qSuspOthers_00.04"]  <- mean(unlist(medList[c("qSuspFlucl_00.04", 
                                                       "qSuspPenic_00.04", 
                                                       "qSuspAmoxy_00.04", 
                                                       "qSuspMetro_00.04", 
                                                       "qSuspEryth_00.04", 
                                                       "qOralDoxyc_00.04", 
                                                       "qSuspCepha_00.04", 
                                                       "qSuspSeptr_00.04")]))
medList["qSuspOthers_05.09"]  <- mean(unlist(medList[c("qSuspFlucl_05.09", 
                                                       "qSuspPenic_05.09", 
                                                       "qOralAmoxy_05.09", 
                                                       "qSuspMetro_05.09", 
                                                       "qSuspEryth_05.09", 
                                                       "qOralDoxyc_05.09", 
                                                       "qSuspCepha_05.09", 
                                                       "qSuspSeptr_05.09")]))
medList["qOralOthers_10.14"]  <- mean(unlist(medList[c("qOralFlucl_15plus", 
                                                       "qOralPenic_15plus", 
                                                       "qOralAmoxy_15plus", 
                                                       "qOralMetro_15plus", 
                                                       "qOralEryth_15plus", 
                                                       "qOralDoxyc_15plus", 
                                                       "qOralCepha_15plus", 
                                                       "qOralSeptr_15plus")]))
medList["qOralOthers_15plus"] <- mean(unlist(medList[c("qOralFlucl_15plus", 
                                                       "qOralPenic_15plus", 
                                                       "qOralAmoxy_15plus", 
                                                       "qOralMetro_15plus", 
                                                       "qOralEryth_15plus", 
                                                       "qOralDoxyc_15plus", 
                                                       "qOralCepha_15plus", 
                                                       "qOralSeptr_15plus")]))

save(medsCost,  file = "data/medsCost.Rdata")
save(medsDose,  file = "data/medsDose.Rdata")