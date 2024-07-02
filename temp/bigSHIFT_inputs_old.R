
# Load packages needed
rm(list = ls())
library(tidyverse)
library(haven)
library(gtsummary)
library(flextable)
library(officer) # to export to ms word
library(labelled) # to remove stata variable names
library(forcats)
library(readxl)



##### Parameters ####
#
# Population figures for Fiji 2017 census

################################################ TASIU 01
# pop <- list(
#   "total" = 131914,
#   "male"  =  67519
# )

popTotal      <- 131914 # total population
popMale       <-  67519 # male population
popFemale     <-  64395 # female population
popiTaukei    <-  73115 # iTaukei ethnic group population
popIndian     <-  51273 # Fiji of Indian origin population
popOthers     <-   7526 # Other ethnicity population
pop0004       <-  14550 # population below 5 years
pop0514       <-  27423 # population 5-14 years
pop1524       <-  19934 # population 15 - 24 years
pop2534       <-  18214 # population 25 - 34 years
pop3544       <-  17335 # population 35 - 44 years
pop4554       <-  15406 # population 45 - 54 years
pop5564       <-  11076 # population 55 - 64 years
pop65plus     <-   7976 # population aged 65 plus
popMacuata    <-  65978 # Macuata division total population
popCakaudrove <-  34993 # Cakaudrove division total population
popBua        <-  15489 # Bua division total population
popTaveuni    <-  15454 # Taveuni division population
popUrban      <-  38806 # urban population
popRural      <-  93108 # rural population

# Official exchange rate https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=FJ
xrate2010       <- 1.92 # USD to FJD exchange rate in 2010
xrate2013       <- 1.84 # USD to FJD exchange rate in 2013
xrate2020       <- 2.17 # USD to FJD exchange rate in 2020

# GDP deflator date from https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=FJ
defl2020.2013   <- 109.6 / 88.9 # GDP deflator 2013/2020
defl2020.2010   <- 109.6 / 75.7 # GDP deflator 2010/2020

# cHospBedIrava   <-  57.31 
# cICUBedIrava    <- 189.69
# cLabTestIrava   <-  15.04
# cHospVisitIrava <-  40.12
# cHospBedWHO     <-  44.10 
# cHospVisitWHO   <-   7.06
# cPHCVisitWHO    <-   4.82

# Unit costs of health services from WHO-CHOICE and a Fiji costing study (Irava)
# https://www.who.int/publications/m/item/who-choice-estimates-of-cost-for-inpatient-and-outpatient-health-service-delivery
# https://www.health.gov.fj/wp-content/uploads/2018/03/Costing-Study-of-Selected-Health-Facilities-in-Fiji.pdf
cVisitChoice    <- (3.71 * xrate2010 * defl2020.2010) / xrate2020
cVisitNausori   <- (17.39 * defl2020.2010) / xrate2020
cVisitLautoka   <- (43.57 * defl2020.2010) / xrate2020
cVisitOPD       <- c("base" = cVisitNausori, # Nausori as base case
                     "low"  = cVisitChoice,  # WHO-CHOICE as low value
                     "high" = cVisitLautoka) # Lautoka as high value
cProcedur       <-  c("base" = cVisitLautoka) # Use higher value for surgical procedure costs


# Unit costs of injetable medicines. For other (unnamed) medications, used mean
# IV = IntraVenous
# IM = IntraMuscular
# the prices are 2013 FJD, so need to adjust to 2020 USD
cCloxacIV <- c("base" = 0.760 * defl2020.2013 / xrate2020) # cloxacillin
cGentamIV <- c("base" = 0.120 * defl2020.2013 / xrate2020) # gentamicin
cPeniciIV <- c("base" = 1.410 * defl2020.2013 / xrate2020) # penicillin procaine
cMetronIV <- c("base" = 1.800 * defl2020.2013 / xrate2020) # metronidazole (flagyl)
cErythrIV <- c("base" = 10.20 * defl2020.2013 / xrate2020) # erythromycin
cCeftriIV <- c("base" = 2.470 * defl2020.2013 / xrate2020) # ceftriaxone
cCiprofIV <- c("base" = 6.300 * defl2020.2013 / xrate2020) # ciprofloxacin
cMeropeIV <- c("base" = 40.00 * defl2020.2013 / xrate2020) # meropenem
cSeptriIV <- c("base" = 1.800 * defl2020.2013 / xrate2020) # septrin
cOthersIV <- c("base" = mean(cCloxacIV, cGentamIV, cPeniciIV, cMetronIV, cErythrIV, cCeftriIV, cCiprofIV, cMeropeIV))
cPeniciIM <- c("base" = 1.410 * defl2020.2013 / xrate2020)

# Unit costs of oral medicines. For other (unnamed) medications, used mean
# Tb - Tablet; Sn - Solution/Suspension for children
# the prices are 2020 prices
cFlucloTb <- c("base" = 0.153)
cPeniciTb <- c("base" = 0.105)
cAmoxycTb <- c("base" = 0.119)
cMetronTb <- c("base" = 0.030)
cErythrTb <- c("base" = 0.193)
cDoxycyTb <- c("base" = 0.193)
cCephalTb <- c("base" = 0.153)
cSeptriTb <- c("base" = 0.034)
cOthersTb <- c("base" = mean(cFlucloTb, cPeniciTb, cAmoxycTb, cMetronTb, cErythrTb, cDoxycyTb, cCephalTb, cSeptriTb))
cFlucloSn <- c("base" = 2.350)
cPeniciSn <- c("base" = 2.990)
cAmoxycSn <- c("base" = 1.710)
cMetronSn <- c("base" = 1.050)
cErythrSn <- c("base" = 2.460)
cCephalSn <- c("base" = 2.350)
cSeptriSn <- c("base" = 1.050)
cOthersSn <- c("base" = mean(cFlucloSn, cPeniciSn, cAmoxycSn, cMetronSn, cErythrSn, cDoxycySn, cCephalSn, cSeptriSn))

# Unit cost of topical medicines
cPermetCr <- c("base" = 0.905) # Permethrin cream for scabies



##### Data management ####
# Read data: meds - medicines data
phc      <- read_dta("data/PHC.dta") #  phc - primary care presentations
ssti     <- read_dta("data/SSTI.dta") # ssti - hospital admissions for skin and soft tissue infections; 
meds    <- read_excel("data/Drugs.xlsx", sheet = "Doses", range = cell_cols("A:B"))
meds    <- as.data.frame(meds)



################################################ TASIU 02
# medList <- as.list(setNames(meds$qty, meds$name)) # TO RUN
# list2env(medList, envir = .GlobalEnv) # DO NOT RUN


# Clean PHC data
var_label(phc) <- NULL # remove variable names from Stata file

phc <- phc %>% 
  # remove mass drug administration intervention period
  filter(timept==1|timept==2) %>% 
  
  # recode ethnicity into iTaukei and others; create some factor columns
  mutate(ethnicity_monthly_report = case_when(ethnicity_monthly_report == 3 ~ 2, TRUE ~ ethnicity_monthly_report),
         division  = factor(division, levels = c(1,2,3,4), 
                            labels = c("Macuata", "Cakaudrove", "Taveuni", "Bua")),
         sex       = factor(sex_monthly_report, levels = c(1,2), 
                            labels = c("Male", "Female")),
         ethnicity = factor(ethnicity_monthly_report, levels = c(1,2), 
                            labels = c("I-Taukei", "Other ethnicity")),
         residence = factor(urban_rural, levels = c(1,2), 
                            labels = c("Urban", "Rural")),
         timept    = factor(timept, levels = c(1,2), 
                            labels = c("Pre", "Post")),
         # age_grp   = factor(age_grp, levels = c(0,1,2,3,4,5,6,7),
         #                    labels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         age_5yr   = cut(age_integer, 
                         breaks = c(0, 5, 10, 15, 25, 35, 45, 55, 65, Inf),
                         labels = c("0-4", "5-9", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         age_grp   = fct_collapse(age_5yr, 
                                  "5-14" = c("5-9",   "10-14"),  
                                  "15+"  = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         ageD  = fct_collapse(age_5yr, 
                              "15+" = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         txClinicVisit = 1) %>%  # everyone has one clinic visit
  
  # rename columns to consistent format
  select(record_id      = record_id, 
         timept         = timept, 
         division       = division, 
         ethnicity      = ethnicity, 
         residence      = residence, 
         sex            = sex, 
         age_integer    = age_integer,
         age_grp        = age_grp,
         ageD           = ageD,
         context        = context_seen,
         dxScabies      = condition___sc,
         dxScabiesInf   = condition___is,
         dxImpetigo     = condition___im,
         dxCellulitis   = condition___ce,
         dxAbscess      = condition___ab,
         dxSevereSkin   = condition___ss,
         txClinicVisit  = txClinicVisit,   # people who had a health centre/clinic/school visit (everybody)
         txMedsTopical  = management___pe, # people given permethrin cream
         txMedsperOral  = management___po, # people given tablets (septrin)
         txMedsInjectn  = management___im, # people given pencillin G injection
         txAntibioticIV = management___iv, # people given other injections
         txProcSurgery  = management___pr, # people who had a surgical procedure
         txAdmittedIn   = management___ad, # people who were admitted
         txReferredOut  = management___ro, # people who were referred out to a hospital
         year           = myyear, 
         date           = mydate) %>% 
  
  # order observations by unique record ID
  arrange(record_id) %>% 
  
  # ensure indicator variables are in numeric format
  mutate(across(c(context:txReferredOut), as.numeric))

# If missing age group and child was seen in IMCI, then child is below 5 years
phc$age_grp[is.na(phc$age_grp) & phc$context == 3] <- "0-4"

# If missing age group and child was seen during school visit, then 5-14 years
phc$age_grp[is.na(phc$age_grp) & phc$context == 4] <- "5-14"

# Collapse age groups above 15 years to single category
# phc$age_grp <- fct_collapse(phc$age_grp, "15+" = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))

# Change coding for those without scabies from 0 to 2 (helps creation of table summaries)
phc$dxScabies[phc$dxScabies == 0] <- 2

# Zero hospital stay for PHC patients (variable created for consistency with ssti data)
phc$dHospStay <- 0










################################################ TASIU 03
## Calculate PHC cost by category
phc <- phc %>% 
  mutate(# clinic visits
         cClinicVisit = txClinicVisit * cVisitOPD["base"], 
         # topical medicines
         cMedsTopical = txMedsTopical * cPermetCr["base"], 
         # Cost of oral medications differ by age
         cMedsperOral = if_else(condition = ageD=="0-4", true = txMedsperOral * meds[16,2] * cSeptriSn["base"], 
                               if_else(condition = ageD=="5-9", true = txMedsperOral * meds[32,2] * cSeptriSn["base"], 
                                       if_else(condition = ageD=="10-14", true = txMedsperOral * meds[48,2] * cSeptriTb["base"], 
                                               false = txMedsperOral * meds[64,2] * cSeptriTb["base"]))),
         # injectable (IM penicillin)
         cMedsInjectn = txMedsInjectn * cPeniciIM["base"], 
         # surgical procedure
         cProcSurgery = txProcSurgery * cProcedur["base"], 
         # total costs
         cTotalPHC    = rowSums(pick(cClinicVisit:cProcSurgery), na.rm = T)) 









# Clean SSTI data

var_label(ssti) <- NULL

ssti <- ssti %>% 
  
  filter(timept==1|timept==2) %>% 
  
  mutate(sex       = factor(sex, levels = c(1,2),           
                            labels = c("Male", "Female")),
         ethnicity = factor(ethnicity_d2, levels = c(1,2),  
                            labels = c("I-Taukei", "Other ethnicity")),
         timept    = factor(timept, levels = c(1,2),        
                            labels = c("Pre", "Post")),
         age_5yr   = cut(enrolage_yrs, 
                         breaks = c(0, 5, 10, 15, 25, 35, 45, 55, 65, Inf),
                         labels = c("0-4", "5-9", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         age_grp   = fct_collapse(age_5yr, 
                                  "5-14" = c("5-9",   "10-14"),  
                                  "15+"  = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
         ageD  = fct_collapse(age_5yr, 
                                   "15+" = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))) %>% 
  
  select(record_id           = record_id,
         sex                 = sex,
         age_integer         = enrolage_yrs,
         age_grp             = age_grp,
         ageD                = ageD,
         ethnicity           = ethnicity,
         timept              = timept,
         admOutcome          = adm_outcome,
         dHospStay           = los,
         dICUAdmit           = icudays,
         dVentilation        = vent_days,
         dxScabies           = scabies,
         dxImpetigo          = impetigoyn,
         dxCellulitis        = sub_cel,
         dxAbscess           = sub_abs,
         dxNecrotisingFas    = sub_nf,
         dxWoundInf          = sub_wi,
         dxWoundInfSurg      = sub_swi,
         dxImpetigoSevere    = sub_si,
         dxScabiesInf        = sub_if,
         dxScabiesCrus       = sub_cs,
         dxLymphadenitis     = sub_la,
         dxPyomyositis       = sub_my,
         dxScabies.possibly  = pos_scabies,
         dxScabies.unlikely  = unlike_scabies,
         txProcAmputat       = amputation,
         txProcSurgery       = surgery,
         labCultureBlood     = bc,
         labCultureTissue    = tissueculture_yn,
         labSkinSwab         = skin_swabyn,
         ivab_check___clox, ivab_check___gent, ivab_check___pen, 
         ivab_check___met, ivab_check___ery, ivab_check___cef, 
         ivab_check___cip, ivab_check___mer, ivab_check___sep, 
         ivab_check___oth, oralab_checkad___flu, oralab_checkad___pen, 
         oralab_checkad___amo, oralab_checkad___met, oralab_checkad___ery,
         oralab_checkad___dox, oralab_checkad___cep, oralab_checkad___sep, 
         oralab_checkad___oth, oralab_typdc___flu, oralab_typdc___pen, 
         oralab_typdc___amo, oralab_typdc___met, oralab_typdc___ery, 
         oralab_typdc___dox, oralab_typdc___cep, oralab_typdc___sep, 
         oralab_typdc___oth, oralabduration, oralabdc_days,
         dInjnMeds        = ivabduration) %>% 
  
  mutate(admOutcome = factor(x = admOutcome, levels = c(1,2,3,4), 
                             labels = c("Discharged", "Transferred", "Disability", "Death"))) %>% 
  
  mutate(across(c(dxScabies:dInjnMeds), as.numeric)) %>% 
  
  mutate(dOralMeds   = rowSums(across(c(oralabduration,  # antibiotics taken while admitted
                                        oralabdc_days)), # antibiotics taken upon discharge 
                                  na.rm=TRUE),
         numInjnMeds    = rowSums(across(c(ivab_check___clox, ivab_check___gent, ivab_check___pen, ivab_check___met, 
                                          ivab_check___ery, ivab_check___cef, ivab_check___cip, ivab_check___mer, 
                                          ivab_check___sep, ivab_check___oth)),  
                                  na.rm=TRUE),
         numOralMeds    = rowSums(across(c(oralab_checkad___flu, oralab_checkad___pen, oralab_checkad___amo, 
                                           oralab_checkad___met, oralab_checkad___ery, oralab_checkad___dox, 
                                           oralab_checkad___cep, oralab_checkad___sep, oralab_checkad___oth, 
                                           oralab_typdc___flu, oralab_typdc___pen, oralab_typdc___amo, 
                                           oralab_typdc___met, oralab_typdc___ery, oralab_typdc___dox, 
                                           oralab_typdc___cep ,oralab_typdc___sep, oralab_typdc___oth)),  
                                  na.rm=TRUE)) %>%   
  
  mutate_at(c("dICUAdmit","dVentilation","dInjnMeds","dOralMeds"), ~replace_na(.,0)) 



ssti <- ssti %>%
  
  mutate(dInjnCloxac = if_else(condition = numInjnMeds!=0, true = ivab_check___clox/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnGentam = if_else(condition = numInjnMeds!=0, true = ivab_check___gent/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnPenici = if_else(condition = numInjnMeds!=0, true = ivab_check___pen/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnMetron = if_else(condition = numInjnMeds!=0, true = ivab_check___met/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnErythr = if_else(condition = numInjnMeds!=0, true = ivab_check___ery/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnCeftri = if_else(condition = numInjnMeds!=0, true = ivab_check___cef/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnCiprof = if_else(condition = numInjnMeds!=0, true = ivab_check___cip/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnMerope = if_else(condition = numInjnMeds!=0, true = ivab_check___mer/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnSeptri = if_else(condition = numInjnMeds!=0, true = ivab_check___sep/numInjnMeds*dInjnMeds, false = 0),
         
         dInjnOthers = if_else(condition = numInjnMeds!=0, true = ivab_check___oth/numInjnMeds*dInjnMeds, false = 0),
         
         
         dOralFluclo = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___flu+oralab_typdc___flu)/numOralMeds*dOralMeds, false = 0),
         
         dOralPenici = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___pen+oralab_typdc___pen)/numOralMeds*dOralMeds, false = 0),
         
         dOralAmoxyc = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___amo+oralab_typdc___amo)/numOralMeds*dOralMeds, false = 0),
         
         dOralMetron = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___met+oralab_typdc___met)/numOralMeds*dOralMeds, false = 0),
         
         dOralErythr = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___ery+oralab_typdc___ery)/numOralMeds*dOralMeds, false = 0),
         
         dOralDoxycy = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___dox+oralab_typdc___dox)/numOralMeds*dOralMeds, false = 0),
         
         dOralCephal = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___cep+oralab_typdc___cep)/numOralMeds*dOralMeds, false = 0),
         
         dOralSeptri = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___sep+oralab_typdc___sep)/numOralMeds*dOralMeds, false = 0),
         
         dOralOthers = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___oth+oralab_typdc___oth)/numOralMeds*dOralMeds, false = 0)) %>% 
  
  ################################################ TASIU 04
  mutate(qInjnCloxac = if_else(condition = ageD=="0-4", true = meds[1,2], 
                               if_else(condition = ageD=="5-9", true = meds[17,2], 
                                       if_else(condition = ageD=="10-14", true = meds[33,2], false = meds[49,2]))),
         
         qInjnGentam = if_else(condition = ageD=="0-4", true = meds[2,2], 
                               if_else(condition = ageD=="5-9", true = meds[18,2], 
                                       if_else(condition = ageD=="10-14", true = meds[34,2], false = meds[50,2]))),
         
         qInjnPenici = if_else(condition = ageD=="0-4", true = meds[3,2], 
                               if_else(condition = ageD=="5-9", true = meds[19,2], 
                                       if_else(condition = ageD=="10-14", true = meds[35,2], false = meds[51,2]))),
         
         qInjnMetron = if_else(condition = ageD=="0-4", true = meds[4,2], 
                               if_else(condition = ageD=="5-9", true = meds[20,2], 
                                       if_else(condition = ageD=="10-14", true = meds[36,2], false = meds[52,2]))),
         
         qInjnErythr = if_else(condition = ageD=="0-4", true = meds[5,2], 
                               if_else(condition = ageD=="5-9", true = meds[21,2], 
                                       if_else(condition = ageD=="10-14", true = meds[37,2], false = meds[53,2]))),
         
         qInjnCeftri = if_else(condition = ageD=="0-4", true = meds[6,2], 
                               if_else(condition = ageD=="5-9", true = meds[22,2], 
                                       if_else(condition = ageD=="10-14", true = meds[38,2], false = meds[54,2]))),
         
         qInjnCiprof = if_else(condition = ageD=="0-4", true = meds[7,2], 
                               if_else(condition = ageD=="5-9", true = meds[23,2], 
                                       if_else(condition = ageD=="10-14", true = meds[39,2], false = meds[55,2]))),
         
         qInjnMerope = if_else(condition = ageD=="0-4", true = meds[8,2], 
                               if_else(condition = ageD=="5-9", true = meds[24,2], 
                                       if_else(condition = ageD=="10-14", true = meds[40,2], false = meds[56,2]))),
         
         qInjnOthers = if_else(condition = ageD=="0-4", true = mean(meds$qty[1:8]), 
                              if_else(condition = ageD=="5-9", true = mean(meds$qty[17:24]), 
                                     if_else(condition = ageD=="10-14", true = mean(meds$qty[33:40]), 
                                             false = mean(meds$qty[49:56])))),
         
         qOralFluclo = if_else(condition = ageD=="0-4", true = meds[9,2],  
                               if_else(condition = ageD=="5-9", true = meds[25,2], 
                                       if_else(condition = ageD=="10-14", true = meds[41,2], meds[57,2]))),
         
         qOralPenici = if_else(condition = ageD=="0-4", true = meds[10,2], 
                               if_else(condition = ageD=="5-9", true = meds[26,2], 
                                       if_else(condition = ageD=="10-14", true = meds[42,2], meds[58,2]))),
         
         qOralAmoxyc = if_else(condition = ageD=="0-4", true = meds[11,2], 
                               if_else(condition = ageD=="5-9", true = meds[27,2], 
                                       if_else(condition = ageD=="10-14", true = meds[43,2], meds[59,2]))),
         
         qOralMetron = if_else(condition = ageD=="0-4", true = meds[12,2], 
                               if_else(condition = ageD=="5-9", true = meds[28,2], 
                                       if_else(condition = ageD=="10-14", true = meds[44,2], meds[60,2]))),
         
         qOralErythr = if_else(condition = ageD=="0-4", true = meds[13,2], 
                               if_else(condition = ageD=="5-9", true = meds[29,2], 
                                       if_else(condition = ageD=="10-14", true = meds[45,2], meds[61,2]))),
         
         qOralDoxycy = if_else(condition = ageD=="0-4", true = meds[14,2], 
                               if_else(condition = ageD=="5-9", true = meds[30,2], 
                                       if_else(condition = ageD=="10-14", true = meds[46,2], meds[62,2]))),
         
         qOralCephal = if_else(condition = ageD=="0-4", true = meds[15,2], 
                               if_else(condition = ageD=="5-9", true = meds[31,2], 
                                       if_else(condition = ageD=="10-14", true = meds[47,2], meds[63,2]))),
         
         qOralSeptri = if_else(condition = ageD=="0-4", true = meds[16,2], 
                               if_else(condition = ageD=="5-9", true = meds[32,2], 
                                       if_else(condition = ageD=="10-14", true = meds[48,2], meds[64,2]))),
         
         qOralOthers = if_else(condition = ageD=="0-4", true = mean(meds$qty[9:16]), 
                              if_else(condition = ageD=="5-9", true = mean(meds$qty[25:32]), 
                                     if_else(condition = ageD=="10-14", true = mean(meds$qty[41:48]), 
                                             false = mean(meds$qty[57:64]))))) %>% 

    
  mutate(# Quantity of meds = Quantity of meds * Duration of treatment
         qInjnCloxac = ceiling(qInjnCloxac * dInjnCloxac),
         qInjnGentam = ceiling(qInjnGentam * dInjnGentam),
         qInjnPenici = ceiling(qInjnPenici * dInjnPenici),
         qInjnMetron = ceiling(qInjnMetron * dInjnMetron),
         qInjnErythr = ceiling(qInjnErythr * dInjnErythr),
         qInjnCeftri = ceiling(qInjnCeftri * dInjnCeftri),
         qInjnCiprof = ceiling(qInjnCiprof * dInjnCiprof),
         qInjnMerope = ceiling(qInjnMerope * dInjnMerope),
         qInjnOthers = ceiling(qInjnOthers * dInjnOthers),
         qOralFluclo = ceiling(qOralFluclo * dOralFluclo),
         qOralPenici = ceiling(qOralPenici * dOralPenici),
         qOralAmoxyc = ceiling(qOralAmoxyc * dOralAmoxyc),
         qOralMetron = ceiling(qOralMetron * dOralMetron),
         qOralErythr = ceiling(qOralErythr * dOralErythr),
         qOralDoxycy = ceiling(qOralDoxycy * dOralDoxycy),
         qOralCephal = ceiling(qOralCephal * dOralCephal),
         qOralSeptri = ceiling(qOralSeptri * dOralSeptri),
         qOralOthers = ceiling(qOralOthers * dOralOthers),
         qInjnTotals = rowSums(across(c(qInjnCloxac:qInjnOthers))),
         qOralTotals = rowSums(across(c(qOralFluclo:qOralOthers)))) %>%
         
  
         # Remove unwanted variables
  select(-c(dInjnCloxac:dOralOthers, ivab_check___clox:oralabdc_days))




ssti <- ssti %>%
  mutate(# Costs of IV antibiotics
         cInjnCloxac = qInjnCloxac * cCloxacIV["base"],
         cInjnGentam = qInjnGentam * cGentamIV["base"],
         cInjnPenici = qInjnPenici * cPeniciIV["base"],
         cInjnMetron = qInjnMetron * cMetronIV["base"],
         cInjnErythr = qInjnErythr * cErythrIV["base"],
         cInjnCeftri = qInjnCeftri * cCeftriIV["base"],
         cInjnCiprof = qInjnCiprof * cCiprofIV["base"],
         cInjnMerope = qInjnMerope * cMeropeIV["base"],
         cInjnOthers = qInjnOthers * cOthersIV["base"],
         
         # Costs of oral antibiotics: only 0-4 take amoxycillin syrup, no doxycycline for children
         cOralFluclo = if_else(condition = ageD=="15+", 
                               true = qOralFluclo*cFlucloTb["base"], 
                               false = qOralFluclo*cFlucloSn["base"]),
         
         cOralPenici = if_else(condition = ageD=="15+", 
                               true = qOralPenici*cPeniciTb["base"], 
                               false = qOralPenici*cPeniciSn["base"]),
         
         cOralAmoxyc = if_else(condition = ageD=="0-4", 
                               true = qOralAmoxyc*cAmoxycSn["base"], 
                               false = qOralAmoxyc*cAmoxycTb["base"]),
         
         cOralMetron = if_else(condition = ageD=="15+", 
                               true = qOralMetron*cMetronTb["base"], 
                               false = qOralMetron*cMetronSn["base"]),
         
         cOralErythr = if_else(condition = ageD=="15+", 
                               true = qOralErythr*cErythrTb["base"], 
                               false = qOralErythr*cErythrSn["base"]),
         
         cOralDoxycy = qOralDoxycy*cDoxycyTb["base"], 
         
         cOralCephal = if_else(condition = ageD=="15+", 
                               true = qOralCephal*cCephalTb["base"], 
                               false = qOralCephal*cCephalSn["base"]),
         
         cOralSeptri = if_else(condition = ageD=="15+", 
                               true = qOralSeptri*cSeptriTb["base"], 
                               false = qOralSeptri*cSeptriSn["base"]),
         
         cOralOthers = if_else(condition = ageD=="15+", 
                               true = qOralOthers*cOthersTb["base"], 
                               false = qOralOthers*cOthersSn["base"]),
         
         # Total costs of all medications
         cInjnTotals = round(rowSums(across(c(cInjnCloxac:cInjnOthers))), 0),
         
         cOralTotals = round(rowSums(across(c(cOralFluclo:cOralOthers))), 0),
         
         cMedsTotals = round(rowSums(across(c(cInjnCloxac:cOralOthers))), 0))

ssti$dxScabies[ssti$dxScabies == 0] <- 2
ssti$dxScabies[is.na(ssti$dxScabies)] <- 2

# Those with scabies along with other SSTIs, but their SSTIs were not related to scabies, then hospitalization not scabies-related
ssti$dxScabies[ssti$dxScabies == 1 & ssti$dxScabies.unlikely == 1] <- 2



ssti <- ssti %>% 
  mutate(residence = NA,
         residence = factor(residence, levels = c(1,2), labels = c("Urban", "Rural"))) 

# mean(ssti$age_integer[ssti$ageD == "0-4"])
# mean(ssti$age_integer[ssti$ageD == "5-9"])
# mean(ssti$age_integer[ssti$ageD == "10-14"])
# 
# 
# mean(ssti$qInjnTotals - ssti$qInjnCloxac)
# mean(ssti$qInjnTotals)
# 
# min(ssti$qInjnTotals)
# max(ssti$qInjnTotals)
# 
# mean(ssti$qOralTotals)
# median(ssti$qOralTotals)
# 
# min(ssti$qOralTotals)
# max(ssti$qOralTotals)
# 
# 
# 
# mean(ssti$dHospStay)
# mean(ssti$dInjnMeds)
# mean(ssti$dOralMeds + ssti$dOralMeds.dc)


