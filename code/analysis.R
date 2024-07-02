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
               "low"  = (4.58   * xrate2010 * defl2020.2010 / xrate2020),  # WHO-CHOICE (with beds) 
               "high" = (43.57  * defl2020.2010 / xrate2020))              # LTK hospital
cWard     <- c("base" = (84.35  * defl2020.2010 / xrate2020),              # CWM hospital (overall)
               "low"  = (33.83  * xrate2010 * defl2020.2010 / xrate2020),  # WHO-CHOICE (tertiary)
               "high" = (91.77  * defl2020.2010 / xrate2020))              # LTK hospital (overall)
cICU      <- c("base" = ((178.93+577.80)/2 * defl2020.2010 / xrate2020),   # LTK hospital (ICU)
               "low"  = (178.93 * defl2020.2010 / xrate2020),              # CWM hospital (overall)
               "high" = (577.80 * defl2020.2010 / xrate2020))              # CWM hospital (ICU)
cLabtest  <- c("base" = ((16.97+22.14)/2  * defl2020.2010 / xrate2020),    # Lautoka hospital
               "low"  = (16.97  * defl2020.2010 / xrate2020),              # Lautoka hospital
               "high" = (22.14  * defl2020.2010 / xrate2020))              # Lautoka hospital

# Unit costs of medicines. Use mean if unknown. Prices from 2013 FJD to 2020 USD
cCloxacIV <- c("base" = 0.480 / xrate2020) # intravenous cloxacillin
cGentamIV <- c("base" = 0.074 / xrate2020) # intravenous gentamicin
cPeniciIV <- c("base" = 1.414 / xrate2020) # intravenous penicillin procaine
cMetronIV <- c("base" = 1.940 / xrate2020) # intravenous metronidazole
cErythrIV <- c("base" = 10.17 / xrate2020) # intravenous erythromycin
cCeftriIV <- c("base" = 0.899 / xrate2020) # intravenous ceftriaxone
cCiprofIV <- c("base" = 6.300 / xrate2020) # intravenous ciprofloxacin
cMeropeIV <- c("base" = 29.00 / xrate2020) # intravenous meropenem
cOthersIV <- c("base" = mean(cCloxacIV, cGentamIV, cPeniciIV, cMetronIV, 
                             cErythrIV, cCeftriIV, cCiprofIV, cMeropeIV))
cPeniciIM <- c("base" = 0.950 / xrate2020) # intramuscular penicillin
cFlucloTb <- c("base" = 0.080 / xrate2020)                 # tablet flucloxacillin
cPeniciTb <- c("base" = 0.028 / xrate2020)                 # tablet penicillin
cAmoxycTb <- c("base" = 0.040 / xrate2020)                 # tablet amoxicillin
cMetronTb <- c("base" = 0.010 / xrate2020)                 # tablet metronidazole
cErythrTb <- c("base" = 0.060 / xrate2020)                 # tablet erythromycin
cDoxycyTb <- c("base" = 0.027 / xrate2020)                 # tablet flucloxacillin
cCephalTb <- c("base" = 0.034)                 # tablet cephalexin
cSeptriTb <- c("base" = 0.016 / xrate2020)                 # tablet septrin
cOthersTb <- c("base" = mean(cFlucloTb, cPeniciTb, cAmoxycTb, cMetronTb,
                             cErythrTb, cDoxycyTb, cCephalTb, cSeptriTb))
cFlucloSn <- c("base" = 0.238 / xrate2020)                 # suspension flucloxacillin
cPeniciSn <- c("base" = 2.990000 / xrate2020)                 # suspension penicillin
cAmoxycSn <- c("base" = 0.850 / xrate2020)                 # suspension amoxicillin
cMetronSn <- c("base" = 0.560)                 # suspension metronidazole
cErythrSn <- c("base" = 2.460 / xrate2020)                 # suspension erythromycin
cCephalSn <- c("base" = 0.560)                 # suspension cephalexin
cSeptriSn <- c("base" = 0.630 / xrate2020)                 # suspension septrin
cOthersSn <- c("base" = mean(cFlucloSn, cPeniciSn, cAmoxycSn, cMetronSn, 
                             cErythrSn, cDoxycySn, cCephalSn, cSeptriSn))
cPermetCr <- c("base" = 1.165)                 # cream permethrin


#### Clean medication data ####

# Read medication data from Excel file
medsCost        <- read_excel("data/Drugs.xlsx", sheet = "Costs", range = cell_cols("A:G"))
medsDose        <- as.data.frame(read_excel("data/Drugs.xlsx", sheet = "Doses"))

# Create list called medList for daily doses. Use mean by age and form if unknown. 
medList                       <- as.list(setNames(medsDose$DoseDaily, medsDose$NameAge))
medList["qInjnOthers_00.04"]  <- mean(unlist(medList[c("qInjnCloxac_00.04", "qInjnGentam_00.04", 
                                                       "qInjnPenici_00.04", "qInjnMetron_00.04", 
                                                       "qInjnErythr_00.04", "qInjnCeftri_00.04", 
                                                       "qInjnCiprof_00.04", "qInjnMerope_00.04")]))
medList["qInjnOthers_05.09"]  <- mean(unlist(medList[c("qInjnCloxac_05.09", "qInjnGentam_05.09", 
                                                       "qInjnPenici_05.09", "qInjnMetron_05.09", 
                                                       "qInjnErythr_05.09", "qInjnCeftri_05.09", 
                                                       "qInjnCiprof_05.09", "qInjnMerope_05.09")]))
medList["qInjnOthers_10.14"]  <- mean(unlist(medList[c("qInjnCloxac_10.14", "qInjnGentam_10.14", 
                                                       "qInjnPenici_10.14", "qInjnMetron_10.14", 
                                                       "qInjnErythr_10.14", "qInjnCeftri_10.14", 
                                                       "qInjnCiprof_10.14", "qInjnMerope_10.14")]))
medList["qInjnOthers_15plus"] <- mean(unlist(medList[c("qInjnCloxac_15plus", "qInjnGentam_15plus", 
                                                       "qInjnPenici_15plus", "qInjnMetron_15plus", 
                                                       "qInjnErythr_15plus", "qInjnCeftri_15plus", 
                                                       "qInjnCiprof_15plus", "qInjnMerope_15plus")]))
medList["qSuspOthers_00.04"]  <- mean(unlist(medList[c("qSuspFlucl_00.04", "qSuspPenic_00.04", 
                                                       "qSuspAmoxy_00.04", "qSuspMetro_00.04", 
                                                       "qSuspEryth_00.04", "qOralDoxyc_00.04", 
                                                       "qSuspCepha_00.04", "qSuspSeptr_00.04")]))
medList["qSuspOthers_05.09"]  <- mean(unlist(medList[c("qSuspFlucl_05.09", "qSuspPenic_05.09", 
                                                       "qOralAmoxy_05.09", "qSuspMetro_05.09", 
                                                       "qSuspEryth_05.09", "qOralDoxyc_05.09", 
                                                       "qSuspCepha_05.09", "qSuspSeptr_05.09")]))
medList["qOralOthers_10.14"]  <- mean(unlist(medList[c("qOralFlucl_15plus", "qOralPenic_15plus", 
                                                       "qOralAmoxy_15plus", "qOralMetro_15plus", 
                                                       "qOralEryth_15plus", "qOralDoxyc_15plus", 
                                                       "qOralCepha_15plus", "qOralSeptr_15plus")]))
medList["qOralOthers_15plus"] <- mean(unlist(medList[c("qOralFlucl_15plus", "qOralPenic_15plus", 
                                                       "qOralAmoxy_15plus", "qOralMetro_15plus", 
                                                       "qOralEryth_15plus", "qOralDoxyc_15plus", 
                                                       "qOralCepha_15plus", "qOralSeptr_15plus")]))


#### Clean PHC presentations data ####

# Read dataset and remove variable names from Stata file where needed
phc             <- read_dta("data/PHC.dta")
var_label(phc)  <- NULL 

# Change coding for those without scabies from 0 to 2 (for table 2)
phc$condition___sc[phc$condition___sc == 0] <- 2

# pre-MDA only; recode ethnicity into iTaukei and others; create factor columns
phc <- phc %>% 
  filter(timept == 1) %>% 
  mutate(ethnicity_monthly_report = case_when(ethnicity_monthly_report == 3 ~ 2, 
                                              TRUE ~ ethnicity_monthly_report),
         division  = factor(division, levels = c(1,2,3,4), 
                            labels = c("Macuata", "Cakaudrove", "Taveuni", "Bua")),
         sex       = factor(sex_monthly_report, levels = c(1,2), 
                            labels = c("Male", "Female")),
         ethnicity = factor(ethnicity_monthly_report, levels = c(1,2), 
                            labels = c("I-Taukei", "Others")),
         residence = factor(urban_rural, levels = c(1,2), 
                            labels = c("Urban", "Rural")),
         timept    = factor(timept, levels = c(1,2), 
                            labels = c("Pre", "Post")),
         dxScabies   = as.numeric(condition___sc),
         dxSsti    = if_else(rowSums(pick(condition___im:condition___ss), na.rm = T)>=1, 
                            true = 1, false = 0),
         age_5yr   = cut(age_integer, 
                         breaks = c(-Inf, 5, 10, 15, 25, 35, 45, 55, 65, Inf),
                         labels = c("0-4", "5-9", "10-14", "15-24", "25-34", 
                                    "35-44", "45-54", "55-64", "65+")),
         age_grp   = fct_collapse(age_5yr, 
                                  "5-14" = c("5-9",   "10-14"),  
                                  "15+"  = c("15-24", "25-34", "35-44", 
                                             "45-54", "55-64", "65+")),
         ageD   = cut(age_integer, 
                         breaks = c(-Inf, 5, 10, 15, Inf),
                         labels = c("0-4", "5-9", "10-14", "15+")),
         txClinicVisit = 1.0,     # one clinic visit
         txDiagnostics = 0,     # no diagnostic test
         txMedsTopical = management___pe,
         dWardAdmit    = 1, # overnight admissions stay
         txICUAdmit    = 0,
         dICUAdmit     = 0,
         dHospStay     = dWardAdmit + dICUAdmit) %>% 
  
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
         dxScabies      = dxScabies,
         dxSsti         = dxSsti,
         dxScabiesInf   = condition___is,
         dxImpetigo     = condition___im,
         dxCellulitis   = condition___ce,
         dxAbscess      = condition___ab,
         dxSevereSkin   = condition___ss,
         txClinicVisit  = txClinicVisit, 
         txDiagnostics  = txDiagnostics,
         txWardAdmit    = management___ad,
         dWardAdmit     = dWardAdmit,
         txICUAdmit     = txICUAdmit,
         dICUAdmit      = dICUAdmit,
         dHospStay      = dHospStay,
         txMedsTopical  = txMedsTopical, # permethrin cream
         txMedsperOral  = management___po, # tablets (septrin)
         txMedsInjectn  = management___im, # pencillin G injection
         txAntibioticIV = management___iv, # other injections
         txProcSurgery  = management___pr, # surgical procedure
         txReferredOut  = management___ro, # referred out to a hospital
         year           = myyear, 
         date           = mydate) %>% 
  
  arrange(record_id) %>% 
  
  mutate(across(c(dxScabiesInf:txReferredOut), as.numeric)) %>% 
  
  mutate(ageD = addNA(ageD), # add NA to missing levels of ageD
         ageD = if_else(dxScabies == 1,
                        true  = fct_collapse(ageD, "0-4" = c("0-4", NA)),  # NA to 0-4 for scabies
                        false = fct_collapse(ageD, "15+" = c("15+", NA))), # NA to 15+ for SSTI
         
         qInjnCloxac = if_else(condition = ageD=="0-4", true = medList$qInjnCloxac_00.04, 
                        if_else(condition = ageD=="5-9", true = medList$qInjnCloxac_05.09, 
                                if_else(condition = ageD=="10-14", true = medList$qInjnCloxac_10.14, 
                                        false = medList$qInjnCloxac_15plus))))


#### Cost of PHC presentations ####
phc <- phc %>% 
  mutate(cClinicVisit = txClinicVisit * cVisit["base"],
         
         # Cost of ward and ICU admissions (no ICU, but included for completeness)
         cWardAdmit   = txWardAdmit * dWardAdmit  * cWard["base"],
         cICUAdmit    = dICUAdmit   * cICU["base"],
         
         # Cost of permethrin cream for the whole household
         cMedsTopical = txMedsTopical * cPermetCr["base"] * hhsize,
         
         # Cost of co-trimoxazole by age for impetigo.
         # Note: Tabs 5 days for 10+, 1 suspension bottle for children.
         cImpetigoperOral = if_else(condition = ageD=="0-4", true = txMedsperOral * cSeptriSn["base"], 
                                    if_else(condition = ageD=="5-9", true = txMedsperOral * cSeptriSn["base"], 
                                            if_else(condition = ageD=="10-14", 
                                                    true=txMedsperOral*5*medList$qOralSeptr_10.14*cSeptriTb["base"], 
                                                    false=txMedsperOral*5*medList$qOralSeptr_15plus*cSeptriTb["base"]))),
         
         # Cost of flucloxacillin by age for other SSTIs
         # Note: quantities are got from medList file
         cOthersperOral = if_else(condition = ageD=="0-4", true = medList$qSuspFlucl_00.04,  
                                  if_else(condition = ageD=="5-9", true = medList$qSuspFlucl_05.09, 
                                          if_else(condition = ageD=="10-14", true = medList$qOralFlucl_10.14, 
                                                  false = medList$qOralFlucl_15plus))),
         
         # Cost of oral medications by age: co-trimoxazole for impetigo and flucloxacillin for others
         cMedsperOral = if_else(condition = dxImpetigo==1, true = cImpetigoperOral, 
                                false = cOthersperOral),
        
         # Cost of injectable medications by age: penicillin G for impetigo and cloxacillin for others                            
         cMedsInjectn = if_else(condition = dxImpetigo==1,  true = txMedsInjectn * cPeniciIM["base"], 
                                false = txMedsInjectn * qInjnCloxac * cCloxacIV["base"]),                
         
         # Cost of all medicines
         cTotalMeds   = rowSums(pick(cMedsTopical:cMedsInjectn), na.rm = T),
         
         # Cost of diagnostics
         cDiagnostics = txDiagnostics * cLabtest["base"],
         
         # Total cost of PHC presentation
         cMeanTotal    = rowSums(pick(cClinicVisit, cWardAdmit, cICUAdmit, 
                                     cTotalMeds, cDiagnostics), na.rm = T))




#### Clean hospitalisations data ####

# Read dataset and remove variable names from Stata file where needed
ssti            <- read_dta("data/SSTI.dta")
var_label(ssti) <- NULL

# Change coding for those without scabies from 0 to 2 (for table 2)
ssti$pos_scabies[ssti$pos_scabies == 0]   <- 2 

ssti <- ssti %>% 
  # Pre-MDA only
  filter(timept==1) %>% 
  
  mutate(sex       = factor(sex, levels = c(1,2), labels = c("Male", "Female")),
         ethnicity = factor(ethnicity_d2, levels = c(1,2), labels = c("I-Taukei", "Others")),
         timept    = factor(timept, levels = c(1,2), labels = c("Pre", "Post")),
         residence = 0,
         residence = factor(residence, levels = c(1,2), labels = c("Urban", "Rural")),
         age_5yr   = cut(enrolage_yrs, breaks = c(0, 5, 10, 15, 25, 
                                                  35, 45, 55, 65, Inf),
                         labels = c("0-4","5-9","10-14","15-24","25-34", 
                                    "35-44","45-54", "55-64","65+")),
         age_grp   = fct_collapse(age_5yr, 
                                  "5-14" = c("5-9",   "10-14"),  
                                  "15+"  = c("15-24","25-34","35-44","45-54","55-64","65+")),
         ageD      = fct_collapse(age_5yr, 
                              "15+" = c("15-24","25-34","35-44","45-54","55-64","65+")),
         txWardAdmit = 1, # everyone admitted to hospital
         txICUAdmit = if_else(!is.na(icudays), true = 1, false = 0),
         txDiagnostics    = if_else(rowSums(pick(c(bc, tissueculture_yn, skin_swabyn)), na.rm = T)>=1,
                                    true = 1, false = 0),
         txMedsperOral    = if_else(rowSums(pick(c(oralab_checkad___flu, oralab_checkad___pen, 
                                                  oralab_checkad___amo, oralab_checkad___met, 
                                                  oralab_checkad___ery, oralab_checkad___dox, 
                                                  oralab_checkad___cep, oralab_checkad___sep, 
                                                  oralab_checkad___oth, oralab_typdc___flu, 
                                                  oralab_typdc___pen, oralab_typdc___amo, 
                                                  oralab_typdc___met, oralab_typdc___ery, 
                                                  oralab_typdc___dox, oralab_typdc___cep, 
                                                  oralab_typdc___sep, oralab_typdc___oth)), na.rm = T)>=1,
                                    true = 1, false = 0),
         txMedsInjectn    = if_else(rowSums(pick(c(ivab_check___clox, ivab_check___gent, 
                                                  ivab_check___pen, ivab_check___met, 
                                                  ivab_check___ery, ivab_check___cef, 
                                                  ivab_check___cip, ivab_check___mer, 
                                                  ivab_check___sep, ivab_check___oth)), na.rm = T)>=1,
                                    true = 1, false = 0)) %>% 
  select(record_id           = record_id,
         sex                 = sex,
         age_integer         = enrolage_yrs,
         age_grp             = age_grp,
         ageD                = ageD,
         ethnicity           = ethnicity,
         residence           = residence,
         timept              = timept,
         admOutcome          = adm_outcome,
         txWardAdmit         = txWardAdmit,
         txICUAdmit          = txICUAdmit,
         txDiagnostics       = txDiagnostics,
         txMedsperOral       = txMedsperOral,
         txMedsInjectn       = txMedsInjectn,
         dHospStay           = los,
         dICUAdmit           = icudays,
         dVentilation        = vent_days,
         dxScabies           = pos_scabies,
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
         dInjnMeds        = ivabduration) 

# Change missing values to 0 in dummy variables
ssti$dICUAdmit[is.na(ssti$dICUAdmit)]           <- 0
ssti$dVentilation[is.na(ssti$dVentilation)]     <- 0
ssti$oralabduration[is.na(ssti$oralabduration)] <- 0
ssti$dInjnMeds[is.na(ssti$dInjnMeds)]           <- 0

# Change empty cells to 0, text cells to number
ssti$oralabdc_days[ssti$oralabdc_days == ""] <- "0"
ssti$oralabdc_days <- gsub("[^0-9]", "", ssti$oralabdc_days)

# Factor admission outcomes and calculate total number of medications
ssti <- ssti %>% 
  mutate(admOutcome = factor(x = admOutcome, levels = c(1,2,3,4), 
                             labels = c("Discharged", "Transferred", 
                                        "Disability", "Death"))) %>% 
  mutate(across(c(dxImpetigo:dInjnMeds), as.numeric)) %>% 
  mutate(dOralMeds   = rowSums(across(c(oralabduration, oralabdc_days)), 
                               na.rm=TRUE),
         numInjnMeds = rowSums(across(c(ivab_check___clox, ivab_check___gent, 
                                        ivab_check___pen, ivab_check___met, 
                                        ivab_check___ery, ivab_check___cef, 
                                        ivab_check___cip, ivab_check___mer, 
                                        ivab_check___sep, ivab_check___oth)),
                               na.rm=TRUE),
         numOralMeds = rowSums(across(c(oralab_checkad___flu, oralab_checkad___pen, 
                                        oralab_checkad___amo, oralab_checkad___met, 
                                        oralab_checkad___ery, oralab_checkad___dox, 
                                        oralab_checkad___cep, oralab_checkad___sep, 
                                        oralab_checkad___oth, oralab_typdc___flu, 
                                        oralab_typdc___pen, oralab_typdc___amo, 
                                        oralab_typdc___met, oralab_typdc___ery, 
                                        oralab_typdc___dox, oralab_typdc___cep, 
                                        oralab_typdc___sep, oralab_typdc___oth)),
                               na.rm=TRUE))   


ssti <- ssti %>%
  # DaysOfParticularMed = I / TotalNumberOfMeds * TotalDaysOfMeds
  # I is 1 if patient was given particular med, 0 otherwise
  mutate(dInjnCloxac = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___clox/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnGentam = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___gent/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnPenici = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___pen/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnMetron = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___met/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnErythr = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___ery/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnCeftri = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___cef/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnCiprof = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___cip/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnMerope = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___mer/numInjnMeds*dInjnMeds, 
                               false = 0),
         dInjnOthers = if_else(condition = numInjnMeds!=0, 
                               true = ivab_check___oth/numInjnMeds*dInjnMeds, 
                               false = 0),
         dOralFluclo = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___flu+oralab_typdc___flu)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralPenici = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___pen+oralab_typdc___pen)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralAmoxyc = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___amo+oralab_typdc___amo)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralMetron = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___met+oralab_typdc___met)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralErythr = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___ery+oralab_typdc___ery)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralDoxycy = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___dox+oralab_typdc___dox)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralCephal = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___cep+oralab_typdc___cep)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralSeptri = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___sep+oralab_typdc___sep)/numOralMeds*dOralMeds, 
                               false = 0),
         dOralOthers = if_else(condition = numOralMeds!=0, 
                               true = (oralab_checkad___oth+oralab_typdc___oth)/numOralMeds*dOralMeds, 
                               false = 0)) %>% 
  # Different quantities of medications required for different age groups
  mutate(qInjnCloxac = if_else(condition = ageD=="0-4", true = medList$qInjnCloxac_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnCloxac_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnCloxac_10.14, 
                                               false = medList$qInjnCloxac_15plus))),
         qInjnGentam = if_else(condition = ageD=="0-4", true = medList$qInjnGentam_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnGentam_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnGentam_10.14, 
                                               false = medList$qInjnGentam_15plus))),
         qInjnPenici = if_else(condition = ageD=="0-4", true = medList$qInjnPenici_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnPenici_05.09, 
                                       if_else(condition = ageD=="10-14", true =medList$qInjnPenici_10.14, 
                                               false = medList$qInjnPenici_15plus))),
         qInjnMetron = if_else(condition = ageD=="0-4", true = medList$qInjnMetron_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnMetron_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnMetron_10.14, 
                                               false = medList$qInjnMetron_15plus))),
         qInjnErythr = if_else(condition = ageD=="0-4", true = medList$qInjnErythr_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnErythr_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnErythr_10.14, 
                                               false = medList$qInjnErythr_15plus))),
         qInjnCeftri = if_else(condition = ageD=="0-4", true = medList$qInjnCeftri_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnCeftri_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnCeftri_10.14, 
                                               false = medList$qInjnCeftri_15plus))),
         qInjnCiprof = if_else(condition = ageD=="0-4", true = medList$qInjnCiprof_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnCiprof_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnCiprof_10.14, 
                                               false = medList$qInjnCiprof_15plus))),
         qInjnMerope = if_else(condition = ageD=="0-4", true = medList$qInjnMerope_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnMerope_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qInjnMerope_10.14, 
                                               false = medList$qInjnMerope_15plus))),
         qInjnOthers = if_else(condition = ageD=="0-4", true = medList$qInjnOthers_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qInjnOthers_05.09,
                                       if_else(condition = ageD=="10-14", true = medList$qInjnOthers_10.14, 
                                               false = medList$qInjnOthers_15plus))),
         qOralFluclo = if_else(condition = ageD=="0-4", true = medList$qSuspFlucl_00.04,  
                               if_else(condition = ageD=="5-9", true = medList$qSuspFlucl_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralFlucl_10.14, 
                                               false = medList$qOralFlucl_15plus))),
         qOralPenici = if_else(condition = ageD=="0-4", true = medList$qSuspPenic_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qSuspPenic_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralPenic_10.14, 
                                               false = medList$qOralPenic_15plus))),
         qOralAmoxyc = if_else(condition = ageD=="0-4", true = medList$qSuspAmoxy_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qOralAmoxy_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralAmoxy_10.14, 
                                               false = medList$qOralAmoxy_15plus))),
         qOralMetron = if_else(condition = ageD=="0-4", true = medList$qSuspMetro_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qSuspMetro_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralMetro_10.14, 
                                               false = medList$qOralMetro_15plus))),
         qOralErythr = if_else(condition = ageD=="0-4", true = medList$qSuspEryth_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qSuspEryth_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralEryth_10.14, 
                                               false = medList$qOralEryth_15plus))),
         qOralDoxycy = if_else(condition = ageD=="0-4", true = medList$qOralDoxyc_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qOralDoxyc_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralDoxyc_10.14, 
                                               false = medList$qOralDoxyc_15plus))),
         qOralCephal = if_else(condition = ageD=="0-4", true = medList$qSuspCepha_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qSuspCepha_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralCepha_10.14, 
                                               false = medList$qOralCepha_15plus))),
         qOralSeptri = if_else(condition = ageD=="0-4", true = medList$qSuspSeptr_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qSuspSeptr_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralSeptr_10.14, 
                                               false = medList$qOralSeptr_15plus))),
         qOralOthers = if_else(condition = ageD=="0-4", true = medList$qSuspOthers_00.04, 
                               if_else(condition = ageD=="5-9", true = medList$qSuspOthers_05.09, 
                                       if_else(condition = ageD=="10-14", true = medList$qOralOthers_10.14, 
                                               false = medList$qOralOthers_15plus)))) %>% 
  
  # Total quantity of meds = Quantity of meds * Duration of treatment
  mutate(qInjnCloxac = ceiling(qInjnCloxac * dInjnCloxac),
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


#### Cost of hospitalisations #### 

ssti <- ssti %>%
  # Costs of healthcare services
  mutate(txClinicVisit  = 0,
         txMedsTopical  = 0,
         dWardAdmit     = dHospStay  - dICUAdmit,     # days for ward
         cWardAdmit     = dWardAdmit * cWard["base"], # cost for ward
         cICUAdmit      = dICUAdmit  * cICU["base"],  # cost for ICU
         cHospStay      = cWardAdmit + cICUAdmit,     # cost for both
         cClinicVisit   = txClinicVisit * cVisit["base"], # no clinic visit
         cMedsTopical   = txMedsTopical * cPermetCr,  # no topical meds
  # Costs of laboratory tests
         nDiagnostics  = rowSums(pick(labCultureBlood, labCultureTissue, labSkinSwab), na.rm = T),
         cCultureBlood  = labCultureBlood   * cLabtest["base"],
         cCultureTissue = labCultureTissue  * cLabtest["base"],
         cSkinSwab      = labSkinSwab       * cLabtest["base"],
         cDiagnostics   = rowSums(pick(cCultureBlood, cCultureTissue, cSkinSwab), na.rm = T),
  # Costs of IV antibiotics
         cInjnCloxac = qInjnCloxac * cCloxacIV["base"],
         cInjnGentam = qInjnGentam * cGentamIV["base"],
         cInjnPenici = qInjnPenici * cPeniciIV["base"],
         cInjnMetron = qInjnMetron * cMetronIV["base"],
         cInjnErythr = qInjnErythr * cErythrIV["base"],
         cInjnCeftri = qInjnCeftri * cCeftriIV["base"],
         cInjnCiprof = qInjnCiprof * cCiprofIV["base"],
         cInjnMerope = qInjnMerope * cMeropeIV["base"],
         cInjnOthers = qInjnOthers * cOthersIV["base"],
  # Costs of oral antibiotics (only 0-4 take syrup amoxil)
         cOralFluclo = if_else(condition = ageD=="15+", true = qOralFluclo*cFlucloTb["base"], 
                          false = qOralFluclo*cFlucloSn["base"]),
         cOralPenici = if_else(condition = ageD=="15+", true = qOralPenici*cPeniciTb["base"], 
                          false = qOralPenici*cPeniciSn["base"]),
         cOralAmoxyc = if_else(condition = ageD=="0-4", true = qOralAmoxyc*cAmoxycSn["base"], 
                          false = qOralAmoxyc*cAmoxycTb["base"]),
         cOralMetron = if_else(condition = ageD=="15+", true = qOralMetron*cMetronTb["base"], 
                          false = qOralMetron*cMetronSn["base"]),
         cOralErythr = if_else(condition = ageD=="15+", true = qOralErythr*cErythrTb["base"], 
                          false = qOralErythr*cErythrSn["base"]),
         cOralDoxycy = qOralDoxycy*cDoxycyTb["base"], # not for children
         cOralCephal = if_else(condition = ageD=="15+", true = qOralCephal*cCephalTb["base"], 
                          false = qOralCephal*cCephalSn["base"]),
         cOralSeptri = if_else(condition = ageD=="15+", true = qOralSeptri*cSeptriTb["base"], 
                          false = qOralSeptri*cSeptriSn["base"]),
         cOralOthers = if_else(condition = ageD=="15+", true = qOralOthers*cOthersTb["base"], 
                          false = qOralOthers*cOthersSn["base"]),
# Total costs of all medications
    cMedsInjectn = rowSums(across(c(cInjnCloxac:cInjnOthers))),
    cMedsperOral = rowSums(across(c(cOralFluclo:cOralOthers))),
    cTotalMeds  = cMedsTopical + cMedsInjectn + cMedsperOral,
    cMeanTotal  = rowSums(pick(cHospStay, cDiagnostics, cTotalMeds), na.rm = T)) 




#### Sensitivity analysis ####

# Low and High unit cost of Clinic visit
phc <- phc %>% 
  mutate(cClinicVisit_low  = txClinicVisit * cVisit["low"],
         cClinicVisit_high = txClinicVisit * cVisit["high"],
         cMeanTotal_cClinicVisit_low  = rowSums(pick(cClinicVisit_low,  cWardAdmit, cTotalMeds, cDiagnostics), na.rm = T),
         cMeanTotal_cClinicVisit_high = rowSums(pick(cClinicVisit_high, cWardAdmit, cTotalMeds, cDiagnostics), na.rm = T))

# Low and High unit cost of ward bed day
ssti <- ssti %>%
  mutate(cWardAdmit_low    = dWardAdmit    * cWard["low"],
         cWardAdmit_high   = dWardAdmit    * cWard["high"],
         cICUAdmit_low     = dICUAdmit     * cICU["low"],
         cICUAdmit_high    = dICUAdmit     * cICU["high"],
         cDiagnostics_low  = nDiagnostics * cLabtest["low"],
         cDiagnostics_high = nDiagnostics * cLabtest["high"],
         cMeanTotal_cWardAdmit_low    = rowSums(pick(cWardAdmit_low, cICUAdmit, cDiagnostics, cTotalMeds), na.rm = T),
         cMeanTotal_cWardAdmit_high   = rowSums(pick(cWardAdmit_high, cICUAdmit, cDiagnostics, cTotalMeds), na.rm = T),
         cMeanTotal_cICUAdmit_low     = rowSums(pick(cWardAdmit, cICUAdmit_low, cDiagnostics, cTotalMeds), na.rm = T),
         cMeanTotal_cICUAdmit_high    = rowSums(pick(cWardAdmit, cICUAdmit_high, cDiagnostics, cTotalMeds), na.rm = T),
         cMeanTotal_cDiagnostics_low  = rowSums(pick(cWardAdmit, cICUAdmit, cDiagnostics_low, cTotalMeds), na.rm = T),
         cMeanTotal_cDiagnostics_high = rowSums(pick(cWardAdmit, cICUAdmit, cDiagnostics_high, cTotalMeds), na.rm = T))




save(ssti, file = "data/ssti.Rdata")
save(phc,  file = "data/phc.Rdata")
save(medsCost,  file = "data/medsCost.Rdata")
save(medsDose,  file = "data/medsDose.Rdata")

