source("code/0_parameters.R")

#### Clean PHC presentations data ####

# Read dataset and remove variable names from Stata file where needed
phc             <- read_dta("data/PHC.dta")
var_label(phc)  <- NULL 

# Change coding for those without scabies from 0 to 2 (for table 2)
phc$condition___sc[phc$condition___sc==0] <- 2

# pre-MDA only; recode ethnicity into iTaukei and others; create factor columns
phc <- phc %>% 
  filter(timept==1) %>% 
  mutate(ethnicity_monthly_report = case_when(ethnicity_monthly_report==3 ~ 2, 
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
                             true=1, 
                             false=0),
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
         ageD = if_else(dxScabies==1,
                        true=fct_collapse(ageD, "0-4"=c("0-4", NA)),   # NA to 0-4 for scabies
                        false=fct_collapse(ageD, "15+"=c("15+", NA))), # NA to 15+ for SSTI
         # Number of cloxacillin injections by age for PHC
         qInjnCloxac = if_else(ageD=="0-4", 
                               true=medList$qInjnCloxac_00.04,
                               if_else(ageD=="5-9",
                                       true=medList$qInjnCloxac_05.09,
                                       if_else(ageD=="10-14", 
                                               true=medList$qInjnCloxac_10.14,
                                               false=medList$qInjnCloxac_15plus))))


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
         cImpetigoperOral = if_else(ageD=="0-4", 
                                    true=txMedsperOral * cSeptriSn["base"], 
                                    if_else(ageD=="5-9", 
                                            true=txMedsperOral * cSeptriSn["base"], 
                                            if_else(ageD=="10-14", 
                                                    true=txMedsperOral*5*
                                                      medList$qOralSeptr_10.14*
                                                      cSeptriTb["base"], 
                                                    false=txMedsperOral*5*
                                                      medList$qOralSeptr_15plus*
                                                      cSeptriTb["base"]))),
         
         # Cost of flucloxacillin by age for other SSTIs
         # Note: quantities are got from medList file
         cOthersperOral = if_else(ageD=="0-4", 
                                  true=medList$qSuspFlucl_00.04,  
                                  if_else(ageD=="5-9", 
                                          true=medList$qSuspFlucl_05.09, 
                                          if_else(ageD=="10-14", 
                                                  true=medList$qOralFlucl_10.14, 
                                                  false=medList$qOralFlucl_15plus))),
         
         # Cost of oral meds: co-trimoxazole for impetigo and flucloxacillin for others
         cMedsperOral = if_else(dxImpetigo==1, 
                                true=cImpetigoperOral, 
                                false=cOthersperOral),
         
         # Cost of inj meds: penicillin G for impetigo and cloxacillin for others    
         cMedsInjectn = if_else(dxImpetigo==1,  
                                true=txMedsInjectn * cPeniciIM["base"], 
                                false=txMedsInjectn * qInjnCloxac * cCloxacIV["base"]),                
         
         # Cost of all medicines
         cTotalMeds   = rowSums(pick(cMedsTopical:cMedsInjectn), na.rm = T),
         
         # Cost of diagnostics
         cDiagnostics = txDiagnostics * cLabtest["base"],
         
         # Total cost of PHC presentation
         cMeanTotal    = rowSums(pick(cClinicVisit, cWardAdmit, cICUAdmit, 
                                      cTotalMeds, cDiagnostics), na.rm = T))




#### Sensitivity analysis ####

# Low and High unit cost of Clinic visit
phc <- phc %>% 
  mutate(cClinicVisit_low  = txClinicVisit * cVisit["low"],
         cClinicVisit_high = txClinicVisit * cVisit["high"],
         cMeanTotal_cClinicVisit_low  = rowSums(pick(cClinicVisit_low,  cWardAdmit, 
                                                     cTotalMeds, cDiagnostics), 
                                                na.rm = T),
         cMeanTotal_cClinicVisit_high = rowSums(pick(cClinicVisit_high, cWardAdmit, 
                                                     cTotalMeds, cDiagnostics), 
                                                na.rm = T))



# Save datasets for analysis
save(phc,  file = "data/phc.Rdata")
