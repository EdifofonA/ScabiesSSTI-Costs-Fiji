* Read data

frame create Exams
frame create Invasive
frame create PSGN
frame create SSTI
frame create PHC

frame Exams: use "Data\Exams.dta", clear
frame Invasive: use "Data\Invasive.dta", clear
frame PSGN: use "Data\PSGN.dta", clear
frame SSTI: use "Data\SSTI.dta", clear
frame PHC: use "Data\PHC.dta", clear


frame change PHC
drop if timept==0
rename sex_monthly_report sex
rename ethnicity_monthly_report ethnicity
rename urban_rural residence

keep record_id division sex ethnicity age_grp age_integer urban_rural timept myyear mydate condition___sc condition___is condition___im condition___ce condition___ab condition___ss management___pe management___po management___im management___iv management___pr management___ad management___ro

lab var age_grp     "Age group"
lab var sex         "Sex"
lab var ethnicity   "Ethnicity"
lab var division    "Division"
lab var urban_rural "Residence"
lab var timept      "Time point"


**** Table 1 *****************************
// net install table1_mc.pkg
table1_mc if condition___sc==1, by(timept) vars(age_grp cat %5.0f \ sex cat %5.0f \ ethnicity cat %5.0f \ division cat %5.0f \ residence cat %5.0f) onecol



table1 <- table1(~ age_grp + sex + ethnicity + division | timept, data=subset(phc,(condition___sc==1)))
as.data.frame(table1)
write.csv(table1, file = "table1.csv", row.names = FALSE)

























