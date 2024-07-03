#### Load packages and data ####
rm(list = ls())

library(tidyverse)
library(gt)
library(gtsummary)
library(flextable)
library(officer) # to export to ms word
library(scales)
library(cowplot)

load(file = "data/phc.Rdata")
load(file = "data/hosp.Rdata")

pop <- list("total" = 884887,
            "north" = 131914,
            "other" = 752973,
            "male" = 448595,
            "female" = 436292,
            "iTaukei" = round(884887 * 0.5682, 0),
            "otherEthnic" = round(884887 * 0.4318, 0),
            "p00_04" = 14550,
            "p05_14" = 167891,
            "p15plus" = 702446)



#### Helper functions ####
# Function for pretty table using Flextable package
ftTheme <- function(table, tblwidth){
  table <- table %>% 
    width(width = tblwidth) %>%
    align(i = NULL, j = NULL, align = "center",  part = "all") %>% 
    align(i = 1,    j = NULL, align = "center", part = "header") %>% 
    align(i = NULL, j = 1,    align = "left",   part = "all") %>% 
    fontsize(size = 11, part = "all") %>% 
    border_inner(border = fp_border(color="black", width = 0.5), part = "body")  %>% 
    border_outer(border = fp_border(color="black", width = 0.5), part = "body")  %>% 
    border_inner(border = fp_border(color="black", width = 0.5), part = "header") %>% 
    border_outer(border = fp_border(color="black", width = 0.5), part = "header")
  table
}

# Function to extrapolate cases from Northern division to Other divisions
yearCases <- function(cases, fTime, result, year=1, OR=1/1.3, popNorth=131914, popOther=752973){
  rateNorth  <- cases / (popNorth * fTime)
  riskNorth  <- 1 - exp(-rateNorth*year)
  RR = OR /(1-riskNorth+(riskNorth*OR))
  riskOther  <- riskNorth * RR
  casesNorth <- riskNorth * popNorth
  casesOther <- riskOther * popOther
  casesTotal <- casesNorth + casesOther
  ifelse(result=="North", return(casesNorth), 
         ifelse(result=="Total", return(casesTotal),
                NULL))
}

Include = c("sex", "age_integer", "age_grp", "ethnicity", 
            "residence", "dHospStay")
Label   = list(sex         = "Sex, no. (%)", 
               age_integer = "Age, median (IQR)",
               age_grp     = "Age category, no. (%)",
               ethnicity   = "Ethnicity, no. (%)",
               residence   = "Residence, no. (%)",
#               txAdmission = "Admitted, no. (%)",
               dHospStay   = "Bed days, mean (SD)")

theme_gtsummary_language("en", big.mark = "")


#### Table 2 and S2: Characteristics of patients ####

nBigshiftPhc  = c("scabies"=length(which(phc$dxScabies==1)),
                  "ssti"   =length(which(phc$dxSsti==1)))
nBigshiftHosp = c("related"   =length(which(hosp$dxScabies==1)),
                  "nonrelated"=length(which(hosp$dxScabies==2)))

# Run yearCases function to get estimated number of cases for Northern Division
nNorthPhc  <- round(yearCases(nBigshiftPhc,  fTime = 50/(365.25/7), result = "North"), 0)
nNorthHosp <- round(yearCases(nBigshiftHosp, fTime = 48/(365.25/7), result = "North"), 0)

# Create multipliers for each group
xScabies    <- nNorthPhc["scabies"]/ nBigshiftPhc["scabies"]
xSsti       <- nNorthPhc["ssti"] / nBigshiftPhc["ssti"]
xRelated    <- nNorthHosp["related"] / nBigshiftHosp["related"]
xNonrelated <- nNorthHosp["nonrelated"] / nBigshiftHosp["nonrelated"]


# Scabies PHC patients
tabCount <- phc %>% 
  filter(dxScabies==1)%>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              type = list(dHospStay ~ "continuous"),
              statistic = list(all_categorical() ~ "{n}"),
              digits    = all_continuous() ~ c(1,1),) %>% 
  modify_header(stat_0 ~ "Scabies presentations, N={n*xScabies}") %>% 
  modify_footnote(update = stat_0 ~ NA) %>%
  modify_table_body(~ .x %>%
                      mutate(stat_0 = round(as.numeric(stat_0)*xScabies, 0)))

tabStats <- phc %>% 
  filter(dxScabies==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              type = list(dHospStay ~ "continuous"),
              statistic = list(all_categorical() ~ "({p}%)",
                               dHospStay ~ "{mean} ({sd})"),
              digits    = list(all_continuous() ~ c(0,0),
                               dHospStay ~ c(1,1))) %>% 
  modify_header(stat_0 ~ "Scabies presentations, N={n*xScabies}") %>% 
  modify_footnote(update = stat_0 ~ NA)

table2A  <- tbl_merge(list(tabCount, tabStats), tab_spanner=FALSE) %>% 
  modify_column_merge(pattern = "{stat_0_1} {stat_0_2}", rows = !is.na(stat_0_2))


# Ssti PHC patients
tabCount <- phc %>% 
  filter(dxSsti==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              type = list(dHospStay ~ "continuous"),
              statistic = list(all_categorical() ~ "{n}"),
              digits    = all_continuous() ~ c(1,1),) %>% 
  modify_header(stat_0 ~ "Scabies-related SSTI presentations, N={n*xSsti}") %>% 
  modify_footnote(update = stat_0 ~ NA) %>%
  modify_table_body(~ .x %>%
                      mutate(stat_0 = round(as.numeric(stat_0)*xSsti, 0)))

tabStats <- phc %>% 
  filter(dxSsti==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              type = list(dHospStay ~ "continuous"),
              statistic = list(all_categorical() ~ "({p}%)",
                               dHospStay ~ "{mean} ({sd})"),
              digits    = list(all_continuous() ~ c(0,0),
                               dHospStay ~ c(1,1))) %>% 
  modify_header(stat_0 ~ "Scabies-related SSTI presentations, N={n*xSsti}") %>% 
  modify_footnote(update = stat_0 ~ NA)

table2B  <- tbl_merge(list(tabCount, tabStats), tab_spanner=FALSE) %>% 
  modify_column_merge(pattern = "{stat_0_1} {stat_0_2}", rows = !is.na(stat_0_2))


# Scabies-related hospital patients
tabCount <- hosp %>% 
  filter(dxScabies==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = list(all_categorical() ~ "{n}"),
              digits    = all_continuous() ~ c(1,1),) %>% 
  modify_header(stat_0 ~ "Scabies-related SSTI admissions, N={n*xRelated}") %>% 
  modify_footnote(update = stat_0 ~ NA) %>%
  modify_table_body(~ .x %>%
                      mutate(stat_0 = round(as.numeric(stat_0)*xRelated, 0)))

tabStats <- hosp %>% 
  filter(dxScabies==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = list(all_categorical() ~ "({p}%)",
                               dHospStay ~ "{mean} ({sd})"),
              digits    = list(all_continuous() ~ c(0,0),
                               dHospStay ~ c(1,1))) %>% 
  modify_header(stat_0 ~ "Scabies-related SSTI admissions, N={n*xRelated}") %>% 
  modify_footnote(update = stat_0 ~ NA)

table2C  <- tbl_merge(list(tabCount, tabStats), tab_spanner=FALSE) %>% 
  modify_column_merge(pattern = "{stat_0_1} {stat_0_2}", rows = !is.na(stat_0_2))


# Non-scabies-related hospital patients
tabCount <- hosp %>% 
  filter(dxScabies==2) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = list(all_categorical() ~ "{n}"),
              digits    = all_continuous() ~ c(1,1),) %>% 
  modify_header(stat_0 ~ "Non scabies-related SSTI admissions, N={n*xNonrelated}") %>% 
  modify_footnote(update = stat_0 ~ NA) %>%
  modify_table_body(~ .x %>%
                      mutate(stat_0 = round(as.numeric(stat_0)*xNonrelated, 0)))

tabStats <- hosp %>% 
  filter(dxScabies==2) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = list(all_categorical() ~ "({p}%)",
                               dHospStay ~ "{mean} ({sd})"),
              digits    = list(all_continuous() ~ c(0,0),
                               dHospStay ~ c(1,1))) %>% 
  modify_header(stat_0 ~ "Non scabies-related SSTI admissions, N={n*xNonrelated}") %>% 
  modify_footnote(update = stat_0 ~ NA)


# Table 2 Scabies and scabies-related presentations and admissions
table2 <-  tbl_merge(tbls = list(table2A, table2B, table2C), tab_spanner = FALSE)%>% 
  modify_table_body(fun = ~.x %>% mutate(across(everything(), ~gsub("0 \\(0\\%\\)", "NA", .)))) %>% 
  modify_table_body(fun = ~.x %>% mutate(across(everything(), ~gsub("0 \\(NA\\%\\)", "NA", .)))) %>% 
  as_flex_table() %>% 
  add_footer_lines(value = as_paragraph(c("SD, stabndard deviation; NA, not available"," ")))
table2 <- ftTheme(table = table2, tblwidth = c(1.7, 1.4, 1.4, 1.4))
table2


# Table S2 Non-scabies-related admissions
sTable2  <- tbl_merge(list(tabCount, tabStats), tab_spanner=FALSE) %>% 
  modify_column_merge(pattern = "{stat_0_1} {stat_0_2}", rows = !is.na(stat_0_2))  %>% 
  modify_table_body(fun = ~.x %>% mutate(across(everything(), ~gsub("0 \\(0\\%\\)", "NA", .)))) %>% 
  modify_table_body(fun = ~.x %>% mutate(across(everything(), ~gsub("0 \\(NA\\%\\)", "NA", .)))) %>% 
  as_flex_table()
sTable2 <- ftTheme(table = sTable2, tblwidth = c(2.7, 3.2))
sTable2

# Remove unwanted objects from the environment
rm(tabCount, tabStats, table2A, table2B, table2C, Label, Include, 
   xScabies, xSsti, xRelated, xNonrelated, nNorthPhc, nNorthHosp)



#### Table 3 and S4: Mean (SD) costs per case ####

Include = c("cClinicVisit", "cWardAdmit", "cICUAdmit", "cMedsTopical", 
            "cMedsperOral", "cMedsInjectn", "cDiagnostics", "cMeanTotal")
Label   = list(cClinicVisit = "Clinic visits",
               cWardAdmit   = "Ward bed days",
               cICUAdmit    = "ICU bed days",
               cMedsTopical = "Topical medications",
               cMedsperOral = "Oral medications",
               cMedsInjectn = "Injection medications",
               cDiagnostics = "Diagnostic tests",
               cMeanTotal   = "Mean total costs")

#theme_gtsummary_language("en", big.mark = "")

# Scabies PHC patients
table3A <- phc %>% 
  filter(dxScabies==1)%>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = everything() ~ "{mean} ({sd})",
              digits    = everything() ~ c(1,1),
              type      = list(everything() ~ "continuous")) %>%
  modify_header(stat_0 ~ "Scabies presentations") %>% 
  modify_footnote(update = stat_0 ~ NA)

# Scabies-related SSTI PHC patients
table3B <- phc %>% 
  filter(dxSsti==1)%>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = everything() ~ "{mean} ({sd})",
              digits    = everything() ~ c(1,1),
              type      = list(everything() ~ "continuous")) %>%
  modify_header(stat_0 ~ "Scabies-related SSTI presentations") %>% 
  modify_footnote(update = stat_0 ~ NA)

# Scabies-related hospital patients
table3C <- hosp %>% 
  filter(dxScabies==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = everything() ~ "{mean} ({sd})",
              digits    = everything() ~ c(1,1),
              type      = list(everything() ~ "continuous")) %>%
  modify_header(stat_0 ~ "Scabies-related SSTI admissions") %>% 
  modify_footnote(update = stat_0 ~ NA)

# Scabies and scabies-related SSTIs
table3 <- tbl_merge(tbls = list(table3A, table3B, table3C), tab_spanner = FALSE) %>% 
  as_flex_table()
table3 <- ftTheme(table = table3, tblwidth = c(1.7, 1.4, 1.4, 1.4))
table3



# Non-scabies-related hospital patients
sTable4 <- hosp %>% 
  filter(dxScabies==2)%>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              statistic = everything() ~ "{mean} ({sd})",
              digits    = everything() ~ c(1,1),
              type      = list(everything() ~ "continuous")) %>%
  modify_header(stat_0 ~ "Non-scabies-related SSTI admissions") %>% 
  modify_footnote(update = stat_0 ~ NA) %>% 
  as_flex_table()
sTable4 <- ftTheme(table = sTable4, tblwidth = c(2.9, 3))
sTable4



#### Table S3: Proportion of cases requiring resources ####

Include = c("txClinicVisit", "txWardAdmit", "txICUAdmit", "txMedsTopical", 
            "txMedsperOral", "txMedsInjectn", "txDiagnostics")
Label   = list(txClinicVisit = "Clinic visits",
               txWardAdmit   = "Ward bed days",
               txICUAdmit    = "ICU bed days",
               txMedsTopical = "Topical medications",
               txMedsperOral = "Oral medications",
               txMedsInjectn = "Injection medications",
               txDiagnostics = "Diagnostic tests")

#theme_gtsummary_language("en", big.mark = "")

# Scabies PHC patients
sTable3A <- phc %>% 
  filter(dxScabies==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              digits    = everything() ~ c(1,1),
              statistic = everything() ~ "{p}%") %>%
  modify_header(stat_0 ~ "Scabies presentations") %>% 
  modify_footnote(update = stat_0 ~ NA)

# Scabies-related SSTI PHC patients
sTable3B <- phc %>% 
  filter(dxSsti==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              digits    = everything() ~ c(1,1),
              statistic = everything() ~ "{p}%") %>%
  modify_header(stat_0 ~ "Scabies-related SSTI presentations") %>% 
  modify_footnote(update = stat_0 ~ NA)

# Scabies-related hospital patients
sTable3C <- hosp %>% 
  filter(dxScabies==1) %>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              digits    = everything() ~ c(1,1),
              statistic = everything() ~ "{p}%") %>%
  modify_header(stat_0 ~ "Scabies-related SSTI admissions") %>% 
  modify_footnote(update = stat_0 ~ NA)


# Non-scabies-related hospital patients
sTable3D <- hosp %>% 
  filter(dxScabies==2)%>% 
  tbl_summary(missing="no", include=all_of(Include), label=Label,
              digits    = everything() ~ c(1,1),
              statistic = everything() ~ "{p}%") %>%
  modify_header(stat_0 ~ "Non-scabies-related SSTI admissions") %>% 
  modify_footnote(update = stat_0 ~ NA)


# Scabies and scabies-related SSTIs
sTable3 <- tbl_merge(tbls = list(sTable3A, sTable3B, sTable3C), tab_spanner = FALSE) %>% 
  as_flex_table()
sTable3 <- ftTheme(table = sTable3, tblwidth = c(1.7, 1.4, 1.4, 1.4))
sTable3



#### Table 4: Total annual number of cases and costs ####

# Get mean total costs for each group
mCost <- list(
  scabies    = mean(phc$cMeanTotal[phc$dxScabies==1]),
  ssti       = mean(phc$cMeanTotal[phc$dxSsti==1]),
  related    = mean(hosp$cMeanTotal[hosp$dxScabies==1]),
  nonrelated = mean(hosp$cMeanTotal[hosp$dxScabies==2]))
mCost

# Run yearCases function to get estimated number of cases for all divisions
nTotalPhc  <- yearCases(nBigshiftPhc,  fTime = 50/(365.25/7), result = "Total")
nTotalHosp <- yearCases(nBigshiftHosp, fTime = 48/(365.25/7), result = "Total")

# Calculate PHC, hospital, total, and percapita costs
cTotalPhc   <- nTotalPhc  * c(mCost$scabies, mCost$ssti)
cTotalHosp  <- nTotalHosp * c(mCost$related, mCost$nonrelated)
cCapitaPhc  <- cTotalPhc / pop$total
cCapitaHosp <- cTotalHosp / pop$total

# Total number of cases and costs, and cost per capita
nTotal <- c(nTotalPhc["scabies"], nTotalPhc["ssti"], nTotalHosp["related"],
            "phc" = sum(nTotalPhc["scabies"], nTotalPhc["ssti"]),
            overall = sum(nTotalPhc["scabies"], nTotalPhc["ssti"], nTotalHosp["related"]))
cTotal <- c(cTotalPhc["scabies"], cTotalPhc["ssti"], cTotalHosp["related"],
            "phc" = sum(cTotalPhc["scabies"], cTotalPhc["ssti"]),
            overall = sum(cTotalPhc["scabies"], cTotalPhc["ssti"], cTotalHosp["related"]))
cCapita <- cTotal / pop$total

# Variable names for table
Component <- c("Scabies presentations (a)",
               "Scabies-related SSTI presentations (b)",
               "Scabies-related SSTI admissions (c)",
               "All scabies and scabies-related SSTI presentations (a+b)",
               "All scabies and scabies-related SSTI cases (a+b+c)")

table4 <- data.frame(Component, nTotal=round(nTotal, 0), cTotal=round(cTotal, 0), 
                     cCapita=round(cCapita, 2), row.names = NULL)
table4 <- table4 %>%
  flextable() %>%
  set_header_labels(values = list(Component="Component", nTotal="Number of cases",
                                  cTotal="Total cost ($)", cCapita="Cost per capita ($)")) %>% 
  ftTheme(tblwidth = c(2.2, 1.4, 1.4, 1.4))


#### Sensitivity analysis ####

# Sensitivity analysis on cost of per outpatient visit #
cTempLow  <- sum((nTotalPhc["scabies"] * mean(phc$cMeanTotal_cClinicVisit_low[phc$dxScabies==1])),
                  nTotalPhc["ssti"]    * mean(phc$cMeanTotal_cClinicVisit_low[phc$dxSsti==1]))
cTempHigh <- sum((nTotalPhc["scabies"] * mean(phc$cMeanTotal_cClinicVisit_high[phc$dxScabies==1])),
                  nTotalPhc["ssti"]    * mean(phc$cMeanTotal_cClinicVisit_high[phc$dxSsti==1]))
cOwsaClinic <-  c("Low"  = unname(cTotalHosp["related"] + cTempLow),
                  "High" = unname(cTotalHosp["related"] + cTempHigh))

# Sensitivity analysis on cost of per bed day in ward
cTempLow  <- nTotalHosp["related"] * mean(hosp$cMeanTotal_cWardAdmit_low[hosp$dxScabies==1])
cTempHigh <- nTotalHosp["related"] * mean(hosp$cMeanTotal_cWardAdmit_high[hosp$dxScabies==1])
cOwsaWard <- c("Low"  = unname(sum(cTotalPhc) + cTempLow),
               "High" = unname(sum(cTotalPhc) + cTempHigh))

# Sensitivity analysis on cost of per bed day in ICU
cTempLow  <- nTotalHosp["related"] * mean(hosp$cMeanTotal_cICUAdmit_low[hosp$dxScabies==1])
cTempHigh <- nTotalHosp["related"] * mean(hosp$cMeanTotal_cICUAdmit_high[hosp$dxScabies==1])
cOwsaIcu  <- c("Low"  = unname(sum(cTotalPhc) + cTempLow),
               "High" = unname(sum(cTotalPhc) + cTempHigh))

# Sensitivity analysis on cost per laboratory test
cTempLow  <- nTotalHosp["related"] * mean(hosp$cMeanTotal_cDiagnostics_low[hosp$dxScabies==1])
cTempHigh <- nTotalHosp["related"] * mean(hosp$cMeanTotal_cDiagnostics_high[hosp$dxScabies==1])
cOwsaDiag <- c("Low"  = unname(sum(cTotalPhc) + cTempLow),
               "High" = unname(sum(cTotalPhc) + cTempHigh))



owsa <- data.frame(parameter = c("Unit cost of one PHC visit", 
                                 "Unit cost per ward bed-day",
                                 "Unit cost per ICU bed-day", 
                                 "Unit cost of diagnostic test"),
                  Low  = c(cOwsaClinic["Low"], cOwsaWard["Low"], 
                           cOwsaIcu["Low"], cOwsaDiag["Low"]),
                  High = c(cOwsaClinic["High"], cOwsaWard["High"], 
                           cOwsaIcu["High"], cOwsaDiag["High"]))


# Get the sensitivity analyses results
owsa <- owsa %>%
  mutate(Low = Low/1000000,
         High = High/1000000,
         range = abs(High - Low)) %>%
  arrange(range) %>%
  mutate(parameter=factor(x=parameter, levels=parameter)) %>%
  pivot_longer(names_to='type', values_to='value', Low:High)


### Function for rescaling plots to a tornado
offset_trans <- function(offset=0) {
  trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x+offset)
}

baseCase <- cTotal["overall"]/1000000 # Get the base case value

# Tornado plot for one-way sensitivity analysis
ggplot(owsa, aes(x=parameter,y=value, fill=type, colour=type)) +
  geom_bar(data=owsa[owsa$type=="Low",],  aes(x=parameter,y=value), stat="identity", linewidth=0.3, width = 0.5) +
  geom_bar(data=owsa[owsa$type=="High",], aes(x=parameter,y=value), stat="identity", linewidth=0.3, width = 0.5) +
  geom_hline(yintercept = baseCase, linetype = "solid", linewidth=0.25) +
  theme_bw() +
  theme(axis.title.x      = element_text(size = 14),
        axis.title.y      = element_text(size = 14),
        axis.text         = element_text(size = 12,  color = "black"),
        axis.line         = element_line(linewidth = 0.1, color = "#444444"),
        axis.ticks        = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        panel.grid.major  = element_line(linewidth = 0.2, colour = "gray90"),
        plot.title        = element_text(size = 14, colour = "black", margin=margin(t=5, b=5), hjust = 0.5),
        legend.position.inside = c(1, 0),
        legend.justification = c(1.1, 0),
        legend.background = element_rect(fill="transparent"),
        legend.text = element_text(size=10),   # change legend text font size
        legend.title = element_blank(),        # change legend title font size
        legend.key.size = unit(0.3, 'cm')) +  # change legend key size
  scale_x_discrete(name=" ") + coord_flip() + xlab("") + ylab("Cost per DALY averted ($)") +
  panel_border(color = "#444444", size = 0.3, linetype = 1) +
  scale_fill_manual(values = c("#FEDAB8", "#A162D0")) +
  scale_color_manual(values = c("black", "black")) +
  scale_y_continuous(name="Total costs of scabies and related SSTIs (million $)", 
                     trans=offset_trans(offset=baseCase),
                     limits = c(2, 5)) +
  geom_text(data = subset(owsa, type == "Low"), show.legend = FALSE,
            aes(label = round(value, 2)),
            hjust= ifelse(subset(owsa, type == "Low", select = value) < baseCase, 1.15, -0.15),
            size = 3.6) +
  geom_text(data = subset(owsa, type == "High"), show.legend = FALSE,
            aes(label = round(value, 2)),
            hjust= ifelse(subset(owsa, type == "High", select = value) < baseCase, 1.15, -0.15),
            size = 3.6)
ggsave(height=4, width=8, dpi=600, file="output/Fig1.png")
ggsave(height=4, width=8, dpi=600, file="output/Fig1.eps")
ggsave(height=4, width=8, dpi=300, file="output/Fig1.tiff")

