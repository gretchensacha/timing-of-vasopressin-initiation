
# eICU Database ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PACKAGES 
library(readr)
library(readxl)
library(writexl)
library(lubridate)
library(conflicted)
library(tidyverse)
library(vroom)
library(janitor)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::recode)
conflicts_prefer(dplyr::rename)

## LRI Network Server #### 
meds <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/medication.csv.gz")
eicuinfusion <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/infusionDrug.csv.gz")
patient <- read_csv("~/eICU Data/eicu-collaborative-research-database-2.0/patient.csv.gz")
lab <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/lab.csv.gz")
# micro <- read_csv("~/eICU Data/eicu-collaborative-research-database-2.0/microLab.csv.gz")
# respcare <- read_csv("~/eICU Data/eicu-collaborative-research-database-2.0/respiratoryCare.csv.gz")
respchart <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/respiratoryCharting.csv.gz")
apacheapsvar <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/apacheApsVar.csv.gz")
apacheresult <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/apachePatientResult.csv.gz")
vitalperiodic <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/vitalPeriodic.csv.gz")
vitalaperiodic <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/vitalAperiodic.csv.gz")
admissiondx <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/admissionDx.csv.gz")
diagnosis <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/diagnosis.csv.gz")
eicupmh <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/pastHistory.csv.gz")
apachevariables <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/apacheApsVar.csv.gz")
apacheresults <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/apachePatientResult.csv.gz")
apachepredicted <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/apachePredVar.csv.gz")
ios <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/intakeOutput.csv.gz")

# VASOPRESSOR DATA ####
### Vasopressin ####
avp <- eicuinfusion %>% filter(grepl("vasopressin", drugname, ignore.case = TRUE)) %>% 
  mutate(drugrate = as.numeric(drugrate)) %>%
  filter(!is.na(drugrate)) %>%
  arrange(patientunitstayid, infusionoffset) %>%
  mutate(conc = as.character(if_else(!is.na(drugamount) & !is.na(volumeoffluid), drugamount/volumeoffluid, NA))) %>%
  mutate(conc = round_half_up(as.numeric(case_when(
    drugname == "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (units/min)" ~ "0.4",
    drugname == "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (Unknown)" ~ "0.4",
    drugname == "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (units/hr)" ~ "0.4",
    drugname == "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (units/kg/hr)" ~ "0.4",
    drugname == "Vasopressin 40 Units Sodium Chloride 0.9% 200 ml (units/min)" ~ "0.2",
    drugname == "Vasopressin 20 Units Sodium Chloride 0.9% 250 ml (units/hr)" ~ "0.08",
    drugname == "Vasopressin 20 Units Sodium Chloride 0.9% 100 ml (units/hr)" ~ "0.2", .default = conc)),4)) %>%
  relocate(conc, .after = drugname) %>%
  mutate(drugname_f = fct_recode(factor(drugname), #shorten drug name as a factor
                                 "vaso u/min" = "Vasopressin (units/min)",
                                 "vaso ml/hr" = "Vasopressin (ml/hr)",
                                 "vaso mcg/kg/min" = "Vasopressin (mcg/kg/min)",
                                 "vaso u/hr" = "Vasopressin (units/hr)",
                                 "vaso u/min" = "Vasopressin (mcg/min)",
                                 "vaso u/min" = "Vasopressin (mg/min)",
                                 "vaso u/hr" = "Vasopressin (mg/hr)",
                                 "vaso u/min" = "vasopressin (units/min)",
                                 "vaso u/min" = "VAsopressin (units/min)",
                                 "vaso unknown units" = "Vasopressin ()",
                                 "vaso ml/hr" =  "vasopressin (ml/hr)",
                                 "vaso u/min" = "Vasopressin (units/kg/min)",
                                 "vaso u/min" = "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (units/min)",
                                 "vaso u/hr" = "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (units/hr)",
                                 "vaso u/min" = "Vasopressin 40 Units Sodium Chloride 0.9% 200 ml (units/min)", 
                                 "vaso u/hr" = "Vasopressin 20 Units Sodium Chloride 0.9% 250 ml (units/hr)",
                                 "vaso u/hr" = "Vasopressin 20 Units Sodium Chloride 0.9% 100 ml (units/hr)",
                                 "vaso u/min" = "Vasopressin 40 Units Sodium Chloride 0.9% 100 ml (units/kg/hr)"), .after = drugname) %>%
  mutate(drugrate = as.numeric(case_when(drugname_f == "vaso ml/hr" & !is.na(conc) ~ (drugrate*conc)/60,
    drugrate <= 0.1 ~ drugrate, #if drugrate is <=0.1 then assume this is a units/min dose
    (drugname_f == "vaso ml/hr" | drugname_f == "vaso u/hr") & 
      (drugrate > 0.1 & drugrate <= 6) ~ drugrate/60, #if drugrate >0.1 to 6 assume units/hr
    drugname_f == "vaso ml/hr" & conc == "0.2" & (drugrate == 12.12 | drugrate == 12) ~ 0.04,
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & 
      (drugrate == 15 | drugrate == 15.1) &
      is.na(conc) ~ 0.1, #concentration and units unknown, assume conc 40/100 = 0.4 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 9 &
      is.na(conc) ~ 0.06, #concentration and units unknown, assume conc 40/100 = 0.4 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 7.5 &
      is.na(conc) ~ 0.05, #concentration and units unknown, assume conc 40/100 = 0.4 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 6 &
      is.na(conc) ~ 0.04, #concentration and units unknown, assume conc 40/100 = 0.4 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 4.5 & 
      is.na(conc) ~ 0.03, #concentration and units unknown, assume conc 40/100 = 0.4 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 30 &
      is.na(conc) ~ 0.1, #concentration and units unknown, assume conc 20/100 = 0.2 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 18 &
      is.na(conc) ~ 0.06, #concentration and units unknown, assume conc 20/100 = 0.2 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & 
      (drugrate == 12.12 | drugrate == 12) & 
      is.na(conc) ~ 0.04, #concentration unknown, assume 20/100 = 0.2 based on rate, 
    #but this could also very well be 0.08 u/min with conc 0.4
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 10 & 
      is.na(conc) ~ 0.03, #concentration unknown, assume 20/100 = 0.2 based on rate (0.033 u/min), 
    #but this could also very well be 0.06 u/min (0.067 u/min) with conc 0.4
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 11.3 &
      is.na(conc) ~ 0.03, #concentration and units unknown, assume conc 40/250 = 0.16 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 22.6 &
      is.na(conc) ~ 0.06, #concentration and units unknown, assume conc 40/250 = 0.16 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 45 &
      is.na(conc) ~ 0.03, #concentration and units unknown, assume conc 10/250 = 0.04 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 60 &
      is.na(conc) ~ 0.04, #concentration and units unknown, assume conc 10/250 = 0.04 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 75 &
      is.na(conc) ~ 0.05, #concentration and units unknown, assume conc 10/250 = 0.04 based on rate
    (drugname_f == "vaso unknown units" | drugname_f == "vaso ml/hr") & drugrate == 90 &
      is.na(conc) ~ 0.06, #concentration and units unknown, assume conc 10/250 = 0.04 based on rate
    drugname_f == "vaso unknown units" & (drugrate >= 0.6 & drugrate <= 6) &
      is.na(conc) ~ drugrate/60, #concentration and units unknown, assume rate is in ml/hr
    #and divide by 60 to give dose between 0.01 to 0.1 units/min
    drugname_f == "vaso u/min" & drugrate >0.1 & 
      drugrate <= 0.2 ~ drugrate, #if dosing in units/min specified then doses up to 0.2 reasonable
    .default = NA))) %>% 
  filter(complete.cases(drugrate))

avp1 <- avp %>% group_by(patientunitstayid) %>%
  arrange(patientunitstayid, infusionoffset) %>% 
  mutate(drugratedifference = drugrate - lag(drugrate)) %>% # see below
  mutate(delete = if_else(drugratedifference == "0" & drugrate == "0", "delete", "")) %>% # need to remove the rates that are charted 0 multiple times in a row
  filter(delete == "") %>% # removed all 0 charting after the first 0 
  mutate(offsetdifference = infusionoffset - lag(infusionoffset)) %>% # obtains the difference in infusionoffset
  mutate(priordrugrate = lag(drugrate)) %>% # getting prior drugrate 
  mutate(nextdrugrate = lead(drugrate)) %>% # getting next drugrate
  mutate(nextoffsetdifference = lead(offsetdifference)) %>%
  mutate(infusionoffsetstop = lead(infusionoffset)) %>%
  left_join(patient %>% select(patientunitstayid, unitdischargeoffset, unitdischargestatus), 
            by = join_by("patientunitstayid")) %>%  # getting ICU discharge time for those without  
  # a drugrate of zero charted when stopped
  select(infusiondrugid, patientunitstayid, unitdischargeoffset, 
         unitdischargestatus, infusionoffset, infusionoffsetstop, 
         drugname_f, drugrate, priordrugrate, nextdrugrate, offsetdifference, 
         nextoffsetdifference) %>% # reorganizing and removing unused variables
  mutate(infusionoffsetstop = if_else(is.na(infusionoffsetstop), infusionoffset, infusionoffsetstop)) %>%
  mutate(unitdischargestatus = as.factor(unitdischargestatus)) %>%
  mutate(status = as.factor(if_else(is.na(offsetdifference) | is.na(priordrugrate), "initial start", 
                            if_else(drugrate == 0 & priordrugrate > 0 & offsetdifference < 1440 & !is.na(nextdrugrate) & nextoffsetdifference >= 1440, "stopped",
                            if_else(drugrate == 0 & priordrugrate > 0 & is.na(nextdrugrate), "final stop", if_else(drugrate > 0 & !is.na(priordrugrate) & is.na(nextdrugrate), "final stop",
                            if_else(drugrate > 0 & priordrugrate == 0 & offsetdifference >= 1440, "restarted", ""))))))) %>%
  group_by(patientunitstayid, status) %>%
  mutate(count = 1:n()) %>% # sequentially counting each status to get 
  # 1st, 2nd, 3rd, etc. stop/restart
  ungroup() 

write_csv(avp1, file = "~/eICU Data/avp1.csv")

### Norepinephrine ####

## Adding weight to ne df from admit/dc weight on patient df
patient <- patient %>% mutate(weight = ifelse(is.na(admissionweight), dischargeweight,admissionweight))
# preferenced admission weight, if no admission weight, took discharge weight

#269625
eicune <- eicuinfusion %>% filter(grepl("norepinephrine|levophed", drugname, ignore.case = TRUE)) %>% 
  filter(!is.na(drugrate)) %>% 
  mutate(drugrate = as.numeric(drugrate)) %>%
  filter(complete.cases(drugrate))%>% 
  left_join(patient %>% dplyr::select(patientunitstayid, weight)) %>%
  arrange(patientunitstayid, infusionoffset) %>%
  mutate(conc = as.character(if_else(!is.na(drugamount) & drugamount >0 & !is.na(volumeoffluid) & volumeoffluid >0,
                                     drugamount/volumeoffluid, NA))) %>%
  mutate(conc = round_half_up(as.numeric(case_when(
    drugname == "Norepinephrine STD 4 mg Dextrose 5% 250 ml (mcg/min)" ~ "0.016",
    drugname == "Norepinephrine MAX 32 mg Dextrose 5% 250 ml (mcg/min)" ~ "0.128",
    drugname == "Norepinephrine STD 8 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.016",
    drugname == "Norepinephrine STD 32 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.064",
    drugname == "Norepinephrine STD 4 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.008",
    drugname == "Norepinephrine MAX 32 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.064",
    drugname == "Norepinephrine STD 32 mg Dextrose 5% 282 ml (mcg/min)" ~ "0.128",
    drugname == "Norepinephrine STD 8 mg Dextrose 5% 250 ml (mcg/min)" ~ "0.032",
    .default = conc)),4)*1000) %>%
  relocate(conc, .after = drugname) %>%
  mutate(drugrate = as.numeric(drugrate)) %>%
  mutate(drugname_f = fct_recode(factor(drugname), 
                                 "ne mcg/min" = "Norepinephrine (mcg/min)",
                                 "ne mcg/min" = "levophed  (mcg/min)",
                                 "ne mcg/min" = "levophed (mcg/min)",
                                 "ne mcg/min" = "Levophed (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine (units/min)",
                                 "ne mcg/min" = "Norepinephrine (mg/min)",
                                 "ne mcg/min" = "Norepinephrine (mg/hr)",
                                 "ne mcg/min" = "Levophed (mg/hr)",
                                 "ne mcg/min" = "Norepinephrine (mcg/hr)",
                                 "ne mcg/min" = "Norepinephrine STD 4 mg Dextrose 5% 250 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine MAX 32 mg Dextrose 5% 250 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 8 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 32 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 4 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine MAX 32 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 32 mg Dextrose 5% 282 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 8 mg Dextrose 5% 250 ml (mcg/min)",
                                 "ne ml/hr" = "Norepinephrine (ml/hr)",
                                 "ne ml/hr" = "Levophed (ml/hr)",
                                 "ne ml/hr" = "levophed (ml/hr)",
                                 "ne ml/hr" = "Norepinephrine (ml/hr)",
                                 "ne ml/hr" = "norepinephrine Volume (ml) (ml/hr)",
                                 "ne mcg/kg/min" = "Norepinephrine (mcg/kg/min)",
                                 "ne mcg/kg/min" = "Levophed (mcg/kg/min)",
                                 "ne mcg/kg/min" = "Norepinephrine (mg/kg/min)",
                                 "ne unknown units" = "Norepinephrine ()",
                                 "ne mcg/kg/hr" = "Norepinephrine (mcg/kg/hr)",), .after = drugname) %>%
  mutate(drugname_f = case_when(# erroneous use of mcg/kg/hr, reviewed, generated below
    drugname_f == "ne mcg/kg/hr" & drugrate <0.5 ~ "ne mcg/kg/min",
    drugname_f == "ne mcg/kg/hr" & drugrate >=0.5 ~ "ne mcg/min",
    .default = drugname_f)) %>%
  mutate(drugname_f = case_when(# unknown units with conc available, reviewed, generated below
    drugname_f == "ne unknown units" & !is.na(conc) & drugrate <0.5 ~ "ne mcg/kg/min",
    drugname_f == "ne unknown units" & !is.na(conc) & drugrate >=0.5 ~ "ne mcg/min",
    .default = drugname_f)) %>% 
  mutate(drugname_f = if_else(drugname_f == "ne unknown units" & drugrate == 0, 
                              "ne mcg/min", drugname_f)) %>%
  mutate(drugrate = as.numeric(case_when( # calculate ne dosage in mcg/min
    (drugname_f == "ne mcg/min" & drugrate <=1000) ~ drugrate, # account for single outlier
    drugname_f == "ne mcg/kg/min" & drugrate >=5 ~ drugrate,
    drugname_f == "ne mcg/kg/min" & 
      !is.na(patientweight) ~ drugrate*patientweight, # prefer patientweight when available
    drugname_f == "ne mcg/kg/min" & 
      is.na(patientweight) ~ drugrate*weight, # use weight when patientweight not available
    drugname_f == "ne ml/hr" & !is.na(conc) ~ (drugrate*conc)/60,
    .default = NA))) %>%
  relocate(drugrate, .after = drugname_f) %>% 
  filter(!is.na(drugrate))

# Adding in the start/stop times
ne1 <- eicune %>% group_by(patientunitstayid) %>%
  arrange(patientunitstayid, infusionoffset) %>% 
  mutate(drugratedifference = drugrate - lag(drugrate)) %>% # see below
  mutate(delete = if_else(drugratedifference == "0" & drugrate == "0", "delete", "")) %>% # need to remove the rates that are charted 0 multiple times in a row
  filter(delete == "") %>% # removed all 0 charting after the first 0 
  mutate(offsetdifference =  infusionoffset - lag(infusionoffset)) %>% # obtains the difference in infusionoffset
  mutate(priordrugrate = lag(drugrate)) %>% # getting prior dose 
  mutate(nextdrugrate = lead(drugrate)) %>% # getting next dose
  mutate(nextoffsetdifference = lead(offsetdifference)) %>% 
  mutate(infusionoffsetstop = lead(infusionoffset)) %>% 
  left_join(patient %>% select(patientunitstayid, unitdischargeoffset, unitdischargestatus), by = join_by("patientunitstayid")) %>%  
  # getting ICU discharge time for those without  
  # a dose of zero charted when stopped
  select(infusiondrugid, patientunitstayid, unitdischargeoffset, unitdischargestatus, infusionoffset, infusionoffsetstop, 
         drugname_f, drugrate, priordrugrate, nextdrugrate, offsetdifference, nextoffsetdifference) %>% # reorganizing and removing unused variables
  mutate(infusionoffsetstop = if_else(is.na(infusionoffsetstop), infusionoffset, infusionoffsetstop)) %>%
  mutate(unitdischargestatus = as.factor(unitdischargestatus)) %>%
  mutate(status = as.factor(if_else(is.na(offsetdifference) | is.na(priordrugrate), "initial start", 
                            if_else(drugrate == 0 & priordrugrate > 0 & offsetdifference < 1440 & !is.na(nextdrugrate) & nextoffsetdifference >= 1440, "stopped",
                            if_else(drugrate == 0 & priordrugrate > 0 & is.na(nextdrugrate), "final stop",
                            if_else(drugrate > 0 & !is.na(priordrugrate) & is.na(nextdrugrate), "final stop",
                            if_else(drugrate > 0 & priordrugrate == 0 & offsetdifference >= 1440, "restarted", ""))))))) %>% 
  group_by(patientunitstayid, status) %>% 
  mutate(count = 1:n()) %>% # sequentially counting each status to get 1st, 2nd, 3rd, etc. stop/restart
  ungroup() %>% 
  # 11 patients were found to have no discontinuation time
  filter(! patientunitstayid %in% 
           c(960408, 960592, 963226, 963555, 966541, 975574, 
             1737209, 1785848, 3147029, 3340939, 3351643))

write_csv(ne1, file = "~/eICU Data/ne1.csv")
ne1 <- read_csv("~/eICU Data/ne1.csv")


## Evaluating patients excluded for not being able to interpret the NE Dose
eicuneeval <- eicuinfusion %>% filter(grepl("norepinephrine|levophed", drugname, ignore.case = TRUE)) %>% 
  filter(!is.na(drugrate)) %>% 
  mutate(drugrate = as.numeric(drugrate)) %>%
  filter(complete.cases(drugrate))%>% 
  left_join(patient %>% dplyr::select(patientunitstayid, weight)) %>%
  arrange(patientunitstayid, infusionoffset) %>%
  mutate(conc = as.character(if_else(!is.na(drugamount) & drugamount >0 & !is.na(volumeoffluid) & volumeoffluid >0,
                                     drugamount/volumeoffluid, NA))) %>%
  mutate(conc = round_half_up(as.numeric(case_when(
    drugname == "Norepinephrine STD 4 mg Dextrose 5% 250 ml (mcg/min)" ~ "0.016",
    drugname == "Norepinephrine MAX 32 mg Dextrose 5% 250 ml (mcg/min)" ~ "0.128",
    drugname == "Norepinephrine STD 8 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.016",
    drugname == "Norepinephrine STD 32 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.064",
    drugname == "Norepinephrine STD 4 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.008",
    drugname == "Norepinephrine MAX 32 mg Dextrose 5% 500 ml (mcg/min)" ~ "0.064",
    drugname == "Norepinephrine STD 32 mg Dextrose 5% 282 ml (mcg/min)" ~ "0.128",
    drugname == "Norepinephrine STD 8 mg Dextrose 5% 250 ml (mcg/min)" ~ "0.032",
    .default = conc)),4)*1000) %>%
  relocate(conc, .after = drugname) %>%
  mutate(drugrate = as.numeric(drugrate)) %>%
  mutate(drugname_f = fct_recode(factor(drugname), 
                                 "ne mcg/min" = "Norepinephrine (mcg/min)",
                                 "ne mcg/min" = "levophed  (mcg/min)",
                                 "ne mcg/min" = "levophed (mcg/min)",
                                 "ne mcg/min" = "Levophed (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine (units/min)",
                                 "ne mcg/min" = "Norepinephrine (mg/min)",
                                 "ne mcg/min" = "Norepinephrine (mg/hr)",
                                 "ne mcg/min" = "Levophed (mg/hr)",
                                 "ne mcg/min" = "Norepinephrine (mcg/hr)",
                                 "ne mcg/min" = "Norepinephrine STD 4 mg Dextrose 5% 250 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine MAX 32 mg Dextrose 5% 250 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 8 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 32 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 4 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine MAX 32 mg Dextrose 5% 500 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 32 mg Dextrose 5% 282 ml (mcg/min)",
                                 "ne mcg/min" = "Norepinephrine STD 8 mg Dextrose 5% 250 ml (mcg/min)",
                                 "ne ml/hr" = "Norepinephrine (ml/hr)",
                                 "ne ml/hr" = "Levophed (ml/hr)",
                                 "ne ml/hr" = "levophed (ml/hr)",
                                 "ne ml/hr" = "Norepinephrine (ml/hr)",
                                 "ne ml/hr" = "norepinephrine Volume (ml) (ml/hr)",
                                 "ne mcg/kg/min" = "Norepinephrine (mcg/kg/min)",
                                 "ne mcg/kg/min" = "Levophed (mcg/kg/min)",
                                 "ne mcg/kg/min" = "Norepinephrine (mg/kg/min)",
                                 "ne unknown units" = "Norepinephrine ()",
                                 "ne mcg/kg/hr" = "Norepinephrine (mcg/kg/hr)",), .after = drugname) %>%
  mutate(drugname_f = case_when(# erroneous use of mcg/kg/hr, reviewed, generated below
    drugname_f == "ne mcg/kg/hr" & drugrate <0.5 ~ "ne mcg/kg/min",
    drugname_f == "ne mcg/kg/hr" & drugrate >=0.5 ~ "ne mcg/min",
    .default = drugname_f)) %>%
  mutate(drugname_f = case_when(# unknown units with conc available, reviewed, generated below
    drugname_f == "ne unknown units" & !is.na(conc) & drugrate <0.5 ~ "ne mcg/kg/min",
    drugname_f == "ne unknown units" & !is.na(conc) & drugrate >=0.5 ~ "ne mcg/min",
    .default = drugname_f)) %>% 
  mutate(drugname_f = if_else(drugname_f == "ne unknown units" & drugrate == 0, 
                              "ne mcg/min", drugname_f)) %>%
  mutate(drugrate = as.numeric(case_when( # calculate ne dosage in mcg/min
    (drugname_f == "ne mcg/min" & drugrate <=1000) ~ drugrate, # account for single outlier
    drugname_f == "ne mcg/kg/min" & drugrate >=5 ~ drugrate,
    drugname_f == "ne mcg/kg/min" & 
      !is.na(patientweight) ~ drugrate*patientweight, # prefer patientweight when available
    drugname_f == "ne mcg/kg/min" & 
      is.na(patientweight) ~ drugrate*weight, # use weight when patientweight not available
    drugname_f == "ne ml/hr" & !is.na(conc) ~ (drugrate*conc)/60,
    .default = NA))) %>%
  relocate(drugrate, .after = drugname_f) %>% 
  filter(is.na(drugrate))

neonlist <- eicune %>% select(patientunitstayid) %>% distinct() %>% mutate(onlist = "Yes")
neremovelist <- eicuneeval %>% select(patientunitstayid) %>% distinct() %>% mutate(removedfromne = "Yes") %>%
  left_join(neonlist, by = "patientunitstayid") %>%
  filter(is.na(onlist)) %>% select(-onlist)
rm(neonlist)
write.csv(neremovelist, file = "eICU Data/neremovelist.csv", row.names = FALSE)

includedptlist <- eicu %>% filter(include_exclude_sepsis3nomapbosch == "include") %>%
  rename("patientunitstayid" = "hospitalid") %>% select(patientunitstayid, neuse, epiuse, peuse, dopamineuse) %>%
  left_join(neremovelist, by = "patientunitstayid") %>% 
  filter(removedfromne == "Yes")
# 178 patients removed that meet the modified sepsis-3 criteria that were may need to be removed because NE doses
# were not included but were on other vasopressors.... 

dfne <- eicune %>% filter(patientunitstayid == "554883")
dfne2 <- eicuneeval %>% filter(patientunitstayid == "554883")
dfepi <- eicuepi %>% filter(patientunitstayid == "554883")

### Epinephrine ####
eicuepi <- eicuinfusion %>% filter(grepl("epinephrine", drugname, ignore.case = TRUE)) %>% 
  filter(!(grepl("norepinephrine", drugname, ignore.case = TRUE))) %>%
  filter(!is.na(drugrate))
# max(epi$drugrate) # 18705
# unique(epi$drugname) # 11 drug names to clean up

# Cleaning Epi
## Adding weight to ne df from admit/dc weight on patient df
# patient <- patient %>% mutate(weight = ifelse(is.na(admissionweight), dischargeweight,admissionweight))
# preferenced admission weight, if no admission weight, took discharge weight
eicuepi <- eicuepi %>% left_join(patient %>% dplyr::select(patientunitstayid, weight))

## Renaming categories of Epi names
## Had to change all ml/hr, mg/hr, mcg/hr, (), to mcg/min because looked like non-weight based
eicuepi <- eicuepi %>% mutate(drugname = as.factor(drugname), 
                    drugname = recode_factor(drugname, "Epinephrine (mcg/min)" = "epi mcg/min", 
                                             "Epinephrine (ml/hr)" = "epi mcg/min",
                                             "Epinephrine (mg/hr)" = "epi mcg/min",
                                             "Epinephrine ()"  = "epi mcg/min", 
                                             "Epinephrine (mcg/hr)" = "epi mcg/min", 
                                             "EPINEPHrine(Adrenalin)MAX 30 mg Sodium Chloride 0.9% 250 ml (mcg/min)" = "epi mcg/min", 
                                             "EPINEPHrine(Adrenalin)STD 4 mg Sodium Chloride 0.9% 500 ml (mcg/min)" = "epi mcg/min", 
                                             "EPINEPHrine(Adrenalin)STD 4 mg Sodium Chloride 0.9% 250 ml (mcg/min)" = "epi mcg/min", 
                                             "EPINEPHrine(Adrenalin)STD 7 mg Sodium Chloride 0.9% 250 ml (mcg/min)" = "epi mcg/min"))


eicuepi <- eicuepi %>% mutate(drugname = as.factor(drugname), 
                    drugname = recode_factor(drugname, "Epinephrine (mcg/kg/min)" = "epi mcg/kg/min", 
                                             "Epinephrine (mg/kg/min)" = "epi mcg/kg/min")) %>% 
  mutate(weight = ifelse(is.na(patientweight), weight, patientweight)) %>% 
  mutate(weight = as.numeric(weight), 
         drugrate = as.numeric(drugrate)) %>%
  mutate(drugrate = ifelse(drugname == "epi mcg/kg/min" & drugrate < 1, round(drugrate*weight,2), drugrate)) %>% 
  mutate(drugname = recode_factor(drugname, "epi mcg/kg/min" = "epi mcg/min"))

# Incorporating start/stop/restart/pause times (stopped -- >=24 hours off vasopressors)
# leaving as new epi1 df just incase need to clean more doses with ne then can clean and rerun the epi1 code 
epi1 <- eicuepi %>% group_by(patientunitstayid) %>% arrange(patientunitstayid, infusionoffset) %>% 
  mutate(drugratedifference = drugrate - lag(drugrate)) %>% # see below
  mutate(delete = ifelse(drugratedifference == "0" & drugrate == "0", "delete", "")) %>% # need to remove the rates that are charted 0 multiple times in a row
  filter(delete == "") %>% # removed all 0 charting after the first 0 
  mutate(offsetdifference =  infusionoffset - lag(infusionoffset)) %>% # obtains the difference in infusionoffset
  mutate(priorrate = lag(drugrate)) %>% # getting the prior rate 
  mutate(nextrate = lead(drugrate)) %>% # getting next infusion rate
  mutate(nextoffsetdifference = lead(offsetdifference)) %>% 
  mutate(infusionoffsetstop = lead(infusionoffset)) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset, unitdischargestatus), by = "patientunitstayid") %>% # getting ICU discharge time for those without 0 charted when stopped
  select(infusiondrugid, patientunitstayid, unitdischargeoffset, unitdischargestatus, infusionoffset, infusionoffsetstop, drugname, drugrate, priorrate, nextrate, offsetdifference, nextoffsetdifference) %>% # reorganizing and removing unused variables
  mutate(infusionoffsetstop = ifelse(is.na(infusionoffsetstop), unitdischargeoffset, infusionoffsetstop)) %>%
  mutate(unitdischargestatus = as.factor(unitdischargestatus)) %>% 
  mutate(status = ifelse(is.na(offsetdifference) | is.na(priorrate), "initial start", 
                  ifelse(drugrate == 0 & priorrate > 0 & offsetdifference < 1440 & !is.na(nextrate) & nextoffsetdifference >= 1440, "stopped", 
                  ifelse(drugrate == 0 & priorrate > 0 & is.na(nextrate), "final stop",
                  ifelse(drugrate > 0 & !is.na(priorrate) & is.na(nextrate), "final stop",
                  ifelse(drugrate > 0 & priorrate == 0 & offsetdifference >= 1440, "restarted", "")))))) %>% 
  mutate(status = as.factor(status)) %>% 
  group_by(patientunitstayid, status) %>% mutate(count = 1:n()) %>% ungroup() # sequentially counting each status to get 1st, 2nd, 3rd stop/restart

# df <- epi1 %>% filter(status == "initial start" & drugrate == 0) # no initial drug rate of 0

### Phenylephrine #### 
pe <- eicuinfusion %>% filter(grepl("phenylephrine|neo-synephrine|neosynephrine|neo synephrine", drugname, ignore.case = TRUE)) %>% 
  filter(!is.na(drugrate))
# max(pe$drugrate) # 9200
# unique(pe$drugname) # 21 drug names to clean up

# Cleaning PE
## Adding weight to ne df from admit/dc weight on patient df
# patient <- patient %>% mutate(weight = ifelse(is.na(admissionweight), dischargeweight,admissionweight))
# preferenced admission weight, if no admission weight, took discharge weight
pe <- pe %>% left_join(patient %>% dplyr::select(patientunitstayid, weight))

## Renaming categories of NE names
## Had to change all ml/hr, mg/hr, mcg/hr, (), to mcg/min because looked like non-weight based
pe <- pe %>% mutate(drugname = as.factor(drugname), 
                      drugname = recode_factor(drugname, "Phenylephrine (mcg/min)" = "pe mcg/min", 
                                               "Phenylephrine ()" = "pe mcg/min",
                                               "Phenylephrine (mcg/hr)" = "pe mcg/min",
                                               "NEO-SYNEPHRINE (mcg/min)"  = "pe mcg/min", 
                                               "Neo Synephrine (mcg/min)" = "pe mcg/min", 
                                               "neo-synephrine (mcg/min)" = "pe mcg/min", 
                                               "NeoSynephrine (mcg/min)" = "pe mcg/min", 
                                               "Neo-Synephrine (mcg/min)" = "pe mcg/min", 
                                               "Neosynephrine (mcg/min)" = "pe mcg/min",
                                               "neosynephrine (mcg/min)" = "pe mcg/min", 
                                               "Phenylephrine (ml/hr)" = "pe mcg/min", 
                                               "Neosynephrine (ml/hr)" = "pe mcg/min", 
                                               "Phenylephrine (mcg/min) (mcg/min)" = "pe mcg/min", 
                                               "Phenylephrine  STD 20 mg Sodium Chloride 0.9% 250 ml (mcg/min)" = "pe mcg/min",
                                               "Phenylephrine  MAX 100 mg Sodium Chloride 0.9% 250 ml (mcg/min)" = "pe mcg/min", 
                                               "Phenylephrine  STD 20 mg Sodium Chloride 0.9% 500 ml (mcg/min)" = "pe mcg/min", 
                                               "Volume (ml) Phenylephrine ()" = "pe mcg/min",
                                               "Phenylephrine (mg/hr)" = "pe mcg/min"))

pe <- pe %>% mutate(drugname = as.factor(drugname), 
                      drugname = recode_factor(drugname, "Phenylephrine (mcg/kg/min)" = "pe mcg/kg/min", 
                                               "Phenylephrine (mcg/kg/min) (mcg/kg/min)" = "pe mcg/kg/min",
                                               "Phenylephrine (mg/kg/min)" = "pe mcg/kg/min")) %>% 
  mutate(weight = ifelse(is.na(patientweight), weight, patientweight)) %>% 
  mutate(weight = as.numeric(weight), 
         drugrate = as.numeric(drugrate)) %>%
  mutate(drugrate = ifelse(drugname == "pe mcg/kg/min" & drugrate < 10, round(drugrate*weight,2), drugrate)) %>% 
  mutate(drugname = recode_factor(drugname, "pe mcg/kg/min" = "pe mcg/min"))

pe1 <- pe %>% group_by(patientunitstayid) %>% arrange(patientunitstayid, infusionoffset) %>% 
  mutate(drugratedifference = drugrate - lag(drugrate)) %>% # see below
  mutate(delete = ifelse(drugratedifference == "0" & drugrate == "0", "delete", "")) %>% # need to remove the rates that are charted 0 multiple times in a row
  filter(delete == "") %>% # removed all 0 charting after the first 0 
  mutate(offsetdifference =  infusionoffset - lag(infusionoffset)) %>% # obtains the difference in infusionoffset
  mutate(priorrate = lag(drugrate)) %>% # getting the prior rate 
  mutate(nextrate = lead(drugrate)) %>% # getting next infusion rate
  mutate(nextoffsetdifference = lead(offsetdifference)) %>% 
  mutate(infusionoffsetstop = lead(infusionoffset)) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset, unitdischargestatus), by = "patientunitstayid") %>% # getting ICU discharge time for those without 0 charted when stopped
  select(infusiondrugid, patientunitstayid, unitdischargeoffset, unitdischargestatus, infusionoffset, infusionoffsetstop, drugname, drugrate, priorrate, nextrate, offsetdifference, nextoffsetdifference) %>% # reorganizing and removing unused variables
  mutate(infusionoffsetstop = ifelse(is.na(infusionoffsetstop), unitdischargeoffset, infusionoffsetstop)) %>%
  mutate(unitdischargestatus = as.factor(unitdischargestatus)) %>% 
  mutate(status = ifelse(is.na(offsetdifference) | is.na(priorrate), "initial start", 
                         ifelse(drugrate == 0 & priorrate > 0 & offsetdifference < 1440 & !is.na(nextrate) & nextoffsetdifference >= 1440, "stopped", 
                                ifelse(drugrate == 0 & priorrate > 0 & is.na(nextrate), "final stop",
                                       ifelse(drugrate > 0 & !is.na(priorrate) & is.na(nextrate), "final stop",
                                              ifelse(drugrate > 0 & priorrate == 0 & offsetdifference >= 1440, "restarted", "")))))) %>% 
  mutate(status = as.factor(status)) %>% 
  group_by(patientunitstayid, status) %>% mutate(count = 1:n()) %>% ungroup() # sequentially counting each status to get 1st, 2nd, 3rd stop/restart


### Dopamine ####
dop <- eicuinfusion %>% filter(grepl("dopamine", drugname, ignore.case = TRUE)) %>% 
  filter(!is.na(drugrate))
# max(dop$drugrate) # 19200
# unique(dop$drugname) # 13 drug names to clean up


# Cleaning Dopamine
## Adding weight to ne df from admit/dc weight on patient df
# patient <- patient %>% mutate(weight = ifelse(is.na(admissionweight), dischargeweight,admissionweight))
# preferenced admission weight, if no admission weight, took discharge weight
dop <- dop %>% left_join(patient %>% dplyr::select(patientunitstayid, weight))

## Renaming categories of NE names
## Had to change all ml/hr, mg/hr, mcg/hr, (), to mcg/kg/min because looked like typical weight based doses
dop <- dop %>% mutate(drugname = as.factor(drugname), 
                    drugname = recode_factor(drugname, "Dopamine (mcg/kg/min)" = "dop mcg/kg/min", 
                                             "Dopamine ()" = "dop mcg/kg/min",
                                             "dopamine (mcg/kg/min)" = "dop mcg/kg/min",
                                             "Dopamine (mg/hr)" = "dop mcg/kg/min", 
                                             "Dopamine (nanograms/kg/min)" = "dop mcg/kg/min", 
                                             "Dopamine (ml/hr)" = "dop mcg/kg/min", 
                                             "Dopamine (mcg/min)" = "dop mcg/kg/min", 
                                             "Dopamine (mcg/kg/hr)" = "dop mcg/kg/min", 
                                             "DOPamine STD 400 mg Dextrose 5% 250 ml  Premix (mcg/kg/min)" = "dop mcg/kg/min",
                                             "DOPamine STD 15 mg Dextrose 5% 250 ml  Premix (mcg/kg/min)" = "dop mcg/kg/min", 
                                             "DOPamine MAX 800 mg Dextrose 5% 250 ml  Premix (mcg/kg/min)" = "dop mcg/kg/min", 
                                             "DOPamine STD 400 mg Dextrose 5% 500 ml  Premix (mcg/kg/min)" = "dop mcg/kg/min")) 

dop <- dop %>% mutate(weight = ifelse(is.na(patientweight), weight, patientweight)) %>% 
  mutate(weight = as.numeric(weight), 
         drugrate = as.numeric(drugrate)) %>%
  mutate(drugrate = ifelse(drugname != "dop mcg/kg/min", round(drugrate/(60*weight),2), drugrate)) %>% 
  mutate(drugname = recode_factor(drugname, "Dopamine (mcg/hr)" = "dop mcg/kg/min"))

dop1 <- dop %>% group_by(patientunitstayid) %>% arrange(patientunitstayid, infusionoffset) %>% 
  mutate(drugratedifference = drugrate - lag(drugrate)) %>% # see below
  mutate(delete = ifelse(drugratedifference == "0" & drugrate == "0", "delete", "")) %>% # need to remove the rates that are charted 0 multiple times in a row
  filter(delete == "") %>% # removed all 0 charting after the first 0 
  mutate(offsetdifference =  infusionoffset - lag(infusionoffset)) %>% # obtains the difference in infusionoffset
  mutate(priorrate = lag(drugrate)) %>% # getting the prior rate 
  mutate(nextrate = lead(drugrate)) %>% # getting next infusion rate
  mutate(nextoffsetdifference = lead(offsetdifference)) %>% 
  mutate(infusionoffsetstop = lead(infusionoffset)) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset, unitdischargestatus), by = "patientunitstayid") %>% # getting ICU discharge time for those without 0 charted when stopped
  select(infusiondrugid, patientunitstayid, unitdischargeoffset, unitdischargestatus, infusionoffset, infusionoffsetstop, drugname, drugrate, priorrate, nextrate, offsetdifference, nextoffsetdifference) %>% # reorganizing and removing unused variables
  mutate(infusionoffsetstop = ifelse(is.na(infusionoffsetstop), unitdischargeoffset, infusionoffsetstop)) %>%
  mutate(unitdischargestatus = as.factor(unitdischargestatus)) %>% 
  mutate(status = ifelse(is.na(offsetdifference) | is.na(priorrate), "initial start", 
                  ifelse(drugrate == 0 & priorrate > 0 & offsetdifference < 1440 & !is.na(nextrate) & nextoffsetdifference >= 1440, "stopped", 
                  ifelse(drugrate == 0 & priorrate > 0 & is.na(nextrate), "final stop",
                  ifelse(drugrate > 0 & !is.na(priorrate) & is.na(nextrate), "final stop",
                  ifelse(drugrate > 0 & priorrate == 0 & offsetdifference >= 1440, "restarted", "")))))) %>% 
  mutate(status = as.factor(status)) %>% 
  group_by(patientunitstayid, status) %>% mutate(count = 1:n()) %>% ungroup() # sequentially counting each status to get 1st, 2nd, 3rd stop/restart

### Vasopressor DF ####

# Combining all vasopressors into one df will all initiation and DC times

vasopressors <- ne1 %>% distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(firstnestart = infusionoffset) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart) %>% 
  full_join(ne1 %>% dplyr::filter(status == "stopped" | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(firstnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(firstnestop = ifelse(is.na(firstnestop), unitdischargeoffset, firstnestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop) %>% 
  full_join(ne1 %>% dplyr::filter(status == "restarted" & count == 1) # start of second start/stop
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("secondnestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart) %>% 
  full_join(ne1 %>% dplyr::filter((status == "stopped" & count ==2) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(secondnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(secondnestop = ifelse(is.na(secondnestop), unitdischargeoffset, secondnestop)) %>% 
  mutate(secondnestop = ifelse(is.na(secondnestart), NA, secondnestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop) %>% # end of second start/stop
  full_join(ne1 %>% dplyr::filter(status == "restarted" & count == 2) %>% # start of third start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("thirdnestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart) %>% 
  full_join(ne1 %>% dplyr::filter((status == "stopped" & count ==3) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(thirdnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(thirdnestop = ifelse(is.na(thirdnestop), unitdischargeoffset, thirdnestop)) %>% 
  mutate(thirdnestop = ifelse(is.na(thirdnestart), NA, thirdnestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop) %>% # end of third start/stop
  full_join(ne1 %>% dplyr::filter(status == "restarted" & count == 3) %>% # start of fourth start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("fourthnestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart) %>% 
  full_join(ne1 %>% dplyr::filter((status == "stopped" & count ==4) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(fourthnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(fourthnestop = ifelse(is.na(fourthnestop), unitdischargeoffset, fourthnestop)) %>% 
  mutate(fourthnestop = ifelse(is.na(fourthnestart), NA, fourthnestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop) %>% # end of fourth start/stop
  full_join(ne1 %>% dplyr::filter(status == "restarted" & count == 4) %>% # start of fifth start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("fifthnestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart) %>% 
  full_join(ne1 %>% dplyr::filter((status == "stopped" & count ==5) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(fifthnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(fifthhnestop = ifelse(is.na(fifthnestop), unitdischargeoffset, fifthnestop)) %>% 
  mutate(fifthnestop = ifelse(is.na(fifthnestart), NA, fifthnestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop) %>% # end of fifth start/stop
  full_join(ne1 %>% dplyr::filter(status == "restarted" & count == 5) %>% # start of sixth start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("sixthnestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart) %>% 
  full_join(ne1 %>% dplyr::filter((status == "stopped" & count ==6) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(sixthnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(sixthnestop = ifelse(is.na(sixthnestop), unitdischargeoffset, sixthnestop)) %>% 
  mutate(sixthnestop = ifelse(is.na(sixthnestart), NA, sixthnestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop) %>% # end of sixth start/stop
  full_join(ne1 %>% dplyr::filter(status == "restarted" & count == 6) %>% # start of 7th start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("seventhnestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart) %>% 
  full_join(ne1 %>% dplyr::filter((status == "stopped" & count ==7) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(seventhnestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(seventhnestop = ifelse(is.na(seventhnestop), unitdischargeoffset, seventhnestop)) %>% 
  mutate(seventhnestop = ifelse(is.na(seventhnestart), NA, seventhnestop)) %>% 
  select(patientunitstayid, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop) %>% # end of seventh start/stop
  full_join(avp1 %>% dplyr::filter(status == "initial start") 
            %>% dplyr::select(patientunitstayid, infusionoffset), by = "patientunitstayid") %>% # start of avp
  rename("firstavpstart" = "infusionoffset") %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset)) %>% # some avp pts never got NE and weren't in the list, had to add back in discharge time
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart) %>% 
  full_join(avp1 %>% dplyr::filter(status == "stopped" | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(firstavpstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(firstavpstop = ifelse(is.na(firstavpstop), unitdischargeoffset, firstavpstop)) %>% 
  mutate(firstavpstop = ifelse(is.na(firstavpstart), NA, firstavpstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart, firstavpstop) %>%
  full_join(avp1 %>% dplyr::filter(status == "restarted" & count == 1) # start of second start/stop
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("secondavpstart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart, firstavpstop, secondavpstart) %>% 
  full_join(avp1 %>% dplyr::filter((status == "stopped" & count ==2) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(secondavpstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(secondavpstop = ifelse(is.na(secondavpstop), unitdischargeoffset, secondavpstop)) %>% 
  mutate(secondavpstop = ifelse(is.na(secondavpstart), NA, secondavpstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart, firstavpstop, secondavpstart, secondavpstop) %>% # end of second start/stop
  full_join(avp1 %>% dplyr::filter(status == "restarted" & count == 2) %>% # start of third start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("thirdavpstart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart) %>% 
  full_join(avp1 %>% dplyr::filter((status == "stopped" & count ==3) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(thirdavpstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(thirdavpstop = ifelse(is.na(thirdavpstop), unitdischargeoffset, thirdavpstop)) %>% 
  mutate(thirdavpstop = ifelse(is.na(thirdavpstart), NA, thirdavpstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop) %>% # end of third start/stop
  full_join(avp1 %>% dplyr::filter(status == "restarted" & count == 3) %>% # start of fourth start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("fourthavpstart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart) %>% 
  full_join(avp1 %>% dplyr::filter((status == "stopped" & count ==4) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(fourthavpstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(fourthavpstop = ifelse(is.na(fourthavpstop), unitdischargeoffset, fourthavpstop)) %>% 
  mutate(fourthavpstop = ifelse(is.na(fourthavpstart), NA, fourthavpstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop) %>% # end of fourth start/stop
  full_join(avp1 %>% dplyr::filter(status == "restarted" & count == 4) %>% # start of fifth start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("fifthavpstart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop) %>% 
  full_join(avp1 %>% dplyr::filter((status == "stopped" & count ==5) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(fifthavpstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(fifthhavpstop = ifelse(is.na(fifthavpstop), unitdischargeoffset, fifthavpstop)) %>% 
  mutate(fifthavpstop = ifelse(is.na(fifthavpstart), NA, fifthavpstop)) %>% 
  select(patientunitstayid, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop) %>% # end of fifth start/stop 
  full_join(epi1 %>% dplyr::filter(status == "initial start") 
            %>% dplyr::select(patientunitstayid, infusionoffset), by = "patientunitstayid") %>% # start of epi
  rename("firstepistart" = "infusionoffset") %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset)) %>% # some epi pts never got NE and weren't in the list, had to add back in discharge time
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart) %>% 
  full_join(epi1 %>% dplyr::filter(status == "stopped" | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(firstepistop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(firstepistop = ifelse(is.na(firstepistop), unitdischargeoffset, firstepistop)) %>% 
  mutate(firstepistop = ifelse(is.na(firstepistart), NA, firstepistop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop) %>%
  full_join(epi1 %>% dplyr::filter(status == "restarted" & count == 1) # start of second start/stop
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("secondepistart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset,firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart) %>% 
  full_join(epi1 %>% dplyr::filter((status == "stopped" & count ==2) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(secondepistop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(secondepistop = ifelse(is.na(secondepistop), unitdischargeoffset, secondepistop)) %>% 
  mutate(secondepistop = ifelse(is.na(secondepistart), NA, secondepistop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop) %>% # end of second start/stop
  full_join(epi1 %>% dplyr::filter(status == "restarted" & count == 2) %>% # start of third start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("thirdepistart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart) %>% 
  full_join(epi1 %>% dplyr::filter((status == "stopped" & count ==3) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(thirdepistop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(thirdepistop = ifelse(is.na(thirdepistop), unitdischargeoffset, thirdepistop)) %>% 
  mutate(thirdepistop = ifelse(is.na(thirdepistart), NA, thirdepistop)) %>% 
  select(patientunitstayid, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop) %>% # end of third start/stop
  full_join(pe1 %>% dplyr::filter(status == "initial start") 
            %>% dplyr::select(patientunitstayid, infusionoffset), by = "patientunitstayid") %>% # start of pe
  rename("firstpestart" = "infusionoffset") %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset)) %>% # some pe pts never got NE and weren't in the list, had to add back in discharge time
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart) %>% 
  full_join(pe1 %>% dplyr::filter(status == "stopped" | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(firstpestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(firstpestop = ifelse(is.na(firstpestop), unitdischargeoffset, firstpestop)) %>% 
  mutate(firstpestop = ifelse(is.na(firstpestart), NA, firstpestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop) %>%
  full_join(pe1 %>% dplyr::filter(status == "restarted" & count == 1) # start of second start/stop
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("secondpestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset,firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart) %>% 
  full_join(pe1 %>% dplyr::filter((status == "stopped" & count ==2) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(secondpestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(secondpestop = ifelse(is.na(secondpestop), unitdischargeoffset, secondpestop)) %>% 
  mutate(secondpestop = ifelse(is.na(secondpestart), NA, secondpestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop) %>% # end of second start/stop
  full_join(pe1 %>% dplyr::filter(status == "restarted" & count == 2) %>% # start of third start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("thirdpestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart) %>% 
  full_join(pe1 %>% dplyr::filter((status == "stopped" & count ==3) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(thirdpestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(thirdpestop = ifelse(is.na(thirdpestop), unitdischargeoffset, thirdpestop)) %>% 
  mutate(thirdpestop = ifelse(is.na(thirdpestart), NA, thirdpestop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop) %>% # end of third start/stop
  full_join(pe1 %>% dplyr::filter(status == "restarted" & count == 3) %>% # start of fourth start/stop
            dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("fourthpestart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart) %>% 
  full_join(pe1 %>% dplyr::filter((status == "stopped" & count ==4) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(fourthpestop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(fourthpestop = ifelse(is.na(fourthpestop), unitdischargeoffset, fourthpestop)) %>% 
  mutate(fourthpestop = ifelse(is.na(fourthpestart), NA, fourthpestop)) %>% 
  select(patientunitstayid, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop) %>% # end of fourth start/stop
    full_join(dop1 %>% dplyr::filter(status == "initial start") 
          %>% dplyr::select(patientunitstayid, infusionoffset), by = "patientunitstayid") %>% # start of dop
  rename("firstdopstart" = "infusionoffset") %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, unitdischargeoffset)) %>% # some dopamine pts never got NE and weren't in the list, had to add back in discharge time
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, firstdopstart) %>% 
  full_join(dop1 %>% dplyr::filter(status == "stopped" | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(firstdopstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(firstdopstop = ifelse(is.na(firstdopstop), unitdischargeoffset, firstdopstop)) %>% 
  mutate(firstdopstop = ifelse(is.na(firstdopstart), NA, firstdopstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, thirdepistop, firstdopstart, firstdopstop) %>%
  full_join(dop1 %>% dplyr::filter(status == "restarted" & count == 1) # start of second start/stop
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("seconddopstart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset,firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, firstdopstart, firstdopstop, seconddopstart) %>% 
  full_join(dop1 %>% dplyr::filter((status == "stopped" & count ==2) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(seconddopstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(seconddopstop = ifelse(is.na(seconddopstop), unitdischargeoffset, seconddopstop)) %>% 
  mutate(seconddopstop = ifelse(is.na(seconddopstart), NA, seconddopstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, firstdopstart, firstdopstop, seconddopstart, seconddopstop) %>% # end of second start/stop
  full_join(dop1 %>% dplyr::filter(status == "restarted" & count == 2) %>% # start of third start/stop
              dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  rename("thirddopstart" = "infusionoffset") %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, firstdopstart, firstdopstop, seconddopstart, seconddopstop, thirddopstart) %>% 
  full_join(dop1 %>% dplyr::filter((status == "stopped" & count ==3) | status == "final stop") 
            %>% dplyr::select(patientunitstayid, infusionoffset, infusionoffsetstop, drugrate, status, count), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate(thirddopstop = ifelse(drugrate == 0, infusionoffset, ifelse(drugrate > 0, infusionoffsetstop, unitdischargeoffset))) %>% 
  mutate(thirddopstop = ifelse(is.na(thirddopstop), unitdischargeoffset, thirddopstop)) %>% 
  mutate(thirddopstop = ifelse(is.na(thirddopstart), NA, thirddopstop)) %>% 
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, firstdopstart, firstdopstop, seconddopstart, seconddopstop, thirddopstart, thirddopstop) %>%
  group_by(patientunitstayid) %>% 
  mutate(vasoactivestart = min(c(firstnestart, firstepistart, firstpestart, firstavpstart, firstdopstart), na.rm = TRUE)) %>%  
  mutate(vasoactivestop = max(c(firstavpstop, firstnestop, firstepistop, firstpestop, firstdopstop), na.rm = TRUE)) %>% 
  select(patientunitstayid, unitdischargeoffset, vasoactivestart, vasoactivestop, firstavpstart, firstavpstop, secondavpstart, secondavpstop, thirdavpstart, thirdavpstop, fourthavpstart, fourthavpstop, fifthavpstart, fifthavpstop, firstnestart, firstnestop, secondnestart, secondnestop, thirdnestart, thirdnestop, fourthnestart, fourthnestop, fifthnestart, fifthnestop, sixthnestart, sixthnestop, seventhnestart, seventhnestop, firstepistart, firstepistop, secondepistart, secondepistop, thirdepistart, thirdepistop, firstpestart, firstpestop, secondpestart, secondpestop, thirdpestart, thirdpestop, fourthpestart, fourthpestop, firstdopstart, firstdopstop, seconddopstart, seconddopstop, thirddopstart, thirddopstop)

write.csv(vasopressors, file = "eICU Data/eicu_vasopressors.csv", row.names = FALSE)


# Rearranging vasopressors to be columns (have to rerun the below if re-creating vasopressor vertical from scratch)
# Vasopressin
avp0 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(patientunitstayid, firstavpstart, firstavpstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "firstavpstart", "discontinuationoffset" = "firstavpstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "vasopressin", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "1", "")) %>% 
  filter(!is.na(initiationoffset))
avp2 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(secondavpstart)) %>% 
              dplyr::select(patientunitstayid, secondavpstart, secondavpstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "secondavpstart", "discontinuationoffset" = "secondavpstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "vasopressin", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "2", "")) %>% 
  filter(!is.na(initiationoffset))
avp3 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(thirdavpstart)) %>% 
              dplyr::select(patientunitstayid, thirdavpstart, thirdavpstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "thirdavpstart", "discontinuationoffset" = "thirdavpstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "vasopressin", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "3", "")) %>% 
  filter(!is.na(initiationoffset))
avp4 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(fourthavpstart)) %>% 
              dplyr::select(patientunitstayid, fourthavpstart, fourthavpstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "fourthavpstart", "discontinuationoffset" = "fourthavpstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "vasopressin", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "4", "")) %>% 
  filter(!is.na(initiationoffset))
avp5 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(fifthavpstart)) %>% 
              dplyr::select(patientunitstayid, fifthavpstart, fifthavpstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "fifthavpstart", "discontinuationoffset" = "fifthavpstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "vasopressin", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "5", "")) %>% 
  filter(!is.na(initiationoffset))
avpvertical <- rbind(avp0, avp2, avp3, avp4, avp5) %>% arrange(patientunitstayid, initiationoffset)
rm(avp0, avp2, avp3, avp4, avp5)
# Norepinephrine
ne0 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstnestart)) %>% 
              dplyr::select(patientunitstayid, firstnestart, firstnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "firstnestart", "discontinuationoffset" = "firstnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "1", "")) %>% 
  filter(!is.na(initiationoffset))
ne2 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(secondnestart)) %>% 
              dplyr::select(patientunitstayid, secondnestart, secondnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "secondnestart", "discontinuationoffset" = "secondnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "2", "")) %>% 
  filter(!is.na(initiationoffset))
ne3 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(thirdnestart)) %>% 
              dplyr::select(patientunitstayid, thirdnestart, thirdnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "thirdnestart", "discontinuationoffset" = "thirdnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "3", "")) %>% 
  filter(!is.na(initiationoffset))
ne4 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(fourthnestart)) %>% 
              dplyr::select(patientunitstayid, fourthnestart, fourthnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "fourthnestart", "discontinuationoffset" = "fourthnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "4", "")) %>% 
  filter(!is.na(initiationoffset))
ne5 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(fifthnestart)) %>% 
              dplyr::select(patientunitstayid, fifthnestart, fifthnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "fifthnestart", "discontinuationoffset" = "fifthnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "5", "")) %>% 
  filter(!is.na(initiationoffset))
ne6 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(sixthnestart)) %>% 
              dplyr::select(patientunitstayid, sixthnestart, sixthnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "sixthnestart", "discontinuationoffset" = "sixthnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "6", "")) %>% 
  filter(!is.na(initiationoffset))
ne7 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(seventhnestart)) %>% 
              dplyr::select(patientunitstayid, seventhnestart, seventhnestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "seventhnestart", "discontinuationoffset" = "seventhnestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "norepinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "7", "")) %>% 
  filter(!is.na(initiationoffset))
nevertical <- rbind(ne0, ne2, ne3, ne4, ne5, ne6, ne7) %>% arrange(patientunitstayid, initiationoffset)
rm(ne0, ne2, ne3, ne4, ne5, ne6, ne7)
# Epinephrine
epi0 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstepistart)) %>% 
              dplyr::select(patientunitstayid, firstepistart, firstepistop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "firstepistart", "discontinuationoffset" = "firstepistop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "epinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "1", "")) %>% 
  filter(!is.na(initiationoffset))
epi2 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(secondepistart)) %>% 
              dplyr::select(patientunitstayid, secondepistart, secondepistop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "secondepistart", "discontinuationoffset" = "secondepistop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "epinephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "2", "")) %>% 
  filter(!is.na(initiationoffset))
epivertical <- rbind(epi0, epi2) %>% arrange(patientunitstayid, initiationoffset)
rm(epi0, epi2)
# Phenylephrine
pe0 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstpestart)) %>% 
              dplyr::select(patientunitstayid, firstpestart, firstpestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "firstpestart", "discontinuationoffset" = "firstpestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "phenylephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "1", "")) %>% 
  filter(!is.na(initiationoffset))
pe2 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(secondpestart)) %>% 
              dplyr::select(patientunitstayid, secondpestart, secondpestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "secondpestart", "discontinuationoffset" = "secondpestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "phenylephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "2", "")) %>% 
  filter(!is.na(initiationoffset))
pe3 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(thirdpestart)) %>% 
              dplyr::select(patientunitstayid, thirdpestart, thirdpestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "thirdpestart", "discontinuationoffset" = "thirdpestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "phenylephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "3", "")) %>% 
  filter(!is.na(initiationoffset))
pe4 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(fourthpestart)) %>% 
              dplyr::select(patientunitstayid, fourthpestart, fourthpestop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "fourthpestart", "discontinuationoffset" = "fourthpestop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "phenylephrine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "4", "")) %>% 
  filter(!is.na(initiationoffset))
pevertical <- rbind(pe0, pe2, pe3, pe4) %>% arrange(patientunitstayid, initiationoffset)
rm(pe0, pe2, pe3, pe4)
# Dopamine
dop0 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstdopstart)) %>% 
              dplyr::select(patientunitstayid, firstdopstart, firstdopstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "firstdopstart", "discontinuationoffset" = "firstdopstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "dopamine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "1", "")) %>% 
  filter(!is.na(initiationoffset))
dop2 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(seconddopstart)) %>% 
              dplyr::select(patientunitstayid, seconddopstart, seconddopstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "seconddopstart", "discontinuationoffset" = "seconddopstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "dopamine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "2", "")) %>% 
  filter(!is.na(initiationoffset))
dop3 <- patient %>% select(patientunitstayid, unitdischargeoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(thirddopstart)) %>% 
              dplyr::select(patientunitstayid, thirddopstart, thirddopstop), by = "patientunitstayid") %>% 
  rename("initiationoffset" = "thirddopstart", "discontinuationoffset" = "thirddopstop") %>% 
  mutate(vasopressor = ifelse(!is.na(initiationoffset), "dopamine", ""),
         regimennumber = ifelse(!is.na(initiationoffset), "3", "")) %>% 
  filter(!is.na(initiationoffset))
dopvertical <- rbind(dop0, dop2, dop3) %>% arrange(patientunitstayid, initiationoffset)
rm(dop0, dop2, dop3)

vasopressorsvertical <- rbind(avpvertical, nevertical, epivertical, pevertical, dopvertical) %>% 
  arrange(patientunitstayid, initiationoffset) %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>%
              dplyr::select(patientunitstayid, firstavpstart, firstavpstop), by = "patientunitstayid") %>%
  select(patientunitstayid, unitdischargeoffset, firstavpstart, firstavpstop, initiationoffset, discontinuationoffset, vasopressor, regimennumber) %>% 
  left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  select(patientunitstayid, unitdischargeoffset, vasoactivestart, vasoactivestop, firstavpstart, firstavpstop, initiationoffset, discontinuationoffset, vasopressor, regimennumber) %>% 
  mutate(vasopressinuse = ifelse((firstavpstart >= initiationoffset) & (firstavpstart < discontinuationoffset), "yes", NA))

write_csv(vasopressorsvertical, file = "~/eICU Data/vasopressorsvertical.csv")

rm(avpvertical, nevertical, epivertical, pevertical, dopvertical)

### Dobutamine ####
dobut <- eicuinfusion %>% filter(grepl("dobutamine", drugname, ignore.case = TRUE)) %>% 
  filter(!is.na(drugrate))

### Milrinone ####
milrinone <- eicuinfusion %>% filter(grepl("milrinone", drugname, ignore.case = TRUE)) %>% 
  filter(!is.na(drugrate))

### Datapoints ####
#### Norepinephrine Equivalent Dose ####
# Norepinephrine
ne2 <- ne1 %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(patientunitstayid, firstavpstart, firstavpstop), by = "patientunitstayid") %>% 
  mutate(neqdose = ifelse((firstavpstart >= infusionoffset) & (firstavpstart < infusionoffsetstop), drugrate, NA), 
         vasopressor = "norepinephrine") %>% 
  mutate(neqdose6hr = ifelse(((firstavpstart + 360) >= infusionoffset) & ((firstavpstart + 360) < infusionoffsetstop), drugrate, NA), 
         vasopressor = "norepinephrine")
# Epinephrine
epi2 <- epi1 %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(patientunitstayid, firstavpstart, firstavpstop), by = "patientunitstayid") %>% 
  mutate(neqdose = ifelse((firstavpstart >= infusionoffset) & (firstavpstart < infusionoffsetstop), drugrate, NA), 
         vasopressor = "epinephrine") %>% 
  mutate(neqdose6hr = ifelse(((firstavpstart + 360) >= infusionoffset) & ((firstavpstart + 360) < infusionoffsetstop), drugrate, NA), 
         vasopressor = "epinephrine")
# Phenylephrine
pe2 <- pe1 %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(patientunitstayid, firstavpstart, firstavpstop), by = "patientunitstayid") %>% 
  mutate(neqdose = ifelse((firstavpstart >= infusionoffset) & (firstavpstart < infusionoffsetstop), (drugrate/10), NA), 
         vasopressor = "phenylephrine") %>% 
  mutate(neqdose6hr = ifelse(((firstavpstart + 360) >= infusionoffset) & ((firstavpstart + 360) < infusionoffsetstop), drugrate, NA), 
         vasopressor = "phenylephrine")
# Dopamine
dop2 <- dop1 %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(patientunitstayid, firstavpstart, firstavpstop), by = "patientunitstayid") %>% 
  mutate(neqdose = ifelse((firstavpstart >= infusionoffset) & (firstavpstart < infusionoffsetstop), (drugrate/2), NA), 
         vasopressor = "dopamine") %>% 
  mutate(neqdose6hr = ifelse(((firstavpstart + 360) >= infusionoffset) & ((firstavpstart + 360) < infusionoffsetstop), drugrate, NA), 
         vasopressor = "dopamine")


# Adding all to vasopressorsvertical
vasopressorsverticalneq <- vasopressorsvertical %>%
  left_join(ne2 %>% dplyr::select(patientunitstayid, neqdose, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("NEneqdose" = "neqdose") %>%
  left_join(epi2 %>% dplyr::select(patientunitstayid, neqdose, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("EPIneqdose" = "neqdose") %>%
  left_join(pe2 %>% dplyr::select(patientunitstayid, neqdose, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("PEneqdose" = "neqdose") %>%
  left_join(dop2 %>% dplyr::select(patientunitstayid, neqdose, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("DOPneqdose" = "neqdose") %>% 
  left_join(ne2 %>% dplyr::select(patientunitstayid, neqdose6hr, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose6hr)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("NEneqdose6hr" = "neqdose6hr") %>%
  left_join(epi2 %>% dplyr::select(patientunitstayid, neqdose6hr, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose6hr)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("EPIneqdose6hr" = "neqdose6hr") %>%
  left_join(pe2 %>% dplyr::select(patientunitstayid, neqdose6hr, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose6hr)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("PEneqdose6hr" = "neqdose6hr") %>%
  left_join(dop2 %>% dplyr::select(patientunitstayid, neqdose6hr, vasopressor) %>% 
              dplyr::filter(!is.na(neqdose6hr)), by = c("patientunitstayid", "vasopressor")) %>%
  rename("DOPneqdose6hr" = "neqdose6hr") %>% 
  filter(vasopressor != "vasopressin") %>%
  mutate(neqdose = rowSums(across(c(NEneqdose, EPIneqdose, PEneqdose, DOPneqdose)), na.rm = TRUE)) %>% 
  mutate(neqdose = ifelse(vasopressinuse == "yes", neqdose, NA)) %>% 
  select(-NEneqdose, -EPIneqdose, -PEneqdose, -DOPneqdose) %>%
  group_by(patientunitstayid) %>% mutate(totalneqdose = sum(neqdose, na.rm = TRUE)) %>% ungroup() %>% # totalneqdose = NEQ dose at AVP initiation
  mutate(neqdose6hr = rowSums(across(c(NEneqdose6hr, EPIneqdose6hr, PEneqdose6hr, DOPneqdose6hr)), na.rm = TRUE)) %>% 
  mutate(neqdose6hr = ifelse(vasopressinuse == "yes", neqdose6hr, NA)) %>% 
  select(-NEneqdose6hr, -EPIneqdose6hr, -PEneqdose6hr, -DOPneqdose6hr) %>%
  group_by(patientunitstayid) %>% mutate(totalneqdose6hr = sum(neqdose6hr, na.rm = TRUE)) # totalneqdose6hr = NEQ dose 6hr after AVP initiation (for HDS)

write_csv(vasopressorsverticalneq, file = "~/eICU Data/vasopressorsverticalneq.csv")

rm(ne2, epi2, pe2, dop2)

#### Vasopressor use during shock course ####
neuse <- ne1 %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(neuse = ifelse((infusionoffset >= vasoactivestart) & (infusionoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(neuse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# epi
epiuse <- epi1 %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(epiuse = ifelse((infusionoffset >= vasoactivestart) & (infusionoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(epiuse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# pe
peuse <- pe1 %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(peuse = ifelse((infusionoffset >= vasoactivestart) & (infusionoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(peuse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# dopamine
dopuse <- dop1 %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(dopuse = ifelse((infusionoffset >= vasoactivestart) & (infusionoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(dopuse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# dobutamine
dobutuse <- dobut %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(dobutuse = ifelse((infusionoffset >= vasoactivestart) & (infusionoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(dobutuse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# milrinone
miluse <- milrinone %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(miluse = ifelse((infusionoffset >= vasoactivestart) & (infusionoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(miluse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# hydrocortisone use
hydrocort <- meds %>% filter(grepl("hydrocortisone", drugname, ignore.case = TRUE)) %>% 
  left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  mutate(hydrocortuse = ifelse((drugstartoffset >= vasoactivestart) & (drugstartoffset <= vasoactivestop), "Yes", NA)) %>% 
  filter(hydrocortuse == "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# unique(hydrocort$routeadmin)

vasopressoruse <- neuse %>% full_join(epiuse %>% dplyr::select(patientunitstayid, epiuse), by = "patientunitstayid") %>% 
  full_join(peuse %>% dplyr::select(patientunitstayid, peuse), by = "patientunitstayid") %>% 
  full_join(dopuse %>% dplyr::select(patientunitstayid, dopuse), by = "patientunitstayid") %>%
  full_join(dobutuse %>% dplyr::select(patientunitstayid, dobutuse), by = "patientunitstayid") %>% 
  full_join(miluse %>% dplyr::select(patientunitstayid, miluse), by = "patientunitstayid") %>% 
  full_join(hydrocort %>% dplyr::select(patientunitstayid, hydrocortuse), by = "patientunitstayid")
rm(neuse, epiuse, peuse, dopuse, dobutuse, miluse)

write_csv(vasopressoruse, file = "~/eICU Data/vasopressoruse.csv")

# SCREENING CRITERIA ####
### Sepsis 3 ####
#### MAP 65 - Sepsis-3 ####

imap <- vitalperiodic %>% filter(!is.na(systemicmean)) %>%
  mutate(source = "invasive") %>% 
  dplyr::select(patientunitstayid, observationoffset, systemicsystolic, systemicdiastolic,
                systemicmean, source) %>%
  rename(sbp = systemicsystolic, dbp = systemicdiastolic, map = systemicmean)
  # creating df of just invasive blood pressures

nimap <- vitalaperiodic %>% filter(!is.na(noninvasivemean)) %>%
  mutate(source = "noninvasive") %>% 
  dplyr::select(patientunitstayid, observationoffset, noninvasivesystolic, noninvasivediastolic, 
                noninvasivemean, source) %>%
  rename(sbp = noninvasivesystolic, dbp = noninvasivediastolic, map = noninvasivemean)
  # creating df of noninvasive blood pressures  

map <- rbind(imap, nimap) # combining invasive and non-invasive
# rm(imap, nimap)

map <- map %>% arrange(patientunitstayid, observationoffset) %>% 
  full_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart), by = "patientunitstayid") %>%
  filter(!is.na(vasoactivestart)) %>% 
  mutate(map65 = ifelse(observationoffset >= (vasoactivestart - 180) & 
                          observationoffset <= (vasoactivestart + 180) & 
                          map < 65, "MAP < 65", ""))
  # labeling times when MAP < 65 within -/+ 180 min of vasoactive initiation

map65 <- map %>% filter(map65 == "MAP < 65") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  select(patientunitstayid, map65)
  # selecting unique patientunitstayid with MAP < 65 within 180 min of vasoactive initiation   

#### Lactate > 2.0 - Sepsis-3 ####
# Get lactate in 24 hours surrounding vasopressor utilization

lactate <- lab %>% filter(grepl("lactate", labname, ignore.case = TRUE)) %>% 
  full_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>%
  filter(!is.na(vasoactivestart)) %>% 
  mutate(lactate2 = if_else(labresultoffset >= (vasoactivestart - 1440) & 
                          labresultoffset <= (vasoactivestart + 1440) & 
                          labresult >= 2.0, "Lactate >= 2.0", "")) %>%
  mutate(lactate2at72hr = if_else(labresultoffset >= (vasoactivestart - 1440) & 
                              labresultoffset <= (vasoactivestart + 4320) & 
                              labresult >= 2.0, "Lactate >= 2.0", ""))

lactate2 <- lactate %>% filter(lactate2 == "Lactate >= 2.0") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  select(patientunitstayid, labresult, lactate2)

lactate2at72hr <- lactate %>% filter(lactate2at72hr == "Lactate >= 2.0") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  select(patientunitstayid, labresult, lactate2at72hr)
  # isolating list of just patients with lactate >= 2.0

#### Antibiotic Use - Sepsis-3 ####

qadabx <- meds %>% filter(grepl("amikacin|ampicillin|azithromycin|aztreonam|cefamandole|cefazolin|cefepime|cefmetazole|cefonicid|cefoperazone|cefotaxime|cefotetan|cefotetan|cefoxitin|ceftaroline|ceftazidime|ceftizoxime|ceftolozane|ceftriaxone|cefuroxime|cephalothin|cephapirin|chloramphenicol|ciprofloxacin|clindamycin|cloxacillin|colistin|dalbavancin|daptomycin|doripenem|doxycycline|ertapenem|gatifloxacin|gentamicin|mipenem|kanamycin|levofloxacin|lincomycin|linezolid|meropenem|methicillin|metronidazole|mezlocillin|minocycline|moxifloxacin|minocycline|moxifloxacin|nafcillin|oritavancin|oxacillin|penicillin|piperacillin|polymyxin|quinupristin|streptomycin|tedizolid|telavancin|ticarcillin|tigecycline|tobramycin|trimethoprim|vancomycin|amoxicillin|cefaclor|cefadroxil|cefdinir|cefditoren|cefixime|cefpodoxime|cefprozil|ceftibuten|cefuroxime|cephalexin|cephradine|cefixime|cefpodoxime|cefprozil|ceftibuten|cefuroxime|cephalexin|cephradine|cinoxacin|clarithromycin|dicloxacillin|fidaxomicin|fosfomycin|gatifloxacin|lincomycin|minocycline|moxifloxacin|nitrofurantoin|norfloxacin|ofloxacin|penicillin|pivampicillin|rifampin|sulfadiazine|sulfamethoxazole|sulfisoxazole|tedizolid|telithromycin|tetracycline|amphotericin|anidulafungin|caspofungin|fluconazole|isavucon|itraconazole|micafungin|posaconazole|voriconazole|acyclovir|ganciclovir|cidofovir|foscarnet|peramivir|oseltamivir", 
                                drugname, ignore.case = TRUE)) %>% 
  arrange(patientunitstayid, drugstartoffset) %>% 
  mutate(duration = (drugstopoffset - drugstartoffset)/60) %>% 
  mutate(drugstartoffsetdays = floor(drugstartoffset/1440)) %>% 
  mutate(drugname = as.factor(drugname)) %>% 
  mutate(abxname = recode_factor(drugname,  "PIPERACILLIN SOD-TAZOBACTAM SO 3-0.375 G IV SOLR" = "piperacillin tazobactam",
                                 "VANCOMYCIN 1.25 GM IN NS 250 ML IVPB (REPACKAGE)" = "vancomycin", 
                                 "LEVOFLOXACIN IN D5W 750 MG/150ML IV SOLN" = "levofloxacin", 
                                 "VANCOMYCIN 1.5 GM IN NS 250 ML IVPB (REPACKAGE)" = "vancomycin", 
                                 "LEVOFLOXACIN 500 MG PO TABS" = "levofloxacin", 
                                 "100 ML  -  METRONIDAZOLE IN NACL 5-0.79 MG/ML-% IV SOLN" = "metronidazole",
                                 "1 EACH VIAL : CEFEPIME HCL 1 GM IJ SOLR" = "cefepime", "CEFEPIME HCL 2 G IJ SOLR" = "cefepime", 
                                 "LEVOFLOXACIN IN D5W 500 MG/100ML IV SOLN" = "levofloxacin", 
                                 "CEFAZOLIN 2 GM IN NS 100 ML IVPB (REPACKAGE)" = "cefazolin", "VANCOMYCIN HCL 1000 MG IV SOLR" = "vancomycin",
                                 "METRONIDAZOLE 500 MG PO TABS" = "metronidazole", "VANCOMYCIN HCL 10 G IV SOLR" = "vancomycin", 
                                 "MEROPENEM 500 MG IV SOLR" = "meropenem", "CEFTRIAXONE SODIUM 1 G IJ SOLR" = "ceftriaxone", 
                                 "LEVOFLOXACIN 750 MG PO TABS" = "levofloxacin", "AZITHROMYCIN 500 MG IV SOLR" = "azithromycin", 
                                 "1 EACH VIAL : CEFTRIAXONE SODIUM 1 G IJ SOLR" = "ceftriaxone", 
                                 "CIPROFLOXACIN IN D5W 400 MG/200ML IV SOLN" = "ciprofloxacin", 
                                 "MEROPENEM 1 G IV SOLR" = "meropenem", "VANCOMYCIN HCL IN DEXTROSE 1 GM/200ML IV SOLN" = "vancomycin",
                                 "VANCOMYCIN INJ 1,000 MG VIAL." = "vancomycin", "metroNIDAZOLE" = "metronidazole", 
                                 "AZITHROMYCIN" = "azithromycin", "VANCOMYCIN" = "vancomycin", "LEVOFLOXACIN" = "levofloxacin", 
                                 "VANCOMYCIN HCL" = "vancomycin", "ceFAZolin 2 Gm in NS 100 mL IVPB" = "azithromycin", 
                                 "LEVOFLOXACIN 500mg in D5W 100mL RT" = "levofloxacin", "LEVOFLOXACIN 750 mg in D5W 150mL" = "levofloxacin", 
                                 "cefTRIAXone 1 GRAM in NS 50 mL IVPB" = "ceftriaxone", "VANCOmycin 1.5 GM in NS 500ML IVPB" = "vancomycin",
                                 "AZITHromycin 500 mg in NS 250 mL IV" = "azithromycin", "VANCOmycin 750 MG in NS 250 mL IVPB" = "vancomycin",
                                 "VANCOmycin 1 GM in NS 250 mL IVPB" = "vancomycin", "VANCOmycin 1.25 GM in NS 500ML IVPB" = "vancomycin", 
                                 "metroNIDAZOLE 500 MG in 100ML RTU-PB" = "metronidazole", "ceFAZolin 1 g powder for Inj" = "cefazolin", 
                                 "cefTRIAXone" = "ceftriaxone", "piperacillin-tazobactam" = "piperacillin tazobactam", 
                                 "ceFAZolin" = "cefazolin", "CEFEPIME" = "cefepime", "vancomycin in D5W (VANCOCIN) ivpb 1 g" = "vancomycin",
                                 "ceFAZolin (ANCEF) in Dextrose ivpb 2 g" = "cefazolin", "cefepime in D5W (MAXIPIME) ivpb 1 g" = "cefepime", 
                                 "cefTRIAXone in D5W (ROCEPHIN) ivpb 1 g" = "ceftriaxone", "vancomycin hcl 1000 mg iv solr" = "vancomycin",
                                 "PIPERACILLIN-TAZOBACTAM 3.375 G IVPB" = "piperacillin tazobactam", 
                                 "20 ML SYRINGE : CEFAZOLIN 2 G IN STERILE WATER 20 ML SYRINGE IV" = "cefazolin", 
                                 "150 ML FLEX CONT : LEVOFLOXACIN IN D5W 5 MG/ML IV SOLN" = "levofloxacin", 
                                 "PIPERACILLIN/TAZOBACTAM" = "piperacillin tazobactam", "VANCOMYCIN HCL 1 GM VIAL" = "vancomycin", 
                                 "PIPERACILLIN/TAZOBACTAM SOD 3.375 GM  VIAL" = "piperacillin tazobactam", 
                                 "150 ML : LEVOFLOXACIN IN D5W 750 MG/150ML IV SOLN" = "levofloxacin", 
                                 "PIPERACILLIN-TAZOBACTAM 3.375 G MINI-BAG PLUS" = "piperacillin tazobactam", 
                                 "VANCOMYCIN 1 G MINI-BAG PLUS (VIAL MATE)" = "vancomycin", 
                                 "VANCOMYCIN 1,000 MG IV SOLUTION : 1 EACH VIAL" = "vancomycin", "VANCOMYCIN CONSULT TO PHARMACY" = "vancomycin", 
                                 "CEFAZOLIN 2GM SYR" = "cefazolin", "METRONIDAZOLE 500 MG/100 ML" = "metronidazole", 
                                 "VANCOMYCIN 1.25 G/250 ML NS" = "vancomycin", "CEFAZOLIN 2 G/100 ML NS" = "cefazolin", 
                                 "VANCOMYCIN 1 G/200 ML D5W" = "vancomycin", "VANCOMYCIN 1.5 G/500 ML NS" = "vancomycin"))
# creating df of antibiotics that meet ASE definition but there aren't many in eICU period..... 
# unique(qadabx$drugname) # 70 unique drug names
# unique(qadabx$abxname) # 12 unique antibiotic names, I feel like this is very low numbers...... 

# nonabxmeds <- meds %>% anti_join(qadabx, by = "patientunitstayid") # looking at med list to see what wasn't in qadabx
# unique(nonabxmeds$drugname) # scan of the first 1000 medication names doens't look like any antibiotics were missing in my code
# rm(nonabxmeds)

qadabx1 <- qadabx %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  select(patientunitstayid, drugstartoffset, drugname, dosage, routeadmin, frequency, drugstopoffset, duration, drugstartoffsetdays, 
         abxname, vasoactivestart, vasoactivestop) %>% 
  mutate(vasoactiveinitiationoffsetdays = floor(vasoactivestart/1440)) %>% 
  mutate(vasoactivedcoffsetdays = floor(vasoactivestop/1440)) %>% 
  filter(!is.na(vasoactivestart)) %>% 
  arrange(patientunitstayid, drugstartoffset) %>% 
  mutate(abxduringvasoactive = if_else(drugstartoffsetdays >= (vasoactiveinitiationoffsetdays - 1) & 
                                         drugstartoffsetdays <= (vasoactiveinitiationoffsetdays + 1), "Yes", "No")) %>% 
  select(patientunitstayid, abxduringvasoactive) %>%
  filter(abxduringvasoactive == "Yes") %>%
  distinct(patientunitstayid, abxduringvasoactive)
  # isolate all the patients who received vasopressors and give yes if abx were +/- 1 day from vasoactive agent initiation 

qadabx3 <- qadabx %>% left_join(vasopressors %>% dplyr::select(patientunitstayid, vasoactivestart, vasoactivestop), by = "patientunitstayid") %>% 
  select(patientunitstayid, drugstartoffset, drugname, dosage, routeadmin, frequency, drugstopoffset, duration, drugstartoffsetdays, 
         abxname, vasoactivestart, vasoactivestop) %>% 
  mutate(vasoactiveinitiationoffsetdays = floor(vasoactivestart/1440)) %>% 
  mutate(vasoactivedcoffsetdays = floor(vasoactivestop/1440)) %>% 
  filter(!is.na(vasoactivestart)) %>% 
  arrange(patientunitstayid, drugstartoffset) %>% 
  mutate(abxduringvasoactive72h = if_else(drugstartoffsetdays >= (vasoactiveinitiationoffsetdays - 1) & 
                                         drugstartoffsetdays <= (vasoactiveinitiationoffsetdays + 3), "Yes", "No")) %>% 
  select(patientunitstayid, abxduringvasoactive72h) %>%
  filter(abxduringvasoactive72h == "Yes") %>%
  distinct(patientunitstayid, abxduringvasoactive72h)
# isolate all the patients who received vasopressors and give yes if abx were +/- 1 day from vasoactive agent initiation 

#### Final Sepsis-3 DF ####

sepsis3 <- vasopressors %>% 
  select(patientunitstayid, unitdischargeoffset, vasoactivestart, vasoactivestop, firstavpstart, firstavpstop) %>% 
  left_join(lactate2, by = "patientunitstayid") %>% 
  left_join(lactate2at72hr %>% dplyr::select(patientunitstayid, lactate2at72hr), by = "patientunitstayid") %>% 
  left_join(qadabx1, by = "patientunitstayid") %>% 
  left_join(qadabx3, by = "patientunitstayid") %>% 
  left_join(map65, by = "patientunitstayid") %>% 
  mutate(sepsis3 = if_else(map65 == "MAP < 65" & lactate2 == "Lactate >= 2.0" & abxduringvasoactive == "Yes", "Yes", "No")) %>% 
  mutate(sepsis3bosch = if_else(map65 == "MAP < 65" & lactate2at72hr == "Lactate >= 2.0" & abxduringvasoactive72h == "Yes", "Yes", "No")) %>%
  mutate(sepsis3nomap = if_else(lactate2 == "Lactate >= 2.0" & abxduringvasoactive == "Yes", "Yes", "No")) %>%
  mutate(sepsis3nomapbosch = if_else(lactate2at72hr == "Lactate >= 2.0" & abxduringvasoactive72h == "Yes", "Yes", "No"))
rm(lactate2, lactate2at72hr, qadabx1, map65, imap, nimap, qadabx, qadabx3)
# Total number should be 18166
# just patients who received vasoactive agents
# need to rerun lactate2, qadabx1, map65 to replicate this df
write.csv(sepsis3, file = "eICU Data/eicu_sepsis3screening.csv", row.names = FALSE)


### Sepsis Coding ####
# no ICD numeric coding is used, all text, need to look for lines that are "septic shock" "severe sepsis" and "sepsis" and "shock on same line
#### Admission Diagnosis ####
# df <- diagnosis %>% filter(grepl("sepsis", diagnosisstring, ignore.case = TRUE)) # 115375 results
# df <- diagnosis %>% filter(grepl("septic shock", diagnosisstring, ignore.case = TRUE)) # 35132 results
# df <- diagnosis %>% filter(grepl("severe sepsis", diagnosisstring, ignore.case = TRUE)) # 0 results

admissiondxsepsis <- admissiondx %>% filter(grepl("sepsis|septic shock", admitdxpath, ignore.case = TRUE)) %>% 
  mutate(admitsepsisdx = "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)
# no patients admitted with "septic shock" or "severe sepsis" 13 with "septic" but all septic arthritis

admissiondxsepticshock <- admissiondx %>% filter(grepl("septic shock", admitdxpath, ignore.case = TRUE)) %>% 
  mutate(admitsepticshockdx = "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)

#### Hospital Diagnosis ####
# only in patients who received vasopressors
diagnosissepsis <- diagnosis %>% filter(grepl("sepsis|septic shock", diagnosisstring, ignore.case = TRUE)) %>% 
  mutate(sepsisdx = "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)

diagnosissepticshock <- diagnosis %>% filter(grepl("septic shock", diagnosisstring, ignore.case = TRUE)) %>% 
  mutate(septicshockdx = "Yes") %>% distinct(patientunitstayid, .keep_all = TRUE)

### Sepsis Coding with Angus Criteria ####
# Angus criteria with sepsis-3 as the sensitivity analysis (with or without MAP) - look at supplemental appendix of premier study???
# Can't use the actual ICD codes because not all the diagnoses have an associated numeric code
# on final df infection = presence of infection codes, organ dysfunction = presence of organ dysfunction codes
# and angus "Yes" = either presense of septic shock or presence of infection code and organ dysfunction code regardless of offset day

# Combine diagnosis codes with admission diagnosis codes
admit <- admissiondx %>% select(patientunitstayid, admitdxenteredoffset, admitdxpath) %>% 
  rename("dxoffset" = "admitdxenteredoffset", "diagnosis" = "admitdxpath") %>% 
  mutate(source = "on admission")

dx <- diagnosis %>% select(patientunitstayid, diagnosisoffset, diagnosisstring) %>%
  rename("dxoffset" = "diagnosisoffset", "diagnosis" = "diagnosisstring") %>%
  mutate(source = "during admission")

alldx <- rbind(admit,dx)
alldx <- alldx %>% arrange(patientunitstayid, dxoffset)
rm(admit, dx)

# Identify sepsis, septic shock, infection and organ dysfunction diagnosis criteria (then remove just sepsis codes)
library(stringr)
alldx <- alldx %>% mutate(diagnosis = str_to_lower(diagnosis)) %>% 
  mutate(sepsis = ifelse(str_detect(diagnosis, "sepsis"), "sepsis", "")) %>% 
  mutate(shock = ifelse(str_detect(diagnosis, "shock"), "shock", "")) %>% 
  mutate(ss = ifelse(str_detect(diagnosis, "septic shock"), "septic shock", "")) %>% 
  mutate(septicshock = ifelse((sepsis == "sepsis" & shock == "shock") | ss == "septic shock", "septic shock", "")) %>% # teasing out when sepsis and shock on same diagnosis line = septic shock
  mutate(infection = ifelse(str_detect(diagnosis, "cholera|typhoid|salmonella|shigell|food poisoning|intestinal infection|tuberculosis|plague|tularemia|anthrax|brucell|glanders|melioidosis|rat-bite fever|leprosy|mycobacterial|diphtheria|whooping cough|streptococcal/scarlet fever|erysipelas|meningococcal infection|tetanus|septicemia|actinomy|other bacterial|syphilis|gonococcal|leptospirosis|vincent’s angina|yaws|pinta|spirochetal|dermatophytosis|dermatomycosis|candidiasis|coccidioidomycosis|histo|blasto|mycoses|meningitis|abscess|phlebitis of intracranial sinus|pericarditis|endocarditis|thrombophlebitis|sinusitis|pharyngitis|tonsillitis|laryngitis|tracheitis|upper respiratory infection|pneumococcal|pneumonia|bronchopneumonia|bronchitis|bronchiectasis|empyema|lung abscess|appendicitis|diverticulitis|peritonitis|perforation of intestine|portal pyemia|cholecystitis|kidney infection|urethritis|urethral syndrome|urinary tract infection|prostatic inflammation|female pelvic inflammation disease|uterine inflammatory disease|genital inflammation|cellulitis|lymphadenitis|skin infection|pyogenic arthritis|osteomyelitis|bacteremia|infection|inflammation of device|postoperative infection"), "infection", "")) %>%
  mutate(organdysfunction = ifelse(str_detect(diagnosis, "hypotension|mechanical ventilation|encephalophathy|psychosis|anoxic brain damage|thrombocytopenia|coagulation defect|defibrination syndrome|necrosis of liver|hepatic infarction|acute renal failure"), "organ dysfunction", "")) %>% 
  select(patientunitstayid, dxoffset, diagnosis, source, septicshock, infection, organdysfunction)

# Filter down to patientunitstayid level - to isolate each piece and then combine back together 
septicshock <- alldx %>% filter(septicshock == "septic shock") %>% select(patientunitstayid, septicshock) %>% distinct(patientunitstayid, septicshock, .keep_all = TRUE)
infection <- alldx %>% filter(infection == "infection") %>% select(patientunitstayid, infection) %>% distinct(patientunitstayid, infection)
organdysfunction <- alldx %>% filter(organdysfunction == "organ dysfunction") %>% select(patientunitstayid, organdysfunction) %>% distinct(patientunitstayid, organdysfunction)

# matched based on patientunitstayid ignoring offset day (decided doesn't matter if organ dysfunction and infection were on same day)
  angus <- septicshock %>% 
    full_join(infection, by = "patientunitstayid") %>% 
    full_join(organdysfunction, by = "patientunitstayid") %>% 
    mutate(angus = ifelse((infection == "infection" & organdysfunction == "organ dysfunction") | septicshock == "septic shock", "Yes", ""))

angus1 <- angus %>% filter(angus == "Yes") %>% 
  select(patientunitstayid, angus) %>% 
  distinct(patientunitstayid, angus)
# final angus criteria = angus1 and final septic shock diagnosis coding (alone) is septicshock

# Final Screening Dataframe ####
# should start with 200859 observations
# need to ensure all dataframes (sepsis3, septicshock, infection, organdysfunction, angus1) are active to run this
allscreening <- patient %>% select(patientunitstayid, uniquepid, patienthealthsystemstayid, gender, age, ethnicity, hospitaldischargeyear, 
                              hospitaladmitoffset, hospitaldischargestatus, unittype, unitadmitsource, unitvisitnumber) %>% 
  left_join(sepsis3, by = "patientunitstayid") %>% 
  left_join(septicshock %>% dplyr::select(patientunitstayid, septicshock), by = "patientunitstayid") %>% 
  mutate(vasopressors = if_else(is.na(vasoactivestart), "no vasopressors", "vasopressors")) %>% 
  left_join(infection, by = "patientunitstayid") %>% 
  left_join(organdysfunction, by = "patientunitstayid") %>% 
  left_join(angus1, by = "patientunitstayid") %>% 
  mutate(vasopressin = ifelse(!is.na(firstavpstart), "yes", NA), 
         septicshockcoding = ifelse(septicshock == "septic shock" & vasopressors == "vasopressors", "Yes", NA), 
         anguscriteria = ifelse(angus == "Yes" & vasopressors == "vasopressors", "Yes", NA)) %>%
  select(patientunitstayid, uniquepid, sepsis3, sepsis3nomap, sepsis3bosch, sepsis3nomapbosch, septicshockcoding, anguscriteria, vasoactivestart, 
         vasoactivestop, firstavpstart, firstavpstop, vasopressin, labresult, lactate2, lactate2at72hr, abxduringvasoactive, abxduringvasoactive72h, map65,  
         septicshock, vasopressors, infection, organdysfunction, angus)
write.csv(allscreening, file = "eICU Data/eicu_finalscreening.csv", row.names = FALSE) # saved all the screening for all 200859 patients if need to open reference later, can then manipulate final df as needed 

# FINAL DATAFRAME - Inclusion/Exclusion ####

### Inclusion Criteria ####
### Age 
# unique(finaldf$age)
# df <- finaldf %>% filter(age == "> 89") # 7081 patientunitstayid "> 89" will change these to 90 
# df <- finaldf %>% filter(is.na(age)) # 95 patients with NA as age - exclude
# df <- finaldf %>% filter(age < 18) # 11133 patients with age < 18 - exclude 
# Formula below incorporated into exclusion criteria

### Exclusion Criteria ####
# AVP before or at same time as CA start # 306 excluded for this
# AVP started > 48 hrs after CAs # 109 excluded for this
# No lactate concentration at time of vasopressin initiation (within 24 hours prior to vasopressin initation)
lactate <- lab %>% filter(grepl("lactate", labname, ignore.case = TRUE)) %>% 
  left_join(vasopressors %>% dplyr::select(patientunitstayid, firstavpstart) %>% 
              dplyr::filter(!is.na(firstavpstart)), by = "patientunitstayid") %>% 
  filter(!is.na(firstavpstart)) %>% 
  arrange(patientunitstayid, labresultoffset) %>% 
  mutate(avplactatedifference = round(((firstavpstart - labresultoffset) / 60), 3)) %>% 
  mutate(lactateatavp = ifelse(avplactatedifference <= 24 & avplactatedifference >= 0, "yes", "")) %>% 
  group_by(patientunitstayid, lactateatavp) %>% mutate(closestlactatediff = min(avplactatedifference)) %>% ungroup() %>%
  filter(closestlactatediff == avplactatedifference, lactateatavp == "yes") %>% 
  distinct(patientunitstayid, .keep_all = TRUE)
  # isolating closest lactate to avp but within 24 hours prior to avp starting


allscreening <- read_csv("eICU Data/eicu_finalscreening.csv")

inclusionexclusion <- allscreening %>% # total number should always be 200859
  left_join(patient %>% dplyr::select(patientunitstayid, age), by = "patientunitstayid") %>% 
  left_join(lactate %>% dplyr::select(patientunitstayid, labresult, labresultoffset, avplactatedifference), by = "patientunitstayid") %>% 
  rename("lactateforinclusion" = "labresult.x", "lactateatavp" = "labresult.y", "lactateatavpoffsetmin" = "labresultoffset") %>%
  mutate(age = as.factor(age)) %>% 
  mutate(age = recode_factor(age, "> 89" = "90")) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(timefromshockonset_min = firstavpstart - vasoactivestart, 
         timefromshockonset_hr = timefromshockonset_min / 60) %>% 
  mutate(timefromshockonset_hr = round(timefromshockonset_hr, 1)) %>% 
  mutate(include_exclude_sepsis3 = ifelse(is.na(sepsis3) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria
                                   ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                   ifelse(age < 18, "exclude", # not included if age < 18
                                   ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                   ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                   ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_sepsis3 = ifelse(is.na(sepsis3), "does not meet sepsis3 criteria",
                                   ifelse(is.na(vasopressin), "did not receive vasopressin", 
                                   ifelse(is.na(age), "unknown age",
                                   ifelse(age < 18, "age < 18", 
                                   ifelse(vasoactivestart == firstavpstart, "vaso before or at same time as CA", 
                                   ifelse(timefromshockonset_hr >= 48, "vaso start > 48hrs after CA",
                                   ifelse(is.na(lactateatavp), "no lactate at avp start", NA)))))))) %>%
  mutate(include_exclude_sepsis3nomap = ifelse(is.na(sepsis3nomap) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria without map
                                        ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                        ifelse(age < 18, "exclude", # not included if age < 18
                                        ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                        ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                        ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_sepsis3nomap = ifelse(is.na(sepsis3nomap), "does not meet sepsis3nomap criteria",
                                        ifelse(is.na(vasopressin), "did not receive vasopressin", 
                                        ifelse(is.na(age), "unknown age",
                                        ifelse(age < 18, "age < 18", 
                                        ifelse(vasoactivestart == firstavpstart, "vaso before or at same time as CA", 
                                        ifelse(timefromshockonset_hr >= 48, "vaso start > 48hrs after CA",
                                        ifelse(is.na(lactateatavp), "no lactate at avp start", NA)))))))) %>% 
  mutate(include_exclude_sepsis3bosch = ifelse(is.na(sepsis3bosch) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria
                                        ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                        ifelse(age < 18, "exclude", # not included if age < 18
                                        ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                        ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                        ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_sepsis3bosch = ifelse(is.na(sepsis3bosch), "does not meet sepsis3 bosch criteria",
                                        ifelse(is.na(vasopressin), "did not receive vasopressin", 
                                        ifelse(is.na(age), "unknown age",
                                        ifelse(age < 18, "age < 18", 
                                        ifelse(vasoactivestart == firstavpstart, "vaso before or at same time as CA", 
                                        ifelse(timefromshockonset_hr >= 48, "vaso start > 48hrs after CA",
                                        ifelse(is.na(lactateatavp), "no lactate at avp start", NA)))))))) %>%
  mutate(include_exclude_sepsis3nomapbosch = ifelse(is.na(sepsis3nomapbosch) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria without map
                                             ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                             ifelse(age < 18, "exclude", # not included if age < 18
                                             ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                             ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                             ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_sepsis3nomapbosch = ifelse(is.na(sepsis3nomapbosch), "does not meet sepsis3nomap bosch criteria",
                                             ifelse(is.na(vasopressin), "did not receive vasopressin", 
                                             ifelse(is.na(age), "unknown age",
                                             ifelse(age < 18, "age < 18", 
                                             ifelse(vasoactivestart == firstavpstart, "vaso before or at same time as CA", 
                                             ifelse(timefromshockonset_hr >= 48, "vaso start > 48hrs after CA",
                                             ifelse(is.na(lactateatavp), "no lactate at avp start", NA)))))))) %>% 
  mutate(include_exclude_sscoding = ifelse(is.na(septicshockcoding) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria without map
                                    ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                    ifelse(age < 18, "exclude", # not included if age < 18
                                    ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                    ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                    ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_sscoding = ifelse(is.na(septicshockcoding), "does not meet sscoding criteria",
                                    ifelse(is.na(vasopressin), "did not receive vasopressin", 
                                    ifelse(is.na(age), "unknown age",
                                    ifelse(age < 18, "age < 18", 
                                    ifelse(vasoactivestart == firstavpstart, "vaso before or at same time as CA", 
                                    ifelse(timefromshockonset_hr >= 48, "vaso start > 48hrs after CA",
                                    ifelse(is.na(lactateatavp), "no lactate at avp start", NA)))))))) %>%
  mutate(include_exclude_angus = ifelse(is.na(anguscriteria) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria without map
                                 ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                 ifelse(age < 18, "exclude", # not included if age < 18
                                 ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                 ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                 ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_angus = ifelse(is.na(anguscriteria), "does not meet angus criteria",
                                 ifelse(is.na(vasopressin), "did not receive vasopressin", 
                                 ifelse(is.na(age), "unknown age",
                                 ifelse(age < 18, "age < 18", 
                                 ifelse(vasoactivestart == firstavpstart, "vaso before or at same time as CA", 
                                 ifelse(timefromshockonset_hr >= 48, "vaso start > 48hrs after CA",
                                 ifelse(is.na(lactateatavp), "no lactate at avp start", NA)))))))) %>%
  select(patientunitstayid, uniquepid, sepsis3, include_exclude_sepsis3, exclusionreason_sepsis3, sepsis3nomap, 
         include_exclude_sepsis3nomap, exclusionreason_sepsis3nomap, sepsis3bosch, include_exclude_sepsis3bosch, exclusionreason_sepsis3bosch, sepsis3nomapbosch, 
         include_exclude_sepsis3nomapbosch, exclusionreason_sepsis3nomapbosch, septicshockcoding, include_exclude_sscoding, exclusionreason_sscoding, 
         anguscriteria, include_exclude_angus, exclusionreason_angus, age, timefromshockonset_min, timefromshockonset_hr, vasoactivestart:vasopressin, lactateatavp, 
         lactateatavpoffsetmin, avplactatedifference, lactateforinclusion:angus)

write.csv(inclusionexclusion, file = "eICU Data/eicu_inclusionexclusion.csv", row.names = FALSE)


# ------------------------------------------- #
# FINAL DATAFRAME - data collection points ####
# ------------------------------------------- #

### Step 1 #### 
# Step 1: Import the below dataframes
inclusionexclusioneicu <- read_csv("~/eICU Data/eicu_inclusionexclusion.csv")
patient <- read_csv("~/eICU Data/eicu-collaborative-research-database-2.0/patient.csv.gz") %>% mutate(weight = ifelse(is.na(admissionweight), dischargeweight,admissionweight))
eicupmh <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/pastHistory.csv.gz")
avp1 <- read_csv("~/eICU Data/avp1.csv")
vasopressors <- read_csv("~/eICU Data/eicu_vasopressors.csv")
vasopressorsverticalneq <- read_csv("~/eICU Data/vasopressorsverticalneq.csv")
vasopressorsvertical <- read_csv("~/eICU Data/vasopressorsvertical.csv")
vasopressoruse <- read_csv("~/eICU Data/vasopressoruse.csv")
apacheresults <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/apachePatientResult.csv.gz")
sofa <- read_csv("~/eICU Data/eicu_sofascores.csv")
neremovelist <- read_csv("~/eICU Data/neremovelist.csv")

### Step 2 ####
# Step 2: Run Below code
finaldf <- inclusionexclusioneicu %>% filter(vasopressors == "vasopressors") %>% 
  select(patientunitstayid, uniquepid, age, timefromshockonset_hr, lactateatavp, abxduringvasoactive) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, gender, ethnicity, unittype, hospitaldischargestatus, unitdischargestatus, 
                                      unitdischargeoffset, hospitaldischargeoffset, weight), by = "patientunitstayid") %>% 
  select(patientunitstayid, uniquepid, age, gender, "race" = "ethnicity", "iculocation" = "unittype", weight, abxduringvasoactive, 
         timefromshockonset_hr, lactateatavp, "icu_los_min" = "unitdischargeoffset", "inhospital_mortality" = "hospitaldischargestatus", 
         "icu_mortality" = "unitdischargestatus", hospitaldischargeoffset) %>% 
  mutate(race = recode(race, "Caucasian" = "White", "Native American" = "Other", "Other/Unknown" = "Other", "Asian" = "Other", 
                       "Hispanic" = "Other", "African American" = "Black", "NA" = "Other"),
         inhospital_mortality = recode(inhospital_mortality, "Alive" = "No", "Expired" = "Yes"),
         iculocation = recode(iculocation, "CCU-CTICU" = "Cardiac ICU",  "CSICU" = "Cardiac ICU", "CTICU" = "Cardiac ICU", 
                              "Med-Surg ICU" = "Mixed ICU", "Neuro ICU" = "NICU"), 
         icu_mortality = recode(icu_mortality, "Alive" = "No", "Expired" = "Yes"), 
         icu_los = round(icu_los_min/1440, 2), 
         abxduringvasoactive = ifelse(is.na(abxduringvasoactive), "No", abxduringvasoactive)) %>%
  mutate(race = replace_na(race, "Other"), # changing unknown race to "Other" 
         icu_mortality = replace_na(icu_mortality, "No")) %>%  # changing NA icu_mortality to No as none had inhospital_mortality
  mutate(inhospital_mortality = ifelse(is.na(inhospital_mortality) & icu_mortality == "Yes", "Yes", 
                                       ifelse(icu_mortality == "Yes" & inhospital_mortality == "No", "Yes", inhospital_mortality)),
         inhospital_mortality = replace_na(inhospital_mortality, "Unknown"),
         inhospital_mortality = as.factor(inhospital_mortality)) %>% 
  left_join(apacheresults %>% dplyr::select(patientunitstayid, apachescore, apacheversion, unabridgedactualventdays) %>% 
              dplyr::filter(apacheversion == "IV"), by = "patientunitstayid") %>% 
  left_join(vasopressors %>% dplyr::select(patientunitstayid, firstavpstart, firstavpstop, vasoactivestart, vasoactivestop), 
            by = "patientunitstayid") %>% 
  mutate(time0 = firstavpstart, time0_day = floor(firstavpstart/1440), 
         avpduration_days = round(((firstavpstop - firstavpstart)/1440), 2)) %>%
  #time0 = vasopressin initation, time0_day = offsetday of avp initiation
  left_join(avp1 %>% dplyr::select(patientunitstayid, drugrate, status) %>% dplyr::filter(status == "initial start"), 
            by = "patientunitstayid") %>%
  mutate(initialavpdose_category = ifelse(!is.na(firstavpstart) & drugrate == 0.03, "0.03", 
                                          ifelse(!is.na(firstavpstart) & drugrate == 0.04, "0.04", "other"))) %>% 
  mutate(initialavpdose_category = ifelse(is.na(firstavpstart), NA, initialavpdose_category)) %>%
  left_join(vasopressorsverticalneq %>% dplyr::select(patientunitstayid, totalneqdose), by = "patientunitstayid") %>% 
  mutate(totalneqdose = ifelse(is.na(firstavpstart), NA, totalneqdose)) %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, neuse), dplyr::filter(neuse == "Yes"), by = "patientunitstayid") %>%
  mutate(neuse = ifelse(is.na(neuse), "No", neuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, epiuse), dplyr::filter(epiuse == "Yes"),by = "patientunitstayid") %>%
  mutate(epiuse = ifelse(is.na(epiuse), "No", epiuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, peuse), dplyr::filter(peuse == "Yes"), by = "patientunitstayid") %>%
  mutate(peuse = ifelse(is.na(peuse), "No", peuse)) %>% 
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, dopuse), dplyr::filter(dopuse == "Yes"), by = "patientunitstayid") %>%
  mutate(dopuse = ifelse(is.na(dopuse), "No", dopuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, dobutuse), dplyr::filter(dobutuse == "Yes"), by = "patientunitstayid") %>%
  mutate(dobutuse = ifelse(is.na(dobutuse), "No", dobutuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, miluse), dplyr::filter(miluse == "Yes"), by = "patientunitstayid") %>%
  mutate(miluse = ifelse(is.na(miluse), "No", miluse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, hydrocortuse), dplyr::filter(hydrocortuse == "Yes"), by = "patientunitstayid") %>%
  mutate(hydrocortuse = ifelse(is.na(hydrocortuse), "No", hydrocortuse))
  
### Step 3 ####
# Step 3: Need to run all below Data Collection Points
meds <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/medication.csv.gz")
lab <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/lab.csv.gz")
respchart <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/respiratoryCharting.csv.gz")
vitalperiodic <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/vitalPeriodic.csv.gz")
vitalaperiodic <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/vitalAperiodic.csv.gz")
ios <- vroom("~/eICU Data/eicu-collaborative-research-database-2.0/intakeOutput.csv.gz")

#### Past Medical History ####
pmh1 <- eicupmh %>% 
  mutate(diabetes = ifelse(grepl("diabetes", pasthistorypath, ignore.case = TRUE), "Yes", NA)) %>% # diabetes identified 
  mutate(cirrhosis = ifelse(grepl("cirrhosis", pasthistorypath, ignore.case = TRUE), "Yes", NA)) %>% # cirrhosis identified (no hepatic failure)
  mutate(immune_suppression = ifelse(grepl("transplant|cancer|immunosuppression|aids", pasthistorypath, ignore.case = TRUE), "Yes", NA)) %>% # immunesuppression identified
  mutate(copd = ifelse(grepl("copd", pasthistorypath, ignore.case = TRUE), "Yes", NA)) %>% # copd identified
  mutate(dialysis = ifelse(grepl("dialysis", pasthistorypath, ignore.case = TRUE), "Yes", NA)) %>% # dialysis identified
  mutate(no_chronic_health = ifelse(grepl("no health problems", pasthistorypath, ignore.case = TRUE), "Yes", NA)) # no chronic health comorbidities
# cirrhosis: cirrhosis only (no hepatic failure and liver = liver cancer/mets or liver transplant)
# immune suppression: organ transplant, cancer, immune suppressant use (steroid >20 mg prednisone), AIDS 
# searched by (in pasthistorypath): immunosuppression, cancer, transplant, aids (this will comprise immunosuppressant use, transplant, cancer, and AIDS patients )
# for ESRD: no "ESRD", or end stage renal disease, only think is dialysis and peritoneal dialysis under renal 

#### Fluid balance ####
# cumulative amount prior to time0
# run finaldf through hydrocort use
fluidbalance <- ios %>% arrange(patientunitstayid, intakeoutputoffset) %>% 
  left_join(finaldf %>% dplyr::select(patientunitstayid, firstavpstart) %>% dplyr::filter(!is.na(firstavpstart)), by = "patientunitstayid") %>% 
  filter(!is.na(firstavpstart)) %>% 
  mutate(fluidbalance = ifelse(intakeoutputoffset <= firstavpstart, "Yes", "No")) %>% 
  filter(fluidbalance == "Yes") %>% 
  group_by(patientunitstayid) %>% mutate(fluidbalance = sum(nettotal))


#### Fluid bolus ####
# fluid bolus - crystalloids
crystalloid <- meds %>% filter(grepl("sodium chloride|lactated", drugname, ignore.case = TRUE)) %>% # lactated and ringer both reveal same number of observations
  mutate(drugname = as.factor(drugname), 
         fluid = recode_factor(drugname, # rename for ease of filtering
                               "1000 ML  -  LACTATED RINGERS IV SOLN" = "LR 1000 mL", "SODIUM CHLORIDE 0.9 % IV SOLN" = "NS", 
                               "SODIUM CHLORIDE 0.9% 500 ML BAG" = "NS 500 mL", "LACTATED RINGER'S 1,000 ML BAG." = "LR 1000 mL", 
                               "SODIUM CHLORIDE 0.9% 1,000 ML BAG" = "NS 1000 mL", "LACTATED RINGERS" = "LR", "SODIUM CHLORIDE 0.9%" = "NS", 
                               "SODIUM CHLORIDE 0.9 % IV 1000 ML BAG" = "NS 1000 mL", "SODIUM CHLORIDE 0.9 % IV 250 ML BAG" = "NS 250 mL", 
                               "SODIUM CHLORIDE 0.9 % SYRINGE 10 ML SYRINGE" = "NS 10 mL syringe", "SODIUM CHLORIDE 0.9% 50 ML PB" = "NS 50 mL", 
                               "SODIUM CHLORIDE 0.9% 500 ML PB" = "NS 500 mL", "SODIUM CHLORIDE 0.9% 250 ML LVP" = "NS 250 mL", 
                               "SODIUM CHLORIDE 0.45% 1000 ML LVP" = "Half NS 1000 mL", "Lactated Ringers Injection 1,000 mL" = "LR 1000 mL", 
                               "RINGERS SOLUTION,LACTATED" = "LR", "1000 ml flex cont : sodium chloride 0.9 % iv soln" = "NS", 
                               "500 ml : sodium chloride 0.9 % iv soln" = "NS 500 mL", "SODIUM CHLORIDE 0.9 % IJ SOLN" = "NS", 
                               "1000 ML : LACTATED RINGERS IV SOLN" = "LR 1000 mL", "500 ML FLEX CONT : SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 500 mL", 
                               "1000 ML FLEX CONT : SODIUM CHLORIDE 0.45 % IV SOLN" = "Half NS 1000 mL", "SODIUM CHLORIDE 0.45%" = "Half NS", 
                               "SODIUM CHLORIDE 0.9% P.F." = "NS", "sodium chloride 0.9 %" = "NS", 
                               "1000 ML FLEX CONT: LACTATED RINGERS IV SOLN" = "LR 1000 mL", 
                               "SODIUM CHLORIDE 0.9 % SYRINGE : 10 ML SYRINGE" = "NS 10 mL syringe", 
                               "SODIUM CHLORIDE 0.9 % IV : 1000 ML" = "NS 1000 mL", "SODIUM CHLORIDE 0.9 % IV : 500 ML" = "NS 500 mL", 
                               "SODIUM CHLORIDE 0.9%SOLP (1000 ML)" = "NS 1000 mL", "LACTATED RINGERS IV : 1000 ML BAG" = "LR 1000 mL", 
                               "SODIUM CHLORIDE 0.9% 1000 ML" = "NS 1000 mL", "500 ML : SODIUM CHLORIDE 0.9 % 0.9 % IV SOLP" = "NS 500 mL", 
                               "1000 ML : SODIUM CHLORIDE 0.9 % 0.9 % IV SOLP"  = "NS 1000 mL", "SODIUM CHLORIDE 0.9 % IV SOLP" = "NS", 
                               "SODIUM CHLORIDE 0.9 % INJ SYRINGE" = "NS", "LACTATED RINGERS IV SOLP" = "LR", "sodium chloride infusion 0.9%" = "NS", 
                               "sodium chloride 0.9 % flush 10 mL" = "NS 10 mL syringe", "Sodium Chloride 0.9%  SSO" = "NS", 
                               "LACTATED RINGERS IV SOLN BOLUS" = "LR", "MAGNESIUM SULFATE 2G/SODIUM CHLORIDE 0.9% 100ML STOCK BAG" = "Magnesium", 
                               "SODIUM CHLORIDE FLUSH"  = "NS flush", "sodium chloride 0.9% (NS) infusion" = "NS", 
                               "sodium chloride 0.9 % flush peripheral lok 3 mL" = "NS flush", "1000 ML  -  SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 1000 mL", 
                               "500 ML  -  SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 500 mL", "250 ML  -  SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 250 mL", 
                               "100 ML FLEX CONT : SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 100 mL", "SODIUM CHLORIDE" = "sodium chloride", 
                               "Sodium Chloride 0.9%" = "NS", "SODIUM CHLORIDE 0.9% 1000 ML LVP" = "NS 1000 mL", 
                               "SODIUM CHLORIDE 0.9% 500 ML LVP" = "NS 500 mL", "SODIUM CHLORIDE 0.9% 100 ML PB" = "NS 100 mL", 
                               "SODIUM CHLORIDE 0.9% 1000 ML PB" = "NS 1000 mL", "SODIUM CHLORIDE 0.9% 250 ML PB" = "NS 250 mL", 
                               "LACTATED RINGERS 1000 ML LVP" = "LR 1000 mL", "Sodium Chloride 0.9% 1,000 mL" = "NS 1000 mL", 
                               "Lactated Ringers Injection" = "LR", "sodium chloride 0.9% flush 10 mL" = "NS flush", 
                               "250 ml flex cont : sodium chloride 0.9 % iv soln" = "NS 250 mL", 
                               "150 ml flex cont : sodium chloride 0.9 % iv soln" = "NS 150 mL", 
                               "sodium chloride 0.9%" = "NS", "1000 ML FLEX CONT : SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 1000 mL", 
                               "250 ML FLEX CONT : SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 250 mL", 
                               "Sodium Chloride 0.9% 1,000 ML BAG" = "NS 1000 mL", "Sodium Chloride 0.9% 500 ML BAG" = "NS 500 mL",
                               "LACTATED RINGER'S 1,000 ML BAG" = "LR 1000 mL", "1000 ML FLEX CONT: SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 1000 mL", 
                               "500 ML FLEX CONT EXCEL : SODIUM CHLORIDE 0.9 % IV SOLN" = "NS 500 mL", "SODIUM CHLORIDE 0.9 % IV : 250 ML" = "NS 250 mL", 
                               "100 ML BAG : SODIUM CHLORIDE 0.9 % 0.9 % IV SOLP" = "NS 100 mL", "1000 ML BAG : LACTATED RINGERS IV SOLP" = "LR 1000 mL", 
                               "SODIUM CHLORIDE 0.9 % IV BOLUS" = "NS", "LACTATED RINGERS IV SOLN" = "LR", "SODIUM CHLORIDE 0.9 % IV SOLN BOLUS" = "NS", 
                               "SODIUM CHLORIDE FLUSH 0.9 % IV SOLN 10 ML PREFILL SYRINGE" = "NS 10 mL syringe", "Sodium Chloride 0.9% Syringe SSO" = "NS flush", 
                               "sodium chloride 0.9% (NS) bolus 1,000 mL" = "NS 1000 mL", "SODIUM CHLORIDE 0.9 % BOLUS" = "NS",                             
                               "Sodium Chloride 0.9% FLUSH" = "NS flush", "SODIUM CHLORIDE BACTERIOSTATIC 0.9 % INJ SOLN" = "NS")) %>% 
  filter(fluid != "Half NS") %>% filter(fluid != "Half NS 1000 mL") %>% # removing half NS records == 431877 records at this step
  filter(fluid != "Magnesium") %>%  # removing magnesium records # 429336 records at this step
  mutate(routeadmin = as.factor(routeadmin)) %>% 
  mutate(route = recode_factor(routeadmin, # remove extraneous routes that are not IV
                               "IK" = "remove", "LOK" = "remove", "Intrapleural" = "remove", "EPID" = "remove", 
                               "Intra-articular" = "remove", "INTRA-ARTERIAL" = "remove", "ED" = "remove", "IRRG" = "remove", 
                               "JT" = "remove", "INTRAPLEUR" = "remove", "INHALATION" = "remove", "INTRAPERIT" = "remove", 
                               "INTRATHECA" = "remove", "Other" = "remove", "IPL" = "remove","IRRIGATION" = "remove", 
                               "INTRAARTER" = "remove", "IRRIG" = "remove", "INH" = "remove", "DIALYSIS" = "remove", 
                               "INTRA-CATHET" = "remove", "BLADDER INST" = "remove", "IR" = "remove", "CRRT" = "remove", 
                               "IRR" = "remove", "Arterial" = "remove", "NG" = "remove", "OTHER" = "remove", "IU" = "remove", 
                               "Irrigation" = "remove", "TUBE" = "remove", "Intra-arterial" = "remove", "Hemodialysis" = "remove", 
                               "MISC" = "remove", "Intra-Catheter" = "remove", "NERVE BLOCK" = "remove", "HEMODIALYSIS" = "remove", 
                               "DEVICE" = "remove", "Cath" = "remove", "IArt" = "remove", "LAVAGE" = "remove", 
                               "Miscellaneou" = "remove", "IT" = "remove", "Top" = "remove", "COSIG INH" = "remove", 
                               "TLC" = "remove", "SubQ" = "remove", "IA" = "remove", "NGtube" = "remove", "PGtube" = "remove", 
                               "MIS" = "remove", "PO" = "remove", "IVS" = "remove", "TF" = "remove", "Intraperitoneal" = "remove", 
                               "IntraLumenal" = "remove", "misc" = "remove", "J-Tube" = "remove", "Oral" = "remove", "ORAL" = "remove", 
                               "Nebulization" = "remove", "Feeding Tube" = "remove", "G-TUBE" = "remove", "Each Nare" = "remove", 
                               "Tube" = "remove", "PEG" = "remove", "IRRIGATE" = "remove", "NEB" = "remove", ".SEE ORDER" = "remove", 
                               "GASTRIC TU" = "remove", "X" = "remove", "BOTH NOSTR" = "remove", "PEG TUBE" = "remove", "GT" = "remove", 
                               "ORAL/TUBE" = "remove", "NGTUBE" = "remove", "ARTERIAL" = "remove", "IP" = "remove", "IM" = "remove", 
                               "FT" = "remove", "PR" = "remove", "Intrauteral" = "remove", "IntraArterial" = "remove", 
                               "PO or NG" = "remove", "Device" = "remove", "NASL" = "remove", "Intracatheter" = "remove", 
                               "Intercatheter" = "remove", "Nasal" = "remove", "Intra-catheter (arterial)" = "remove", 
                               "EP" = "remove", "RE" = "remove", "PEG-TUBE" = "remove", "NG-TUBE" = "remove", "OG" = "remove", 
                               "Intradermal" = "remove")) %>%
  filter(route != "remove") %>% select(-route) %>% # removing those identified to be removed # 412099 observations left
  mutate(dosage_raw = dosage) %>% 
  separate(dosage, c("volume", "unit"), " ") %>% # splitting the dosage column to get rates and volumes 
  mutate(fluid_raw = fluid) %>% 
  separate(fluid, c("fluid", "volume_order", "unit_order")) %>% # splitting the fluid column to get the volume on order
  mutate(unit = as.factor(unit), # cleaning up units
         unit = recode_factor(unit, "mL/hr" = "mL/hr", "1" = "mL", "ML" = "mL", "ml" = "mL", "g" = "delete", "mg" = "delete", "dose(s)" = "delete", 
                              "EA" = "delete", "MEQ" = "delete", "41" = "mL", "mg/min" = "delete", "NA" = "mL"))  %>% 
  filter(unit != "delete" | is.na(unit)) %>% # 412028 observations left
  mutate(duration = round((drugstopoffset - drugstartoffset)/60, 2)) %>% 
  mutate(duration = ifelse(duration <= 0, 0.5, duration)) %>% 
  filter(volume != "10-40" | is.na(volume)) %>% # removing volume of 10-40 which are likely flushes # 412023 observations left
  mutate(volume = as.factor(volume), volume = recode_factor(volume, "1,000" = "1000", "2,000" = "2000")) %>% 
  mutate(volume = as.character(volume), 
         volume = ifelse(volume == "ML", NA, volume), 
         volume = as.numeric(volume)) %>% 
  mutate(volume = ifelse(is.na(volume), volume_order, volume), 
         volume = ifelse(volume == "chloride", NA, 
                         ifelse(volume == "flush", NA, volume)), 
         volume = as.numeric(volume)) %>%
  filter(!is.na(volume)) %>% # 401016 observations left
  mutate(unit = as.factor(unit), 
         unit = ifelse(is.na(unit), "mL", unit), 
         unit = ifelse(unit == "mL", "mL", 
                       ifelse(unit == 1, "mL/hr", "mL"))) %>% 
  mutate(rate_mlhr = ifelse(unit == "mL/hr", round(volume, 0), round(volume/duration, 0))) %>% 
  arrange(patientunitstayid, drugorderoffset) %>% 
  select(patientunitstayid, drugstartoffset, drugstopoffset, duration, drugname, fluid, volume_order, unit_order, dosage_raw, volume, unit, rate_mlhr)
# should have 401016 total with this code

# fluid bolus - albumin
albumin <- meds %>% filter(grepl("albumin", drugname, ignore.case = TRUE)) %>%
  mutate(drugname = as.factor(drugname), 
         name = recode_factor(drugname, "ALBUMIN HUMAN 25%" = "albumin 25", "ALBUMIN 5% 250 ML INJ" = "albumin 5 250 mL", 
                              "ALBUMIN 25% 100 ML INJ" = "albumin 25 100 mL", "ALBUMIN 5% 500 ML INJ" = "albumin 5 500 mL", 
                              "albumin human" = "albumin", "100 ML FLEX CONT : ALBUMIN HUMAN 25 % IV SOLN" = "albumin 25 100 mL", 
                              "ALBUMIN HUMAN 5 % IV SOLN" = "albumin 5", "50 ML  -  ALBUMIN HUMAN 25 % IV SOLN" = "albumin 25 50 mL", 
                              "250 ML  -  ALBUMIN HUMAN 5 % IV SOLN" = "albumin 5 250 mL", "ALBUMIN HUMAN 5%" = "albumin 5", 
                              "albumin 5%" = "albumin 5", "albumin (human) (PLASBUMIN-5) 5% infusion 25 g" = "albumin 5 500 mL", 
                              "ALBUMIN HUMAN 5 % IV SOLP" = "albumin 5", "ALBUMIN HUMAN 25 % IV SOLN" = "albumin 25")) %>% 
  separate(name, c("fluid", "albumin_percent", "volume"), " ") %>% 
  mutate(dosage_raw = dosage) %>% 
  separate(dosage, c("dose", "unit"), " ") %>% 
  mutate(unit = as.factor(unit), 
         unit = recode_factor(unit, "GM" = "gm", "G" = "gm", "Gm" = "gm", "Charge" = "delete", "g" = "gm", "ML" = "mL", "1" = "mL", "4" = "gm", 
                              "mL/hr"= "delete", "mg" = "gm")) %>% 
  filter(unit != "delete") %>% 
  arrange(patientunitstayid, drugstartoffset) %>% 
  mutate(albumin_dose = as.numeric(dose), 
         volume = ifelse(albumin_percent == "25" & unit == "gm", (albumin_dose*100)/25,
                         ifelse(albumin_percent == "25" & unit == "mL", albumin_dose, 
                                ifelse(albumin_percent == "5" & unit == "gm", (albumin_dose*100)/5, 
                                       ifelse(albumin_percent == "5" & unit == "mL", albumin_dose, volume))))) %>% 
  mutate(volume = ifelse(is.na(volume), "0", volume), # changing volume to be 0 if NA
         volume = as.numeric(volume)) %>% 
  mutate(volume = ifelse(albumin_percent == "25", volume*7, 
                         ifelse(albumin_percent == "5", volume*1.4, volume))) %>% # converting volume to crystalloid equivalent
  select(patientunitstayid, drugstartoffset, drugstopoffset, drugname, fluid, albumin_percent, dosage_raw, volume, albumin_dose, unit)

# total fluid bolus, joining both crystalloids and albumin
fluidbolus <- bind_rows(crystalloid, albumin) %>% arrange(patientunitstayid, drugstartoffset) %>% 
  left_join(finaldf %>% dplyr::select(patientunitstayid, vasoactivestart, firstavpstart, firstavpstop), by = "patientunitstayid") %>% 
  filter(!is.na(vasoactivestart)) %>% 
  mutate(fluid_bolus = ifelse(fluid == "albumin" & drugstartoffset >= vasoactivestart - 360 & drugstartoffset <= vasoactivestart + 360, "yes", 
                              ifelse(drugstartoffset >= vasoactivestart - 360 & drugstartoffset <= vasoactivestart + 360 & rate_mlhr > 125, "yes", NA))) %>% 
  # fluid bolus = receipt of fluid bolus > 125 mL/hr 6 hr before and 6 hr after vasoactive start/shock onset or any albumin admin
  group_by(patientunitstayid, fluid_bolus) %>% mutate(totalfluidbolus = sum(volume)) %>% ungroup() %>%
  filter(fluid_bolus == "yes") %>% distinct(patientunitstayid, .keep_all = TRUE) # only data point that matters at end is "totalfluidbolus"
rm(crystalloid, albumin) 

#### Hemodynamic stability #### 
# map df (need vital periodic and vitalaperiodic)
imap <- vitalperiodic %>% filter(!is.na(systemicmean)) %>%
  mutate(source = "invasive") %>% 
  dplyr::select(patientunitstayid, observationoffset, systemicsystolic, systemicdiastolic,
                systemicmean, source) %>%
  rename(sbp = systemicsystolic, dbp = systemicdiastolic, map = systemicmean)
# creating df of just invasive blood pressures
nimap <- vitalaperiodic %>% filter(!is.na(noninvasivemean)) %>%
  mutate(source = "noninvasive") %>% 
  dplyr::select(patientunitstayid, observationoffset, noninvasivesystolic, noninvasivediastolic, 
                noninvasivemean, source) %>%
  rename(sbp = noninvasivesystolic, dbp = noninvasivediastolic, map = noninvasivemean)
# creating df of noninvasive blood pressures  
map <- rbind(imap, nimap) %>% arrange(patientunitstayid, observationoffset) # combining invasive and non-invasive
rm(imap, nimap)
rm(vitalaperiodic, vitalperiodic)

# isolate MAP at 6 hours 
maphds <- map %>% left_join(vasopressorsverticalneq %>% dplyr::select(patientunitstayid, firstavpstart), by = "patientunitstayid") %>% 
  filter(!is.na(firstavpstart)) %>% 
  arrange(patientunitstayid, desc(observationoffset)) %>%
  distinct(patientunitstayid, observationoffset, map, .keep_all = TRUE) %>% 
  mutate(yes_map_6h = ifelse(observationoffset >= firstavpstart + 180 & observationoffset <= firstavpstart + 360, "yes", "")) %>%
  filter(yes_map_6h == "yes") %>%
  distinct(patientunitstayid, .keep_all = TRUE)
# went back 3 hours for closest MAP value 

# create final HDS df 
hds <- vasopressorsverticalneq %>% filter(!is.na(firstavpstart)) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, hospitaldischargestatus, hospitaldischargeoffset), by = "patientunitstayid") %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  select(-neqdose, -neqdose6hr) %>% 
  left_join(maphds %>% dplyr::select(patientunitstayid, map), by = "patientunitstayid") %>%
  rename(map_6h = map) %>% 
  mutate(diedin6hr = ifelse(hospitaldischargeoffset <= firstavpstart + 360 & hospitaldischargestatus == "Expired", "yes", "no")) %>% 
  mutate(hds = ifelse(totalneqdose6hr < totalneqdose & map_6h >= 65 & diedin6hr == "no", "yes", "no")) %>% 
  mutate(hds = ifelse(totalneqdose6hr == 0 & diedin6hr == "no" & firstavpstop <= firstavpstart + 360 & is.na(map_6h), "yes", hds)) %>%
  mutate(hds = ifelse(is.na(hds), "no", hds)) %>% 
  mutate_if(is.character, as.factor)

rm(map)

#### AKI at Vasopressin Initiation ####
# 1. Calculate baseline SrCr: median SrCr PTA (not available in eICU), minimum SrCr from hospital admit as long as pt admitted 
#    7 days prior to NE initiation, MDRD back calculation estimating eGFR of 60 
# 2. Determine AKI development vs. baseline at time of vasopressin initiation (ESRD vs. no AKI vs. Stage 1, 2, and 3)

conflicted::conflicts_prefer(base::min)

srcr <- lab %>% filter(labname == "creatinine") %>% 
  left_join(vasopressorsvertical %>% dplyr::select(patientunitstayid, vasoactivestart), by = "patientunitstayid") %>% 
  filter(!is.na(vasoactivestart)) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, hospitaladmitoffset), by = "patientunitstayid") %>% 
  mutate(hosptovasoactive = (abs(hospitaladmitoffset) + vasoactivestart)) %>%
  mutate(hosptovasoactive = ifelse(hosptovasoactive < 0, 0, hosptovasoactive)) %>% 
  mutate(minsrcr = ifelse(hosptovasoactive >= 10080 & labresultoffset <= vasoactivestart, "min srcr", "")) %>% 
  filter(minsrcr == "min srcr") %>% 
  group_by(patientunitstayid) %>% mutate(minsrcr = min(labresult)) %>% ungroup()
# formula to get minimum SrCr if admitted at least 7 days prior to vasoactive start
# add to final DF and do calculation in FinalDF 

srcr1 <- lab %>% filter(labname == "creatinine") %>% 
  left_join(vasopressorsvertical %>% dplyr::select(patientunitstayid, firstavpstart), by = "patientunitstayid") %>% 
  filter(!is.na(firstavpstart)) %>% 
  mutate(srcravp = ifelse(labresultoffset >= firstavpstart - 1440 & labresultoffset <= firstavpstart, "yes", "")) %>% 
  filter(srcravp == "yes") %>% 
  group_by(patientunitstayid) %>% mutate(srcravp = max(labresult)) %>% ungroup()
# formula to get maximum SrCr within 24 hrs prior to AVP start
# add to final DF and do calculation in FinalDF 

rm(lab)

#### Mechanical Ventilation at Baseline ####

mvbl <- respchart %>% arrange(patientunitstayid, respchartoffset) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, hospitaldischargeoffset, hospitaldischargestatus), by = "patientunitstayid") %>% 
  left_join(apacheresults %>% dplyr::select(patientunitstayid, unabridgedactualventdays), by = "patientunitstayid") %>% 
  distinct() %>% 
  mutate(ventstatus = respchartvaluelabel) %>% 
  mutate(ventstatus = if_else(ventstatus == "Oxygen Delivery Method", respchartvalue, ventstatus)) %>% 
  mutate(ventstatus = as.factor(ventstatus), 
         ventstatus = recode_factor(ventstatus, "RT Vent On/Off" = "on vent", 
                                    "Vent Rate" = "on vent", 
                                    "Tidal Volume (set)" = "on vent", 
                                    "TV/kg IBW"= "on vent", 
                                    "Tidal Volume Observed (VT)" = "on vent", 
                                    "PEEP" = "on vent", 
                                    "Plateau Pressure" = "on vent", 
                                    "Pressure Support" = "on vent",
                                    "Ventilator Type" = "on vent", 
                                    "LPM O2" = "off vent", 
                                    "Pressure Control" = "on vent", 
                                    "Peak Insp. Pressure" = "on vent", 
                                    "Endotracheal Position at Lip" = "on vent",
                                    "Endotracheal Tube Placement" = "on vent", 
                                    "Tube Size" = "on vent", 
                                    "Exhaled TV (patient)" = "on vent",
                                    "Mean Airway Pressure" = "on vent", 
                                    "Ventilator Heater Temperature" = "on vent",
                                    "Compliance" = "on vent", 
                                    "Exhaled TV (machine)" = "on vent", 
                                    "Pressure to Trigger PS" = "on vent",    
                                    "Minute Volume, Spontaneous" = "on vent", 
                                    "Inspiratory Pressure, Set" = "on vent", 
                                    "Inspiratory Flow Rate" = "on vent", 
                                    "Insp Flow (l/min)" = "on vent",  
                                    "Bipap Delivery Mode" = "off vent", 
                                    "Non-invasive Ventilation Mode" = "off vent",
                                    "Mechanical Ventilator Compliance" = "on vent", 
                                    "Endotracheal Tube Placement Checked" = "on vent",
                                    "Mechanical Ventilator Mode" = "on vent", 
                                    "Ventilator Support Mode" = "on vent", 
                                    "Peak Pressure" = "on vent",
                                    "ET Tube Repositioned" = "on vent", 
                                    "Position at lip" = "on vent", 
                                    "Postion at Lip" = "on vent",
                                    "Trachestomy Tube Size" = "on vent", 
                                    "Tracheostomy Type" = "on vent", 
                                    "NIV Setting EPAP" = "off vent",
                                    "Adult Con Pt/Vent Spont Rate" = "on vent",
                                    "Adult Con Pt/Vent MinuteVentil" = "on vent",              
                                    "Adult Con Pt/Vent InspiratorTV"  = "on vent",              
                                    "NIV Pt/Vent Spont_Rate" = "off vent",                       
                                    "NIV Pt/Vent Spont_TidalV" = "off vent",  
                                    "NIV Pt/Vent SpO2_5" = "off vent",                            
                                    "NIV Setting Set_RR" = "off vent",                           
                                    "NIV Setting Total RR_5" = "off vent",                       
                                    "NIV Setting Spont Exp Vt_5" = "off vent",                    
                                    "NIV Setting Leak_" = "off vent",
                                    "Room air" = "off vent",
                                    "Non-rebreather" = "off vent",
                                    "Nasal cannula" = "off vent",    
                                    "CPAP" = "off vent",
                                    "Mechanical ventilator" = "on vent",
                                    "BiPAP" = "off vent",          
                                    "Venturi mask" = "off vent",
                                    "High flow nasal cannula" = "off vent",
                                    "Aerosol mask" = "off vent",
                                    "Venturi Mask" = "off vent",
                                    "Venti Mask" = "off vent", 
                                    "Face tent" = "off vent", 
                                    "High Flow Nasal Cannula" = "off vent")) %>% 
  mutate(ventstatus = as.character(ventstatus), 
         ventstatus = if_else(respchartvalue == "off" | respchartvalue == "Off" | respchartvalue == "Suspended", "off vent", ventstatus)) %>% 
  mutate(ventstatus = if_else(ventstatus == "on vent", "on vent", 
                              if_else(ventstatus == "off vent", "off vent", ""))) %>% 
  filter(ventstatus != "") %>% 
  distinct(patientunitstayid, respchartoffset, ventstatus, .keep_all = TRUE) %>% 
  group_by(patientunitstayid) %>% 
  mutate(nextchartoffset = lead(respchartoffset)) %>% 
  mutate(nextchartoffset = ifelse(is.na(nextchartoffset), hospitaldischargeoffset, nextchartoffset)) %>% 
  left_join(finaldf %>% dplyr::select(patientunitstayid, time0), by = "patientunitstayid") %>% 
  mutate(mv_baseline = ifelse(time0 >= respchartoffset & time0 <= nextchartoffset & ventstatus == "on vent", "yes", "")) %>% 
  filter(!is.na(time0)) %>% filter(mv_baseline == "yes")
# provides the "on vent" documentation that occurred at time of avp initiation

rm(respchart)

#### Duplicate Patients ####
duplicateicu <- finaldf %>% arrange(uniquepid, patientunitstayid, time0) %>% select(uniquepid, patientunitstayid, time0) %>% 
  filter(!is.na(time0)) %>% filter(duplicated(uniquepid)) %>% mutate(duplicatept = "Yes") 



### Step 4 ####
# Final Dataframe
# Run Below Code for final data frame
finaldf <- inclusionexclusioneicu %>% filter(vasopressors == "vasopressors") %>% 
  select(patientunitstayid, uniquepid, age, timefromshockonset_hr, lactateatavp, abxduringvasoactive) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, gender, ethnicity, unittype, hospitaldischargestatus, unitdischargestatus, 
                                      unitdischargeoffset, hospitaldischargeoffset, weight), by = "patientunitstayid") %>% 
  select(patientunitstayid, uniquepid, age, gender, "race" = "ethnicity", "iculocation" = "unittype", weight, abxduringvasoactive, 
         timefromshockonset_hr, lactateatavp, "icu_los_min" = "unitdischargeoffset", "inhospital_mortality" = "hospitaldischargestatus", 
         "icu_mortality" = "unitdischargestatus", hospitaldischargeoffset) %>% 
  mutate(race = recode(race, "Caucasian" = "White", "Native American" = "Other", "Other/Unknown" = "Other", "Asian" = "Other", 
                       "Hispanic" = "Other", "African American" = "Black", "NA" = "Other"),
         inhospital_mortality = recode(inhospital_mortality, "Alive" = "No", "Expired" = "Yes"),
         iculocation = recode(iculocation, "CCU-CTICU" = "Cardiac ICU",  "CSICU" = "Cardiac ICU", "CTICU" = "Cardiac ICU", 
                              "Med-Surg ICU" = "Mixed ICU", "Neuro ICU" = "NICU"), 
         icu_mortality = recode(icu_mortality, "Alive" = "No", "Expired" = "Yes"), 
         icu_los = round(icu_los_min/1440, 2), 
         abxduringvasoactive = ifelse(is.na(abxduringvasoactive), "No", abxduringvasoactive)) %>%
  mutate(race = replace_na(race, "Other"), # changing unknown race to "Other" 
         icu_mortality = replace_na(icu_mortality, "No")) %>%  # changing NA icu_mortality to No as none had inhospital_mortality
  mutate(inhospital_mortality = ifelse(is.na(inhospital_mortality) & icu_mortality == "Yes", "Yes", 
                                ifelse(icu_mortality == "Yes" & inhospital_mortality == "No", "Yes", inhospital_mortality)),
         inhospital_mortality = replace_na(inhospital_mortality, "Unknown"),
         inhospital_mortality = as.factor(inhospital_mortality)) %>% 
  left_join(pmh1 %>% dplyr::select(patientunitstayid, diabetes) %>% dplyr::filter(diabetes == "Yes"), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(diabetes = ifelse(is.na(diabetes), "No", diabetes)) %>% 
  left_join(pmh1 %>% dplyr::select(patientunitstayid, cirrhosis) %>% dplyr::filter(cirrhosis == "Yes"), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(cirrhosis = ifelse(is.na(cirrhosis), "No", cirrhosis)) %>% 
  left_join(pmh1 %>% dplyr::select(patientunitstayid, immune_suppression) %>% dplyr::filter(immune_suppression == "Yes"), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(immune_suppression = ifelse(is.na(immune_suppression), "No", immune_suppression)) %>% 
  left_join(pmh1 %>% dplyr::select(patientunitstayid, copd) %>% dplyr::filter(copd == "Yes"), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(copd = ifelse(is.na(copd), "No", copd)) %>% 
  left_join(pmh1 %>% dplyr::select(patientunitstayid, dialysis) %>% dplyr::filter(dialysis == "Yes"), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(dialysis = ifelse(is.na(dialysis), "No", dialysis)) %>% 
  left_join(pmh1 %>% dplyr::select(patientunitstayid, no_chronic_health) %>% dplyr::filter(no_chronic_health == "Yes"), by = "patientunitstayid") %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  mutate(no_chronic_health = ifelse(is.na(no_chronic_health), "No", no_chronic_health)) %>% 
  left_join(apacheresults %>% dplyr::select(patientunitstayid, apachescore, apacheversion, unabridgedactualventdays) %>% 
              dplyr::filter(apacheversion == "IV"), by = "patientunitstayid") %>% 
  left_join(vasopressors %>% dplyr::select(patientunitstayid, firstavpstart, firstavpstop, vasoactivestart, vasoactivestop), 
            by = "patientunitstayid") %>% 
  mutate(time0 = firstavpstart, time0_day = floor(firstavpstart/1440), 
         avpduration_days = round(((firstavpstop - firstavpstart)/1440), 2)) %>%
  #time0 = vasopressin initation, time0_day = offsetday of avp initiation
  left_join(avp1 %>% dplyr::select(patientunitstayid, drugrate, status) %>% dplyr::filter(status == "initial start"), 
            by = "patientunitstayid") %>%
  mutate(initialavpdose_category = ifelse(!is.na(firstavpstart) & drugrate == 0.03, "0.03", 
                                   ifelse(!is.na(firstavpstart) & drugrate == 0.04, "0.04", "other"))) %>% 
  mutate(initialavpdose_category = ifelse(is.na(firstavpstart), NA, initialavpdose_category)) %>%
  left_join(vasopressorsverticalneq %>% dplyr::select(patientunitstayid, totalneqdose), by = "patientunitstayid") %>% 
  mutate(totalneqdose = ifelse(is.na(firstavpstart), NA, totalneqdose)) %>%
  distinct(patientunitstayid, .keep_all = TRUE) %>% 
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, neuse), dplyr::filter(neuse == "Yes"), by = "patientunitstayid") %>%
  mutate(neuse = ifelse(is.na(neuse), "No", neuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, epiuse), dplyr::filter(epiuse == "Yes"),by = "patientunitstayid") %>%
  mutate(epiuse = ifelse(is.na(epiuse), "No", epiuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, peuse), dplyr::filter(peuse == "Yes"), by = "patientunitstayid") %>%
  mutate(peuse = ifelse(is.na(peuse), "No", peuse)) %>% 
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, dopuse), dplyr::filter(dopuse == "Yes"), by = "patientunitstayid") %>%
  mutate(dopuse = ifelse(is.na(dopuse), "No", dopuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, dobutuse), dplyr::filter(dobutuse == "Yes"), by = "patientunitstayid") %>%
  mutate(dobutuse = ifelse(is.na(dobutuse), "No", dobutuse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, miluse), dplyr::filter(miluse == "Yes"), by = "patientunitstayid") %>%
  mutate(miluse = ifelse(is.na(miluse), "No", miluse)) %>%
  left_join(vasopressoruse %>% dplyr::select(patientunitstayid, hydrocortuse), dplyr::filter(hydrocortuse == "Yes"), by = "patientunitstayid") %>%
  mutate(hydrocortuse = ifelse(is.na(hydrocortuse), "No", hydrocortuse)) %>%
  left_join(fluidbalance %>% dplyr::select(patientunitstayid, fluidbalance), by = "patientunitstayid") %>% 
  left_join(fluidbolus %>% dplyr::select(patientunitstayid, totalfluidbolus), by = "patientunitstayid") %>%
  mutate(totalfluidbolus = ifelse(is.na(totalfluidbolus), "0", totalfluidbolus)) %>%
  left_join(hds %>% dplyr::select(patientunitstayid, hds), by = "patientunitstayid") %>% 
  left_join(srcr %>% dplyr::select(patientunitstayid, minsrcr), by = "patientunitstayid") %>% 
  
  # AKI
  mutate(mdrdsrcr = ifelse(race == "Black" & gender == "F", (60 / (175 * age^(-0.203) * 1.212 * 0.742))^(1/-1.154), 
                    ifelse(race != "Black" & gender == "F", (60 / (175 * age^(-0.203) * 0.742))^(1/-1.154),
                    ifelse(race != "Black" & gender == "M", (60 / (175 * age^(-0.203)))^(1/-1.154), 
                    ifelse(race == "Black" & gender == "M", (60 / (175 * age^(-0.203) * 1.212))^(1/-1.154),""))))) %>% 
  mutate(mdrdsrcr = as.numeric(mdrdsrcr)) %>%
  mutate(baselinesrcr = ifelse(!is.na(minsrcr), minsrcr, mdrdsrcr)) %>% 
  mutate(baselinesrcr = as.numeric(baselinesrcr)) %>% 
  mutate(baselinesrcr = round(baselinesrcr, 2)) %>% 
  left_join(srcr1 %>% dplyr::select(patientunitstayid, srcravp), by = "patientunitstayid") %>% 
  mutate(akivasostart = ifelse(srcravp >= baselinesrcr*3 | srcravp >= 4, "AKI Stage 3", 
                        ifelse(srcravp >= baselinesrcr*2 & srcravp < baselinesrcr*3, "AKI Stage 2",
                        ifelse((srcravp - baselinesrcr) >= 0.3 | (srcravp >= baselinesrcr*1.5 & srcravp < baselinesrcr*2), "AKI Stage 1", 
                        ifelse(dialysis == "yes", "ESRD", "No AKI"))))) %>%
  mutate(akimva = ifelse(akivasostart == "ESRD" | akivasostart == "No AKI", akivasostart, 
                         ifelse(akivasostart == "AKI Stage 1" | akivasostart == "AKI Stage 2" | akivasostart == "AKI Stage 3", "AKI", ""))) %>% 
  mutate(akivasostart = replace_na(akivasostart, "No AKI"), akimva = replace_na(akimva, "No AKI")) %>% 
  # if NA for AKI, means no SrCr checked before AVP, assumed no AKI in those situations
  mutate(neqdoseatavp_kg = totalneqdose/weight) %>%
  
  # SOFA Score and SOFA Score Change
  left_join(sofa %>% dplyr::select(patientunitstayid, laboffsetday, totalsofa), by = c("patientunitstayid" = "patientunitstayid", "time0_day" = "laboffsetday")) %>% 
  rename(sofa_baseline = totalsofa) %>% 
  mutate(sofa48_offsetday = time0_day + 2) %>% 
  left_join(sofa %>% dplyr::select(patientunitstayid, laboffsetday, totalsofa), by = c("patientunitstayid" = "patientunitstayid", "sofa48_offsetday" = "laboffsetday")) %>% 
  rename(sofa_48hr = totalsofa) %>% 
  mutate(hospitaldischargeoffsetdays = floor(hospitaldischargeoffset/1440)) %>%
  mutate(sofa_baseline = as.numeric(sofa_baseline), sofa_48hr = as.numeric(sofa_48hr)) %>% # only need if pull in SOFA from csv file
  mutate(sofa_48hr = case_when(hospitaldischargeoffsetdays > (time0_day + 2) ~ sofa_48hr, TRUE ~ NA_real_)) %>% 
  mutate(sofa_change = sofa_48hr - sofa_baseline) %>% 
  
  # ICU and vasoactive free days
  mutate(icufreedays = round(((time0 + (28*1440)) - icu_los_min)/1440, 2)) %>% 
  mutate(icufreedays = case_when(inhospital_mortality == "Yes" & (time0 + (28*1440)) >= icu_los_min ~ 0, 
                                 icufreedays > 0 ~ icufreedays, TRUE ~ NA_real_), 
         icufreedays = ifelse(is.na(icufreedays), 0, icufreedays), 
         icufreedays = ifelse(icufreedays > 28, 28, icufreedays)) %>% 
  mutate(vasoactivefreedays = round(((time0 + (28*1440)) - vasoactivestop)/1440, 2)) %>% 
  mutate(vasoactivefreedays = case_when(inhospital_mortality == "Yes" & (time0 + (28*1440)) >= vasoactivestop ~ 0, 
                                        vasoactivefreedays > 0 ~ vasoactivefreedays, TRUE ~ NA_real_), 
         vasoactivefreedays = ifelse(is.na(vasoactivefreedays), 0, vasoactivefreedays), 
         vasoactivefreedays = ifelse(vasoactivefreedays > 28, 28, vasoactivefreedays)) %>% 
  # ICU Free days and Vasoactive Free days calculated as time of last ICU/vasoactive to 28 days in survivors
  left_join(mvbl %>% dplyr::select(patientunitstayid, mv_baseline), by = "patientunitstayid") %>% 
  mutate(mv_baseline = ifelse(is.na(mv_baseline), "No", "Yes")) %>% 
  distinct(patientunitstayid, .keep_all = TRUE) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(fluidbalance = coalesce(fluidbalance,0),
         sofa_baseline = coalesce(sofa_baseline, 0),
         hydrocortuse = coalesce(hydrocortuse, "No"), 
         dobutuse = coalesce(dobutuse, "No"), 
         miluse = coalesce(miluse, "No"),
         mv_baseline = coalesce(mv_baseline, "No"),
         dialysis = coalesce(dialysis, "No"),
         diabetes = coalesce(diabetes, "No"),
         cirrhosis = coalesce(cirrhosis, "No"),
         copd = coalesce(copd, "No"),
         no_chronic_health = coalesce(no_chronic_health, "No"),
         immune_suppression = coalesce(immune_suppression, "No")) %>% 
  mutate(database = "eICU") %>%
  
    # Below adding in screening critiera identifiers
  left_join(inclusionexclusioneicu %>% dplyr::select(patientunitstayid, include_exclude_sepsis3, exclusionreason_sepsis3,
                                                 include_exclude_sepsis3nomap, exclusionreason_sepsis3nomap,
                                                 include_exclude_sepsis3bosch, exclusionreason_sepsis3bosch,
                                                 include_exclude_sepsis3nomapbosch, exclusionreason_sepsis3nomapbosch,
                                                 include_exclude_sscoding, exclusionreason_sscoding, 
                                                 include_exclude_angus, exclusionreason_angus), by = "patientunitstayid") %>%
  
  # selecting out to match eICU dataframe to combine
  select(uniquepid, patientunitstayid, database, time0, age, gender, race, 
         iculocation, weight, diabetes, cirrhosis, immune_suppression, copd, dialysis, no_chronic_health, 
         mv_baseline, "apacheiv" = "apachescore", sofa_baseline, fluidbalance, totalfluidbolus, akivasostart, akimva, vasoactivestart, 
         vasoactivestop, "avpstart" = "firstavpstart", "avpstop" = "firstavpstop", "avpstartdose" = "drugrate",
         "neqdoseatavp" = "totalneqdose", neqdoseatavp_kg, timefromshockonset_hr, lactateatavp, abxduringvasoactive, 
         neuse, epiuse, peuse, "dopamineuse" = "dopuse", dobutuse, "milrinoneuse" = "miluse", hydrocortuse,
         inhospital_mortality, icu_mortality, icufreedays, hds, sofa_48hr, sofa_change, avpduration_days, vasoactivefreedays, 
         include_exclude_sepsis3, exclusionreason_sepsis3, include_exclude_sepsis3nomap, exclusionreason_sepsis3nomap,
         include_exclude_sepsis3bosch, exclusionreason_sepsis3bosch, include_exclude_sepsis3nomapbosch, exclusionreason_sepsis3nomapbosch,
         include_exclude_sscoding, exclusionreason_sscoding, include_exclude_angus, exclusionreason_angus) %>% 
  
  # addressing duplicate patients (need duplicatemimic df below first) 
  left_join(duplicateicu %>% dplyr::select(patientunitstayid, duplicatept), by = "patientunitstayid") %>% 
  left_join(neremovelist %>% dplyr:: select(patientunitstayid, removedfromne), by = "patientunitstayid") %>% 
  mutate(include_exclude_sepsis3 = ifelse(!is.na(duplicatept) & include_exclude_sepsis3 == "include", "exclude", include_exclude_sepsis3),
         exclusionreason_sepsis3= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3), "duplicate patient", exclusionreason_sepsis3)) %>% 
  mutate(include_exclude_sepsis3nomap = ifelse(!is.na(duplicatept) & include_exclude_sepsis3nomap == "include", "exclude", include_exclude_sepsis3nomap),
         exclusionreason_sepsis3nomap= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3nomap), "duplicate patient", exclusionreason_sepsis3nomap)) %>% 
  mutate(include_exclude_sepsis3bosch = ifelse(!is.na(duplicatept) & include_exclude_sepsis3bosch == "include", "exclude", include_exclude_sepsis3bosch),
         exclusionreason_sepsis3bosch= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3bosch), "duplicate patient", exclusionreason_sepsis3bosch)) %>% 
  mutate(include_exclude_sepsis3nomapbosch = ifelse(!is.na(duplicatept) & include_exclude_sepsis3nomapbosch == "include", "exclude", include_exclude_sepsis3nomapbosch),
         exclusionreason_sepsis3nomapbosch= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3nomapbosch), "duplicate patient", exclusionreason_sepsis3nomapbosch)) %>%  
  mutate(include_exclude_sscoding = ifelse(!is.na(duplicatept) & include_exclude_sscoding == "include", "exclude", include_exclude_sscoding),
         exclusionreason_sscoding= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sscoding), "duplicate patient", exclusionreason_sscoding)) %>% 
  mutate(include_exclude_angus = ifelse(!is.na(duplicatept) & include_exclude_angus == "include", "exclude", include_exclude_angus),
         exclusionreason_angus = ifelse(!is.na(duplicatept) & is.na(exclusionreason_angus), "duplicate patient", exclusionreason_angus)) %>% 
  mutate(include_exclude_sepsis3 = ifelse(!is.na(removedfromne) & include_exclude_sepsis3 == "include", "exclude", include_exclude_sepsis3),
         exclusionreason_sepsis3= ifelse(!is.na(removedfromne) & is.na(exclusionreason_sepsis3), "NE dose unreliable", exclusionreason_sepsis3)) %>% 
  mutate(include_exclude_sepsis3nomap = ifelse(!is.na(removedfromne) & include_exclude_sepsis3nomap == "include", "exclude", include_exclude_sepsis3nomap),
         exclusionreason_sepsis3nomap= ifelse(!is.na(removedfromne) & is.na(exclusionreason_sepsis3nomap), "NE dose unreliable", exclusionreason_sepsis3nomap)) %>% 
  mutate(include_exclude_sepsis3bosch = ifelse(!is.na(removedfromne) & include_exclude_sepsis3bosch == "include", "exclude", include_exclude_sepsis3bosch),
         exclusionreason_sepsis3bosch= ifelse(!is.na(removedfromne) & is.na(exclusionreason_sepsis3bosch), "NE dose unreliable", exclusionreason_sepsis3bosch)) %>% 
  mutate(include_exclude_sepsis3nomapbosch = ifelse(!is.na(removedfromne) & include_exclude_sepsis3nomapbosch == "include", "exclude", include_exclude_sepsis3nomapbosch),
         exclusionreason_sepsis3nomapbosch= ifelse(!is.na(removedfromne) & is.na(exclusionreason_sepsis3nomapbosch), "NE dose unreliable", exclusionreason_sepsis3nomapbosch)) %>%  
  mutate(include_exclude_sscoding = ifelse(!is.na(removedfromne) & include_exclude_sscoding == "include", "exclude", include_exclude_sscoding),
         exclusionreason_sscoding= ifelse(!is.na(removedfromne) & is.na(exclusionreason_sscoding), "NE dose unreliable", exclusionreason_sscoding)) %>% 
  mutate(include_exclude_angus = ifelse(!is.na(removedfromne) & include_exclude_angus == "include", "exclude", include_exclude_angus),
         exclusionreason_angus = ifelse(!is.na(removedfromne) & is.na(exclusionreason_angus), "NE dose unreliable", exclusionreason_angus)) %>%
  mutate(include_exclude_sepsis3 = ifelse(inhospital_mortality == "Unknown" & include_exclude_sepsis3 == "include", "exclude", include_exclude_sepsis3),
         exclusionreason_sepsis3= ifelse(inhospital_mortality == "Unknown" & is.na(exclusionreason_sepsis3), "no outcome data", exclusionreason_sepsis3)) %>% 
  mutate(include_exclude_sepsis3nomap = ifelse(inhospital_mortality == "Unknown" & include_exclude_sepsis3nomap == "include", "exclude", include_exclude_sepsis3nomap),
         exclusionreason_sepsis3nomap= ifelse(inhospital_mortality == "Unknown" & is.na(exclusionreason_sepsis3nomap), "no outcome data", exclusionreason_sepsis3nomap)) %>%    
  mutate(include_exclude_sepsis3bosch = ifelse(inhospital_mortality == "Unknown" & include_exclude_sepsis3bosch == "include", "exclude", include_exclude_sepsis3bosch),                  
         exclusionreason_sepsis3bosch= ifelse(inhospital_mortality == "Unknown" & is.na(exclusionreason_sepsis3bosch), "no outcome data", exclusionreason_sepsis3bosch)) %>% 
  mutate(include_exclude_sepsis3nomapbosch = ifelse(inhospital_mortality == "Unknown" & include_exclude_sepsis3nomapbosch == "include", "exclude", include_exclude_sepsis3nomapbosch),
         exclusionreason_sepsis3nomapbosch= ifelse(inhospital_mortality == "Unknown" & is.na(exclusionreason_sepsis3nomapbosch), "no outcome data", exclusionreason_sepsis3nomapbosch)) %>%
  mutate(include_exclude_sscoding = ifelse(inhospital_mortality == "Unknown" & include_exclude_sscoding == "include", "exclude", include_exclude_sscoding),
         exclusionreason_sscoding= ifelse(inhospital_mortality == "Unknown" & is.na(exclusionreason_sscoding), "no outcome data", exclusionreason_sscoding)) %>% 
  mutate(include_exclude_angus = ifelse(inhospital_mortality == "Unknown" & include_exclude_angus == "include", "exclude", include_exclude_angus),
         exclusionreason_angus = ifelse(inhospital_mortality == "Unknown" & is.na(exclusionreason_angus), "no outcome data", exclusionreason_angus)) %>% 
  
    # realized that several patients had neqdoseatavpstart of 0 which was real, excluding them
  mutate(include_exclude_sepsis3 = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3 == "include", "exclude", include_exclude_sepsis3),
         exclusionreason_sepsis3= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3), "NE off at AVP start", exclusionreason_sepsis3)) %>%
  mutate(include_exclude_sepsis3nomap = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3nomap == "include", "exclude", include_exclude_sepsis3nomap),
         exclusionreason_sepsis3nomap= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3nomap), "NE off at AVP start", exclusionreason_sepsis3nomap)) %>%  
  mutate(include_exclude_sepsis3bosch = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3bosch == "include", "exclude", include_exclude_sepsis3bosch),
         exclusionreason_sepsis3bosch= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3bosch), "NE off at AVP start", exclusionreason_sepsis3bosch)) %>%
  mutate(include_exclude_sepsis3nomapbosch = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3nomapbosch == "include", "exclude", include_exclude_sepsis3nomapbosch),
         exclusionreason_sepsis3nomapbosch= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3nomapbosch), "NE off at AVP start", exclusionreason_sepsis3nomapbosch)) %>% 
  mutate(include_exclude_sscoding = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sscoding == "include", "exclude", include_exclude_sscoding),
         exclusionreason_sscoding= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0)& is.na(exclusionreason_sscoding), "NE off at AVP start", exclusionreason_sscoding)) %>% 
  mutate(include_exclude_angus = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0)& include_exclude_angus == "include", "exclude", include_exclude_angus),
         exclusionreason_angus = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_angus), "NE off at AVP start", exclusionreason_angus)) %>% 
  mutate_if(is.character, as.factor) %>%
  select(-duplicatept, -removedfromne) %>%

  # changing patient identifers to combine with eICU
  rename("patientid" = "uniquepid", "hospitalid" = "patientunitstayid")
  
write.csv(finaldf, file = "~/eICU Data/eICU_finaldf.csv", row.names = FALSE)

finaldf %>% tabyl(exclusionreason_sepsis3) %>% adorn_pct_formatting()
finaldf %>% tabyl(exclusionreason_sepsis3nomap) %>% adorn_pct_formatting()
finaldf %>% tabyl(exclusionreason_sepsis3bosch) %>% adorn_pct_formatting()
finaldf %>% tabyl(exclusionreason_sepsis3nomapbosch) %>% adorn_pct_formatting()
finaldf %>% tabyl(exclusionreason_sscoding) %>% adorn_pct_formatting()
finaldf %>% tabyl(exclusionreason_angus) %>% adorn_pct_formatting()
finaldf %>% tabyl(include_exclude_sepsis3) %>% adorn_pct_formatting()
finaldf %>% tabyl(include_exclude_sepsis3nomap) %>% adorn_pct_formatting()
finaldf %>% tabyl(include_exclude_sepsis3bosch) %>% adorn_pct_formatting()
finaldf %>% tabyl(include_exclude_sepsis3nomapbosch) %>% adorn_pct_formatting()
finaldf %>% tabyl(include_exclude_sscoding) %>% adorn_pct_formatting()
finaldf %>% tabyl(include_exclude_angus) %>% adorn_pct_formatting()


# SOFA SCORE ####

#### SOFA Lab Components ####
unique(lab$labname) # 158 unique lab names, look pretty clean (no NA for lab name no duplicate names)

sofarenal <- lab %>% filter(labname == "creatinine") %>%
  mutate(laboffsetday = floor(labresultoffset/1440)) %>% 
  mutate(labname = as.factor(labname)) %>% 
  arrange(patientunitstayid, laboffsetday, labname) %>% 
  group_by(patientunitstayid, labname, laboffsetday) %>% 
  mutate(maxlabresult = max(labresult)) %>% 
  mutate(sofarenal = case_when(labname == "creatinine" & maxlabresult < 1.2 ~ 0,
                               labname == "creatinine" & maxlabresult >= 1.2 & maxlabresult < 2 ~ 1,
                               labname == "creatinine" & maxlabresult >= 2 & maxlabresult < 3.5 ~ 2,
                               labname == "creatinine" & maxlabresult >= 3.5 & maxlabresult < 5 ~ 3,
                               labname == "creatinine" & maxlabresult >= 5 ~ 4,
                               TRUE ~ NA_real_)) %>%
  rename(labnamesrcr = labname, creatinine = labresult, maxsrcr = maxlabresult, srcroffset = labresultoffset) %>% 
  select(patientunitstayid, labnamesrcr, creatinine, maxsrcr, srcroffset, laboffsetday, sofarenal) %>% 
  distinct(patientunitstayid, labnamesrcr, laboffsetday, .keep_all = TRUE)

sofaliver <- lab %>% filter(labname == "total bilirubin") %>% 
  mutate(laboffsetday = floor(labresultoffset/1440)) %>% 
  mutate(labname = as.factor(labname)) %>% 
  arrange(patientunitstayid, laboffsetday, labname) %>% 
  group_by(patientunitstayid, labname, laboffsetday) %>% 
  mutate(maxlabresult = max(labresult)) %>% 
  mutate(sofaliver = case_when(labname == "total bilirubin" & maxlabresult < 1.2 ~ 0,
                               labname == "total bilirubin" & maxlabresult >= 1.2 & maxlabresult < 2 ~ 1,
                               labname == "total bilirubin" & maxlabresult >= 2 & maxlabresult < 6 ~ 2,
                               labname == "total bilirubin" & maxlabresult >= 6 & maxlabresult < 12 ~ 3,
                               labname == "total bilirubin" & maxlabresult >= 12 ~ 4,
                               TRUE ~ NA_real_)) %>%
  rename(labnamebili = labname, tbili = labresult, maxbili = maxlabresult, bilioffset = labresultoffset) %>% 
  select(patientunitstayid, labnamebili, tbili, maxbili, bilioffset, laboffsetday, sofaliver) %>% 
  distinct(patientunitstayid, labnamebili, laboffsetday, .keep_all = TRUE)

sofacoag <- lab %>% filter(labname == "platelets x 1000") %>% 
  mutate(laboffsetday = floor(labresultoffset/1440)) %>% 
  mutate(labname = as.factor(labname)) %>% 
  arrange(patientunitstayid, laboffsetday, labname) %>% 
  group_by(patientunitstayid, labname, laboffsetday) %>%
  mutate(minlabresult = min(labresult)) %>% 
  mutate(sofacoag = case_when(labname == "platelets x 1000" & minlabresult >= 150 ~ 0,
                              labname == "platelets x 1000" & minlabresult >= 100 & minlabresult < 150 ~ 1,
                              labname == "platelets x 1000" & minlabresult >= 50 & minlabresult < 100 ~ 2,
                              labname == "platelets x 1000" & minlabresult >= 20 & minlabresult < 50 ~ 3,
                              labname == "platelets x 1000" & minlabresult < 20 ~ 4,
                              TRUE ~ NA_real_)) %>%
  rename(labnameplt = labname, platelet = labresult, minplt = minlabresult, pltoffset = labresultoffset) %>% 
  select(patientunitstayid, labnameplt, platelet, minplt, pltoffset, laboffsetday, sofacoag) %>% 
  distinct(patientunitstayid, labnameplt, laboffsetday, .keep_all = TRUE)

#### PF Ratio ####

library(stringr)
pf <- lab %>% mutate(keep = ifelse(labname == "FiO2" | labname == "paO2", "KEEP","DELETE")) %>%
  filter(keep == "KEEP") %>% 
  select(-keep) %>% 
  mutate(laboffsetday = floor(labresultoffset/1440)) %>% 
  mutate(labresulttext = str_remove(labresulttext, "%")) %>% 
  mutate(labresulttext = str_remove(labresulttext, ">")) %>% 
  mutate(labresulttext = str_remove(labresulttext, "<")) %>% 
  mutate(labresult = ifelse(is.na(labresult), labresulttext, labresult)) %>% 
  mutate(labresult = as.numeric(labresult)) %>% 
  filter(!is.na(labresult)) %>% 
  select(patientunitstayid, labname, labresult, labresultoffset, laboffsetday)
#cleaning up the fio2/pao2 data form the lab dataset

pf2 <- respchart %>% mutate(keep = ifelse(respchartvaluelabel == "FiO2" | respchartvaluelabel == "FIO2 (%)" | respchartvaluelabel == "Set Fraction of Inspired Oxygen (FIO2)", "KEEP","DELETE")) %>%
  filter(keep == "KEEP") %>% 
  select(-keep) %>% 
  rename(labresultoffset = respchartoffset, labname = respchartvaluelabel, labresult = respchartvalue) %>%  
  mutate(laboffsetday = floor(labresultoffset/1440)) %>% 
  mutate(labresult = as.numeric(labresult)) %>% 
  select(patientunitstayid, labname, labresult, labresultoffset, laboffsetday)
#cleaning up fio2 data from the respchart dataset

pf3 <- rbind(pf, pf2) # combining the two datasets

pf3 <- pf3 %>% mutate(labname = as.factor(labname)) %>% mutate(labname = recode_factor(labname, "FIO2 (%)" = "FiO2", "Set Fraction of Inspired Oxygen (FIO2)" = "FiO2"))
# cleaning up the names of the fio2 values

pao2 <- pf3 %>% filter(labname == "paO2") # separating out pao2 values

fiO2 <- pf3 %>% filter(labname == "FiO2") # separating out fio2 values

fiO2 <- fiO2 %>% filter(labresult <= 100) %>% filter(labresult >= 21) %>% filter(!is.na(labresult)) %>% 
  mutate(labresult = ifelse(labresult < 1, labresult * 100, labresult)) %>% 
  arrange(patientunitstayid, laboffsetday, labname) %>% 
  group_by(patientunitstayid, laboffsetday) %>% 
  mutate(maxlabresult = max(labresult)) %>% 
  distinct(patientunitstayid, labname, laboffsetday, .keep_all = TRUE)
# cleaning fio2 to remove vales < 21% and > 100% and transforming all data < 1 to the % value
# also calculating the max fio2 on each offset day

pao2 <- pao2 %>% arrange(patientunitstayid, laboffsetday, labname) %>% 
  group_by(patientunitstayid, laboffsetday) %>% 
  mutate(minlabresult = min(labresult)) %>%
  distinct(patientunitstayid, labname, laboffsetday, .keep_all = TRUE)
# calculating min pao2 value on each offset day

#### Mechanical Ventilation Times ####
# respiratoryCharting table
# unique(respchart1$respchartvaluelabel) # 194 to clean through
respchart1 <- respchart %>% arrange(patientunitstayid, respchartoffset) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, hospitaldischargeoffset, hospitaldischargestatus), by = "patientunitstayid") %>% 
  left_join(apacheresults %>% dplyr::select(patientunitstayid, unabridgedactualventdays), by = "patientunitstayid") %>% 
  distinct() %>% 
  mutate(ventstatus = respchartvaluelabel) %>% 
  mutate(ventstatus = if_else(ventstatus == "Oxygen Delivery Method", respchartvalue, ventstatus)) %>% 
  mutate(ventstatus = as.factor(ventstatus), 
         ventstatus = recode_factor(ventstatus, "RT Vent On/Off" = "on vent", 
                                    "Vent Rate" = "on vent", 
                                    "Tidal Volume (set)" = "on vent", 
                                    "TV/kg IBW"= "on vent", 
                                    "Tidal Volume Observed (VT)" = "on vent", 
                                    "PEEP" = "on vent", 
                                    "Plateau Pressure" = "on vent", 
                                    "Pressure Support" = "on vent",
                                    "Ventilator Type" = "on vent", 
                                    "LPM O2" = "off vent", 
                                    "Pressure Control" = "on vent", 
                                    "Peak Insp. Pressure" = "on vent", 
                                    "Endotracheal Position at Lip" = "on vent",
                                    "Endotracheal Tube Placement" = "on vent", 
                                    "Tube Size" = "on vent", 
                                    "Exhaled TV (patient)" = "on vent",
                                    "Mean Airway Pressure" = "on vent", 
                                    "Ventilator Heater Temperature" = "on vent",
                                    "Compliance" = "on vent", 
                                    "Exhaled TV (machine)" = "on vent", 
                                    "Pressure to Trigger PS" = "on vent",    
                                    "Minute Volume, Spontaneous" = "on vent", 
                                    "Inspiratory Pressure, Set" = "on vent", 
                                    "Inspiratory Flow Rate" = "on vent", 
                                    "Insp Flow (l/min)" = "on vent",  
                                    "Bipap Delivery Mode" = "off vent", 
                                    "Non-invasive Ventilation Mode" = "off vent",
                                    "Mechanical Ventilator Compliance" = "on vent", 
                                    "Endotracheal Tube Placement Checked" = "on vent",
                                    "Mechanical Ventilator Mode" = "on vent", 
                                    "Ventilator Support Mode" = "on vent", 
                                    "Peak Pressure" = "on vent",
                                    "ET Tube Repositioned" = "on vent", 
                                    "Position at lip" = "on vent", 
                                    "Postion at Lip" = "on vent",
                                    "Trachestomy Tube Size" = "on vent", 
                                    "Tracheostomy Type" = "on vent", 
                                    "NIV Setting EPAP" = "off vent",
                                    "Adult Con Pt/Vent Spont Rate" = "on vent",
                                    "Adult Con Pt/Vent MinuteVentil" = "on vent",              
                                    "Adult Con Pt/Vent InspiratorTV"  = "on vent",              
                                    "NIV Pt/Vent Spont_Rate" = "off vent",                       
                                    "NIV Pt/Vent Spont_TidalV" = "off vent",  
                                    "NIV Pt/Vent SpO2_5" = "off vent",                            
                                    "NIV Setting Set_RR" = "off vent",                           
                                    "NIV Setting Total RR_5" = "off vent",                       
                                    "NIV Setting Spont Exp Vt_5" = "off vent",                    
                                    "NIV Setting Leak_" = "off vent",
                                    "Room air" = "off vent",
                                    "Non-rebreather" = "off vent",
                                    "Nasal cannula" = "off vent",    
                                    "CPAP" = "off vent",
                                    "Mechanical ventilator" = "on vent",
                                    "BiPAP" = "off vent",          
                                    "Venturi mask" = "off vent",
                                    "High flow nasal cannula" = "off vent",
                                    "Aerosol mask" = "off vent",
                                    "Venturi Mask" = "off vent",
                                    "Venti Mask" = "off vent", 
                                    "Face tent" = "off vent", 
                                    "High Flow Nasal Cannula" = "off vent")) %>% 
  mutate(ventstatus = as.character(ventstatus), 
         ventstatus = if_else(respchartvalue == "off" | respchartvalue == "Off" | respchartvalue == "Suspended", "off vent", ventstatus)) %>% 
  mutate(ventstatus = if_else(ventstatus == "on vent", "on vent", 
                      if_else(ventstatus == "off vent", "off vent", ""))) %>% 
  filter(ventstatus == "on vent") %>% 
  mutate(respsofaoffsetday = floor(respchartoffset/1440)) %>% 
  distinct(patientunitstayid, respsofaoffsetday, .keep_all = TRUE)
# this just results in offsetday where patient was on the vent 

sofaresp <- fiO2 %>% rename(labnamefio2 = labname, fio2 = labresult, maxfio2 = maxlabresult, fio2offset = labresultoffset) %>% 
  full_join(pao2, by = c("patientunitstayid" = "patientunitstayid", "laboffsetday" = "laboffsetday")) %>% 
  rename(labnamepao2 = labname, pao2 = labresult, minpao2 = minlabresult, pao2offset = labresultoffset) %>% 
  mutate(pfratio = round(minpao2 / (maxfio2 / 100), 0)) %>% 
  full_join(respchart1 %>% dplyr::select(patientunitstayid, respsofaoffsetday, ventstatus), 
            by = c("patientunitstayid" = "patientunitstayid", "laboffsetday" = "respsofaoffsetday")) %>% 
  mutate(sofaresp = case_when(pfratio >= 400 ~ 0,
                              pfratio <400 & pfratio >= 300 ~ 1,
                              pfratio <300 & pfratio >= 200  ~ 2,
                              ventstatus == NA & pfratio <= 200 ~ 2,
                              ventstatus == "on vent" & pfratio < 200 & pfratio >= 100 ~ 3,
                              ventstatus == "on vent" & pfratio < 100 ~ 4,
                              TRUE ~ NA_real_)) %>% 
  mutate(sofaresp = if_else(is.na(sofaresp), 0, sofaresp))
# combining by pt identifier and offset day to calculate the pf ratio on each offset day (when able)

rm(pf, pf2, pf3, pao2, fiO2)

#### Cardiac SOFA Score ####

# NE 
nemax <- ne %>% mutate(infusionoffsetday = floor(infusionoffset/1440)) %>% 
  arrange(patientunitstayid, infusionoffsetday) %>%
  group_by(patientunitstayid, infusionoffsetday) %>% 
  mutate(maxnerate = max(drugrate)) %>%
  select(patientunitstayid, infusionoffsetday, drugname, maxnerate) %>%
  distinct(patientunitstayid, infusionoffsetday, .keep_all = TRUE)

# Epi
epimax <- epi %>% mutate(infusionoffsetday = floor(infusionoffset/1440)) %>% 
  arrange(patientunitstayid, infusionoffsetday) %>%
  group_by(patientunitstayid, infusionoffsetday) %>% 
  mutate(maxepirate = max(drugrate)) %>%
  select(patientunitstayid, infusionoffsetday, drugname, maxepirate) %>%
  distinct(patientunitstayid, infusionoffsetday, .keep_all = TRUE)

# PE
pemax <- pe %>% mutate(infusionoffsetday = floor(infusionoffset/1440)) %>% 
  arrange(patientunitstayid, infusionoffsetday) %>%
  group_by(patientunitstayid, infusionoffsetday) %>% 
  mutate(maxperate = max(drugrate)) %>%
  select(patientunitstayid, infusionoffsetday, drugname, maxperate) %>%
  distinct(patientunitstayid, infusionoffsetday, .keep_all = TRUE)

# Dopamine
dopmax <- dop %>% mutate(infusionoffsetday = floor(infusionoffset/1440)) %>% 
  arrange(patientunitstayid, infusionoffsetday) %>%
  group_by(patientunitstayid, infusionoffsetday) %>% 
  mutate(maxdoprate = max(drugrate)) %>%
  select(patientunitstayid, infusionoffsetday, drugname, maxdoprate) %>%
  distinct(patientunitstayid, infusionoffsetday, .keep_all = TRUE)

# Dobutamine
dobutmax <- dobut %>% mutate(infusionoffsetday = floor(infusionoffset/1440)) %>% 
  arrange(patientunitstayid, infusionoffsetday) %>%
  group_by(patientunitstayid, infusionoffsetday) %>% 
  mutate(maxdobutrate = max(drugrate)) %>%
  select(patientunitstayid, infusionoffsetday, drugname, maxdobutrate) %>%
  distinct(patientunitstayid, infusionoffsetday, .keep_all = TRUE)

# Combined Vasopressors
sofacardiac <- nemax %>% full_join(epimax, by = c("patientunitstayid" = "patientunitstayid", "infusionoffsetday" = "infusionoffsetday")) %>% 
  full_join(pemax, by = c("patientunitstayid" = "patientunitstayid", "infusionoffsetday" = "infusionoffsetday")) %>%
  full_join(dopmax, by = c("patientunitstayid" = "patientunitstayid", "infusionoffsetday" = "infusionoffsetday")) %>% 
  full_join(dobutmax, by = c("patientunitstayid" = "patientunitstayid", "infusionoffsetday" = "infusionoffsetday"))%>% 
  select(patientunitstayid, infusionoffsetday, maxnerate, maxepirate, maxperate, maxdoprate, maxdobutrate) %>% 
  mutate(zero = 0) %>% 
  mutate(maxnerate = ifelse(is.na(maxnerate), zero, maxnerate))%>% 
  mutate(maxepirate = ifelse(is.na(maxepirate), zero, maxepirate))%>%
  mutate(maxperate = ifelse(is.na(maxperate), zero, maxperate))%>%
  mutate(maxdoprate = ifelse(is.na(maxdoprate), zero, maxdoprate)) %>%
  mutate(maxdobutrate = ifelse(is.na(maxdobutrate), zero, maxdobutrate)) %>% 
  select(-zero) %>% 
  mutate(maxvasodose = (maxnerate + maxepirate + (maxperate / 10) + (maxdoprate / 2))) %>% 
  left_join(patient %>% dplyr::select(patientunitstayid, weight)) %>% 
  mutate(weight = ifelse(is.na(weight), 70, weight)) %>% # if patient did not have weight, imputed 70 kg 
  mutate(vasowtdose = (maxvasodose/weight))


# MAP values (stole code to create MAP df from Sepsis-3 criteria section)
# df of just invasive BPs
imap <- vitalperiodic %>% filter(!is.na(systemicmean)) %>%
  mutate(source = "invasive") %>% 
  dplyr::select(patientunitstayid, observationoffset, systemicsystolic, systemicdiastolic,
                systemicmean, source) %>%
  rename(sbp = systemicsystolic, dbp = systemicdiastolic, map = systemicmean)
# df of just non-invasive BPs
nimap <- vitalaperiodic %>% filter(!is.na(noninvasivemean)) %>%
  mutate(source = "noninvasive") %>% 
  dplyr::select(patientunitstayid, observationoffset, noninvasivesystolic, noninvasivediastolic, 
                noninvasivemean, source) %>%
  rename(sbp = noninvasivesystolic, dbp = noninvasivediastolic, map = noninvasivemean)
# Combine invasive and non-invasive 
map <- rbind(imap, nimap)
rm(imap, nimap)

# Get minimum MAP on each offset day
minmap <- map %>% mutate(observationoffsetday = floor(observationoffset/1440)) %>% 
  arrange(patientunitstayid, observationoffsetday) %>% 
  group_by(patientunitstayid, observationoffsetday) %>% 
  mutate(minmap = min(map)) %>% 
  select(patientunitstayid, observationoffsetday, minmap) %>% 
  distinct(patientunitstayid, observationoffsetday, .keep_all = TRUE)

# Combine max vasopressors and min MAP and calculating score
sofacardiac <- sofacardiac %>% 
  full_join(minmap, by = c("patientunitstayid" = "patientunitstayid", "infusionoffsetday"= "observationoffsetday")) %>% 
  mutate(vasowtdose = if_else(is.na(vasowtdose), 0, vasowtdose)) %>% 
  mutate(vasodata = if_else(vasowtdose == 0, 1, 2)) %>% 
  mutate(vasodata = as.factor(vasodata)) %>% 
  mutate(vasodata = recode_factor(vasodata,  "1" = "no vasopressor", "2" = "vasopressors")) %>% 
  arrange(patientunitstayid, infusionoffsetday) %>% 
  mutate(sofacardiac = case_when(vasowtdose > 0.1 ~ 4, 
                                 vasowtdose <= 0.1 & vasowtdose > 0 ~ 3,
                                 vasodata == "no vasopressor" & maxdobutrate > 0 ~ 2,
                                 vasodata == "no vasopressor" & minmap < 70 ~ 1, 
                                 vasodata == "no vasopressor" & minmap >= 70 ~ 0,
                                 TRUE ~ NA_real_))

rm(minmap, nemax, epimax, pemax, dobutmax, dopmax)

#### Total SOFA Score #### 
sofa <- sofarenal %>% full_join(sofaliver, by = c("patientunitstayid" = "patientunitstayid", "laboffsetday" = "laboffsetday")) %>%
  full_join(sofacoag, by = c("patientunitstayid" = "patientunitstayid", "laboffsetday" = "laboffsetday")) %>% 
  full_join(sofaresp, by = c("patientunitstayid" = "patientunitstayid", "laboffsetday" = "laboffsetday")) %>% 
  full_join(sofacardiac, by = c("patientunitstayid" = "patientunitstayid", "laboffsetday" = "infusionoffsetday")) %>% 
  mutate(sofarenal = ifelse(is.na(sofarenal), 0, sofarenal)) %>%
  mutate(sofaliver = ifelse(is.na(sofaliver), 0, sofaliver)) %>% 
  mutate(sofacoag = ifelse(is.na(sofacoag), 0, sofacoag)) %>% 
  mutate(sofacardiac = ifelse(is.na(sofacardiac), 0, sofacardiac)) %>%
  mutate(sofaresp = ifelse(is.na(sofaresp), 0, sofaresp)) %>% 
  mutate(totalsofa = sofarenal + sofaliver + sofacoag + sofacardiac + sofaresp)

rm(sofarenal, sofaliver, sofacoag, sofaresp, sofacardiac)

# saving final SOFA scores
write.csv(sofa, file = "eICU Data/eicu_sofascores.csv", row.names = FALSE)
sofa <- read.csv("~/eICU Data/eicu_sofascores.csv")