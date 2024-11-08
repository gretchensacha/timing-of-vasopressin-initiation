
# MIMIC-IV Database ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PACKAGES 
library(readr)
library(readxl)
library(writexl)
library(lubridate)
library(conflicted)
library(tidyverse)
library(vroom)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::recode)
conflicts_prefer(dplyr::rename)
conflicts_prefer(base::sum)
conflicts_prefer(base::min)
conflicts_prefer(base::max)

# Information: https://mimic.mit.edu/

## Importing Data ####
# Run below, uncommented code, to get to the finaldf 

inputevents <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/icu/inputevents.csv.gz")
outputevents <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/icu/outputevents.csv.gz")
emar <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/hosp/emar.csv.gz")
  emar_detail <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/hosp/emar_detail.csv.gz")
#  pharmacy <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/hosp/pharmacy.csv.gz")
#  prescriptions <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/hosp/prescriptions.csv.gz")
# patients <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/hosp/patients.csv.gz")

# Chart Events (includes vital signs and laboratory values - need all three lines below)
# d_items <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/icu/d_items.csv.gz")
# chartevents <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/icu/chartevents.csv.gz")
# chartevents <- chartevents %>% left_join(d_items %>% dplyr::select(itemid, label, abbreviation, linksto, category, unitname), by = "itemid")
# rm(d_items)

# Laboratory Values (need all three lines below)
# d_labitems <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/hosp/d_labitems.csv.gz")
# labevents <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/hosp/labevents.csv.gz")
# labevents <- labevents %>% left_join(d_labitems %>% dplyr::select(itemid, label, category), by = "itemid")
# rm(d_labitems)

# Diagnosis codes
# d_icd_diagnoses <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/hosp/d_icd_diagnoses.csv.gz")
# diagnoses_icd <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/hosp/diagnoses_icd.csv.gz") %>% 
#  left_join(d_icd_diagnoses %>% dplyr::select(icd_code, long_title), by = "icd_code")

# Other Datasets
admissions <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/hosp/admissions.csv.gz")
icustays <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/icu/icustays.csv.gz")
procedureevents <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/icu/procedureevents.csv.gz")

# Notes - to get PMH
discharge <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/mimic-iv-note-deidentified-free-text-clinical-notes-2.2/note/discharge.csv.gz")

# Need infusion for vasopressors and crystalloids
items <- read_csv("~/MIMIC-IV Data/mimic-iv-2.2/icu/d_items.csv.gz")
infusion <- inputevents %>% left_join(items %>% dplyr::select(itemid, label), by = "itemid")
rm(items)

# ---------------------- #
# VASOPRESSOR DATA ####
# ---------------------- #

### Vasopressin #### 

avp <- infusion %>% filter(grepl("vasopressin", label, ignore.case = TRUE))
# 27134 lines of data
# 4284 unique subject_id
# over 8900 different dosing rates
# only drug name == "Vasopressin"
# rates noted as units/min and units/hr - just have to clean up units/hour
# after cleaning and converting units/hr to units/min there are < 60 possible rates
# only 16 subject_ids have vasopressin rate > 1.0 (have to leave these)

avp <- infusion %>% filter(grepl("vasopressin", label, ignore.case = TRUE)) %>% 
  mutate(rate = ifelse(rateuom == "units/hour", round(rate/60, 3), round(rate, 3))) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(rate_weightbased = NA) %>%
  group_by(hadm_id) %>% arrange(subject_id, starttime) %>% 
  mutate(priorrate = lag(rate)) %>% # getting the prior rate 
  mutate(nextrate = lead(rate)) %>% # getting next infusion rate
  mutate(statusatstart = lag(statusdescription)) %>% 
  mutate(infusionoffsetstop = lead(starttime)) %>%
  mutate(priorinfusionoffset = lag(endtime)) %>% 
  mutate(timetorestart = difftime(infusionoffsetstop, endtime, units = "min")) %>%
  mutate(timesincestop = lag(timetorestart)) %>% 
  mutate(status = ifelse(is.na(timesincestop) | is.na(priorrate), "initial start", 
                         ifelse(timetorestart >= 1440 & timesincestop < 1440, "stopped", 
                                ifelse(timesincestop >= 1440, "restarted",
                                       ifelse(is.na(nextrate), "final stop", ""))))) %>% 
  mutate(status = replace_na(status, "final stop")) %>% 
  group_by(hadm_id, status) %>% mutate(count = 1:n()) %>% ungroup() %>% # sequentially counting each status to get 1st, 2nd, 3rd stop/restart
  select(subject_id, hadm_id, stay_id, starttime, statusatstart, endtime, statusdescription, rate, rate_weightbased, rateuom, patientweight, label, priorrate, 
         nextrate, infusionoffsetstop, priorinfusionoffset, timetorestart, timesincestop, status, count)

### Norepinephrine #### 

ne <- infusion %>% filter(grepl("norepinephrine", label, ignore.case = TRUE))
# unique(ne$rateuom) only mcg/kg/min and mg/kg/min but only 2 mg/kg/min and look to be mcg/kg/min

ne <- infusion %>% filter(grepl("norepinephrine", label, ignore.case = TRUE)) %>% 
  mutate(rate_weightbased = round(rate, 3), rate = round((rate * patientweight),2)) %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(hadm_id) %>% arrange(subject_id, starttime) %>% 
  mutate(priorrate = lag(rate)) %>% # getting the prior rate 
  mutate(nextrate = lead(rate)) %>% # getting next infusion rate
  mutate(statusatstart = lag(statusdescription)) %>% 
  mutate(infusionoffsetstop = lead(starttime)) %>%
  mutate(priorinfusionoffset = lag(endtime)) %>% 
  mutate(timetorestart = difftime(infusionoffsetstop, endtime, units = "min")) %>%
  mutate(timesincestop = lag(timetorestart)) %>% 
  mutate(status = ifelse(is.na(timesincestop) | is.na(priorrate), "initial start", 
                  ifelse(timetorestart >= 1440 & timesincestop < 1440, "stopped", 
                  ifelse(timesincestop >= 1440, "restarted",
                  ifelse(is.na(nextrate), "final stop", ""))))) %>% 
  mutate(status = replace_na(status, "final stop")) %>% 
  group_by(hadm_id, status) %>% mutate(count = 1:n()) %>% ungroup() %>% # sequentially counting each status to get 1st, 2nd, 3rd stop/restart
  select(subject_id, hadm_id, stay_id, starttime, statusatstart, endtime, statusdescription, rate, rate_weightbased, rateuom, patientweight, label, priorrate, 
         nextrate, infusionoffsetstop, priorinfusionoffset, timetorestart, timesincestop, status, count)

### Epinephrine #### 

epi <- infusion %>% filter(grepl("epinephrine", label, ignore.case = TRUE)) %>% filter(label != "Norepinephrine") %>% filter(!is.na(rateuom))
# unique(epi$rateuom) # only mcg/kg/min and NA -> all NAs are bolus push epi and need to be removed

epi <- infusion %>% filter(grepl("epinephrine", label, ignore.case = TRUE)) %>% filter(label != "Norepinephrine") %>% filter(!is.na(rateuom)) %>% 
  mutate(rate_weightbased = round(rate, 3), rate = round((rate * patientweight),2)) %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(hadm_id) %>% arrange(subject_id, starttime) %>% 
  mutate(priorrate = lag(rate)) %>% # getting the prior rate 
  mutate(nextrate = lead(rate)) %>% # getting next infusion rate
  mutate(statusatstart = lag(statusdescription)) %>% 
  mutate(infusionoffsetstop = lead(starttime)) %>%
  mutate(priorinfusionoffset = lag(endtime)) %>% 
  mutate(timetorestart = difftime(infusionoffsetstop, endtime, units = "min")) %>%
  mutate(timesincestop = lag(timetorestart)) %>% 
  mutate(status = ifelse(is.na(timesincestop) | is.na(priorrate), "initial start", 
                         ifelse(timetorestart >= 1440 & timesincestop < 1440, "stopped", 
                                ifelse(timesincestop >= 1440, "restarted",
                                       ifelse(is.na(nextrate), "final stop", ""))))) %>% 
  mutate(status = replace_na(status, "final stop")) %>% 
  group_by(hadm_id, status) %>% mutate(count = 1:n()) %>% ungroup() %>% # sequentially counting each status to get 1st, 2nd, 3rd stop/restart
  select(subject_id, hadm_id, stay_id, starttime, statusatstart, endtime, statusdescription, rate, rate_weightbased, rateuom, patientweight, label, priorrate, 
         nextrate, infusionoffsetstop, priorinfusionoffset, timetorestart, timesincestop, status, count)

### Phenylephrine #### 

pe <- infusion %>% filter(grepl("phenylephrine", label, ignore.case = TRUE))
# unique(pe$rateuom) # only mcg/kg/min and mcg/min - only 1 mcg/min and truly does look mcg/min have to convert this one to weight based

pe <- infusion %>% filter(grepl("phenylephrine", label, ignore.case = TRUE)) %>% 
  mutate(label = "Phenylephrine") %>% 
  mutate(rate = ifelse(rateuom == "mcg/min", rate/patientweight, rate)) %>% 
  mutate(rate_weightbased = round(rate, 3), rate = round((rate * patientweight),2)) %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(hadm_id) %>% arrange(subject_id, starttime) %>% 
  mutate(priorrate = lag(rate)) %>% # getting the prior rate 
  mutate(nextrate = lead(rate)) %>% # getting next infusion rate
  mutate(statusatstart = lag(statusdescription)) %>% 
  mutate(infusionoffsetstop = lead(starttime)) %>%
  mutate(priorinfusionoffset = lag(endtime)) %>% 
  mutate(timetorestart = difftime(infusionoffsetstop, endtime, units = "min")) %>%
  mutate(timesincestop = lag(timetorestart)) %>% 
  mutate(status = ifelse(is.na(timesincestop) | is.na(priorrate), "initial start", 
                         ifelse(timetorestart >= 1440 & timesincestop < 1440, "stopped", 
                                ifelse(timesincestop >= 1440, "restarted",
                                       ifelse(is.na(nextrate), "final stop", ""))))) %>% 
  mutate(status = replace_na(status, "final stop")) %>% 
  group_by(hadm_id, status) %>% mutate(count = 1:n()) %>% ungroup() %>% # sequentially counting each status to get 1st, 2nd, 3rd stop/restart
  select(subject_id, hadm_id, stay_id, starttime, statusatstart, endtime, statusdescription, rate, rate_weightbased, rateuom, patientweight, label, priorrate, 
         nextrate, infusionoffsetstop, priorinfusionoffset, timetorestart, timesincestop, status, count)

### Dopamine #### 

dop <- infusion %>% filter(grepl("dopamine", label, ignore.case = TRUE))
# unique(dop$rateuom) # only mcg/kg/min - no manipulation needed

dop <- infusion %>% filter(grepl("dopamine", label, ignore.case = TRUE)) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(rate = round(rate, 3)) %>% 
  mutate(rate_weightbased = rate) %>% 
  group_by(hadm_id) %>% arrange(subject_id, starttime) %>% 
  mutate(priorrate = lag(rate)) %>% # getting the prior rate 
  mutate(nextrate = lead(rate)) %>% # getting next infusion rate
  mutate(statusatstart = lag(statusdescription)) %>% 
  mutate(infusionoffsetstop = lead(starttime)) %>%
  mutate(priorinfusionoffset = lag(endtime)) %>% 
  mutate(timetorestart = difftime(infusionoffsetstop, endtime, units = "min")) %>%
  mutate(timesincestop = lag(timetorestart)) %>% 
  mutate(status = ifelse(is.na(timesincestop) | is.na(priorrate), "initial start", 
                         ifelse(timetorestart >= 1440 & timesincestop < 1440, "stopped", 
                                ifelse(timesincestop >= 1440, "restarted",
                                       ifelse(is.na(nextrate), "final stop", ""))))) %>% 
  mutate(status = replace_na(status, "final stop")) %>% 
  group_by(hadm_id, status) %>% mutate(count = 1:n()) %>% ungroup() %>% # sequentially counting each status to get 1st, 2nd, 3rd stop/restart
  select(subject_id, hadm_id, stay_id, starttime, statusatstart, endtime, statusdescription, rate, rate_weightbased, rateuom, patientweight, label, priorrate, 
         nextrate, infusionoffsetstop, priorinfusionoffset, timetorestart, timesincestop, status, count)


### Vasopressor DF ####

# Vertical vasopressor DF 
# Combining all vasopressors into one df will all initiation and DC times
# NE: most was 18 start/stops
# AVP: most was 8 start/stop
# EPI: most was 4 start/stop
# PE: most was 16 start/stop
# DOP: most was 5 start/stop

vasopressors <- rbind(ne, avp, epi, pe, dop) %>% filter(status != "") %>% 
  select(subject_id, hadm_id, stay_id, label, starttime, endtime, status) %>% 
  group_by(hadm_id, label) %>% 
  mutate(nextstatus = lead(status)) %>% 
  mutate(nextendtime = lead(endtime)) %>% 
  mutate(realendtime = if_else((status == "initial start" | status == "restarted") & (nextstatus == "stopped" | nextstatus == "final stop"), 
                               nextendtime, endtime)) %>% 
  mutate(realendtime = if_else(is.na(realendtime), endtime, realendtime)) %>% 
  filter(status == "initial start" | status == "restarted") %>% 
  group_by(hadm_id, label) %>% 
  mutate(regimen = 1:n()) %>% 
  ungroup() %>% 
  select(subject_id, hadm_id, stay_id,label, status, starttime, "endtime" = "realendtime", regimen) %>% 
  group_by(subject_id, hadm_id) %>% 
  mutate(vasoactivestart = min(starttime)) %>% 
  mutate(vasoactivestop = max(endtime)) %>%  
  arrange(subject_id, hadm_id, starttime)


# run above first to get all vasoactive start/stop with 24 hour period in between start/stop
# below adds first vasopressin start/stop to the df 
vasopressors <- vasopressors %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, label, starttime, endtime, regimen) %>% 
              dplyr::filter(label == "Vasopressin", regimen == 1), by = "hadm_id") %>%
  select("subject_id" = "subject_id.x", hadm_id, stay_id, vasoactivestart, vasoactivestop, "firstavpstart" = "starttime.y", "firstavpstop" = "endtime.y", 
         "label" = "label.x", status, "starttime" = "starttime.x", "endtime" = "endtime.x", "regimen" = "regimen.x") %>% 
  arrange(subject_id, starttime)

### Dobutamine ####
dobut <- infusion %>% filter(grepl("dobutamine", label, ignore.case = TRUE)) %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% 
  filter(!is.na(firstavpstart)) %>% 
  select(subject_id, hadm_id, starttime, endtime, storetime, amount, rate, rateuom, statusdescription, label, firstavpstart) %>% 
  filter(starttime <= firstavpstart) %>% 
  distinct() %>% 
  group_by(hadm_id) %>% 
  slice_max(order_by = starttime) %>% 
  ungroup() %>% 
  distinct(hadm_id, .keep_all = TRUE) %>% 
  mutate(dobut = ifelse(endtime <= firstavpstart & 
                          (statusdescription == "Stopped" | statusdescription == "Paused" | statusdescription == "FinishedRunning"), 
                        "No", "Yes"))

### Milrinone ####
milrinone <- infusion %>% filter(grepl("milrinone", label, ignore.case = TRUE)) %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% 
  filter(!is.na(firstavpstart)) %>% 
  select(subject_id, hadm_id, starttime, endtime, storetime, amount, rate, rateuom, statusdescription, label, firstavpstart) %>% 
  filter(starttime <= firstavpstart) %>% 
  distinct() %>% 
  group_by(hadm_id) %>% 
  slice_max(order_by = starttime) %>% 
  ungroup() %>% 
  distinct(hadm_id, .keep_all = TRUE) %>% 
  mutate(milrinone = ifelse(endtime <= firstavpstart & 
                              (statusdescription == "Stopped" | statusdescription == "Paused" | statusdescription == "FinishedRunning"), 
                            "No", "Yes"))

write_xlsx(vasopressors, "~/MIMIC-IV Data/vasopressors.xlsx")
write_xlsx(ne, "~/MIMIC-IV Data/ne.xlsx")
write_xlsx(epi, "~/MIMIC-IV Data/epi.xlsx")
write_xlsx(pe, "~/MIMIC-IV Data/pe.xlsx")
write_xlsx(dop, "~/MIMIC-IV Data/dop.xlsx")
write_xlsx(avp, "~/MIMIC-IV Data/avp.xlsx")
write_xlsx(dobut, "~/MIMIC-IV Data/dobut.xlsx")
write_xlsx(milrinone, "~/MIMIC-IV Data/milrinone.xlsx")

### Datapoints ####
#### Norepinephrine Equivalent Dose ####
# filtering by patients with firstavpstart time (only vaso recipients)
# Norepinephrine
ne2 <- ne %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(hadm_id, firstavpstart, firstavpstop), by = "hadm_id") %>% 
  distinct(subject_id, hadm_id, stay_id, starttime, rate, .keep_all = TRUE) %>% 
  mutate(nedose_0 = ifelse((firstavpstart >= starttime) & (firstavpstart < endtime), rate, NA)) %>% 
  mutate(nedose_6 = ifelse(((firstavpstart + hours(6)) >= starttime) & ((firstavpstart + hours(6)) < endtime), rate, NA)) %>% 
  select(subject_id, hadm_id, stay_id, nedose_0, nedose_6, rate_weightbased, patientweight) %>% 
  mutate(hour = ifelse(!is.na(nedose_0), 0,
                       ifelse(!is.na(nedose_6), 6, NA))) %>% 
  mutate(nedose_0 = ifelse(!is.na(nedose_6), 0, nedose_0), 
         nedose_6 = ifelse(!is.na(nedose_0) & nedose_0 !=0, 0, nedose_6)) %>%
  filter(!is.na(nedose_0) & !is.na(nedose_6)) %>%
  mutate(rate_kg = ifelse(hour == 0, rate_weightbased, 0), 
         rate6hr_kg = ifelse(hour == 6, rate_weightbased,0)) %>% 
  group_by(stay_id) %>% mutate(nedose_0 = sum(nedose_0), 
                               nedose_6 = sum(nedose_6),
                               rate_kg_ne = sum(rate_kg), 
                               rate6hr_kg_ne = sum(rate6hr_kg)) %>% 
  distinct(stay_id, nedose_0, patientweight, rate_kg_ne, rate6hr_kg_ne, .keep_all = TRUE) %>%
  select(subject_id, hadm_id, stay_id, nedose_0, rate_kg_ne, nedose_6, rate6hr_kg_ne, "patientweight_ne" = "patientweight")
# this sheet finds the baseline NEQ dose and 6 hour NEQ dose for all AVP recipients for NE

# Epinephrine
epi2 <- epi %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(hadm_id, firstavpstart, firstavpstop), by = "hadm_id") %>% 
  distinct(subject_id, hadm_id, stay_id, starttime, rate, .keep_all = TRUE) %>% 
  mutate(epidose_0 = ifelse((firstavpstart >= starttime) & (firstavpstart < endtime), rate, NA)) %>% 
  mutate(epidose_6 = ifelse(((firstavpstart + hours(6)) >= starttime) & ((firstavpstart + hours(6)) < endtime), rate, NA)) %>% 
  select(subject_id, hadm_id, stay_id, epidose_0, epidose_6, rate_weightbased, patientweight) %>% 
  mutate(hour = ifelse(!is.na(epidose_0), 0,
                       ifelse(!is.na(epidose_6), 6, NA))) %>% 
  mutate(epidose_0 = ifelse(!is.na(epidose_6), 0, epidose_0), 
         epidose_6 = ifelse(!is.na(epidose_0) & epidose_0 !=0, 0, epidose_6)) %>%
  filter(!is.na(epidose_0) & !is.na(epidose_6)) %>%
  mutate(rate_kg = ifelse(hour == 0, rate_weightbased, 0), 
         rate6hr_kg = ifelse(hour == 6, rate_weightbased,0)) %>% 
  group_by(stay_id) %>% mutate(epidose_0 = sum(epidose_0), 
                               epidose_6 = sum(epidose_6),
                               rate_kg_epi = sum(rate_kg), 
                               rate6hr_kg_epi = sum(rate6hr_kg)) %>% 
  distinct(stay_id, epidose_0, patientweight, rate_kg_epi, rate6hr_kg_epi, .keep_all = TRUE) %>%
  select(subject_id, hadm_id, stay_id, epidose_0, rate_kg_epi, epidose_6, rate6hr_kg_epi, "patientweight_epi" = "patientweight")

# Phenylephrine
pe2 <- pe %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(hadm_id, firstavpstart, firstavpstop), by = "hadm_id") %>% 
  distinct(subject_id, hadm_id, stay_id, starttime, rate, .keep_all = TRUE) %>% 
  mutate(pedose_0 = ifelse((firstavpstart >= starttime) & (firstavpstart < endtime), rate, NA)) %>% 
  mutate(pedose_6 = ifelse(((firstavpstart + hours(6)) >= starttime) & ((firstavpstart + hours(6)) < endtime), rate, NA)) %>% 
  select(subject_id, hadm_id, stay_id, pedose_0, pedose_6, rate_weightbased, patientweight) %>% 
  mutate(hour = ifelse(!is.na(pedose_0), 0,
                       ifelse(!is.na(pedose_6), 6, NA))) %>% 
  mutate(pedose_0 = ifelse(!is.na(pedose_6), 0, pedose_0), 
         pedose_6 = ifelse(!is.na(pedose_0) & pedose_0 !=0, 0, pedose_6)) %>%
  filter(!is.na(pedose_0) & !is.na(pedose_6)) %>%
  mutate(rate_kg = ifelse(hour == 0, rate_weightbased, 0), 
         rate6hr_kg = ifelse(hour == 6, rate_weightbased,0)) %>% 
  group_by(stay_id) %>% mutate(pedose_0 = sum(pedose_0), 
                               pedose_6 = sum(pedose_6),
                               rate_kg_pe = sum(rate_kg), 
                               rate6hr_kg_pe = sum(rate6hr_kg)) %>% 
  distinct(stay_id, pedose_0, patientweight, rate_kg_pe, rate6hr_kg_pe, .keep_all = TRUE) %>%
  select(subject_id, hadm_id, stay_id, pedose_0, rate_kg_pe, pedose_6, rate6hr_kg_pe, "patientweight_pe" = "patientweight")

# Dopamine
dop2 <- dop %>% 
  left_join(vasopressors %>% dplyr::filter(!is.na(firstavpstart)) %>% 
              dplyr::select(hadm_id, firstavpstart, firstavpstop), by = "hadm_id") %>% 
  distinct(subject_id, hadm_id, stay_id, starttime, rate, .keep_all = TRUE) %>% 
  mutate(dopdose_0 = ifelse((firstavpstart >= starttime) & (firstavpstart < endtime), rate, NA)) %>% 
  mutate(dopdose_6 = ifelse(((firstavpstart + hours(6)) >= starttime) & ((firstavpstart + hours(6)) < endtime), rate, NA)) %>% 
  select(subject_id, hadm_id, stay_id, dopdose_0, dopdose_6, rate_weightbased, patientweight) %>% 
  mutate(hour = ifelse(!is.na(dopdose_0), 0,
                       ifelse(!is.na(dopdose_6), 6, NA))) %>% 
  mutate(dopdose_0 = ifelse(!is.na(dopdose_6), 0, dopdose_0), 
         dopdose_6 = ifelse(!is.na(dopdose_0) & dopdose_0 !=0, 0, dopdose_6)) %>%
  filter(!is.na(dopdose_0) & !is.na(dopdose_6)) %>%
  mutate(rate_kg = ifelse(hour == 0, rate_weightbased, 0), 
         rate6hr_kg = ifelse(hour == 6, rate_weightbased,0)) %>% 
  group_by(stay_id) %>% mutate(dopdose_0 = sum(dopdose_0), 
                               dopdose_6 = sum(dopdose_6),
                               rate_kg_dop = sum(rate_kg), 
                               rate6hr_kg_dop = sum(rate6hr_kg)) %>% 
  distinct(stay_id, dopdose_0, patientweight, rate_kg_dop, rate6hr_kg_dop, .keep_all = TRUE) %>%
  select(subject_id, hadm_id, stay_id, dopdose_0, dopdose_6, "patientweight_dop" = "patientweight")
# no weight based dose of dopamine because all in mcg/kg/min anyway and the rates are the same

#  Combining all Vasopressor doses at 0 and 6 hours
neqdoses <- ne2 %>% 
  left_join(epi2 %>% dplyr::select(stay_id, epidose_0, rate_kg_epi, epidose_6, rate6hr_kg_epi, patientweight_epi), by = "stay_id") %>% 
  left_join(pe2 %>% dplyr::select(stay_id, pedose_0, rate_kg_pe, pedose_6, rate6hr_kg_pe, patientweight_pe), by = "stay_id") %>% 
  left_join(dop2 %>% dplyr::select(stay_id, dopdose_0, dopdose_6, patientweight_dop), by = "stay_id") %>%
  mutate(neqdose_0 = sum(nedose_0, epidose_0, (pedose_0 / 10), (dopdose_0 / 2), na.rm = TRUE), 
         neqdose_6 = sum(nedose_6, epidose_6, (pedose_6 / 10), (dopdose_6 / 2), na.rm = TRUE))


write_xlsx(neqdoses, "~/MIMIC-IV Data/neqdoses.xlsx")

rm(ne2, epi2, pe2, dop2)

# ---------------------- #
# SCREENING CRITERIA ####
# ---------------------- #

### Sepsis 3 ####
#### MAP 65 - Sepsis-3 ####

map <- chartevents %>% filter(grepl("blood pressure mean", label, ignore.case = TRUE)) %>% 
  full_join(vasopressors %>% dplyr::select(subject_id, hadm_id, vasoactivestart), by = "hadm_id") %>%
  filter(!is.na(vasoactivestart)) %>% 
  mutate(map65 = ifelse(charttime >= (vasoactivestart - hours(3)) & 
                          charttime <= (vasoactivestart + hours(3)) & 
                          valuenum < 65, "MAP < 65", "")) %>% 
  select(-subject_id.y) %>% rename("subject_id" = "subject_id.x")
# labeling times when MAP < 65 within -/+ 180 min (3 hours) of vasoactive initiation

# save(map, file = "~/MIMIC-IV Data/map.RData")
load("~/MIMIC-IV Data/map.RData")

map65 <- map %>% filter(map65 == "MAP < 65") %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE) %>% 
  select(subject_id, hadm_id, map65)
# selecting unique subject_id with MAP < 65 within 180 min of vasoactive initiation 
# 14444  

# write_xlsx(map65, "~/MIMIC-IV Data/map65.xlsx")

#### Lactate > 2.0 - Sepsis-3 ####
# Get lactate in 24 hours surrounding vasopressor utilization

# labs come from both chartevents and from labevents tables
lactatechartevents <- chartevents %>% filter(label == "Lactic Acid") %>% 
  full_join(vasopressors %>% dplyr::select(hadm_id, vasoactivestart, vasoactivestop), by = "hadm_id") %>%
  filter(!is.na(vasoactivestart)) %>% 
  mutate(lactate2 = if_else(charttime >= (vasoactivestart - hours(24)) & 
                              charttime <= (vasoactivestart + hours(24)) & 
                              valuenum >= 2.0, "Lactate >= 2.0", "")) %>% 
  mutate(lactate2at72hr = if_else(charttime >= (vasoactivestart - hours(24)) & 
                              charttime <= (vasoactivestart + hours(72)) & 
                              valuenum >= 2.0, "Lactate >= 2.0", "")) %>%
  select(subject_id, hadm_id, charttime, itemid, label, category, valuenum, valueuom, vasoactivestart, vasoactivestop, lactate2, lactate2at72hr)

lactatelabevents <- labevents %>% filter(label == "Lactate") %>% 
  full_join(vasopressors %>% dplyr::select(hadm_id, vasoactivestart, vasoactivestop), by = "hadm_id") %>%
  filter(!is.na(vasoactivestart)) %>% 
  mutate(lactate2 = if_else(charttime >= (vasoactivestart - hours(24)) & 
                              charttime <= (vasoactivestart + hours(24)) & 
                              valuenum >= 2.0, "Lactate >= 2.0", "")) %>% 
  mutate(lactate2at72hr = if_else(charttime >= (vasoactivestart - hours(24)) & 
                              charttime <= (vasoactivestart + hours(72)) & 
                              valuenum >= 2.0, "Lactate >= 2.0", "")) %>% 
  select(subject_id, hadm_id, charttime, itemid, label, category, valuenum, valueuom, vasoactivestart, vasoactivestop, lactate2, lactate2at72hr)

lactate <- rbind(lactatechartevents, lactatelabevents) %>% arrange(subject_id, charttime)
rm(lactatechartevents, lactatelabevents)

save(lactate, file = "~/MIMIC-IV Data/lactate.xlsx")
save(lactate, file = "~/MIMIC-IV Data/lactate.RData")

# isolating list of just patients with lactate >= 2.0
lactate2 <- lactate %>% filter(lactate2 == "Lactate >= 2.0") %>% 
  distinct(hadm_id, .keep_all = TRUE) %>% 
  select(subject_id, hadm_id, valuenum, lactate2)

lactate2at72hr <- lactate %>% filter(lactate2at72hr == "Lactate >= 2.0") %>% 
  distinct(hadm_id, .keep_all = TRUE) %>% 
  select(subject_id, hadm_id, valuenum, lactate2at72hr)

save(lactate2, file = "~/MIMIC-IV Data/lactate2.xlsx")
save(lactate2at72hr, file = "~/MIMIC-IV Data/lactate2at72hr.xlsx")

#### Antibiotic Use - Sepsis-3 ####

# load from files above saved as R data file
qadabx <- emar %>% filter(grepl("amikacin|ampicillin|azithromycin|aztreonam|cefamandole|cefazolin|cefepime|cefmetazole|cefonicid|cefoperazone|cefotaxime|cefotetan|cefotetan|cefoxitin|ceftaroline|ceftazidime|ceftizoxime|ceftolozane|ceftriaxone|cefuroxime|cephalothin|cephapirin|chloramphenicol|ciprofloxacin|clindamycin|cloxacillin|colistin|dalbavancin|daptomycin|doripenem|doxycycline|ertapenem|gatifloxacin|gentamicin|mipenem|kanamycin|levofloxacin|lincomycin|linezolid|meropenem|methicillin|metronidazole|mezlocillin|minocycline|moxifloxacin|minocycline|moxifloxacin|nafcillin|oritavancin|oxacillin|penicillin|piperacillin|polymyxin|quinupristin|streptomycin|tedizolid|telavancin|ticarcillin|tigecycline|tobramycin|trimethoprim|vancomycin|amoxicillin|cefaclor|cefadroxil|cefdinir|cefditoren|cefixime|cefpodoxime|cefprozil|ceftibuten|cefuroxime|cephalexin|cephradine|cefixime|cefpodoxime|cefprozil|ceftibuten|cefuroxime|cephalexin|cephradine|cinoxacin|clarithromycin|dicloxacillin|fidaxomicin|fosfomycin|gatifloxacin|lincomycin|minocycline|moxifloxacin|nitrofurantoin|norfloxacin|ofloxacin|penicillin|pivampicillin|rifampin|sulfadiazine|sulfamethoxazole|sulfisoxazole|tedizolid|telithromycin|tetracycline|amphotericin|anidulafungin|caspofungin|fluconazole|isavucon|itraconazole|micafungin|posaconazole|voriconazole|acyclovir|ganciclovir|cidofovir|foscarnet|peramivir|oseltamivir", 
                                medication, ignore.case = TRUE)) %>% 
  arrange(hadm_id, storetime) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(event_txt = if_else(is.na(event_txt), "Administered", event_txt)) %>% 
  mutate_if(is.character, as.factor) %>%
  filter(event_txt != "Not Given", event_txt != "Stopped", event_txt != "Hold Dose", event_txt != "in Other Location", 
         event_txt != "Not Given per Sliding Scale", event_txt != "Stopped in Other Location", event_txt != "Not Started", 
         event_txt != "Documented in O.R. Holding") %>% # removing documentations that were clear the drug wasn't given
  filter(!grepl("opth|ophth|bulk|placebo|ear|drops|CefTAZidime 2mg/0.1mL|paste|topical|dexamethasone|polymyxin B sulf-trimethoprim|Vancomycin 1mg/0.1mL|neomycin|lock|gel|irrigation|inhalation|nebulized|lidocaine|ointment|oint|cream|otic|sodium citrate|enema|intrathecal|1% solution|vancomycin oral liquid", 
                medication, ignore.case = TRUE)) %>% # filtering out non IV/PO medications
  mutate_if(is.character, as.factor)

unique(qadabx$medication) # 249 unique medication names

qadabx1 <- qadabx %>% inner_join(vasopressors %>% dplyr::select(subject_id, hadm_id, vasoactivestart, vasoactivestop), by = "hadm_id") %>% 
  select("subject_id" = "subject_id.x", hadm_id, storetime, medication, vasoactivestart, vasoactivestop) %>% 
  mutate(difference = difftime(vasoactivestart, storetime, units = "day")) %>%
  mutate(abxduringvasoactive = if_else(difference >= (-1) & difference <= 1, "Yes", "No")) %>% 
  select(subject_id, hadm_id, abxduringvasoactive) %>%
  filter(abxduringvasoactive == "Yes") %>%
  distinct(hadm_id, subject_id, abxduringvasoactive)

# isolate all the patients who received vasopressors and give yes if abx were +/- 1 day from vasoactive agent initiation 
# One thing I don't think I did was the 4 QAD of antibiotics, just looked at if they were within the first day of vasoactive agent initiation.....

qadabx3 <- qadabx %>% inner_join(vasopressors %>% dplyr::select(subject_id, hadm_id, vasoactivestart, vasoactivestop), by = "hadm_id") %>% 
  select("subject_id" = "subject_id.x", hadm_id, storetime, medication, vasoactivestart, vasoactivestop) %>% 
  mutate(difference = difftime(vasoactivestart, storetime, units = "day")) %>%
  mutate(abxduringvasoactive72h = if_else(difference >= (-1) & difference <= 3, "Yes", "No")) %>% 
  select(subject_id, hadm_id, abxduringvasoactive72h) %>%
  filter(abxduringvasoactive72h == "Yes") %>%
  distinct(hadm_id, subject_id, abxduringvasoactive72h)

save(qadabx, file = "~/MIMIC-IV Data/qadabx.RData")
save(qadabx1, file = "~/MIMIC-IV Data/qadabx1.xlsx")
save(qadabx3, file = "~/MIMIC-IV Data/qadabx3.xlsx")

# isolating and expanding to antibiotics within 72 hours (-1 and + 3 days) after NE start for modified Bosch criteria

#### Final Sepsis-3 DF ####

# if running this need vasopressors, lactate2, qadabx1 and 3, map65, diagnosissepsis, 
# diagnosissepticshock, angusinfection, angusorgandysf to run this
# diagnosis dfs are below

sepsis3 <- vasopressors %>% 
  select(hadm_id, subject_id, stay_id, vasoactivestart, vasoactivestop, firstavpstart, firstavpstop) %>% 
  left_join(lactate2 %>% dplyr::select(hadm_id, valuenum, lactate2), by = "hadm_id") %>% 
  left_join(lactate2at72hr %>% dplyr::select(hadm_id, lactate2at72hr), by = "hadm_id") %>% 
  left_join(qadabx1 %>% dplyr::select(hadm_id, abxduringvasoactive), by = "hadm_id") %>% 
  left_join(qadabx3 %>% dplyr::select(hadm_id, abxduringvasoactive72h), by = "hadm_id") %>%
  left_join(map65 %>% dplyr::select(hadm_id, map65), by = "hadm_id") %>% 
  mutate(sepsis3 = if_else(map65 == "MAP < 65" & lactate2 == "Lactate >= 2.0" & abxduringvasoactive == "Yes", "Yes", "No")) %>% 
  mutate(sepsis3bosch = if_else(map65 == "MAP < 65" & lactate2at72hr == "Lactate >= 2.0" & abxduringvasoactive72h == "Yes", "Yes", "No")) %>%
  mutate(sepsis3nomap = if_else(lactate2 == "Lactate >= 2.0" & abxduringvasoactive == "Yes", "Yes", "No")) %>%
  mutate(sepsis3nomapbosch = if_else(lactate2at72hr == "Lactate >= 2.0" & abxduringvasoactive72h == "Yes", "Yes", "No")) %>%
  distinct(hadm_id, firstavpstart, .keep_all = TRUE) %>%
  left_join(diagnosissepsis %>% dplyr::select(hadm_id, sepsisdx), by = "hadm_id") %>% 
  left_join(diagnosissepticshock %>% dplyr::select(hadm_id, septicshockdx), by = "hadm_id") %>%
  left_join(angusinfection %>% dplyr::select(hadm_id, angusinfection), by = "hadm_id") %>% 
  left_join(angusorgandysf %>% dplyr::select(hadm_id, angusorgandysf), by = "hadm_id") %>%
  mutate(anguscriteria = ifelse(sepsisdx == "Yes" | septicshockdx == "Yes" | (angusinfection == "Yes" & angusorgandysf == "Yes"), "Yes", "No"))

# Total number should be 22703
# just patients who received vasoactive agents

write_xlsx(sepsis3, "~/MIMIC-IV Data/sepsis3.xlsx")

### Sepsis Coding ####
# df <- diagnoses_icd %>% filter(grepl("R65", icd_code, ignore.case = TRUE)) # 4565 results
# unique(df$icd_code) # need to remove R6511 and R6510
# df <- diagnoses_icd %>% filter(grepl("R6520|R6521", icd_code, ignore.case = TRUE)) # 4394
# df <- diagnoses_icd %>% filter(grepl("severe sepsis", long_title, ignore.case = TRUE)) # 4565
# df <- diagnoses_icd %>% filter(grepl("septic shock", long_title, ignore.case = TRUE)) # 7826
# df <- diagnoses_icd %>% filter(grepl("septic shock|severe sepsis", long_title, ignore.case = TRUE)) # 13096
# unique(df$icd_code) # "R6520"  "99592"   "78552"   "R6521"   "T8112XA"
# df <- diagnoses_icd %>% filter(icd_code == "78552") # 3378 ICD-9 code for septic shock
# df <- diagnoses_icd %>% filter(icd_code == "99592") # 5270 ICD-9 code for severe sepsis
# df <- diagnoses_icd %>% filter(icd_code == "T8112XA") # 54 post-procedural ICD-10 code
# # need to decide if keeping the post procedural codes (removing for now)


# Making savable files of each diagnosis
diagnosissepsis <- diagnoses_icd %>% filter(grepl("R6520|R6521|78552|99592", icd_code, ignore.case = TRUE)) %>% 
  mutate(sepsisdx = "Yes") %>% distinct(hadm_id, .keep_all = TRUE) 
# 9657 patients with ICD 9 or 10 codes of severe sepsis or septic shock
write_xlsx(diagnosissepsis, "~/MIMIC-IV Data/diagnosissepsis.xlsx")

diagnosissepticshock <- diagnoses_icd %>% filter(grepl("R6521|78552", icd_code, ignore.case = TRUE)) %>% 
  mutate(septicshockdx = "Yes") %>% distinct(hadm_id, .keep_all = TRUE)
# 6550 patients with ICD 9 or 10 codes of septic shock\
write_xlsx(diagnosissepticshock, "~/MIMIC-IV Data/diagnosissepticshock.xlsx")

# df <- sepsis3 %>% filter(septicshockdx == "Yes") # 5325 with septic shock
# df <- sepsis3 %>% filter(septicshockdx == "Yes") %>% filter(!is.na(firstavpstart)) # 2130 with septic shock who received vasopressin
# df <- sepsis3 %>% filter(sepsisdx == "Yes") # 6007 with severe sepsis
# df <- sepsis3 %>% filter(sepsisdx == "Yes") %>% filter(!is.na(firstavpstart)) # 2327 with severe sepsis who received vasopressin
# # all septic shock also have severe sepsis diagnosis code

### Sepsis Coding with Angus Criteria ####
# Infection criteria
# ICD-9 and ICD-10 codes are incorporated
angusinfection <- diagnoses_icd %>% filter(str_detect(icd_code, "^001|^002|^003|^004|^005|^008|^009|^010|^011|^012|^013|^014|^015|^016|^017|^018|^020|^021|^022|^023|^024|^025|^026|^027|^030|^031|^032|^033|^034|^035|^036|^037|^038|^039|^040|^041|^090|^091|^092|^093|^094|^095|^096|^097|^098|^100|^101|^102|^103|^104|^110|^111|^112|^114|^115|^116|^117|^118|^320|^322|^324|^325|^420|^421|^451|^461|^462|^463|^464|^465|^481|^482|^485|^486|^49121|^494|^510|^513|^540|^541|^542|^56201|^56203|^56211|^56213|^566|^567|^5695|^56983|^5720|^5721|^5750|^590|^597|^5990|^601|^614|^615|^616|^681|^682|^683|^686|^7110|^730|^7907|^9966|^9985|^9993|^A00|^A01|^A02|^A03|^A05|^A04|^A09|^A15|^A16|^A17|^A18|^A19|^A20|^A21|^A22|^A23|^A24|^A24|^A25|^A28|^A30|^A31|^A36|^A37|^A38|^A46|^A39|^A33|^A41|^A42|^A48|^A49|^A50|^A51|^A52|^A53|^A54|^A27|^A69|^A66|^A67|^A69|^B35|^B35|^B37|^B38|^B39|^B40|^B41|^B42|^B43|^B44|^B45|^B46|^B48|^G00|^G01|^G02|^G03|^G06|^G07|^G08|^I30|^I33|^I80|^J01|^J02|^J03|^J04|^J06|^J13|^J15|^J16|^J17|^J18|^J44|^J47|^J86|^J85|^K35|^K37|^K38|^K36|^K57|^K61|^K65|^K63|^K75|^K81|^N39|^N34|^N41|^N73|^N71|^N76|^L03|^L02|^L04|^L08|^M00|^M86|^R7881|^T814|^T802|^T826|^T827|^T835|^T836|^T845|^T846|^T847")) %>% 
  mutate(angusinfection = "Yes") %>% 
  distinct(hadm_id, .keep_all = TRUE) 
# 131581 entries
write_xlsx(angusinfection, "~/MIMIC-IV Data/angusinfection.xlsx")

# Organ dysfunction criteria
# ICD-9 codes are incorporated, need to get the ICD-10 codes
angusorgandysf <- diagnoses_icd %>% filter(str_detect(icd_code, "^7855$|^458|^967|^3483|^293|^3481|^2874|^2875|^2869|^2866|^570|^5734|^584|^I95|^Z991|^G934|^F06|^G931|^D6959|^D696|^D68|^D65|^K72|^K763|^N179")) %>% mutate(angusorgandysf = "Yes") %>% distinct(hadm_id, .keep_all = TRUE) 
unique(angusorgandysf$icd_code)
# 99364 entries
write_xlsx(angusorgandysf, "~/MIMIC-IV Data/angusorgandysf.xlsx")

# These are incorporated into sepsis3 df above already

# # Getting screening criteria for angus criteria
# df <- sepsis3 %>% filter(anguscriteria == "Yes") # 10984 with angus criteria for severe sepsis or septic shock
# df <- sepsis3 %>% filter(anguscriteria == "Yes") %>% filter(!is.na(firstavpstart)) # 3195 with angus criteria for severe sepsis or septic shock who received vasopressin


# Final Screening Dataframe ####
# should start with 454324
# need sepsis3, admissions, and patients to run this

allscreening <- admissions %>% 
  left_join(sepsis3, by = "hadm_id") %>% 
  rename("subject_id" = "subject_id.x") %>% 
  left_join(patients %>% dplyr::select(subject_id, anchor_age, gender), by = "subject_id") %>% 
  mutate(vasopressors = if_else(is.na(vasoactivestart), "no vasopressors", "vasopressors")) %>% 
  mutate(vasopressin = ifelse(!is.na(firstavpstart), "yes", NA)) %>% 
  select(hadm_id, subject_id, stay_id, "age" = "anchor_age", gender, admittime, dischtime, deathtime, race, vasopressors, vasoactivestart, vasoactivestop, vasopressin, firstavpstart, 
         firstavpstop, lactate2, "lactate" = "valuenum", lactate2at72hr, abxduringvasoactive, abxduringvasoactive72h, map65, sepsis3, sepsis3bosch, sepsis3nomap, sepsis3nomapbosch,
         sepsisdx, septicshockdx, angusinfection, angusorgandysf, anguscriteria) %>% 
  arrange(subject_id, admittime)

write_xlsx(allscreening, "~/MIMIC-IV Data/allscreening.xlsx")

# FINAL DATAFRAME - Inclusion/Exclusion ####
### Inclusion Criteria ####
### Age 
min(patients$anchor_age)# minimum age is 18, no removals for this
max(patients$anchor_age) # 91, no text that needs to be converted

### Exclusion Criteria ####
# AVP before or at same time as CA start # 306 excluded for this -- eICU numbers
# AVP started > 48 hrs after CAs # 109 excluded for this -- eICU numbers

# No lactate concentration at time of vasopressin initiation (within 24 hours prior to vasopressin initation)
lactateinclusion <- lactate %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart) %>% 
              dplyr::filter(!is.na(firstavpstart)), by = "hadm_id") %>% 
  filter(!is.na(firstavpstart)) %>% 
  distinct(hadm_id, charttime,valuenum, .keep_all = TRUE) %>% 
  arrange(hadm_id, charttime) %>% 
  mutate(avplactatedifference = difftime(firstavpstart, charttime, units = "hour")) %>% 
  mutate(lactateatavp = ifelse(avplactatedifference <= 24 & avplactatedifference >= 0, "yes", "")) %>% 
  group_by(hadm_id, lactateatavp) %>% mutate(closestlactatediff = min(avplactatedifference)) %>% ungroup() %>%
  filter(closestlactatediff == avplactatedifference, lactateatavp == "yes") %>% 
  distinct(hadm_id, .keep_all = TRUE)
# isolating closest lactate to avp but within 24 hours prior to avp starting

inclusionexclusion <- allscreening %>% # total number should always be 317873
  left_join(lactateinclusion %>% dplyr::select(hadm_id, valuenum, charttime, avplactatedifference), by = "hadm_id") %>% 
  rename("lactateforinclusion" = "valuenum", "lactateatavp" = "lactate", "lactateforinclusiontime" = "charttime") %>%
  mutate(timefromshockonset_min = difftime(firstavpstart, vasoactivestart,  units = "min"), 
         timefromshockonset_hr = difftime(firstavpstart, vasoactivestart,  units = "hour")) %>% 
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
  mutate(include_exclude_sscoding = ifelse(is.na(septicshockdx) | is.na(vasopressin), "exclude", # not included if doens't meet sepsis3 criteria without map
                                           ifelse(is.na(age), "exclude", # not included if age is NA (no patients)
                                           ifelse(age < 18, "exclude", # not included if age < 18
                                           ifelse(vasoactivestart == firstavpstart, "exclude", # excluded if AVP first vasoactive (either at same time or before CA)
                                           ifelse(timefromshockonset_hr >= 48, "exclude", # excluded if AVP start > 48 hrs after first vasoactive
                                           ifelse(is.na(lactateatavp), "exclude", "include"))))))) %>% # excluded if no lactate within 24 hrs prior to AVP start
  mutate(exclusionreason_sscoding = ifelse(is.na(septicshockdx), "does not meet sscoding criteria",
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
  select(hadm_id, subject_id, stay_id, age, gender, race, sepsis3, include_exclude_sepsis3, exclusionreason_sepsis3, sepsis3nomap, 
         include_exclude_sepsis3nomap, exclusionreason_sepsis3nomap, sepsis3bosch, include_exclude_sepsis3bosch, exclusionreason_sepsis3bosch, sepsis3nomapbosch, 
         include_exclude_sepsis3nomapbosch, exclusionreason_sepsis3nomapbosch, septicshockdx, include_exclude_sscoding, exclusionreason_sscoding, 
         anguscriteria, include_exclude_angus, exclusionreason_angus, timefromshockonset_min, timefromshockonset_hr, vasopressors,
         vasoactivestart:vasopressin, firstavpstart, firstavpstop, lactateatavp, abxduringvasoactive, abxduringvasoactive72h)

write_xlsx(inclusionexclusion, "~/MIMIC-IV Data/inclusionexclusion.xlsx")

# ------------------------------------------- #
# FINAL DATAFRAME - data collection points ####
# ------------------------------------------- #

## Step 1 #### 
# Step 1: Import the below dataframes
admissions <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/hosp/admissions.csv.gz")
inclusionexclusion <- read_excel("~/MIMIC-IV Data/inclusionexclusion.xlsx")
neqdoses <- read_excel("~/MIMIC-IV Data/neqdoses.xlsx")
icustays <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/icu/icustays.csv.gz")
load("~/MIMIC-IV Data/weight.RData")
avp <- read_excel("~/MIMIC-IV Data/avp.xlsx")
load("~/MIMIC-IV Data/sofa.RData")
dobut <- read_excel("~/MIMIC-IV Data/dobut.xlsx")
milrinone <- read_excel("~/MIMIC-IV Data/milrinone.xlsx")
load("~/MIMIC-IV Data/map.RData")
vasopressors <- read_excel("~/MIMIC-IV Data/vasopressors.xlsx")

## Step 2 ####
# Step 2: Run Below code
finaldf <- inclusionexclusion %>% filter(vasopressors == "vasopressors") %>% # should have 22703 encounters on vasopressors
  select(hadm_id, subject_id, stay_id, "time0" = "firstavpstart", age, gender, vasoactivestart, vasoactivestop, vasopressin, 
         "avpstop" = "firstavpstop", timefromshockonset_hr, lactateatavp, abxduringvasoactive) %>% 
  left_join(admissions %>% dplyr::select(hadm_id, admittime,dischtime, deathtime, admission_location, discharge_location,
                                         race), by = "hadm_id") %>% 
  mutate(race = ifelse(race == "BLACK/AFRICAN AMERICAN" | race == "BLACK/CAPE VERDEAN" | race == "BLACK/CARIBBEAN ISLAND" | race == "BLACK/AFRICAN", "Black", 
                       ifelse(race == "WHITE - OTHER EUROPEAN" | race == "WHITE - EASTERN EUROPEAN" | race == "WHITE - BRAZILIAN" | race == "WHITE" | race == "WHITE - RUSSIAN", "White",
                              "Other")), 
         inhospital_mortality = ifelse(!is.na(deathtime), "Yes", "No"), 
         abxduringvasoactive = ifelse(is.na(abxduringvasoactive), "No", abxduringvasoactive)) %>%
  mutate(gender = recode(gender, "M" = "Male", "F" = "Female")) %>%
  left_join(neqdoses %>% dplyr::select(hadm_id, stay_id, neqdose_0, neqdose_6), by = c("hadm_id" = "hadm_id", "stay_id" = "stay_id")) %>% 
  distinct(hadm_id, stay_id, .keep_all = TRUE) %>%
  left_join(icustays %>% dplyr::select(stay_id, first_careunit), by = "stay_id") %>% 
  mutate(iculocation = recode(first_careunit, "Medical Intensive Care Unit (MICU)" = "MICU", 
                              "Cardiac Vascular Intensive Care Unit (CVICU)" = "Cardiac ICU", 
                              "Surgical Intensive Care Unit (SICU)" = "SICU", 
                              "Coronary Care Unit (CCU)" = "Cardiac ICU",
                              "Medical/Surgical Intensive Care Unit (MICU/SICU)" = "Mixed ICU", 
                              "Trauma SICU (TSICU)" = "SICU", 
                              "Neuro Surgical Intensive Care Unit (Neuro SICU)" = "Mixed ICU", 
                              "Neuro Intermediate" = "Neurosciences ICU", 
                              "Neuro Stepdown" = "Neurosciences ICU")) %>% select(-first_careunit) %>% 
  left_join(weight %>% dplyr::select(hadm_id, weight), by = "hadm_id") %>% 
  mutate(weight = round(weight, digits = 1),
         time0day = floor_date(time0, unit = "days"), 
         time0day48h = floor_date(time0day + days(2), unit = "days")) %>%
  
  # Vasopressin initial dose
  left_join(avp %>% dplyr::filter(status == "initial start") %>% dplyr::select(hadm_id, rate), 
            by = "hadm_id") %>%
  mutate(initialavpdose_category = ifelse(!is.na(time0) & rate == 0.03, "0.03", 
                                          ifelse(!is.na(time0) & rate == 0.04, "0.04", "other"))) %>% 
  mutate(initialavpdose_category = ifelse(is.na(time0), NA, initialavpdose_category)) %>%
  rename("avpstartdose" = "rate") %>%
  
  # SOFA Score and SOFA Score Change - only evaluated in patients who survived 2.0 days
  left_join(sofa %>% dplyr::select(hadm_id, charttimeday, sofarenal, sofaliver, sofacoag, sofaresp, sofacardiac, totalsofa), 
            by = c("hadm_id" = "hadm_id", "time0day" = "charttimeday")) %>% # SOFA score at baseline
  rename("sofarenal_bl" = "sofarenal", "sofaliver_bl" = "sofaliver", "sofacoag_bl" = "sofacoag", "sofaresp_bl" = "sofaresp", 
         "sofacardiac_bl" = "sofacardiac", "totalsofa_bl" = "totalsofa") %>%
  left_join(sofa %>% dplyr::select(hadm_id, charttimeday, sofarenal, sofaliver, sofacoag, sofaresp, sofacardiac, totalsofa), 
            by = c("hadm_id" = "hadm_id", "time0day48h" = "charttimeday")) %>% # SOFA score at 48h
  rename("sofarenal_48h" = "sofarenal", "sofaliver_48h" = "sofaliver", "sofacoag_48h" = "sofacoag", "sofaresp_48h" = "sofaresp", 
         "sofacardiac_48h" = "sofacardiac", "totalsofa_48h" = "totalsofa") %>% select(-time0day, -time0day48h) %>% 
  mutate(totalsofa_48h = ifelse(difftime(deathtime,time0,units="days")<=2 | is.na(deathtime),NA,totalsofa_48h)) %>%
  mutate(sofa_change = totalsofa_48h - totalsofa_bl) 


## Step 3 ####
# Step 3: Need to run all below Data Collection Points
# Need to import: discharge, icustays, emar, infusion, inputevents, outputevents, procedureevents

### Comorbidities ####
# Creating dataframe where the discharge notes remove all content after "Family History"
# in order to capture the HPI and PMH sections
pmhnotes <- discharge %>% 
  mutate(text = str_replace_all(text, "Family History", "ZZZ")) %>% 
  mutate(text = gsub("ZZZ.*", "", text))
pmh <- pmhnotes %>%
  mutate(dialysis = ifelse(grepl("esrd|IHD|end stage renal disease", text, ignore.case = TRUE), "yes", NA),
         diabetes = ifelse(grepl("diabetes| DM ", text, ignore.case = TRUE), "yes", NA),
         cirrhosis = ifelse(grepl("cirrhosis|liver failure|hepatic failure", text, ignore.case = TRUE), "yes", NA),
         copd = ifelse(grepl("copd|chronic obstructive pulmonary disease", text, ignore.case = TRUE), "yes", NA),
         immune_suppression = ifelse(grepl("transplant|cancer|immunosuppression|aids", text, ignore.case = TRUE), "yes", NA))

### ICU LOS ####
iculos <- icustays %>% 
  arrange(subject_id, hadm_id, stay_id) %>% 
  group_by(hadm_id) %>% 
  mutate(last_icudischarge = max(outtime)) %>% 
  distinct(hadm_id, last_icudischarge)
# gives the last ICU discharge time to determine ICU Free days and ICU mortality 

### Hydrocortisone ####
# Hydrocortisone use within 24 hours of time 0 
# Any HC dose (5, 10, 20, 25, 30, 50, and 100, only 2 instances of 5 mg)
hydrocort <- emar %>% filter(grepl("hydrocortisone|hydrocort", medication, ignore.case = TRUE)) %>%
  filter(!grepl("cream|oint|foam|suppository|enema|acetic", medication, ignore.case = TRUE)) %>%
  filter(event_txt != "Not Given") %>% filter(event_txt != "Hold Dose") %>% filter(event_txt != "in Other Location") %>%
  left_join(emar_detail, by = "emar_id") %>% 
  select(hadm_id, emar_id, charttime, medication, event_txt, scheduletime, storetime, dose_given, dose_given_unit) %>% 
  filter(!is.na(dose_given)) %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% 
  mutate(hydrocort_bl = case_when(charttime < (firstavpstart + days(1))  & charttime > (firstavpstart - days(1)) ~ "Yes", TRUE ~ "No")) %>% 
  filter(hydrocort_bl == "Yes") %>% 
  distinct(hadm_id, hydrocort_bl, .keep_all = TRUE)

### Vasopressor Use 
vasopatavp <- neqdoses %>% 
  filter(neqdose_0 > 0) %>% 
  select(subject_id,hadm_id,nedose_0,epidose_0,pedose_0,dopdose_0) %>%
  mutate(nedose_0 = coalesce(nedose_0,0), 
         epidose_0 = coalesce(epidose_0,0),
         pedose_0 = coalesce(pedose_0,0),
         dopdose_0 = coalesce(dopdose_0,0)) %>% 
  mutate(norepinephrine = ifelse(nedose_0 == 0, "No", "Yes"), 
         epinephrine = ifelse(epidose_0 == 0, "No", "Yes"),
         phenylephrine = ifelse(pedose_0 == 0, "No", "Yes"),
         dopamine = ifelse(dopdose_0 == 0, "No", "Yes")) %>% 
  distinct()

### Fluid Bolus ####
# 6 hr before up to 6 hr after shock onset/vasoactive agent start

# Albumin on emar
# sodium chloride on emar is only flushes
# no searches revealing for lactate, ringer, plasmalyte, LR
# only using this for albumin
albuminemar <- emar %>% filter(grepl("albumin", medication, ignore.case = TRUE)) %>%
  filter(!grepl("flush|inhalation|furosemide|bicarb|7|23.4|haloperidol|nasal|rebif|interferon", medication, ignore.case = TRUE)) %>%
  left_join(emar_detail, by = "emar_id") %>%
  filter(!is.na(product_unit)) %>% filter(product_unit != "TAB") %>%
  filter(event_txt != "Not Given") %>% filter(event_txt != "Hold Dose") %>% filter(event_txt != "in Other Location") %>% 
  select("subject_id" = "subject_id.x", hadm_id, emar_id, charttime, medication, event_txt, dose_given, dose_given_unit, product_unit,
         product_code) %>% 
  mutate(label = ifelse(medication == "Albumin 25% (12.5g / 50mL)", "Albumin 25%", 
                        ifelse(medication == "Albumin 5% (25g / 500mL)" | medication == "Albumin 5% (12.5g / 250mL)", "Albumin 5%", medication))) %>%
  arrange(subject_id, hadm_id, charttime) %>% 
  mutate(volume = ifelse(label == "Albumin 25%", (dose_given*100)/25, 
                         ifelse(label == "Albumin 5%", (dose_given*100)/5, "")), 
         volume = as.numeric(volume)) %>% 
  select(subject_id, hadm_id, label, charttime, volume)

# Albumin on infusion
albumininf <- infusion %>% 
  filter(label == "Albumin 25%" | label == "Albumin 5%") %>% 
  filter(ordercategoryname == "04-Fluids (Colloids)" | ordercategoryname == "03-IV Fluid Bolus" | ordercategoryname == "02-Fluids (Crystalloids)") %>%
  select(subject_id, hadm_id, orderid, label, starttime, ordercategorydescription, rate, rateuom, totalamount, totalamountuom, statusdescription) %>% 
  arrange(subject_id, hadm_id, starttime) %>% 
  select(subject_id, hadm_id, label, "charttime" = "starttime", "volume" = "totalamount")

# Combining Albumin orders
albumin <- rbind(albuminemar, albumininf) %>% arrange(subject_id, charttime) %>% 
  mutate(fluid_bolus = ifelse(label == "Albumin 5%", volume*1.4, 
                              ifelse(label == "Albumin 25%", volume*7, volume)))
# fluid bolus is converted to crystalloid volume equivalent 

# Crystalloids and HES all in infusion
crystalloids <- infusion %>% 
  filter(label == "NaCl 0.9%" | label == "LR" | label == "Hetastarch (Hespan) 6%") %>% 
  filter(ordercategoryname == "04-Fluids (Colloids)" | ordercategoryname == "03-IV Fluid Bolus" | ordercategoryname == "02-Fluids (Crystalloids)") %>%
  select(subject_id, hadm_id, orderid, label, starttime, ordercategorydescription, rate, rateuom, totalamount, totalamountuom, statusdescription) %>% 
  arrange(subject_id, hadm_id, starttime) %>% 
  mutate(volume = ifelse(ordercategorydescription == "Bolus", totalamount, 
                         ifelse(ordercategorydescription == "Continuous IV" & label == "Hetastarch (Hespan) 6%", totalamount*1.32, 
                                ifelse(label == "NaCl 0.9%" & rate > 125, totalamount, 
                                       ifelse(label == "LR" & rate > 125, totalamount, 0))))) %>% 
  # ordercategorydescription == bolus has no rate, assume all are considered bolus volume
  # for continuous IV, if rate > 125 mL/hr then consider the total amount to be the bolus
  # converted HES to 1.32 crystalloid equivalents
  filter(volume >0) %>% 
  select(subject_id, hadm_id, label, "charttime" = "starttime", volume) %>% 
  mutate(fluid_bolus = volume)

# Combining dataframes into one fluids dataframe
fluidbolus <- rbind(albumin, crystalloids) %>% arrange(subject_id, charttime) %>% 
  # first line above will get all fluid bolus volumes regardless of time for all patients in dataset
  # should have 313847 observations
  left_join(finaldf %>% dplyr::select(hadm_id, vasoactivestart, time0, avpstop), by = "hadm_id") %>%
  filter(!is.na(vasoactivestart)) %>% 
  mutate(bolus = ifelse(charttime >= vasoactivestart - hours(6) & charttime <= vasoactivestart + hours(6), "Yes", "No")) %>% 
  filter(bolus == "Yes") %>% 
  group_by(hadm_id) %>% 
  mutate(total_fluid_bolus = sum(volume)) %>% 
  distinct(subject_id, .keep_all = TRUE)
rm(crystalloids, albumin, albumininf, albuminemar)

### Fluid Balance ####
# Use inputevents and outputevents to get cumulative fluid balance
# Definition: Cumulative amount of patient documented ins and outs before time 0 (AVP Start time)
# join in AVP start time, delete any who don't have avp start time and then any thing after AVP start time
# do same with output

# all in mL
ins <- inputevents %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% # should be 9442345
  distinct(hadm_id, starttime, storetime, amount, ordercategoryname, ordercategorydescription, 
           firstavpstart, totalamount, originalamount, orderid, itemid, .keep_all = TRUE) %>% 
  filter(!is.na(firstavpstart)) %>% 
  filter(starttime <= firstavpstart) %>% 
  filter(!is.na(totalamount)) %>%
  group_by(hadm_id) %>% 
  mutate(totalins = sum(totalamount)) %>% ungroup() %>%  
  select(hadm_id, totalins) %>% distinct(hadm_id, totalins)

# all in mL
outs <- outputevents %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% # should be 4450049
  distinct(hadm_id, charttime, storetime, value, itemid, .keep_all = TRUE) %>% 
  filter(!is.na(firstavpstart)) %>% 
  filter(charttime <= firstavpstart) %>% 
  filter(!is.na(value)) %>%
  group_by(hadm_id) %>% 
  mutate(totalouts = sum(value)) %>% ungroup() %>%  
  select(hadm_id, totalouts) %>% distinct(hadm_id, totalouts)

fluidbalance <- ins %>% ungroup() %>% 
  full_join(outs, by = "hadm_id") %>% 
  mutate(totalouts = coalesce(totalouts, 0)) %>% 
  mutate(fluidbalance = totalins-totalouts)
rm(ins,outs)

### AKI at Vasopressin Initiation ####
# 1. Calculate baseline SrCr: median SrCr PTA (not available in eICU), minimum SrCr from hospital admit as long as pt admitted 
#    7 days prior to NE initiation, MDRD back calculation estimating eGFR of 60 
# 2. Determine AKI development vs. baseline at time of vasopressin initiation (ESRD vs. no AKI vs. Stage 1, 2, and 3)

load("~/MIMIC-IV Data/creatinine.RData")
srcr <- creatinine %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, vasoactivestart), by = "hadm_id") %>% 
  filter(!is.na(vasoactivestart)) %>% 
  left_join(admissions %>% dplyr::select(hadm_id, admittime), by = "hadm_id") %>% 
  mutate(hosptovasoactive = difftime(vasoactivestart, admittime, unit = "days"), # getting time between hosp admit and vasopressors
         minsrcr = ifelse(hosptovasoactive >=7 & charttime <= vasoactivestart, "min srcr", "")) %>% # tagging the min cretatinine values 
  filter(minsrcr == "min srcr") %>% 
  group_by(hadm_id) %>% mutate(minsrcr = min(valuenum)) %>% ungroup() %>% 
  distinct(hadm_id, .keep_all = TRUE) %>% 
  filter(!is.na(valuenum))
# formula to get minimum SrCr if admitted at least 7 days prior to vasoactive start
# add to final DF and do calculation in FinalDF 

srcr1 <- creatinine  %>% 
  left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% 
  filter(!is.na(firstavpstart)) %>% 
  mutate(srcravp = ifelse(charttime >= firstavpstart - days(1) & charttime <= firstavpstart, "yes", "")) %>% 
  filter(srcravp == "yes") %>% 
  group_by(hadm_id) %>% mutate(srcravp = max(valuenum)) %>% ungroup() %>% 
  distinct(hadm_id, .keep_all = TRUE) %>% 
  filter(!is.na(valuenum))
# formula to get maximum SrCr within 24 hrs prior to AVP start
# added to final DF and do calculation in FinalDF 

### MV Baseline ####
# DF for mechanical ventilation at baseline - only for patients with AVP (have time 0 of avp start)
vent <- procedureevents %>% filter(ordercategoryname == "Ventilation" | ordercategoryname == "Intubation/Extubation") %>% 
  arrange(subject_id, hadm_id, starttime) %>% 
  group_by(stay_id) %>% # found intubation/extubation time based on each ICU stay
  mutate(intubation = min(starttime), 
         extubation = max(endtime)) %>% 
  distinct(hadm_id, .keep_all = TRUE) %>%
  mutate(mvduration = difftime(extubation, intubation, units = "days")) %>% 
  left_join(finaldf %>% dplyr::select(hadm_id, time0), by = "hadm_id") %>% 
  mutate(mv_time0 = case_when(time0 >= starttime & time0 <= endtime ~ "Yes", TRUE ~ "No"))

mvbl <- vent %>% filter(mv_time0 == "Yes") %>% 
  distinct(hadm_id, mv_time0)

### Hemodynamic stability #### 
# From eICU data so need to customize for MIMIC-IV
# start with map df
# isolate MAP at 6 hours (MAP between 3 and 6 hours after AVP start)

maphds <- map %>% left_join(vasopressors %>% dplyr::select(hadm_id, firstavpstart), by = "hadm_id") %>% 
  filter(!is.na(firstavpstart)) %>% 
  arrange(subject_id, hadm_id, desc(charttime)) %>% 
  distinct(subject_id, hadm_id, charttime, valuenum, .keep_all = TRUE) %>% 
  mutate(yes_map_6h = ifelse(charttime >= firstavpstart + hours(3) & charttime <= firstavpstart + hours(6), "yes", "")) %>%
  filter(yes_map_6h == "yes") %>%
  distinct(hadm_id, .keep_all = TRUE)

# create final HDS df (need to add in admissions and neqdoses dataframes)
hds <- vasopressors %>% filter(!is.na(firstavpstart)) %>% 
  left_join(admissions %>% dplyr::select(hadm_id, hospital_expire_flag, dischtime), by = "hadm_id") %>% 
  distinct(subject_id, hadm_id, firstavpstart, firstavpstop, hospital_expire_flag, dischtime) %>% 
  left_join(maphds %>% dplyr::select(hadm_id, valuenum), by = "hadm_id") %>%
  rename(map_6h = valuenum) %>% 
  left_join(neqdoses %>% dplyr::select(hadm_id, neqdose_0, neqdose_6), by = "hadm_id") %>% 
  mutate(diedin6hr = ifelse(dischtime <= firstavpstart + hours(6) & hospital_expire_flag == 1, "yes", "no")) %>% 
  mutate(hds = ifelse(neqdose_6 < neqdose_0 & map_6h >= 65 & diedin6hr == "no", "yes", "no")) %>% 
  mutate(hds = ifelse(neqdose_6 == 0 & diedin6hr == "no" & firstavpstop <= firstavpstart + hours(6) & is.na(map_6h), "yes", hds)) %>%
  mutate(hds = ifelse(is.na(hds), "no", hds)) %>% 
  mutate_if(is.character, as.factor) %>% 
  distinct(hadm_id, hds) %>% 
  distinct(hadm_id, .keep_all = TRUE)
rm(maphds)

### Duplicate Patients ####
duplicatemimic <- finaldf %>% arrange(subject_id, hadm_id, time0) %>% select(subject_id, hadm_id, time0) %>% 
  filter(!is.na(time0)) %>% filter(duplicated(subject_id)) %>% mutate(duplicatept = "Yes") 

## Step 4 ####
# Final Dataframe
# Run Below Code for final data frame
mimicfinaldf <- inclusionexclusion %>% filter(vasopressors == "vasopressors") %>% # should have 22703 encounters on vasopressors
  select(hadm_id, subject_id, stay_id, "time0" = "firstavpstart", age, gender, vasoactivestart, vasoactivestop, vasopressin, 
         "avpstop" = "firstavpstop", timefromshockonset_hr, lactateatavp, abxduringvasoactive) %>% 
  left_join(admissions %>% dplyr::select(hadm_id, admittime,dischtime, deathtime, admission_location, discharge_location,
                                         race), by = "hadm_id") %>% 
  mutate(race = ifelse(race == "BLACK/AFRICAN AMERICAN" | race == "BLACK/CAPE VERDEAN" | race == "BLACK/CARIBBEAN ISLAND" | race == "BLACK/AFRICAN", "Black", 
                       ifelse(race == "WHITE - OTHER EUROPEAN" | race == "WHITE - EASTERN EUROPEAN" | race == "WHITE - BRAZILIAN" | race == "WHITE" | race == "WHITE - RUSSIAN", "White",
                              "Other")), 
         inhospital_mortality = ifelse(!is.na(deathtime), "Yes", "No"), 
         abxduringvasoactive = ifelse(is.na(abxduringvasoactive), "No", abxduringvasoactive)) %>%
  mutate(gender = recode(gender, "M" = "Male", "F" = "Female")) %>%
  left_join(neqdoses %>% dplyr::select(hadm_id, stay_id, neqdose_0, neqdose_6), by = c("hadm_id" = "hadm_id", "stay_id" = "stay_id")) %>% 
  distinct(hadm_id, stay_id, .keep_all = TRUE) %>%
  left_join(icustays %>% dplyr::select(stay_id, first_careunit), by = "stay_id") %>% 
  mutate(iculocation = recode(first_careunit, "Medical Intensive Care Unit (MICU)" = "MICU", 
                              "Cardiac Vascular Intensive Care Unit (CVICU)" = "Cardiac ICU", 
                              "Surgical Intensive Care Unit (SICU)" = "SICU", 
                              "Coronary Care Unit (CCU)" = "Cardiac ICU",
                              "Medical/Surgical Intensive Care Unit (MICU/SICU)" = "Mixed ICU", 
                              "Trauma SICU (TSICU)" = "SICU", 
                              "Neuro Surgical Intensive Care Unit (Neuro SICU)" = "Mixed ICU", 
                              "Neuro Intermediate" = "Neurosciences ICU", 
                              "Neuro Stepdown" = "Neurosciences ICU")) %>% select(-first_careunit) %>% 
  left_join(weight %>% dplyr::select(hadm_id, weight), by = "hadm_id") %>% 
  mutate(weight = round(weight, digits = 1),
         time0day = floor_date(time0, unit = "days"), 
         time0day48h = floor_date(time0day + days(2), unit = "days")) %>%
  
  # Vasopressin initial dose
  left_join(avp %>% dplyr::filter(status == "initial start") %>% dplyr::select(hadm_id, rate), 
            by = "hadm_id") %>%
  mutate(initialavpdose_category = ifelse(!is.na(time0) & rate == 0.03, "0.03", 
                                          ifelse(!is.na(time0) & rate == 0.04, "0.04", "other"))) %>% 
  mutate(initialavpdose_category = ifelse(is.na(time0), NA, initialavpdose_category)) %>%
  rename("avpstartdose" = "rate") %>%
  
  # SOFA Score and SOFA Score Change - only evaluated in patients who survived 2.0 days
  left_join(sofa %>% dplyr::select(hadm_id, charttimeday, sofarenal, sofaliver, sofacoag, sofaresp, sofacardiac, totalsofa), 
            by = c("hadm_id" = "hadm_id", "time0day" = "charttimeday")) %>% # SOFA score at baseline
  rename("sofarenal_bl" = "sofarenal", "sofaliver_bl" = "sofaliver", "sofacoag_bl" = "sofacoag", "sofaresp_bl" = "sofaresp", 
         "sofacardiac_bl" = "sofacardiac", "totalsofa_bl" = "totalsofa") %>%
  left_join(sofa %>% dplyr::select(hadm_id, charttimeday, sofarenal, sofaliver, sofacoag, sofaresp, sofacardiac, totalsofa), 
            by = c("hadm_id" = "hadm_id", "time0day48h" = "charttimeday")) %>% # SOFA score at 48h
  rename("sofarenal_48h" = "sofarenal", "sofaliver_48h" = "sofaliver", "sofacoag_48h" = "sofacoag", "sofaresp_48h" = "sofaresp", 
         "sofacardiac_48h" = "sofacardiac", "totalsofa_48h" = "totalsofa") %>% select(-time0day, -time0day48h) %>% 
  mutate(totalsofa_48h = ifelse(difftime(deathtime,time0,units="days")<=2 & !is.na(deathtime),NA,totalsofa_48h)) %>%
  mutate(sofa_change = totalsofa_48h - totalsofa_bl) %>% 
  
  # Adding in other variables
  left_join(mvbl %>% dplyr::select(stay_id, hadm_id, mv_time0), by = c("hadm_id" = "hadm_id", "stay_id" = "stay_id")) %>%
  left_join(fluidbolus %>% dplyr::select(hadm_id, total_fluid_bolus), by = "hadm_id") %>% 
  mutate(total_fluid_bolus = case_when(is.na(total_fluid_bolus) ~ 0, TRUE~total_fluid_bolus)) %>% 
  left_join(hydrocort %>% dplyr::select(hadm_id, hydrocort_bl), by = "hadm_id") %>% 
  left_join(vent %>% dplyr::select(hadm_id,stay_id,intubation,extubation), by = c("hadm_id" = "hadm_id", "stay_id" = "stay_id")) %>%
  left_join(srcr %>% dplyr::select(hadm_id, minsrcr), by = "hadm_id") %>%
  left_join(pmh %>% dplyr::select(hadm_id, dialysis, diabetes, cirrhosis, copd, immune_suppression), by = "hadm_id") %>% 
  
  # Calculating AKI
  mutate(mdrdsrcr = ifelse(race == "Black" & gender == "F", (60 / (175 * age^(-0.203) * 1.212 * 0.742))^(1/-1.154), 
                    ifelse(race != "Black" & gender == "F", (60 / (175 * age^(-0.203) * 0.742))^(1/-1.154),
                    ifelse(race != "Black" & gender == "M", (60 / (175 * age^(-0.203)))^(1/-1.154), 
                    ifelse(race == "Black" & gender == "M", (60 / (175 * age^(-0.203) * 1.212))^(1/-1.154),""))))) %>% 
  mutate(mdrdsrcr = as.numeric(mdrdsrcr)) %>%
  mutate(baselinesrcr = ifelse(!is.na(minsrcr), minsrcr, mdrdsrcr)) %>% 
  mutate(baselinesrcr = as.numeric(baselinesrcr)) %>% 
  mutate(baselinesrcr = round(baselinesrcr, 2)) %>% 
  left_join(srcr1 %>% dplyr::select(hadm_id, srcravp), by = "hadm_id") %>% 
  mutate(akivasostart = ifelse(srcravp >= baselinesrcr*3 | srcravp >= 4, "AKI Stage 3", 
                        ifelse(srcravp >= baselinesrcr*2 & srcravp < baselinesrcr*3, "AKI Stage 2",
                        ifelse((srcravp - baselinesrcr) >= 0.3 | (srcravp >= baselinesrcr*1.5 & srcravp < baselinesrcr*2), "AKI Stage 1", 
                        ifelse(dialysis == "yes", "ESRD", "No AKI"))))) %>%
  mutate(akimva = ifelse(akivasostart == "ESRD" | akivasostart == "No AKI", akivasostart, 
                  ifelse(akivasostart == "AKI Stage 1" | akivasostart == "AKI Stage 2" | akivasostart == "AKI Stage 3", "AKI", ""))) %>% 
  mutate(akivasostart = replace_na(akivasostart, "No AKI"), akimva = replace_na(akimva, "No AKI")) %>% 
    # if NA for AKI, means no SrCr checked before AVP, assumed no AKI in those situations 
  
  # Adding extra variables
  mutate(neqdoseatavp_kg = neqdose_0/weight) %>%
  left_join(fluidbalance %>% dplyr::select(hadm_id, fluidbalance), by = "hadm_id") %>% # Fluid balance is in mLs
  left_join(vasopatavp %>% dplyr::select(hadm_id, norepinephrine, epinephrine, phenylephrine, dopamine), by = "hadm_id") %>% 
  left_join(dobut %>% dplyr::select(hadm_id, dobut), by = "hadm_id") %>% 
  left_join(milrinone %>% dplyr::select(hadm_id, milrinone), by = "hadm_id") %>%
  
  # Vasoactive Free Days
  mutate(avpduration_days = difftime(avpstop, time0, unit = "days")) %>%
  mutate(vasoactivefreedays = difftime(time0+days(28),vasoactivestop, units="days"),
         vasoactivefreedays = as.numeric(round(vasoactivefreedays,2))) %>% 
  mutate(vasoactivefreedays = case_when(inhospital_mortality == "Yes" & (time0+days(28)) >= vasoactivestop ~ 0, 
                                        vasoactivefreedays > 0 ~ vasoactivefreedays, TRUE ~ NA_real_), 
         vasoactivefreedays = ifelse(is.na(vasoactivefreedays), 0, vasoactivefreedays), 
         vasoactivefreedays = ifelse(vasoactivefreedays > 28, 28, vasoactivefreedays)) %>% 
  
  # Remaining outcome variables and calculating ICU Free days
  left_join(hds %>% dplyr::select(hadm_id, hds), by = "hadm_id") %>% 
  left_join(iculos %>% dplyr::select(hadm_id, last_icudischarge), by = "hadm_id") %>% 
  mutate(difftime = ifelse(!is.na(deathtime), difftime(dischtime, last_icudischarge, units = "hours"), NA),
         icu_mortality = ifelse(difftime < 48, "Yes", "No"),
         icufreedays = difftime(time0+days(28),last_icudischarge, units="days"),
         icufreedays = as.numeric(round(icufreedays,2)),
         icu_mortality = coalesce(icu_mortality,"No"),
         ifelse(inhospital_mortality == "Yes" & time0 + days(28) >= last_icudischarge, 0, 
                ifelse(icufreedays > 0, icufreedays, NA_real_)),
         icufreedays = ifelse(is.na(icufreedays)|icufreedays<0, 0, icufreedays), 
         icufreedays = ifelse(icufreedays > 28, 28, icufreedays)) %>% 
  select(-difftime) %>%
  
  # Changing to 0 or "No" if NAs
  mutate(fluidbalance = coalesce(fluidbalance,0),
         hydrocort_bl = coalesce(hydrocort_bl, "No"), 
         dobut = coalesce(dobut, "No"), 
         milrinone = coalesce(milrinone, "No"),
         mv_time0 = coalesce(mv_time0, "No"),
         dialysis = coalesce(dialysis, "No"),
         diabetes = coalesce(diabetes, "No"),
         cirrhosis = coalesce(cirrhosis, "No"),
         copd = coalesce(copd, "No"),
         immune_suppression = coalesce(immune_suppression, "No")) %>% 
  mutate(database = "MIMIC-IV") %>% 
  mutate(avpstart = time0) %>%
  mutate(no_chronic_health = NA) %>% # making placeholder to figure out later
  mutate(apacheiv = NA) %>% # making placeholder to figure out later
  mutate_if(is.character, as.factor) %>%
  
  # Below adding in screening criteria identifiers
  left_join(inclusionexclusion %>% dplyr::select(hadm_id, include_exclude_sepsis3, exclusionreason_sepsis3,
                                                 include_exclude_sepsis3nomap, exclusionreason_sepsis3nomap,
                                                 include_exclude_sepsis3bosch, exclusionreason_sepsis3bosch,
                                                 include_exclude_sepsis3nomapbosch, exclusionreason_sepsis3nomapbosch,
                                                 include_exclude_sscoding, exclusionreason_sscoding, 
                                                 include_exclude_angus, exclusionreason_angus), by = "hadm_id") %>%
  
  # selecting out to match eICU dataframe to combine
  select(subject_id, hadm_id, stay_id, database, time0, age, gender, race, 
         iculocation, weight, diabetes, cirrhosis, immune_suppression, copd, dialysis, no_chronic_health, 
         "mv_baseline" = "mv_time0", apacheiv, "sofa_baseline" = "totalsofa_bl", fluidbalance, 
         "totalfluidbolus" = "total_fluid_bolus", akivasostart, akimva, vasoactivestart, vasoactivestop, avpstart, 
         avpstop, avpstartdose, "neqdoseatavp" = "neqdose_0", neqdoseatavp_kg, timefromshockonset_hr, lactateatavp, abxduringvasoactive, 
         "neuse" = "norepinephrine", "epiuse" = "epinephrine", "peuse" = "phenylephrine", "dopamineuse" = "dopamine", 
         "dobutuse" = "dobut", "milrinoneuse" = "milrinone", "hydrocortuse" = "hydrocort_bl", inhospital_mortality, 
         icu_mortality, icufreedays, hds, "sofa_48hr" = "totalsofa_48h", sofa_change, avpduration_days, vasoactivefreedays, 
         include_exclude_sepsis3, exclusionreason_sepsis3, include_exclude_sepsis3nomap, exclusionreason_sepsis3nomap,
         include_exclude_sepsis3bosch, exclusionreason_sepsis3bosch, include_exclude_sepsis3nomapbosch, exclusionreason_sepsis3nomapbosch,
         include_exclude_sscoding, exclusionreason_sscoding, include_exclude_angus, exclusionreason_angus) %>% 
  
  # addressing duplicate patients (need duplicatemimic df below first) 
  left_join(duplicatemimic %>% dplyr::select(hadm_id, duplicatept), by = "hadm_id")  %>% 
  mutate(include_exclude_sepsis3 = ifelse(!is.na(duplicatept) & include_exclude_sepsis3 == "include", "exclude", include_exclude_sepsis3),
         exclusionreason_sepsis3= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3), "duplicate patient", exclusionreason_sepsis3)) %>% 
  mutate(include_exclude_sepsis3nomap = ifelse(!is.na(duplicatept) & include_exclude_sepsis3nomap == "include", "exclude", include_exclude_sepsis3nomap),
         exclusionreason_sepsis3nomap= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3nomap), "duplicate patient", exclusionreason_sepsis3nomap)) %>%
  mutate(include_exclude_sepsis3bosch = ifelse(!is.na(duplicatept) & include_exclude_sepsis3bosch == "include", "exclude", include_exclude_sepsis3bosch),
         exclusionreason_sepsis3bosch= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3bosch), "duplicate patient", exclusionreason_sepsis3bosch)) %>% 
  mutate(include_exclude_sepsis3nomapbosch = ifelse(!is.na(duplicatept) & include_exclude_sepsis3nomapbosch == "include", "exclude", include_exclude_sepsis3nomapbosch),
         exclusionreason_sepsis3nomapbosch = ifelse(!is.na(duplicatept) & is.na(exclusionreason_sepsis3nomapbosch), "duplicate patient", exclusionreason_sepsis3nomapbosch)) %>%
  mutate(include_exclude_sscoding = ifelse(!is.na(duplicatept) & include_exclude_sscoding == "include", "exclude", include_exclude_sscoding),
         exclusionreason_sscoding= ifelse(!is.na(duplicatept) & is.na(exclusionreason_sscoding), "duplicate patient", exclusionreason_sscoding)) %>% 
  mutate(include_exclude_angus = ifelse(!is.na(duplicatept) & include_exclude_angus == "include", "exclude", include_exclude_angus),
         exclusionreason_angus = ifelse(!is.na(duplicatept) & is.na(exclusionreason_angus), "duplicate patient", exclusionreason_angus)) %>% 
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
  mutate(include_exclude_sepsis3 = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3 == "include", "exclude", include_exclude_sepsis3),
         exclusionreason_sepsis3= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3), "NE off at AVP start", exclusionreason_sepsis3)) %>% 
  mutate(include_exclude_sepsis3nomap = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3nomap == "include", "exclude", include_exclude_sepsis3nomap),
         exclusionreason_sepsis3nomap= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3nomap), "NE off at AVP start", exclusionreason_sepsis3nomap)) %>%  
  mutate(include_exclude_sepsis3bosch = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3bosch == "include", "exclude", include_exclude_sepsis3bosch),
         exclusionreason_sepsis3bosch= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3bosch), "NE off at AVP start", exclusionreason_sepsis3bosch)) %>% 
  mutate(include_exclude_sepsis3nomapbosch = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sepsis3nomapbosch == "include", "exclude", include_exclude_sepsis3nomapbosch),
         exclusionreason_sepsis3nomapbosch= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sepsis3nomapbosch), "NE off at AVP start", exclusionreason_sepsis3nomapbosch)) %>%  
  mutate(include_exclude_sscoding = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_sscoding == "include", "exclude", include_exclude_sscoding),
         exclusionreason_sscoding= ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_sscoding), "NE off at AVP start", exclusionreason_sscoding)) %>% 
  mutate(include_exclude_angus = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & include_exclude_angus == "include", "exclude", include_exclude_angus),
         exclusionreason_angus = ifelse((is.na(neqdoseatavp)|neqdoseatavp==0) & is.na(exclusionreason_angus), "NE off at AVP start", exclusionreason_angus)) %>% 
  mutate_if(is.character, as.factor) %>%
  select(-duplicatept) %>%
  
  # changing patient identifiers to combine with eICU
  rename("patientid" = "subject_id", "hospitalid" = "hadm_id", "icuid" = "stay_id")

rm(finaldf)
write.csv(mimicfinaldf, file = "~/MIMIC-IV Data/MIMIC-IV_finaldf.csv", row.names = FALSE)

mimicfinaldf %>% tabyl(include_exclude_sepsis3) %>% adorn_pct_formatting()
mimicfinaldf %>% tabyl(include_exclude_sepsis3bosch) %>% adorn_pct_formatting()
mimicfinaldf %>% tabyl(include_exclude_sepsis3nomap) %>% adorn_pct_formatting()
mimicfinaldf %>% tabyl(include_exclude_sepsis3nomapbosch) %>% adorn_pct_formatting()


## Other Data Collection Points ####
# These are all saved and only need to be imported, should not need to be ran again

### Weight ####
weight <- chartevents %>% filter(label == "Admission Weight (Kg)" | label == "Admission Weight (lbs.)" | label == "Daily Weight") %>% 
  mutate(weight = ifelse(label == "Admission Weight (lbs.)", (valuenum/2.2), valuenum)) %>% 
  filter(weight != 0) %>% filter(!is.na(weight)) %>% 
  arrange(subject_id, charttime) %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)

save(weight, file = "~/MIMIC-IV Data/weight.RData")
load("~/MIMIC-IV Data/weight.RData")

### SOFA SCORE ####

#### SOFA Lab Components ####

# loading lab values
load("~/MIMIC-IV Data/creatinine.RData")
load("~/MIMIC-IV Data/bilirubin.RData")
load("~/MIMIC-IV Data/platelet.RData")
# PaO2 and FiO2 easy to run with below code

# Creatinine Values
crchartevents <- chartevents %>% filter(label == "Creatinine (serum)" | label == "Creatinine (whole blood)") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
crlabevents <- labevents %>% filter(label == "Creatinine, Serum" | label == "Creatinine, Whole Blood" | label == "Creatinine") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
creatinine <- rbind(crchartevents, crlabevents) %>% arrange(subject_id, charttime)
rm(crchartevents, crlabevents)
# save(creatinine, file = "~/MIMIC-IV Data/creatinine.RData")

# Platelet Values
pltchartevents <- chartevents %>% filter(label == "Platelet Count") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
pltlabevents <- labevents %>% filter(label == "Platelet Count") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
platelet <- rbind(pltchartevents, pltlabevents) %>% arrange(subject_id, charttime)
rm(pltchartevents, pltlabevents)
# save(platelet, file = "~/MIMIC-IV Data/platelet.RData")

# Bilirubin Values
bilichartevents <- chartevents %>% filter(label == "Total Bilirubin") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
bililabevents <- labevents %>% filter(label == "Bilirubin, Total" | label == "Bilirubin") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
bilirubin <- rbind(bilichartevents, bililabevents) %>% arrange(subject_id, charttime)
rm(bilichartevents, bililabevents)
# save(bilirubin, file = "~/MIMIC-IV Data/bilirubin.RData")

sofarenal <- creatinine %>% 
  mutate(charttimeday = floor_date(charttime, unit = "day")) %>%  
  arrange(hadm_id, charttimeday) %>% 
  group_by(hadm_id, charttimeday) %>% 
  mutate(maxlabresult = max(valuenum)) %>% 
  mutate(sofarenal = case_when(maxlabresult < 1.2 ~ 0,
                               maxlabresult >= 1.2 & maxlabresult < 2 ~ 1,
                               maxlabresult >= 2 & maxlabresult < 3.5 ~ 2,
                               maxlabresult >= 3.5 & maxlabresult < 5 ~ 3,
                               maxlabresult >= 5 ~ 4, TRUE ~ NA_real_)) %>%
  rename(labnamesrcr = label, creatinine = valuenum, maxsrcr = maxlabresult,) %>% 
  select(hadm_id, labnamesrcr, creatinine, maxsrcr, charttimeday, sofarenal) %>% 
  distinct(hadm_id, labnamesrcr, charttimeday, .keep_all = TRUE)

sofaliver <- bilirubin %>%  filter(!is.na(valuenum)) %>% 
  mutate(charttimeday = floor_date(charttime, unit = "day")) %>%  
  arrange(hadm_id, charttimeday) %>% 
  group_by(hadm_id, charttimeday) %>% 
  mutate(maxlabresult = max(valuenum)) %>%
  mutate(sofaliver = case_when(maxlabresult < 1.2 ~ 0,
                               maxlabresult >= 1.2 & maxlabresult < 2 ~ 1,
                               maxlabresult >= 2 & maxlabresult < 6 ~ 2,
                               maxlabresult >= 6 & maxlabresult < 12 ~ 3,
                               maxlabresult >= 12 ~ 4, TRUE ~ NA_real_)) %>%
  rename(labnamebili = label, bilirubin = valuenum, maxbili = maxlabresult) %>% 
  select(hadm_id, labnamebili, bilirubin, maxbili, charttimeday, sofaliver) %>% 
  distinct(hadm_id, labnamebili, charttimeday, .keep_all = TRUE)

sofacoag <- platelet %>% 
  mutate(charttimeday = floor_date(charttime, unit = "day")) %>%  
  arrange(hadm_id, charttimeday) %>% 
  group_by(hadm_id, charttimeday) %>% 
  mutate(minlabresult = min(valuenum)) %>%
  mutate(sofacoag = case_when(minlabresult >= 150 ~ 0,
                              minlabresult >= 100 & minlabresult < 150 ~ 1,
                              minlabresult >= 50 & minlabresult < 100 ~ 2,
                              minlabresult >= 20 & minlabresult < 50 ~ 3,
                              minlabresult < 20 ~ 4, TRUE ~ NA_real_)) %>%
  rename(labnameplt = label, platelet = valuenum, minplt = minlabresult) %>% 
  select(hadm_id, labnameplt, platelet, minplt, charttimeday, sofacoag) %>% 
  distinct(hadm_id, labnameplt, charttimeday, .keep_all = TRUE)

# SOFA Renal - need chartevents, procedureevents, labevents
# PaO2
pao2chartevents <- chartevents %>% filter(label == "Arterial O2 pressure" | label == "Arterial O2 Saturation") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
pao2labevents <- labevents %>% filter(label == "pO2") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category)
pao2 <- rbind(pao2chartevents, pao2labevents) %>% arrange(subject_id, charttime) %>% 
  rename(labnamepao2 = label, pao2 = valuenum, pao2charttime = charttime) %>% 
  mutate(charttimehour = floor_date(pao2charttime, unit = "hour")) %>%  
  arrange(hadm_id, charttimehour) %>% 
  group_by(hadm_id, charttimehour) %>% 
  mutate(minpao2 = min(pao2)) %>%
  select(subject_id, hadm_id, pao2charttime, pao2, labnamepao2, charttimehour, minpao2)

save(pao2, file ="~/MIMIC-IV Data/pao2.RData")
rm(pao2chartevents, pao2labevents)

# FiO2
fio2 <- chartevents %>% filter(category == "Respiratory") %>% filter(label == "Inspired O2 Fraction") %>% 
  select(subject_id, hadm_id, charttime, storetime, value, valuenum, valueuom, label, category) %>% 
  arrange(subject_id, charttime) %>% 
  rename(labnamefio2 = label, fio2 = valuenum, fio2charttime = charttime) %>% 
  mutate(charttimehour = floor_date(fio2charttime, unit = "hour")) %>%  
  arrange(hadm_id, charttimehour) %>% 
  group_by(hadm_id, charttimehour) %>% 
  mutate(maxfio2 = max(fio2)) %>%
  select(subject_id, hadm_id, fio2charttime, fio2, labnamefio2, charttimehour, maxfio2)

vent <- procedureevents %>% filter(ordercategoryname == "Ventilation" | ordercategoryname == "Intubation/Extubation") %>% 
  arrange(subject_id, hadm_id, starttime) %>% 
  group_by(stay_id) %>% # found intubation/extubation time based on each ICU stay
  mutate(intubation = min(starttime), 
         extubation = max(endtime)) %>% 
  distinct(hadm_id, .keep_all = TRUE) %>%
  mutate(mvduration = difftime(extubation, intubation, units = "days")) %>% 
  left_join(finaldf %>% dplyr::select(hadm_id, time0), by = "hadm_id") %>% 
  mutate(mv_time0 = case_when(time0 >= starttime & time0 <= endtime ~ "Yes", TRUE ~ "No"))

# SOFA Respiratory takes the max FiO2 and min PiO2 within the same hour and then calculates the SOFA Resp score on each day
sofaresp <- fio2 %>% 
  full_join(pao2, by = c("hadm_id" = "hadm_id", "charttimehour" = "charttimehour", "subject_id" = "subject_id")) %>%
  mutate(pfratio = round(minpao2 / (maxfio2 / 100), 0)) %>% 
  filter(!is.na(pfratio)) %>% 
  full_join(vent %>% dplyr::select(subject_id, hadm_id, stay_id, intubation, extubation), by = c("hadm_id" = "hadm_id", "subject_id" = "subject_id")) %>% 
  distinct(hadm_id, charttimehour, stay_id, .keep_all = TRUE) %>% 
  mutate(onvent = ifelse(charttimehour >= intubation & charttimehour <= extubation, "Yes", "No"), 
         charttimeday = floor_date(charttimehour, unit = "day")) %>%
  arrange(hadm_id, charttimeday) %>% 
  group_by(hadm_id, charttimeday) %>% 
  mutate(minpf = min(pfratio)) %>% 
  arrange(hadm_id, charttimeday, pfratio) %>%
  filter(!is.na(pfratio)) %>% 
  distinct(hadm_id, charttimeday, minpf, .keep_all = TRUE) %>% 
  select(subject_id, hadm_id, charttimeday, onvent, minpf) %>% 
  mutate(sofaresp = case_when(minpf >= 400 ~ 0,
                              minpf <400 & minpf >= 300 ~ 1,
                              minpf <300 & minpf >= 200  ~ 2,
                              onvent == "No" & minpf <= 200 ~ 2,
                              onvent == "Yes" & minpf < 200 & minpf >= 100 ~ 3,
                              onvent == "Yes" & minpf < 100 ~ 4,
                              TRUE ~ NA_real_)) %>% 
  mutate(sofaresp = if_else(is.na(sofaresp), 0, sofaresp))

#### Cardiac SOFA Score ####
# Need all pressor dfs and MAP df
# NE 
nemax <- ne %>% mutate(doseday = floor_date(starttime,unit = "day")) %>% 
  arrange(hadm_id, doseday) %>%
  group_by(hadm_id, doseday) %>% 
  mutate(maxnerate = max(rate)) %>%
  select(hadm_id, doseday, rate, maxnerate) %>%
  distinct(hadm_id, doseday, .keep_all = TRUE)

# Epi
epimax <- epi %>% mutate(doseday = floor_date(starttime,unit = "day")) %>% 
  arrange(hadm_id, doseday) %>%
  group_by(hadm_id, doseday) %>% 
  mutate(maxepirate = max(rate)) %>%
  select(hadm_id, doseday, rate, maxepirate) %>%
  distinct(hadm_id, doseday, .keep_all = TRUE)

# PE
pemax <- pe %>% mutate(doseday = floor_date(starttime,unit = "day")) %>% 
  arrange(hadm_id, doseday) %>%
  group_by(hadm_id, doseday) %>% 
  mutate(maxperate = max(rate)) %>%
  select(hadm_id, doseday, rate, maxperate) %>%
  distinct(hadm_id, doseday, .keep_all = TRUE)

# Dopamine
dopmax <- dop %>% mutate(doseday = floor_date(starttime,unit = "day")) %>% 
  arrange(hadm_id, doseday) %>%
  group_by(hadm_id, doseday) %>% 
  mutate(maxdoprate = max(rate)) %>%
  select(hadm_id, doseday, rate, maxdoprate) %>%
  distinct(hadm_id, doseday, .keep_all = TRUE)

# Dobutamine
dobutmax <- dobut %>% mutate(doseday = floor_date(starttime,unit = "day")) %>% 
  arrange(hadm_id, doseday) %>%
  group_by(hadm_id, doseday) %>% 
  mutate(maxdobutrate = max(rate)) %>%
  select(hadm_id, doseday, rate, maxdobutrate) %>%
  distinct(hadm_id, doseday, .keep_all = TRUE)

# Get minimum MAP on each offset day
# Need MAP for this
minmap <- map %>% mutate(charttimeday = floor_date(charttime, unit = "day")) %>% 
  arrange(hadm_id, charttimeday) %>% 
  group_by(hadm_id, charttimeday) %>% 
  mutate(minmap = min(valuenum)) %>% 
  select(hadm_id, charttimeday, minmap) %>% 
  distinct(hadm_id, charttimeday, .keep_all = TRUE)

# Cardiac SOFA - need nemax, epimax, pemax, dopmax, dobutmax, weight, minmap
sofacardiac <- nemax %>% full_join(epimax, by = c("hadm_id" = "hadm_id", "doseday" = "doseday")) %>% 
  full_join(pemax, by = c("hadm_id" = "hadm_id", "doseday" = "doseday")) %>%
  full_join(dopmax, by = c("hadm_id" = "hadm_id", "doseday" = "doseday")) %>% 
  full_join(dobutmax, by = c("hadm_id" = "hadm_id", "doseday" = "doseday"))%>% 
  select(hadm_id, doseday, maxnerate, maxepirate, maxperate, maxdoprate, maxdobutrate) %>% 
  mutate(zero = 0) %>% 
  mutate(maxnerate = ifelse(is.na(maxnerate), zero, maxnerate))%>% 
  mutate(maxepirate = ifelse(is.na(maxepirate), zero, maxepirate))%>%
  mutate(maxperate = ifelse(is.na(maxperate), zero, maxperate))%>%
  mutate(maxdoprate = ifelse(is.na(maxdoprate), zero, maxdoprate)) %>%
  mutate(maxdobutrate = ifelse(is.na(maxdobutrate), zero, maxdobutrate)) %>% 
  select(-zero) %>% 
  mutate(maxvasodose = (maxnerate + maxepirate + (maxperate / 10) + (maxdoprate / 2))) %>% 
  left_join(weight %>% dplyr::select(hadm_id, weight), by = "hadm_id") %>% 
  mutate(weight = ifelse(is.na(weight), 70, weight)) %>% # if patient did not have weight, imputed 70 kg 
  mutate(vasowtdose = (maxvasodose/weight)) %>%
  full_join(minmap, by = c("hadm_id" = "hadm_id", "doseday"= "charttimeday")) %>% 
  mutate(vasowtdose = if_else(is.na(vasowtdose), 0, vasowtdose)) %>% 
  mutate(vasodata = if_else(vasowtdose == 0, 1, 2)) %>% 
  mutate(vasodata = as.factor(vasodata)) %>% 
  mutate(vasodata = recode_factor(vasodata,  "1" = "no vasopressor", "2" = "vasopressors")) %>% 
  arrange(hadm_id, doseday) %>% 
  mutate(sofacardiac = case_when(vasowtdose > 0.1 ~ 4, 
                                 vasowtdose <= 0.1 & vasowtdose > 0 ~ 3,
                                 vasodata == "no vasopressor" & maxdobutrate > 0 ~ 2,
                                 vasodata == "no vasopressor" & minmap < 70 ~ 1, 
                                 vasodata == "no vasopressor" & minmap >= 70 ~ 0,
                                 TRUE ~ NA_real_))

rm(minmap, nemax, epimax, pemax, dobutmax, dopmax)

#### Total SOFA Score #### 
sofa <- sofarenal %>% full_join(sofaliver, by = c("hadm_id" = "hadm_id", "charttimeday" = "charttimeday")) %>%
  full_join(sofacoag, by = c("hadm_id" = "hadm_id", "charttimeday" = "charttimeday")) %>% 
  full_join(sofaresp, by = c("hadm_id" = "hadm_id", "charttimeday" = "charttimeday")) %>% 
  full_join(sofacardiac, by = c("hadm_id" = "hadm_id", "charttimeday" = "doseday")) %>% 
  mutate(sofarenal = ifelse(is.na(sofarenal), 0, sofarenal)) %>%
  mutate(sofaliver = ifelse(is.na(sofaliver), 0, sofaliver)) %>% 
  mutate(sofacoag = ifelse(is.na(sofacoag), 0, sofacoag)) %>% 
  mutate(sofacardiac = ifelse(is.na(sofacardiac), 0, sofacardiac)) %>%
  mutate(sofaresp = ifelse(is.na(sofaresp), 0, sofaresp)) %>% 
  mutate(totalsofa = sofarenal + sofaliver + sofacoag + sofacardiac + sofaresp) %>% 
  distinct(hadm_id, maxsrcr, charttimeday, maxbili, minplt, minpf, sofacardiac, totalsofa, .keep_all = TRUE)

save(sofa, file ="~/MIMIC-IV Data/sofa.RData")

rm(sofarenal, sofaliver, sofacoag, sofaresp, sofacardiac)


### APACHE Score ####
unique(chartevents$label) # maxes out at 1000 labels

# writing text file of all the unique labels
unique_values <- unique(chartevents$label) 
write(unique_values, file = "unique_labels.txt")
# 79 of these contain the word "APACHE" - searched in word document

apache <- chartevents %>% filter(grepl("apache", label, ignore.case = TRUE))
length(unique(apache$hadm_id))
# 28 unique hospital IDs for any APACHE documentation in chartevents
unique(apache$label)

apache3 <- apache %>% filter(label == "APACHEIII")
median(apache3$valuenum) # 92, does not match the median value in the systematic review that states median value of 41
# APACHE score must live somewhere else....

unique(labevents$label) # nothing with "apache" in this list

datetimeevents <- vroom("~/MIMIC-IV Data/mimic-iv-2.2/icu/datetimeevents.csv.gz") %>% 
  left_join(d_items, by = "itemid")
unique(datetimeevents$label) # nothing with "apache" in this list
