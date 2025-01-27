## ---------------------------
##
## Program: 1. Cohort creation
##
## Purposes: Create active-comparator user cohort. 
##
## Inclusion criteria:
## - First-ever prescription for exposure of interest between 01/01/2019 and 31/12/2022.
## - For antidiabetic cohort: active prescription for metformin at time of prescription
## for antidiabetic treatment of interest. 
##
## Exclusion criteria:
## - <18 years old at cohort entry
## - <365 days of look-back available
## - <365 days of follow-up available
## - Not linkable to HES, ONS, and Small Area Data
## - Missing sex or data entry error for birth date/death date

## Author: Gwen Aubrac
## Date Created: 2024-11-20
##
## Notes: 
## For antidiabetic cohorts, patients only contribute to one cohort (whichever treatment was their first treatment).
## Since CPRD data contains patients' lifetime history of prescriptions since they joined a practice,
## no additional washout period was implemented. 
## Results for the respective active comparator cohorts will be saved in separate folders in the analysis stage. 
##
## ---------------------------

#### SET UP ####

# define analysis from the following:
# 'antidepressant', 'antihypertensive', 'antidiabetic'
analysis <- ''

# path data
path_cprdA <- 'Z:/EPI/Protocol 24_004042/dataA'
path_cprdB <- 'Z:/EPI/Protocol 24_004042/dataB'
path_cprdC <- 'Z:/EPI/Protocol 24_004042/dataC (no followup)'
path_linkage_1 <- 'Z:/EPI/Protocol 24_004042/Data linkage/Results/Aurum_linked/Final_pt1'
path_linkage_2 <- 'Z:/EPI/Protocol 24_004042/Data linkage/Results/Aurum_linked/Final_pt2'

# path results
path_codes <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/codes'
path_local <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/local/', analysis)
setwd(path_local) # save intermediate results

# load packages
library(lubridate)
library(readxl)
library(dplyr)
library(magrittr)
library(haven)
library(parallel)
library(data.table)

options(scipen = 999)

# get linkable patients
col_classes <- c("character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
linkage_eligibility1 <- fread(paste(path_linkage_1, '24_004042_linkage_eligibility_aurum.txt', sep = '/'), colClasses = col_classes)
linkage_eligibility2 <- fread(paste(path_linkage_2, '24_004042_linkage_eligibility_aurum.txt', sep = '/'), colClasses = col_classes)
linkage_eligibility <- bind_rows(linkage_eligibility1, linkage_eligibility2)
rm(col_classes, linkage_eligibility1, linkage_eligibility2)

# study design
study_start = ymd(20190101)
study_end = ymd(20221231) 

#### INCLUSION CRITERIA ####
# please skip ahead if you've already run this code once
# for an antidiabetic active comparator cohort

# read treatment codes (keep only those relevant to cohort)
exposure <- data.frame()
exposure_files <- list.files(paste(path_codes, 'exposure', sep = '/'), pattern = '.xlsx', all.files = TRUE, full.names = TRUE)

if (analysis == 'antidepressant_cohort') {
  exposure_files <- exposure_files[grepl('snri.xlsx', exposure_files) | grepl('ssri.xlsx', exposure_files)]
}

if (analysis == 'antihypertensive_cohort') {
  exposure_files <- exposure_files[grepl('arb.xlsx', exposure_files) | grepl('acei.xlsx', exposure_files)]
}

if (analysis == c('antidiabetic_cohort')) {
  exposure_files <- exposure_files[grepl('glp1.xlsx', exposure_files) |
                                     grepl('sglt2.xlsx', exposure_files) |
                                     grepl('su.xlsx', exposure_files) |
                                     grepl('dpp4.xlsx', exposure_files) |
                                     grepl('metformin.xlsx', exposure_files)]
}

for (file in exposure_files) {
  data <- read_excel(file, trim_ws = TRUE, col_type = 'text')
  data$exposure <- tools::file_path_sans_ext(basename(file))
  exposure <- bind_rows(exposure, data)
}
rm(data, file, exposure_files)

# fx to iterate through rx data tables
# and retain rows containing product codes for exposure
filter_prescriptions <- function (file_path) {
  file <- read_sas(file_path) 
  file_filtered <- file %>%
    dplyr::select(id, product_code, date, dosageid, qty, duration) %>% 
    filter (product_code %in% exposure$ProdCodeId) %>%
    group_by (id) %>% 
    arrange (id, date)
  rx_for_exposure <<- bind_rows(rx_for_exposure, file_filtered)
}

# breaking it down into smaller chunks due to data size/memory constraints
therapy_filesA <- list.files(
  path_cprdA,
  pattern = 'therapy',
  all.files = TRUE,
  full.names = TRUE
)

therapy_filesA1 <- therapy_filesA[1:60]
therapy_filesA2 <- therapy_filesA[61:113]

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesA1, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_local, 'rx_A1.rds', sep='/'))
rm(rx_for_exposure)

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesA2, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_local, 'rx_A2.rds', sep='/'))
rm(rx_for_exposure)

therapy_filesB <- list.files(
  path_cprdB,
  pattern = 'therapy',
  all.files = TRUE,
  full.names = TRUE
)

therapy_filesB1 <- therapy_filesB[1:60]
therapy_filesB2 <- therapy_filesB[61:120]

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesB1, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_local, 'rx_B1.rds', sep='/'))
rm(rx_for_exposure)

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesB2, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_local, 'rx_B2.rds', sep='/'))
rm(rx_for_exposure)

therapy_filesC <- list.files(
  path_cprdC,
  pattern = 'therapy',
  all.files = TRUE,
  full.names = TRUE
)

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesC, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_local, 'rx_C.rds', sep='/'))
rm(rx_for_exposure)

rx_A1 <- readRDS(file = paste(path_local, 'rx_A1.rds', sep = '/'))
rx_A2 <- readRDS(file = paste(path_local, 'rx_A2.rds', sep = '/'))
rx_B1 <- readRDS(file = paste(path_local, 'rx_B1.rds', sep = '/'))
rx_B2 <- readRDS(file = paste(path_local, 'rx_B2.rds', sep = '/'))
rx_C <- readRDS(file = paste(path_local, 'rx_C.rds', sep = '/'))

rx_for_exposure <- rbind(rx_A1, rx_A2, rx_B1, rx_B2, rx_C)
rx_for_exposure <- merge(rx_for_exposure, exposure, by.x = 'product_code', by.y = 'ProdCodeId', all.x = TRUE)
saveRDS(rx_for_exposure, file = paste(path_local, 'rx_for_exposure.rds', sep ='/'))
# skip to here for antidiabetic cohorts
# as can use same rx_for_exposure file for all antidiabetic cohorts 

rm(filter_prescriptions)
rm(rx_A1, rx_A2, rx_B1, rx_B2, rx_C)
rm(therapy_filesA, therapy_filesB, therapy_filesA1, therapy_filesA2, therapy_filesB1, therapy_filesB2, therapy_filesC)

# select earliest prescription
# (excluding metformin for antidiabetic cohort)
# patients will only enter in one of the antidiabetic cohort

if (analysis %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  first_rx_for_exposure <- rx_for_exposure %>%
    filter(exposure != 'metformin')
} else {
  first_rx_for_exposure <- rx_for_exposure
}

first_rx_for_exposure %<>%
  arrange(id, date) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  rename(entry_date = date) %>% 
  select(-product_code, -ProductName, -duration, -qty, -dosageid)

# keep only first prescription occurring during study period
first_rx_for_exposure %<>%
  filter (study_start <= entry_date & entry_date <= study_end)

#### ANTIDIABETIC COHORT: EXCLUDE PATIENTS WITH NO ACTIVE METFORMIN RX AT ENTRY
# a) First we get a more accurate variable for the trt duration using an algorithm
# b) Then we determine the start and end date of metformin rx
# and we keep in the cohort only patients who had an active rx
# at their time of entry. 

if (analysis %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  active_met_rx <- rx_for_exposure %>%
    filter(exposure == 'metformin')
  
  # This algorithm to define treatment duration in the CPRD was developed by Pauline Reynier at the Lady Davis Institute
  common_dosages <- fread(
    'Z:/EPI/Protocol 24_004042/202406_lookup/202406_Lookups_CPRDAurum/202406_Lookups_CPRDAurum/common_dosages.txt'
  )
  common_dosages %<>% select(dosageid, dosage_text, daily_dose)
  summary(active_met_rx$duration == 0)
  
  ## a) Get daily dose
  
  # retrieve dose information
  active_met_rx <- merge(active_met_rx,
                         common_dosages,
                         by = 'dosageid',
                         all.x = TRUE)
  length(which(active_met_rx$daily_dose == 0))
  
  zero <- active_met_rx %>%
    filter(daily_dose == 0 & dosage_text != '')
  
  dosage_text <- unique(zero$dosage_text)
  dosage_text_sum <- zero %>%
    group_by(dosage_text) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  # replace 0 or empty doses based on dosage text
  active_met_rx <- active_met_rx %>%
    mutate(daily_dose = if_else(
      (daily_dose == 0 | daily_dose == '') &
        dosage_text %in% c(
          '1',
          '1 AS DIRECTED',
          '1 AS DIRECTED WHEN REQUIRED',
          '1 AS REQUIRED',
          '1 CAPSULE A DAY',
          '1 D',
          '1 WHEN NEEDED',
          '1D',
          'APPLY AS DIRECTED',
          'AS ADV',
          'AS DIR’,’AS DIRECTED',
          'AS DIRECTED BY CONSULTANT',
          'AS DIRECTED BY HOSPITAL',
          'AS DIRECTED BY PAEDIATRICIAN',
          'AS DIRECTED BY PAEDS',
          'AS DIRECTED BY SPECIALIST',
          'AS DIRECTED BY THE HOSPITAL',
          'AS DIRECTED FROM HOSPITAL',
          'AS NEEDED',
          'AS PER INSTRUCTION',
          'ASD',
          'ASD BY HOSPITAL',
          'ASD BY SPECIALIST',
          'ASDIR',
          'D',
          'ID',
          'MDU',
          'ONE AS DIRECTED',
          'ONE TO BE TAKEN AS REQUIRED',
          'ONE WHEN NEEDED',
          'TAKE AS DIRECTED',
          'TAKE ONE AS DIRECTED',
          'TAKE ONE AS NEEDED',
          'TAKE ONE CAPSULE A DAY',
          'TAKE ONE WHEN NEEDED',
          'TAKE ONE WHEN REQUIRED',
          'TO BE TAKEN AS DIRECTED',
          'TO BE USED AS DIRECTED',
          'USE AS DIECTED',
          'USE AS DIRECTED',
          'USE AS DIRECTED BY HOSPITAL',
          'WHEN REQUIRED',
          'AS DIRECTED',
          'AS DIR',
          "ONE TABLET AS DIRECTED",
          "TAKE ONE",
          "1 TABLET AS DIRECTED",
          "TAKE ONE TABLET",
          "ONE TO BE TAKEN",
          "USE ONE A DAY",
          "ONCE NIGHTLY",
          "ONE A TNIGHT",
          "AS DIRECTED.",
          "AS DIRETED",
          "TAKEN AS DIRECTED",
          "AS DISCUSSED"
        ),
      1,
      daily_dose
    ))
  
  active_met_rx <- active_met_rx %>%
    mutate(daily_dose = if_else(
      (daily_dose == 0 | daily_dose == '') &
        dosage_text %in% c('HALF A TABLET AS REQUIRED'),
      0.5,
      daily_dose
    ))
  
  
  active_met_rx <- active_met_rx %>%
    mutate(daily_dose = if_else(
      (daily_dose == 0 | daily_dose == '') &
        dosage_text %in% c(
          '1 -2 AS REQUIRED',
          'ONE OR TWO TO BE TAKEN AS DIRECTED',
          '1-2 AS REQUIRED',
          'ONE OR TWO TO BE TAKEN AS REQUIRED',
          'TAKE 1 OR 2 AS DIRECTED',
          'TAKE 1-2 WHEN REQUIRED',
          'TAKE ONE OR TWO AS DIRECTED',
          "1-2 AS DIRECTED",
          "1-2 AS NEEDED",
          "1-2"
        ),
      1.5,
      daily_dose
    ))
  
  
  active_met_rx <- active_met_rx %>%
    mutate(daily_dose = if_else(
      (daily_dose == 0 | daily_dose == '') &
        dosage_text %in% c('2 D', '2D', 'TAKE TWO CAPSULES A DAY', "2 CAPSULES A DAY"),
      2,
      daily_dose
    ))
  
  # still have some patients with missing daily dose
  # with following dosage text
  length(which(active_met_rx$daily_dose == 0))
  zero <- active_met_rx %>%
    filter(daily_dose == 0 & dosage_text != '')
  dosage_text <- unique(zero$dosage_text)
  dosage_text
  
  length(which(active_met_rx$qty == 0))
  length(which(active_met_rx$daily_dose == 0))
  
  ## Choose between duration1 (from CPRD) and duration2 (calculated from qty/daily_dose)
  
  active_met_rx$duration1 <- NA
  active_met_rx %<>% mutate(duration1 = if_else(1 <= duration &
                                                  duration <= 183, duration, NA))
  active_met_rx %<>% mutate(duration2 = if_else(qty > 0 &
                                                  daily_dose > 0, round(qty / daily_dose, 1), NA))
  active_met_rx %<>% mutate(duration2 = if_else(duration2 < 1 |
                                                  duration2 > 183, NA, duration2))
  
  active_met_rx %<>% mutate(same_duration = if_else(
    !is.na(duration1) &
      !is.na(duration2) & duration1 == duration2,
    1,
    0
  ))
  length(which(is.na(active_met_rx$duration1)))
  length(which(is.na(active_met_rx$duration2)))
  table(active_met_rx$same_duration)
  
  # same duration: use CPRD duration
  active_met_rx$duration <- NA
  active_met_rx %<>% mutate(duration = if_else(
    duration1 > 0 &
      duration2 > 0 & same_duration == 1,
    duration1,
    duration
  ))
  
  # different duration: use standard 28 days for a duration for a prescription
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      duration1 == 28 &
      (duration2 > 28 & duration2 <= 30),
    duration1,
    duration
  ))
  
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      duration2 == 28 &
      (duration1 > 28 & duration1 <= 30),
    duration2,
    duration
  ))
  
  # different duration: durations are within 2 days then take max
  active_met_rx %<>% mutate(
    duration = if_else(
      is.na(duration) &
        same_duration == 0 &
        !is.na(duration1) & !is.na(duration2) &
        duration1 - duration2 >= -2 | duration1 - duration2 <= 2,
      max(duration1, duration2),
      duration
    )
  )
  
  # different duration: duration1 = 1 and there is a value for duration 2 then take duration2
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      duration1 == 1 & !is.na(duration2),
    duration2,
    duration
  ))
  
  # different duration: duration2 = 1 and there is a value for duration 1 then take duration1
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      duration2 == 1 & !is.na(duration1),
    duration1,
    duration
  ))
  
  # daily dose != 1 and duration1 = qty then take duration2
  active_met_rx %<>% mutate(
    duration = if_else(
      is.na(duration) &
        same_duration == 0 &
        daily_dose != 1 &
        qty == duration1 & daily_dose %in% c(0.5, 1.5, 2),
      duration2,
      duration
    )
  )
  
  # daily dose != 1 and duration1 = 28 then take duration2
  active_met_rx %<>% mutate(
    duration = if_else(
      is.na(duration) &
        same_duration == 0 &
        !is.na(duration2) &
        duration1 == 28 & daily_dose %in% c(0.5, 1.5, 2),
      duration2,
      duration
    )
  )
  
  
  # one of the durations is equal to 28 days then use 28 days
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      !is.na(duration1) &
      !is.na(duration2) &
      (duration1 == 28 | duration2 == 28),
    28,
    duration
  ))
  
  # duration1 and duration2 are different with no specific criteria then take duration2
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      !is.na(duration1) &
      !is.na(duration2),
    duration2,
    duration
  ))
  
  # only duration1 available then take duration1
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      !is.na(duration1) &
      is.na(duration2),
    duration1,
    duration
  ))
  
  # only duration2 available then take duration2
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      is.na(duration1) &
      !is.na(duration2),
    duration2,
    duration
  ))
  
  # duration1 and duration2 are not available but qty = 28 or 56 use qty
  active_met_rx %<>% mutate(
    duration = if_else(
      is.na(duration) &
        same_duration == 0 &
        is.na(duration1) &
        is.na(duration2) &
        daily_dose %in% c(0, '') &
        qty %in% c(28, 56),
      qty,
      duration
    )
  )
  
  # still no duration but valid qty then use qty
  active_met_rx %<>% mutate(
    duration = if_else(
      is.na(duration) &
        same_duration == 0 &
        is.na(duration1) &
        is.na(duration2) &
        qty >= 1 & qty <= 183,
      qty,
      duration
    )
  )
  
  
  # qty > 183
  active_met_rx %<>% mutate(duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      daily_dose %in% c(0, '') &
      qty > 183,
    28,
    duration
  ))
  
  # all other scenarios use 28
  active_met_rx %<>% mutate(duration = if_else(is.na(duration), 28, duration))
  
  rm(zero, dosage_text_sum, common_dosages, dosage_text)
  
  ## b) Get metformin rx start and end dates
  active_met_rx %<>%
    mutate(rx_end = date + duration) %>%
    select(
      -dosageid,
      -dosage_text,
      -daily_dose,
      -duration1,
      -duration2,
      -same_duration
    )
  
  entry_dates <- first_rx_for_exposure %>% select(id, entry_date)
  active_met_rx <- merge(active_met_rx, entry_dates, by = 'id', all.x = TRUE)
  
  # keep metformin rx that start prior to entry and end after entry
  active_met_rx %<>%
    filter(date <= entry_date & entry_date <= rx_end)
  
  # keep only patients with an active metformin rx at entry
  first_rx_for_exposure %<>% filter(id %in% active_met_rx$id)
  rm(active_met_rx, common_dosages, entry_dates)
}

summary(first_rx_for_exposure$entry_date)

#### EXCLUSION CRITERIA ####

## Exclude patients:
## 1. <18 years old
## 2. <365 days look-back available since cohort entry
## 3. <365 days of follow-up available from cohort entry. 
## 4. No linkage to HES or ONS
## 5. Missing sex or data entry error for birth date/death date

## Extract patient information

# read patient data
patients1 <- read_sas(paste(path_cprdA, 'patient1.sas7bdat', sep = '/'))
patients2 <- read_sas(paste(path_cprdB, 'patient1.sas7bdat', sep = '/'))
patients3 <- read_sas(paste(path_cprdC, 'patient1.sas7bdat', sep = '/'))
patients <- bind_rows(patients1, patients2, patients3)

rm(patients1, patients2, patients3)

# join with practice data
practice1 <- read_sas(paste(path_cprdA, 'practice1.sas7bdat', sep = '/'))
practice2 <- read_sas(paste(path_cprdB, 'practice1.sas7bdat', sep = '/'))
practice3 <- read_sas(paste(path_cprdB, 'practice1.sas7bdat', sep = '/'))
practice <- bind_rows(practice1, practice2, practice3)

rm(practice1, practice2, practice3)

# may have duplicate practices since merged data from males and females
# so remove duplicates
practice %<>%
  group_by(practice) %>% 
  slice(1)

cohort <- merge(first_rx_for_exposure, patients, by = 'id', all.x = TRUE)
cohort %<>% rename(treatment = exposure)
cohort <- left_join(cohort, practice, by = 'practice', relationship = 'many-to-one')

table(cohort$acceptable)
rm(first_rx_for_exposure)

# merge with ONS dod
col_classes <- c("character", "character", "character", "character", "character", "character", "character", "character")
death_ons1 <- fread(paste(path_linkage_1, 'death_patient_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
death_ons2 <- fread(paste(path_linkage_2, 'death_patient_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
death_ons <- bind_rows(death_ons1, death_ons2)

rm(death_ons1, death_ons2, col_classes)

death_ons %<>% 
  select(patid, dod) %>% 
  rename(ons_dod = dod) %>% 
  mutate(ons_dod = as.Date(ons_dod, format = '%d/%m/%Y'))

cohort <- merge(cohort, death_ons, by.x = 'id', by.y = 'patid', all.x = TRUE)

cohort$death_date <- pmin(
  cohort$emis_dod,
  cohort$cprd_dod,
  cohort$ons_dod,
  na.rm = TRUE
)

rm(death_ons)

## 1. Apply exclusion criteria (1): >18 at cohort entry

# define age at cohort entry as latest possible based on yob/mob
length(which(is.na(cohort$yob)))

cohort %<>%
  mutate (birthdate = if_else(is.na(mob), 
                              ymd(paste(yob, '12', '31' , sep = '')), 
                              ceiling_date(ym(paste(yob, mob, sep = '')), "month") - 1),
          age_at_entry = time_length(difftime(entry_date, birthdate), 'years'))

# exclude patients <18
cohort %<>% filter (age_at_entry>=18)
length(unique(cohort$id))

## 2. Apply exclusion criteria (2): >365 days of look-back available from cohort entry

# exclude patients with less than 1 year medical history
cohort %<>% filter (cohort$entry_date - cohort$regstart >= 365)
length(unique(cohort$id))

## 3. Apply exclusion criteria (3): >365 days of follow-up with practice available since cohort entry
# note: we are not excluding patients who died within a year and whose regimen ended due to this
# because regimen ends with death

# a) patients who did not die and whose regimen ended within a year of entry
cohort %<>%
  mutate(less_than_year_alive = if_else(
    is.na(death_date) &
      !is.na(regend) &
      regend - entry_date < 365,
    1,
    0
  ))

table(cohort$less_than_year_alive)

# b) patients who died after regimen ended and whose regimen ended within a year of entry
cohort %<>%
  mutate(
    less_than_year_died = if_else(
      !is.na(death_date) &
        !is.na(regend) &
        death_date > regend &
        regend - entry_date < 365,
      1,
      0
    )
  )

table(cohort$less_than_year_died)

# exclude patients from a) and b)
cohort %<>%
  filter(less_than_year_alive == 0, 
         less_than_year_died == 0) %>% 
  select(-less_than_year_alive, less_than_year_died)
length(unique(cohort$id))

# c) exclude patients whose practice stopped collecting data within 1 year
table(!is.na(cohort$lcd) & cohort$lcd - cohort$entry_date < 365)
cohort %<>% filter (is.na(lcd) | lcd - entry_date >= 365)
length(cohort$id)

## 4. Apply exclusion criteria (4): linkage to HES and ONS

linkage_eligible_ids <- linkage_eligibility %>% 
  filter(ons_death_e == 1 &
           hes_apc_e == 1 &
           lsoa_e ==1)

table(cohort$id %in% linkage_eligible_ids$patid)
cohort %<>% filter (id %in% linkage_eligible_ids$patid)
length(cohort$id)

rm(linkage_eligible_ids, linkage_eligibility)

## 5. Apply exclusion criteria (5): missing sex or data entry errors for birth or death

table(!is.na(cohort$death_date) & cohort$death_date <= cohort$entry_date)
table(!is.na(cohort$death_date) & cohort$death_date <= cohort$birthdate)
table(is.na(cohort$male))

cohort %<>%
  filter(is.na(death_date) | death_date>entry_date) %>% # remove patients who died before entry
  filter(is.na(death_date) | death_date>birthdate) %>% # remove patients who died before birth date
  filter(!is.na(male)) # remove patients with missing sex
length(cohort$id)

rm(exposure, practice, patients)
table(cohort$treatment)

# save cohort
saveRDS(cohort, file = paste(path_local, 'cohort_creation.rds', sep='/'))
