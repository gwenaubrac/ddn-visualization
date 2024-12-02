## ---------------------------
##
## Program: 1. Cohort creation
##
## Purposes: Create active-comparator user cohort. 
##
## Inclusion criteria:
## - Prescription for exposure of interest between 01/01/2019 and 31/12/2022.
##
## Exclusion criteria:
## - <18 years old at cohort entry
## - <365 days of look-back available
## - <365 days of follow-up available
## - Not linkable to HES, ONS, and Small Area Data
## - Missing sex or data entry error for birth date/death date

## Author: Gwen Aubrac
##
## Date Created: 2024-11-20
##
## ---------------------------

#### SET UP ####

# define analysis
analysis <- 'su_vs_dpp4'

# path data
path_cprdA <- 'Z:/EPI/Protocol 24_004042/dataA'
path_cprdB <- 'Z:/EPI/Protocol 24_004042/dataB'
path_cprdC <- 'Z:/EPI/Protocol 24_004042/dataC (no followup)'
path_linkage_1 <- 'Z:/EPI/Protocol 24_004042/Data linkage/Results/Aurum_linked/Final_pt1'
path_linkage_2 <- 'Z:/EPI/Protocol 24_004042/Data linkage/Results/Aurum_linked/Final_pt2'

# path results
path_codes <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/codes'
path_cohort <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/', analysis)
path_temp <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/', analysis, '/temp')
setwd(path_temp) # save intermediate results

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

# read treatment codes (keep only those for trt in 'analysis')
exposure <- data.frame()
exposure_files <- list.files(paste(path_codes, 'exposure', sep = '/'), pattern = '.xlsx', all.files = TRUE, full.names = TRUE)

keep_trt <- strsplit(analysis, "_")[[1]]
keep_trt <- as.list(keep_trt[c(1, 3)])
keep_trt[[1]] <- paste0(keep_trt[[1]], '.xlsx')
keep_trt[[2]] <- paste0(keep_trt[[2]], '.xlsx')
pattern <- paste(keep_trt, collapse = "|")
exposure_files <- exposure_files[grepl(pattern, exposure_files)]
exposure_files

for (file in exposure_files) {
  data <- read_excel(file, trim_ws = TRUE, col_type = 'text')
  data$exposure <- tools::file_path_sans_ext(basename(file))
  exposure <- bind_rows(exposure, data)
}
rm(data, file, keep_trt, pattern, exposure_files)

# iterate through data tables and retain rows containing product codes for exposure
filter_prescriptions <- function (file_path) {
  file <- read_sas(file_path) 
  file_filtered <- file %>%
    dplyr::select(id, product_code, date, dosageid, qty, duration) %>% 
    filter (product_code %in% exposure$ProdCodeId) %>%
    group_by (id) %>% 
    mutate (first_rx = min(date)) %>%
    filter (first_rx >= study_start & first_rx <= study_end) %>% 
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
saveRDS(rx_for_exposure, file = paste(path_temp, 'rx_A1.rds', sep='/'))
rm(rx_for_exposure)

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesA2, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_temp, 'rx_A2.rds', sep='/'))
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
saveRDS(rx_for_exposure, file = paste(path_temp, 'rx_B1.rds', sep='/'))
rm(rx_for_exposure)

gc()
rx_for_exposure <- data.frame()
mclapply(therapy_filesB2, filter_prescriptions)
saveRDS(rx_for_exposure, file = paste(path_temp, 'rx_B2.rds', sep='/'))
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
saveRDS(rx_for_exposure, file = paste(path_temp, 'rx_C.rds', sep='/'))
rm(rx_for_exposure)

rx_A1 <- readRDS(file = paste(path_temp, 'rx_A1.rds', sep = '/'))
rx_A2 <- readRDS(file = paste(path_temp, 'rx_A2.rds', sep = '/'))
rx_B1 <- readRDS(file = paste(path_temp, 'rx_B1.rds', sep = '/'))
rx_B2 <- readRDS(file = paste(path_temp, 'rx_B2.rds', sep = '/'))
rx_C <- readRDS(file = paste(path_temp, 'rx_C.rds', sep = '/'))

rx_for_exposure <- rbind(rx_A1, rx_A2, rx_B1, rx_B2, rx_C)
rx_for_exposure <- merge(rx_for_exposure, exposure, by.x = 'product_code', by.y = 'ProdCodeId', all.x = TRUE)
saveRDS(rx_for_exposure, file = paste(path_temp, 'rx_for_exposure.rds', sep ='/'))

rm(filter_prescriptions)

# select earliest prescription
first_rx_for_exposure <- rx_for_exposure %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  rename(entry_date = first_rx) %>% 
  select(-product_code, -ProductName, -date, -duration, -qty, -dosageid)

# keep only first prescription occurring during study period
first_rx_for_exposure %<>%
  filter (study_start <= entry_date & entry_date <= study_end)

summary(first_rx_for_exposure$entry_date)
rm(rx_A1, rx_A2, rx_B1, rx_B2, rx_C)

#### EXCLUSION CRITERIA ####

## Exclude patients who are:
## 1. >18 years old
## 2. Less than 365 days look-back available since cohort entry
## 3. Less than 365 days of follow-up available from cohort entry. 
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

rm(linkage_eligible_ids, linkage_eligibility)

## 5. Apply exclusion criteria (5): missing sex or data entry errors for birth or death

table(!is.na(cohort$death_date) & cohort$death_date <= cohort$entry_date)
table(!is.na(cohort$death_date) & cohort$death_date <= cohort$birthdate)
table(is.na(cohort$male))

cohort %<>%
  filter(is.na(death_date) | death_date>entry_date) %>% # remove patients who died before entry
  filter(is.na(death_date) | death_date>birthdate) %>% # remove patients who died before birth date
  filter(!is.na(male)) # remove patients with missing sex

rm(therapy_filesA, therapy_filesB, therapy_filesA1, therapy_filesA2, therapy_filesB1, therapy_filesB2, therapy_filesC)
rm(exposure, practice, patients)

table(cohort$treatment)

# save cohort
saveRDS(cohort, file = paste(path_temp, 'cohort_creation.rds', sep='/'))
