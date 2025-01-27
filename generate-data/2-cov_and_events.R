## ---------------------------
##
## Program: 2. Covariates and Events
##
## Purposes: Add covariates to the cohort dataframe. 
## - For categorical variables, set reference group to largest group. 
## - For events, two dates and variables are created:
##    a) date of first occurrence ever ('event_date-base') -> event at baseline ('event_base') 
##    b) date of first occurrence since entry ('event_date') -> event during study ('event')
## Events are assess from CPRD (GP dx) and HES (hospital dx and events) separately. 
## The earliest date between both is then used, except for hospitalization-based events
## (for which only the HES data is used).
##
## Author: Gwen Aubrac
## Date Created: 2024-11-21
##
## Note: Results for the respective active comparator cohorts will be saved in separate folders in the analysis stage. 
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

path_cprd_event <- paste(path_codes, 'event', 'aurum_codes', sep = '/')
path_hes_event <- paste(path_codes, 'event', 'icd_codes', sep = '/')

# load packages
library(lubridate)
library(readxl)
library(dplyr)
library(magrittr)
library(haven)
library(parallel)
library(data.table)
library(tidyr)

# study design
study_start = ymd(20190101)
study_end = ymd(20221231) 

# load cohort
cohort <- readRDS(paste(path_local, 'cohort_creation.rds', sep = '/'))

#### COVARIATES ####

# calendar month and year of cohort entry
cohort <- transform(cohort,
                    month = factor(format(entry_date, format = '%B'), levels = month.name),
                    year = format(entry_date, format = '%Y'))

cohort$year_month <- format(as.Date(cohort$entry_date), '%Y-%m')
str(cohort$year)
str(cohort$year_month)

cohort %<>%
  mutate(year = as.factor(year),
         year_month = as.factor(year_month))

# set reference group to largest group
most_common_year <- cohort %>% 
  count(year) %>% 
  slice_max(n) %>% 
  pull(year)

#cohort$year <- relevel(cohort$year, ref = as.character(most_common_year))
cohort$year <- relevel(cohort$year, ref = '2019')

# age group
cohort <- cohort %>% 
  mutate (
    age_group = dplyr::case_when(
      age_at_entry >= 18 & age_at_entry < 25 ~ '18-24',
      age_at_entry >= 25 & age_at_entry < 35 ~ '25-34',
      age_at_entry >= 35 & age_at_entry < 45 ~ '35-44',
      age_at_entry >= 45 & age_at_entry < 55 ~ '45-54',
      age_at_entry >= 55 & age_at_entry < 65 ~ '55-64',
      age_at_entry >= 65 & age_at_entry < 75 ~ '65-74',
      age_at_entry >= 75 & age_at_entry < 85 ~ '75-84',
      age_at_entry > 85 ~ '>85'
    ),
    age_group = factor(age_group, levels = c('18-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '>85'))
  )

most_common_age <- cohort %>% 
  count(age_group) %>% 
  slice_max(n) %>% 
  pull(age_group)

#cohort$age_group <- relevel(cohort$age_group, ref = as.character(most_common_age))
cohort$age_group <- relevel(cohort$age_group, ref = '55-64')
table(cohort$age_group)

cohort %<>%
  mutate(above_65 = if_else(age_at_entry >= 65, 1, 0))
table(cohort$above_65)

# ethnicity
col_classes <- c("character", "character", "character", "character")
ethnicity1 <- fread(paste(path_linkage_1, 'hes_patient_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
ethnicity2 <- fread(paste(path_linkage_2, 'hes_patient_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
ethnicity <- bind_rows(ethnicity1, ethnicity2) %>% 
  select(patid, gen_ethnicity) %>% 
  rename (ethnicity = gen_ethnicity)

rm(ethnicity1, ethnicity2)

cohort <- merge(cohort, ethnicity, by.x = 'id', by.y = 'patid', all.x = TRUE)
cohort %<>% mutate(ethnicity = as.factor(ethnicity))
summary(cohort$ethnicity)

cohort %<>% mutate(ethnicity = if_else(ethnicity == '', NA, ethnicity)) # replace blanks by NA and then 'Unknown'
cohort %<>% mutate (ethnicity = as.factor(if_else(is.na(ethnicity), 'Unknown', ethnicity)))
summary(cohort$ethnicity)

most_common_ethnic <- cohort %>% 
  count(ethnicity) %>% 
  slice_max(n) %>% 
  pull(ethnicity)

#cohort$ethnicity <- relevel(cohort$ethnicity, ref = as.character(most_common_ethnic))
cohort$ethnicity <- relevel(cohort$ethnicity, ref = 'White')

# deprivation index
col_classes <- c("character", "character", "character")
deprivation1 <- fread(paste(path_linkage_1, 'patient_2019_imd_24_004042.txt', sep = '/'), colClasses = col_classes)
deprivation2 <- fread(paste(path_linkage_2, 'patient_2019_imd_24_004042.txt', sep = '/'), colClasses = col_classes)
deprivation <- bind_rows(deprivation1, deprivation2) %>% 
  select(patid, e2019_imd_5) %>% 
  rename (deprivation = e2019_imd_5)

rm(deprivation1, deprivation2, col_classes)

cohort <- merge(cohort, deprivation, by.x = 'id', by.y = 'patid', all.x = TRUE)

cohort %<>% mutate(deprivation = as.factor(deprivation))
summary(cohort$deprivation)

cohort %<>% mutate(deprivation = if_else(deprivation == '', NA, deprivation)) # replace blanks by NA and then 'Unknown'
cohort %<>% mutate (deprivation = as.factor(if_else(is.na(deprivation), 'Unknown', deprivation)))
summary(cohort$deprivation)

most_common_depr <- cohort %>% 
  count(deprivation) %>% 
  slice_max(n) %>% 
  pull(deprivation)

#cohort$deprivation <- relevel(cohort$deprivation, ref = as.character(most_common_depr))
cohort$deprivation <- relevel(cohort$deprivation, ref = '5')

rm(most_common_year, most_common_age, most_common_ethnic, most_common_depr, ethnicity, deprivation)

#### ALL-CAUSE MORTALITY ####

cohort %<>% mutate(death = if_else(!is.na(death_date), 1, 0))
table(cohort$death)

#### EVENTS FROM CPRD ####

## Get event dates

# read event codes from CPRD dx
cprd_event_codes <- data.frame()
cprd_event_files <- list.files(path_cprd_event, pattern = '.xlsx', all.files = TRUE, full.names = TRUE)

for (file in cprd_event_files) {
  data <- read_excel(file, trim_ws = TRUE, col_type = 'text')
  data$event_name <- tools::file_path_sans_ext(basename(file))
  cprd_event_codes <- bind_rows(cprd_event_codes, data)
}

cprd_event_codes %<>%
  select(MedCodeId, Term, event_name)

rm(data, file, cprd_event_files)

# identify occurrence of events
cohort_ids <- as.list(cohort$id)

filter_event <- function (file_path) {
  file <- read_sas(file_path)
  file_filtered <- file %>%
    dplyr::select(id, medical_code, date) %>% 
    filter(id %in% cohort_ids & medical_code %in% cprd_event_codes$MedCodeId) %>% 
    arrange (id, date)
  cprd_event <<- bind_rows(cprd_event, file_filtered)
}

observation_filesA <- list.files(
  path_cprdA,
  pattern = 'observation',
  all.files = TRUE,
  full.names = TRUE
)

observation_filesA1 <- observation_filesA[1:60]
observation_filesA2 <- observation_filesA[61:113]
observation_filesA3 <- observation_filesA[114:189]

gc()
cprd_event <- data.frame()
mclapply(observation_filesA1, filter_event)
saveRDS(cprd_event, file = paste(path_local,  'cprd_event_A1.rds', sep='/'))
rm(cprd_event)

gc()
cprd_event <- data.frame()
mclapply(observation_filesA2, filter_event)
saveRDS(cprd_event, file = paste(path_local,  'cprd_event_A2.rds', sep='/'))
rm(cprd_event)

gc()
cprd_event <- data.frame()
mclapply(observation_filesA3, filter_event)
saveRDS(cprd_event, file = paste(path_local,  'cprd_event_A3.rds', sep='/'))
rm(cprd_event)

observation_filesB <- list.files(
  path_cprdB,
  pattern = 'observation',
  all.files = TRUE,
  full.names = TRUE
)

observation_filesB1 <- observation_filesB[1:60]
observation_filesB2 <- observation_filesB[61:113]
observation_filesB3 <- observation_filesB[114:197]

gc()
cprd_event <- data.frame()
mclapply(observation_filesB1, filter_event)
saveRDS(cprd_event, file = paste(path_local, 'cprd_event_B1.rds', sep='/'))
rm(cprd_event)

gc()
cprd_event <- data.frame()
mclapply(observation_filesB2, filter_event)
saveRDS(cprd_event, file = paste(path_local, 'cprd_event_B2.rds', sep='/'))
rm(cprd_event)

gc()
cprd_event <- data.frame()
mclapply(observation_filesB3, filter_event)
saveRDS(cprd_event, file = paste(path_local, 'cprd_event_B3.rds', sep='/'))
rm(cprd_event)

observation_filesC <- list.files(
  path_cprdC,
  pattern = 'observation',
  all.files = TRUE,
  full.names = TRUE
)

gc()
cprd_event <- data.frame()
mclapply(observation_filesC, filter_event)
saveRDS(cprd_event, file = paste(path_local, 'cprd_event_C.rds', sep='/'))

rm(cprd_event)

cprd_event_A1 <- readRDS(file = paste(path_local, 'cprd_event_A1.rds', sep = '/'))
cprd_event_A2 <- readRDS(file = paste(path_local, 'cprd_event_A2.rds', sep = '/'))
cprd_event_A3 <- readRDS(file = paste(path_local, 'cprd_event_A3.rds', sep = '/'))
cprd_event_B1 <- readRDS(file = paste(path_local, 'cprd_event_B1.rds', sep = '/'))
cprd_event_B2 <- readRDS(file = paste(path_local, 'cprd_event_B2.rds', sep = '/'))
cprd_event_B3 <- readRDS(file = paste(path_local, 'cprd_event_B3.rds', sep = '/'))
cprd_event_C <- readRDS(file = paste(path_local, 'cprd_event_C.rds', sep = '/'))

cprd_event <- rbind(cprd_event_A1, cprd_event_A2, cprd_event_A3, cprd_event_B1, cprd_event_B2, cprd_event_B3, cprd_event_C)
cprd_event <- merge(cprd_event, cprd_event_codes, by.x = 'medical_code', by.y = 'MedCodeId')

rm(observation_filesA, observation_filesB, observation_filesA1, observation_filesA2, observation_filesA3, 
   observation_filesB1, observation_filesB2, observation_filesB3, observation_filesC)

rm(cprd_event_A1, cprd_event_A2, cprd_event_A3, cprd_event_B1, cprd_event_B2, cprd_event_B3, cprd_event_C)
rm(cprd_event_codes)

#### EVENTS FROM HES ####

## Get event dates

# read event codes from hes
hes_event_codes <- data.frame()

hes_event_files <- list.files(path_hes_event, pattern = '.xlsx', all.files = TRUE, full.names = TRUE)

for (file in hes_event_files) {
  data <- read_excel(file, trim_ws = TRUE, col_type = 'text')
  data$event_name <- tools::file_path_sans_ext(basename(file))
  hes_event_codes <- bind_rows(hes_event_codes, data)
}

rm(data, file, hes_event_files)

# formatting
hes_event_codes %<>% filter(!is.na(ICD10Code))
hes_event_codes$event_name <- gsub('^icd-', '', hes_event_codes$event_name)
hes_event_codes$ICD10Code <- gsub('\\.x$', '', hes_event_codes$ICD10Code)
hes_event_codes$ICD10Code <- gsub('\\s+', '', hes_event_codes$ICD10Code)

# split by ICD code length
table(nchar(hes_event_codes$ICD10Code))
event_hes_three <- hes_event_codes %>% filter(nchar(ICD10Code) == 3)
event_hes_five <- hes_event_codes %>% filter(nchar(ICD10Code) == 5)
event_hes_six <- hes_event_codes %>% filter(nchar(ICD10Code) == 6)
event_hes_seven <- hes_event_codes %>% filter(nchar(ICD10Code) == 7)

# 1. event from episodes
col_classes <- c("character", "character", "character", "character", "character", "character", "character", "character")
hes_dx_epi1 <- fread(paste(path_linkage_1, 'hes_diagnosis_epi_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
hes_dx_epi2 <- fread(paste(path_linkage_2, 'hes_diagnosis_epi_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
hes_dx_epi <- bind_rows(hes_dx_epi1, hes_dx_epi2)

rm(hes_dx_epi1, hes_dx_epi2, col_classes)

# HES ICD contains up to 5 characters (including .)
# and ICDx contains 4th or 5th ICD character (excluding .), which is 5th or 6th character (including .)
# so we concatenate 5-character long ICD with ICDx if there is an ICDx

hes_event_epi <- hes_dx_epi %>%
  select(patid, ICD, ICDx, epistart) %>%
  rename(id = patid) %>% 
  mutate(
    ICD = if_else(
      !is.na(ICDx) & ICDx != '-' & nchar(ICD) == 5, 
      paste(ICD, ICDx, sep=''), ICD),
    icd_code_length = nchar(ICD)
  ) 

hes_event_epi <- hes_event_epi %>% 
  filter(id %in% cohort$id &
           (
               (icd_code_length == 3 & ICD %in% event_hes_three$ICD10Code) |
               (icd_code_length == 5 & ICD %in% event_hes_five$ICD10Code) |
               (icd_code_length == 6 & ICD %in% event_hes_six$ICD10Code) |
               (icd_code_length == 7 & ICD %in% event_hes_seven$ICD10Code)
           )
  ) %>%
  mutate(epistart = dmy(epistart)) %>% 
  arrange(id, epistart) %>% 
  rename (date = epistart)

hes_event_epi <- merge(hes_event_epi, hes_event_codes, by.x = 'ICD', by.y = 'ICD10Code')
rm(hes_dx_epi)

# 2. event from hospitalizations
col_classes <- c("character", "character", "character", "character", "character", "character")
hes_dx_hosp1 <- fread(paste(path_linkage_1, 'hes_diagnosis_hosp_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
hes_dx_hosp2 <- fread(paste(path_linkage_2, 'hes_diagnosis_hosp_24_004042_DM.txt', sep = '/'), colClasses = col_classes)
hes_dx_hosp <- bind_rows(hes_dx_hosp1, hes_dx_hosp2)

rm(hes_dx_hosp1, hes_dx_hosp2, col_classes)

hes_event_hosp <- hes_dx_hosp %>%
  select(patid, ICD, ICDx, admidate) %>%
  rename(id = patid) %>% 
  mutate(
    ICD = if_else(
      !is.na(ICDx) & ICDx != '-' & nchar(ICD) == 5, 
      paste(ICD, ICDx, sep=''), ICD),
    icd_code_length = nchar(ICD)
  )

hes_event_hosp <- hes_event_hosp %>% 
  filter(id %in% cohort_ids &
           (
             (icd_code_length == 3 & ICD %in% event_hes_three$ICD10Code) |
               (icd_code_length == 5 & ICD %in% event_hes_five$ICD10Code) |
               (icd_code_length == 6 & ICD %in% event_hes_six$ICD10Code) |
               (icd_code_length == 7 & ICD %in% event_hes_seven$ICD10Code)
           )
  ) %>%
  mutate(admidate = dmy(admidate)) %>% 
  arrange(id, admidate) %>% 
  rename (date = admidate)

hes_event_hosp <- merge(hes_event_hosp, hes_event_codes, by.x = 'ICD', by.y = 'ICD10Code')
rm(hes_event_codes, hes_dx_hosp, event_hes_three, event_hes_five, event_hes_six, event_hes_seven, cohort_ids)

#### GET EARLIEST BASELINE AND STUDY EVENT DATES FROM CPRD AND HES ####

hes_event_epi %<>% select(id, event_name, date)
hes_event_hosp %<>% select(id, event_name, date)
all_event <- bind_rows(cprd_event, hes_event_epi, hes_event_hosp)

## Get baseline event

# baseline event: occurrence prior to entry based on first event date
event_base <- all_event %>% 
  group_by(id, event_name) %>% 
  summarise(event_date_base = min(date)) %>% 
  ungroup()

# pivot so that column is event, row is ID, and cell contains date of first event
event_base %<>% 
  pivot_wider(id_cols = id, names_from = event_name, values_from = event_date_base)

# for each event, get the name, date of first occurrence, and merge with cohort
# changing variable names accordingly

for (i in 2:ncol(event_base)) {
  event_name <- names(event_base[i])
  first_event_i <- event_base[c('id', event_name)]
  cohort <- merge(cohort, first_event_i, by = 'id', all.x = TRUE)
  cohort %<>% rename(event_date_base = !!event_name)
  cohort %<>% mutate(event_base = as.factor(if_else(!is.na(event_date_base) & event_date_base < entry_date, 1, 0)))
  cohort %<>% rename(!!paste(event_name, 'date_base', sep = '_') := event_date_base)
  cohort %<>% rename(!!paste(event_name, 'base', sep = '_') := event_base)
}

rm(event_name, first_event_i, i)

## Get first event since entry

entry_dates <- cohort %>% 
  select(id, entry_date)

event_study <- merge(all_event, entry_dates, all.x = TRUE, by = 'id')

event_study %<>% filter(date > entry_date)
event_study %<>% 
  group_by(id, event_name) %>% 
  summarize(event_date_study = min(date)) %>% 
  ungroup()

event_study %<>%
  pivot_wider(id_cols = id, names_from = event_name, values_from = event_date_study)

for (i in 2:length(event_study)) {
  event_name <- names(event_study[i])
  study_event_i <- event_study[, c('id', event_name)]
  cohort <- merge(cohort, study_event_i, by = 'id', all.x = TRUE)
  cohort %<>% rename(study_event_date = !!event_name)
  cohort %<>% mutate(event = as.factor(if_else(!is.na(study_event_date), 1, 0)))
  cohort %<>% rename(!!paste(event_name, 'date', sep = '_') := study_event_date)
  cohort %<>% rename(!!event_name := event)
}

rm(event_name, i, study_event_i)
rm(hes_event_epi, hes_event_hosp, cprd_event, entry_dates, event_base, event_study, all_event)

saveRDS(cohort, file = paste(path_local, 'cohort_cov_and_events.rds', sep='/'))
