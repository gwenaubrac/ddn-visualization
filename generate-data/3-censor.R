## ---------------------------
##
## Program: 3. Censor
##
## Purposes: Define prescription duration, treatment discontinuation and switch:
## - Main analyses: 30-day grace period
## - Sensitivity analyses: 90-day grace period
##
## Author: Gwen Aubrac
## Date Created: 2024-11-22
##
## Note: Results for the respective active comparator cohorts will be saved in separate folders in the analysis stage. 
##
## ---------------------------

#### SET UP ####

# define analysis from the following:
# 'antidepressant', 'antihypertensive', 'antidiabetic'
analysis <- ''

# path results
path_local <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/local/', analysis)
setwd(path_local) # save intermediate results

# load packages
library(lubridate)
library(dplyr)
library(magrittr)
library(parallel)
library(tidyr)
library(data.table)

# study design
study_start = ymd(20190101)
study_end = ymd(20221231) 

# load cohort
cohort <- readRDS(paste(path_local, 'cohort_cov_and_events.rds', sep = '/'))

#### DEFINE PRESCRIPTION DURATION ####

# This algorithm to define treatment duration in the CPRD was developed by Pauline Reynier at the Lady Davis Institute

common_dosages <- fread('Z:/EPI/Protocol 24_004042/202406_lookup/202406_Lookups_CPRDAurum/202406_Lookups_CPRDAurum/common_dosages.txt')
common_dosages %<>% select(dosageid, dosage_text, daily_dose)

rx_for_exposure <- readRDS(paste(path_local, 'rx_for_exposure.rds', sep = '/'))
summary(rx_for_exposure$duration == 0)

## Get daily dose

# retrieve dose information
rx_for_exposure <- merge(rx_for_exposure, common_dosages, by = 'dosageid', all.x = TRUE)
length(which(rx_for_exposure$daily_dose == 0))

zero <- rx_for_exposure %>% 
  filter(daily_dose == 0 & dosage_text != '')

dosage_text <- unique(zero$dosage_text) 
dosage_text_sum <- zero %>% 
  group_by(dosage_text) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# replace 0 or empty doses based on dosage text
rx_for_exposure <- rx_for_exposure %>%
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

rx_for_exposure <- rx_for_exposure %>%
  mutate(daily_dose = if_else(
    (daily_dose == 0 | daily_dose == '') &
      dosage_text %in% c('HALF A TABLET AS REQUIRED'),
    0.5,
    daily_dose
  ))


rx_for_exposure <- rx_for_exposure %>%
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


rx_for_exposure <- rx_for_exposure %>%
  mutate(daily_dose = if_else(
    (daily_dose == 0 | daily_dose == '') &
      dosage_text %in% c('2 D','2D', 'TAKE TWO CAPSULES A DAY', "2 CAPSULES A DAY"),
    2,
    daily_dose
  ))

# still have some patients with missing daily dose
# with following dosage text
length(which(rx_for_exposure$daily_dose == 0))
zero <- rx_for_exposure %>% 
  filter(daily_dose == 0 & dosage_text != '')
dosage_text <- unique(zero$dosage_text) 
dosage_text

length(which(rx_for_exposure$qty == 0))
length(which(rx_for_exposure$daily_dose == 0))
length(which(cohort$id %in% rx_for_exposure$id))

## Choose between duration1 (from CPRD) and duration2 (calculated from qty/daily_dose)

rx_for_exposure$duration1 <- NA
rx_for_exposure %<>% mutate(duration1 = if_else(1<=duration & duration<=183, duration, NA))
rx_for_exposure %<>% mutate(duration2 = if_else(qty>0 & daily_dose>0, round(qty/daily_dose, 1), NA))
rx_for_exposure %<>% mutate(duration2 = if_else(duration2<1 | duration2>183, NA, duration2))

rx_for_exposure %<>% mutate(same_duration = if_else(!is.na(duration1) & !is.na(duration2) & duration1 == duration2, 1, 0))
length(which(is.na(rx_for_exposure$duration1)))
length(which(is.na(rx_for_exposure$duration2)))
table(rx_for_exposure$same_duration)

# same duration: use CPRD duration
rx_for_exposure$duration <- NA
rx_for_exposure %<>% mutate(duration = if_else(duration1>0 & duration2>0 & same_duration == 1, duration1, duration))

# different duration: use standard 28 days for a duration for a prescription
rx_for_exposure %<>% mutate(duration = if_else(
  is.na(duration) &
    same_duration == 0 &
    duration1 == 28 &
    (duration2 > 28 & duration2 <= 30),
  duration1,
  duration
))

rx_for_exposure %<>% mutate(duration = if_else(
  is.na(duration) &
    same_duration == 0 &
    duration2 == 28 &
    (duration1 > 28 & duration1 <= 30),
  duration2,
  duration
))

# different duration: durations are within 2 days then take max
rx_for_exposure %<>% mutate(
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
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      duration1 == 1 & !is.na(duration2),
    duration2,
    duration
  )
)

# different duration: duration2 = 1 and there is a value for duration 1 then take duration1
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      duration2 == 1 & !is.na(duration1),
    duration1,
    duration
  )
)

# daily dose != 1 and duration1 = qty then take duration2
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      daily_dose != 1 & qty == duration1 & daily_dose %in% c(0.5, 1.5, 2),
    duration2, 
    duration
  )
)

# daily dose != 1 and duration1 = 28 then take duration2
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      !is.na(duration2) & duration1 == 28 & daily_dose %in% c(0.5, 1.5, 2),
    duration2, 
    duration
  )
)


# one of the durations is equal to 28 days then use 28 days
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      !is.na(duration1) &
      !is.na(duration2) &
      (duration1 == 28 | duration2 == 28),
    28,
    duration
  )
)

# duration1 and duration2 are different with no specific criteria then take duration2
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      !is.na(duration1) &
      !is.na(duration2),
    duration2,
    duration
  )
)

# only duration1 available then take duration1
rx_for_exposure %<>% mutate(duration = if_else(
  is.na(duration) &
    same_duration == 0 &
    !is.na(duration1) &
    is.na(duration2),
  duration1,
  duration
))

# only duration2 available then take duration2
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      is.na(duration1) &
      !is.na(duration2),
    duration2,
    duration
  )
)

# duration1 and duration2 are not available but qty = 28 or 56 use qty
rx_for_exposure %<>% mutate(
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
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      is.na(duration1) & 
      is.na(duration2) &
      qty>=1 & qty<=183,
    qty,
    duration
  )
)


# qty > 183
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration) &
      same_duration == 0 &
      daily_dose %in% c(0, '') &
      qty > 183, 
    28,
    duration
  )
)

# all other scenarios use 28
rx_for_exposure %<>% mutate(
  duration = if_else(
    is.na(duration),
    28,
    duration
  )
)

rm(zero, dosage_text_sum, common_dosages, dosage_text)

#### TREATMENT DISCONTINUATION ####

## Main analyses: 30-day grace period ##
# retrieve all prescriptions for the trt that led to cohort entry

grace_period <- 30

trt_supply <- rx_for_exposure %>% 
  filter(id %in% cohort$id) %>% 
  select(-dosageid, -dosage_text, -daily_dose, -duration1, -duration2, -same_duration, -ProductName, -product_code, -qty)

id_trt <- cohort %>% select (id, treatment)
trt_supply <- merge(trt_supply, id_trt, by = 'id', all.x = TRUE)

# keep only rx for exposure leading to entry
trt_supply %<>% filter(exposure == treatment)

# calculate number of days between end of rx supply and date of next rx
gc()
trt_supply <- trt_supply %>% 
  group_by (id) %>% 
  arrange (id, date) %>% 
  dplyr::select(id, treatment, date, duration) %>% 
  mutate (refill_number = row_number(),
          supply_end = date+duration,
          gap_in_supply = date - lag(supply_end), # gap between current rx and end of supply from previous rx
          max_refills = max(refill_number, na.rm = TRUE)) # total number of refills for each patient

saveRDS(trt_supply, file = paste(path_local, 'trt_supply.rds', sep='/'))
# trt_supply <- readRDS(file = paste(path_local, 'trt_supply.rds', sep = '/'))
trt_supply_sens <- trt_supply # for sensitivity analyses later

# Three scenarios for treatment discontinuation
# 1. Patient only has 1 fill (single fill)
# 2. Patient has 30+ day gap in refills (gap in refills)
# 3. Patient's last refill does not last beyond study end (last refill)

# 1. for single fill, disc_date is supply_end + grace_period of first rx
trt_supply$disc_date_single <- NA
gc()
trt_supply %<>%
  mutate (disc_date_single = if_else (max_refills == 1, supply_end+grace_period, disc_date_single))

groups(trt_supply) # check that grouped by id

# 2. for multiple refills, disc date is supply end prior to 30-day gap + 30 days
# returns inf if no gap
trt_supply$disc_date_gap <- NA
gc()
trt_supply %<>%
  mutate (prior_supply_end = lag(supply_end),
          gap = if_else (!is.na(gap_in_supply) & gap_in_supply>grace_period, 1, 0),
          disc_date_gap = if_else (gap == 1, prior_supply_end+grace_period, disc_date_gap))

trt_supply %<>% # set earliest disc date from gap, produces warning but still runs
  mutate (disc_date_gap = min(disc_date_gap, na.rm = TRUE))

# 3. for no gap, disc date is supply end for last rx + grace period
# so get patients with no disc from single fill or gap in refills
# and get the supply end of their last rx

gc()
no_disc_dates <- trt_supply %>%
  filter(all(is.na(disc_date_single)) & all(is.na(disc_date_gap) | is.infinite(disc_date_gap))) %>%
  arrange(desc(supply_end)) %>% # already arranged by id
  slice(1) %>% 
  mutate(disc_date_last = supply_end + grace_period)

no_disc_dates %<>% select(id, disc_date_last)

trt_supply <- merge(trt_supply, no_disc_dates, by = 'id', all.x = TRUE)

# get earliest date from 3 scenarios (although should be only 1 disc date per patient)
# if disc date after study end, then patient not considered to have discontinued

gc()
disc_dates <- trt_supply %>%
  group_by(id) %>%
  mutate (
    disc_date_gap = if_else(is.infinite(disc_date_gap), NA, disc_date_gap),
    disc_date = min(disc_date_single, disc_date_gap, disc_date_last, na.rm = TRUE),
    disc_date = if_else(disc_date > study_end, NA, disc_date)
  ) %>%
  slice(1) 

disc_dates %<>% select (id, disc_date) 
cohort <- merge(cohort, disc_dates, by = 'id', all.x = TRUE)

## Sensitivity analyses: 90-day grace period
# Repeat these steps but using 90 days instead of 30 for grace period

grace_period <- 90

# 1. for single fill, disc_date is supply_end + grace_period of first rx
trt_supply_sens$disc_date_single <- NA
gc()
trt_supply_sens %<>%
  mutate (disc_date_single = if_else (max_refills == 1, supply_end+grace_period, disc_date_single))

# 2. for multiple refills, disc date is supply end prior to 30-day gap + 30 days
# returns inf if no gap
trt_supply_sens$disc_date_gap <- NA
gc()
trt_supply_sens %<>%
  mutate (prior_supply_end = lag(supply_end),
          gap = if_else (!is.na(gap_in_supply) & gap_in_supply>grace_period, 1, 0),
          disc_date_gap = if_else (gap == 1, prior_supply_end+grace_period, disc_date_gap))

trt_supply_sens %<>% # set earliest disc date from gap, produces warning but still runs
  mutate (disc_date_gap = min(disc_date_gap, na.rm = TRUE))

# 3. for no gap, disc date is supply end for last rx + grace period
# so get patients with no disc from single fill or gap in refills
# and get the supply end of their last rx

gc()
no_disc_dates <- trt_supply_sens %>%
  filter(all(is.na(disc_date_single)) & all(is.na(disc_date_gap) | is.infinite(disc_date_gap))) %>%
  arrange(desc(supply_end)) %>% # already arranged by id
  slice(1) %>% 
  mutate(disc_date_last = supply_end + grace_period)

no_disc_dates %<>% select(id, disc_date_last)

trt_supply_sens <- merge(trt_supply_sens, no_disc_dates, by = 'id', all.x = TRUE)

# get earliest date from 3 scenarios (although should be only 1 disc date per patient)
# if disc date after study end, then patient not considered to have discontinued

gc()
disc_dates_sens <- trt_supply_sens %>%
  group_by(id) %>%
  mutate (
    disc_date_gap = if_else(is.infinite(disc_date_gap), NA, disc_date_gap),
    disc_date_sens = min(disc_date_single, disc_date_gap, disc_date_last, na.rm = TRUE),
    disc_date_sens = if_else(disc_date_sens > study_end, NA, disc_date_sens)
  ) %>%
  slice(1) 

disc_dates_sens %<>% select (id, disc_date_sens) 
cohort <- merge(cohort, disc_dates_sens, by = 'id', all.x = TRUE)

rm(disc_dates_sens, disc_dates, no_disc_dates, grace_period)

#### TREATMENT SWITCH ####

# skip this step for antidiabetic cohorts
if (!analysis %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  # retrieve all rx for exposures
  trt_switch <- rx_for_exposure %>%
    filter(id %in% cohort$id)
  
  trt_switch <- merge(trt_switch, id_trt, by = 'id', all.x = TRUE)
  
  # keep only rx for an exposure NOT leading to entry
  # (i.e., exposure for other treatment group)
  trt_switch %<>% filter(exposure != treatment)
  
  # switch date is the earliest date of rx for an exposure other than the one leading to entry
  trt_switch %<>%
    group_by(id) %>%
    mutate(switch_date = min(date)) %>%
    slice(1) %>%
    select(id, switch_date)
  
  cohort <- merge(cohort, trt_switch, by = 'id', all.x = TRUE)
  rm(trt_switch, id_trt)
}

saveRDS(cohort, file = paste(path_local, 'cohort_cens.rds', sep='/'))
