## ---------------------------
##
## Program: 3. Analyses
##
## Purposes: Generate descriptive analyses data for visualization tool.
## All data is tagged with the 'region' corresponding to the data partner,
## 'comparison' corresponding to the current active comparator cohort being analyzed,
## and 'outcome' for the current outcome being evaluated. 
## The descriptive data is obtained in 2 steps: 
## 1. General descriptive data
## - a) ps_coef: coefficients of association between covariates and treatment
## - b) x_by_month: monthly number of patients in each treatment group
## - c) covs: number of patients with covariates
## - d) smd: standardized mean differences of covariates between treatment groups
## 2. Outcome-specific descriptive data
## - a) hr_main: hazard ratio for main analyses (using 30-day grace period0)
## - b) hr_sex: hazard ratio in subgroups by sex (male, female)
## - c) hr_year: hazard ratio in subgroups by year of entry (2019-2022)
## - d) hr_age: hazard ratio in subgroups by age (<65, >=65)
## - e) hr_sens: hazard ratio for sensitivity comparison (using 90-day grace period)
## - f) marg_bias: marginal bias terms
## - g) y_by_month: crude monthly outcome incidence rate
##
## Author: Gwen Aubrac
##
## Date Created: 2024-11-22
##
## Notes: Please ensure you define all the multi-category variables in 'cat_variables'
## (otherwise there will be issues computing marginal bias terms).
##
## ---------------------------

#### SET UP ####

# define comparison from the following:
# 'snri_vs_ssri', 'arb_vs_acei', 'su_vs_dpp4', 'su_vs_glp1', su_vs_sglt2'
comparison <- ''
region <- 'cprd'

# path results
path_res <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/', comparison)

if (comparison %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  path_local <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/local/antidiabetic'
} else if (comparison == "snri_vs_ssri") {
    path_local <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/local/antidepressant'
} else if (comparison == 'arb_vs_acei') {
  path_local <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/local/antihypertensive'
  }

path_codes <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/codes'
path_cprd_event <- paste(path_codes, 'event', 'aurum_codes', sep = '/')

# load packages
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)
library(writexl)
library(broom)
library(survival)
library(cobalt)
library(tidysmd)

# study design
study_start = ymd(20190101)
study_end = ymd(20221231) 
cat_variables = c('age_group', 'deprivation', 'ethnicity', 'year')
cov_var <- c('age_group', 'male', 'year', 'deprivation', 'ethnicity')
cov_events <- c(
  'arrhythmia_base',
  'pacemaker_base',
  'hyperlipidemia_base',
  'hypertension_base',
  'depression_base',
  'epilepsy_base',
  'cardio_base',
  'cvd_base',
  'ihd_base',
  'pvd_base',
  'vhd_base',
  'hepatic_base',
  'acute_renal_base',
  'chronic_renal_base',
  'hypokalemia_base',
  'hypocalcemia_base',
  'hypomagnesemia_base',
  'stroke_base',
  'copd_base',
  'mi_base',
  'suicidal_base',
  'chf_base'
)

covariates <- c(cov_var, cov_events)
covariates

# set reference treatment
cohort <- readRDS(paste(path_local, 'cohort_cens.rds', sep = '/'))

cohort %<>% mutate(
  trt = case_when(
    treatment == 'su' ~ 0,
    treatment == 'snri' ~ 0,
    treatment == 'arb' ~ 0,
    .default = 1
  )
)

# for antidiabetic cohort keep only active comparator of interest
cohort <- switch(
  comparison,
  'su_vs_dpp4' = cohort %<>% filter(treatment == 'su' | treatment == 'dpp4'),
  'su_vs_glp1' = cohort %<>% filter(treatment == 'su' | treatment == 'glp1'),
  'su_vs_sglt2' = cohort %<>% filter(treatment == 'su' | treatment == 'sglt2'), 
  cohort
)

#### 1. GENERAL DESCRIPTIVE DATA ####

## a) ps_coef

# fit the propensity score model
summary(cohort[covariates])
ps_model <- reformulate(covariates, 'trt')
ps_fit <- glm(ps_model, family= binomial, data = cohort)
cohort$prop_score <- predict(ps_fit, type = 'response')
cohort$iptw <- if_else(cohort$trt == 0, 1 / (1 - cohort$prop_score), 1 / cohort$prop_score)

summary(cohort$iptw)
sd(cohort$iptw)

ps_coef <- tidy(ps_fit)
ps_coef %<>%
  filter(term != '(Intercept)') %>% 
  mutate(odds_ratio = exp(estimate)) %>% 
  rename(cov_name = term) %>% 
  select(cov_name, estimate, odds_ratio)

ps_coef %<>% mutate(
  estimate = round(estimate, 3),
  odds_ratio = round(odds_ratio, 3)
)

ps_coef$region <- region
ps_coef$comparison <- comparison

write_xlsx(ps_coef, paste(path_res, 'ps_coef.xlsx', sep = '/'))
rm(ps_fit, ps_model)

ps_bal <- cohort %>% 
  select(trt, prop_score, iptw)

ps_bal$region <- region
ps_bal$comparison <- comparison

# save as RDS due to file size
saveRDS(ps_bal, paste(path_res, 'ps_bal.rds', sep = '/'))

## b) x_by_month
x_by_month <- cohort %>% 
  group_by(year_month, trt) %>% 
  summarize(num_patients = n())

x_by_month$region <- region
x_by_month$comparison <- comparison

write_xlsx(x_by_month, paste(path_res, 'x_by_month.xlsx', sep = '/'))

## c) covs

## We will estimate the proportion of patients with given cov values
## separately for binary covs and each of the multi-category covs
## then put the results together

## (1) overall
# for binary covariates
bin_variables <- covariates[!covariates %in% cat_variables]

covs_bin <- cohort %>% 
  summarise(across(all_of(bin_variables), ~sum(. == 1, na.rm = TRUE)))

covs_bin %<>% pivot_longer(cols = -c(), names_to = 'cov_name', values_to = 'count')

# for multi-category variables
covs_age <- cohort %>% 
  group_by(age_group) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  rename(cov_name = age_group) %>% 
  mutate(cov_name = paste0('age_group', cov_name))

covs_deprivation <- cohort %>% 
  group_by(deprivation) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  rename(cov_name = deprivation) %>% 
  mutate(cov_name = paste0('deprivation', cov_name))

covs_ethnicity <- cohort %>% 
  group_by(ethnicity) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  rename(cov_name = ethnicity) %>% 
  mutate(cov_name = paste0('ethnicity', cov_name))

covs_year <- cohort %>% 
  group_by(year) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  rename(cov_name = year) %>% 
  mutate(cov_name = paste0('year', cov_name))

# merge results
covs <- bind_rows(covs_bin, covs_age, covs_deprivation, covs_ethnicity, covs_year)

## (2) for treated only
covs_trt1_bin <- cohort %>% 
  filter(trt == 1) %>% 
  summarise(across(all_of(bin_variables), ~sum(. == 1, na.rm = TRUE)))

covs_trt1_bin %<>% pivot_longer(cols = -c(), names_to = 'cov_name', values_to = 'count_trt1')

covs_trt1_age <- cohort %>% 
  filter(trt == 1) %>% 
  group_by(age_group) %>% 
  summarise(count_trt1 = n(), .groups = 'drop') %>% 
  rename(cov_name = age_group) %>% 
  mutate(cov_name = paste0('age_group', cov_name))

covs_trt1_deprivation <- cohort %>% 
  filter(trt == 1) %>% 
  group_by(deprivation) %>% 
  summarise(count_trt1 = n(), .groups = 'drop') %>% 
  rename(cov_name = deprivation) %>% 
  mutate(cov_name = paste0('deprivation', cov_name))

covs_trt1_ethnicity <- cohort %>% 
  filter(trt == 1) %>% 
  group_by(ethnicity) %>% 
  summarise(count_trt1 = n(), .groups = 'drop') %>% 
  rename(cov_name = ethnicity) %>% 
  mutate(cov_name = paste0('ethnicity', cov_name))

covs_trt1_year <- cohort %>% 
  filter(trt == 1) %>% 
  group_by(year) %>% 
  summarise(count_trt1 = n(), .groups = 'drop') %>% 
  rename(cov_name = year) %>% 
  mutate(cov_name = paste0('year', cov_name))

covs_trt1 <- bind_rows(
  covs_trt1_bin,
  covs_trt1_age,
  covs_trt1_deprivation,
  covs_trt1_ethnicity,
  covs_trt1_year
)

## (3) for untreated only
covs_trt0_bin <- cohort %>% 
  filter(trt == 0) %>% 
  summarise(across(all_of(bin_variables), ~sum(. == 1, na.rm = TRUE)))

covs_trt0_bin %<>% pivot_longer(cols = -c(), names_to = 'cov_name', values_to = 'count_trt0')

covs_trt0_age <- cohort %>% 
  filter(trt == 0) %>% 
  group_by(age_group) %>% 
  summarise(count_trt0 = n(), .groups = 'drop') %>% 
  rename(cov_name = age_group) %>% 
  mutate(cov_name = paste0('age_group', cov_name))

covs_trt0_deprivation <- cohort %>% 
  filter(trt == 0) %>% 
  group_by(deprivation) %>% 
  summarise(count_trt0 = n(), .groups = 'drop') %>% 
  rename(cov_name = deprivation) %>% 
  mutate(cov_name = paste0('deprivation', cov_name))

covs_trt0_ethnicity <- cohort %>% 
  filter(trt == 0) %>% 
  group_by(ethnicity) %>% 
  summarise(count_trt0 = n(), .groups = 'drop') %>% 
  rename(cov_name = ethnicity) %>% 
  mutate(cov_name = paste0('ethnicity', cov_name))

covs_trt0_year <- cohort %>% 
  filter(trt == 0) %>% 
  group_by(year) %>% 
  summarise(count_trt0 = n(), .groups = 'drop') %>% 
  rename(cov_name = year) %>% 
  mutate(cov_name = paste0('year', cov_name))

covs_trt0 <- bind_rows(
  covs_trt0_bin,
  covs_trt0_age,
  covs_trt0_deprivation,
  covs_trt0_ethnicity,
  covs_trt0_year
)

# merge for overall, treated, and untreated
covs <- merge(covs, covs_trt1, by = 'cov_name')
covs <- merge(covs, covs_trt0, by = 'cov_name')

# get proportion
num_pat <- length(cohort$id)
num_pat_trt0 <- sum(cohort$trt==0)
num_pat_trt1 <- sum(cohort$trt==1)

covs %<>%
  mutate(prop = round(count/num_pat, 3),
         prop_trt0 = round(count_trt0/num_pat_trt0, 3),
         prop_trt1 = round(count_trt1/num_pat_trt1, 3))

covs$region <- region
covs$comparison <- comparison

write_xlsx(covs, paste(path_res, 'covs.xlsx', sep = '/'))

rm(
  covs_trt1,
  covs_trt0,
  covs_trt1_bin,
  covs_trt1_age,
  covs_trt1_ethnicity,
  covs_trt1_deprivation,
  covs_trt1_year,
  covs_trt0_bin,
  covs_trt0_age,
  covs_trt0_ethnicity,
  covs_trt0_deprivation,
  covs_trt0_year,
  covs_age,
  covs_bin,
  covs_deprivation,
  covs_ethnicity,
  covs_year,
  num_pat,
  num_pat_trt0,
  num_pat_trt1
)

## d) smd
# (1) for binary variables can use this package
cohort_covs <- cohort %>% select(all_of(c(bin_variables)))
bal_tab <- bal.tab(
  trt ~ cohort_covs,
  data = cohort,
  s.d.denom = 'pooled',
  continuous = 'std',
  binary = 'std', 
  stats = 'm'
)

smd_bin <- bal_tab$Balance
smd_bin$cov_name <- rownames(smd_bin)
rownames(smd_bin) <- NULL

smd_bin %<>%
  select(Diff.Un, cov_name) %>% 
  rename(SMD = Diff.Un)

# (2) for multi-category variables we want a global SMD
smd_cat <- tidy_smd(cohort, all_of(cat_variables), .group = trt)
smd_cat %<>%
  select(variable, smd) %>% 
  rename (cov_name = variable,
          SMD = smd)

smd <- bind_rows(smd_bin, smd_cat)
smd$region <- region
smd$comparison <- comparison

smd %<>% mutate(SMD = round(SMD, 3))

write_xlsx(smd, paste(path_res, 'smd.xlsx', sep = '/'))
rm(cohort_covs, bal_tab, smd_bin, smd_cat)

#### 2. OUTCOME-SPECIFIC DESCRIPTIVE DATA ####

### SET UP ###

## Define list of outcomes to be assessed

outcome_list <- c(
  
  #event-based outcomes
  'death',
  'stroke',
  'hypoglycemia_hosp',
  'amputation',
  'mi',
  'chf_hosp',
  'copd_exacerbation',
  'suicidal_hosp',
  
  # chronic conditions
  'diabetes',
  'hypertension',
  'depression',
  'hyperlipidemia',
  'copd',
  'chf',
  'end_stage_renal',
  
  # procedures
  'retinopathy',
  'breast_cancer_screen',
  'colon_cancer_screen'
  
)

# remove events that are only for certain cohorts
if (!comparison %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  outcome_list <- outcome_list[!outcome_list %in% c('hypoglycemia_hosp', 'amputation', 'retinopathy')]
} 

if (comparison %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  outcome_list <- outcome_list[!outcome_list %in% c('diabetes')]
}

if (comparison == 'arb_vs_acei') {
  outcome_list <- outcome_list[!outcome_list %in% c('hypertension')]
}

## Define outcome

for (outcome in outcome_list) {
  
  # if there is no occurrence of the outcome, skip it
  if (!outcome %in% names(cohort)) {
    next
  }
  
  cohort_analytic <- cohort
  cohort_analytic <- switch(
    outcome,
    "breast_cancer_screen" = cohort_analytic %<>% filter(male == 0), 
    cohort_analytic
  )
  
  cohort_analytic$event <- cohort_analytic[[outcome]]
  cohort_analytic$event_date <- cohort_analytic[[paste(outcome, 'date', sep = '_')]]
  
  ## Define ITT and AT events
  cohort_analytic$itt_exit_date <- pmin(
    cohort_analytic$regend,
    cohort_analytic$lcd,
    cohort_analytic$death_date,
    cohort_analytic$event_date,
    study_end,
    na.rm = TRUE
  )
  
  cohort_analytic %<>%
    mutate(
      itt_event = if_else(!is.na(event_date) &
                            event_date <= itt_exit_date, 1, 0),
      itt_event_date = if_else(itt_event == 1, event_date, NA)
    )
  
  # do not use switch date for antidiabetic cohort
  if (comparison %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
    cohort_analytic$at_exit_date <- pmin(
      cohort_analytic$itt_exit_date,
      cohort_analytic$disc_date,
      na.rm = TRUE
    )
  } else {
    cohort_analytic$at_exit_date <- pmin(
      cohort_analytic$itt_exit_date,
      cohort_analytic$disc_date,
      cohort_analytic$switch_date,
      na.rm = TRUE
    )
  }
  
  cohort_analytic %<>%
    mutate(
      at_event = if_else(!is.na(event_date) &
                           event_date <= at_exit_date, 1, 0),
      at_event_date = if_else(at_event == 1, event_date, NA)
    )
  
  ### ANALYSES ###
  
  ## a) hr_main
  cox_itt <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE
  )
  
  cox_at <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE
  )
  
  hr_main <- data.frame(matrix(nrow = 2, ncol = 3))
  colnames(hr_main) <- c('hr_estimate', 'hr_ci_lower', 'hr_ci_upper')
  hr_main$model <- c('ITT', 'AT')
  
  hr_main[1, 'hr_estimate'] <- exp(cox_itt$coef)
  hr_main[1, 'hr_ci_lower'] <- exp(confint(cox_itt))[1]
  hr_main[1, 'hr_ci_upper'] <- exp(confint(cox_itt))[2]
  
  hr_main[2, 'hr_estimate'] <- exp(cox_at$coef)
  hr_main[2, 'hr_ci_lower'] <- exp(confint(cox_at))[1]
  hr_main[2, 'hr_ci_upper'] <- exp(confint(cox_at))[2]
  
  hr_main %<>% relocate(model)
  
  hr_main$region <- region
  hr_main$comparison <- comparison
  hr_main$outcome <- outcome
  
  hr_main %<>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  write_xlsx(hr_main,
             paste(path_res, outcome, 'hr_main.xlsx', sep = '/'))
  rm(cox_itt, cox_at)
  
  
  ## b) hr_sex
  if (outcome != 'breast_cancer_screen') {
    cox_itt_male <- coxph(
      Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
      data = cohort_analytic,
      weights = iptw,
      cluster = id,
      robust = TRUE,
      subset = male == 1
    )
    
    cox_at_male <- coxph(
      Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
      data = cohort_analytic,
      weights = iptw,
      cluster = id,
      robust = TRUE,
      subset = male == 1
    )
    
    cox_itt_female <- coxph(
      Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
      data = cohort_analytic,
      weights = iptw,
      cluster = id,
      robust = TRUE,
      subset = male == 0
    )
    
    cox_at_female <- coxph(
      Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
      data = cohort_analytic,
      weights = iptw,
      cluster = id,
      robust = TRUE,
      subset = male == 0
    )
    
    hr_sex <- data.frame(matrix(nrow = 2, ncol = 6))
    colnames(hr_sex) <- c(
      'male_hr_estimate',
      'male_hr_ci_lower',
      'male_hr_ci_upper',
      'female_hr_estimate',
      'female_hr_ci_lower',
      'female_hr_ci_upper'
    )
    
    hr_sex$model <- c('ITT', 'AT')
    
    hr_sex[1, 'male_hr_estimate'] <- exp(cox_itt_male$coef)
    hr_sex[1, 'male_hr_ci_lower'] <- exp(confint(cox_itt_male))[1]
    hr_sex[1, 'male_hr_ci_upper'] <- exp(confint(cox_itt_male))[2]
    
    hr_sex[1, 'female_hr_estimate'] <- exp(cox_itt_female$coef)
    hr_sex[1, 'female_hr_ci_lower'] <- exp(confint(cox_itt_female))[1]
    hr_sex[1, 'female_hr_ci_upper'] <- exp(confint(cox_itt_female))[2]
    
    hr_sex[2, 'male_hr_estimate'] <- exp(cox_at_male$coef)
    hr_sex[2, 'male_hr_ci_lower'] <- exp(confint(cox_at_male))[1]
    hr_sex[2, 'male_hr_ci_upper'] <- exp(confint(cox_at_male))[2]
    
    hr_sex[2, 'female_hr_estimate'] <- exp(cox_at_female$coef)
    hr_sex[2, 'female_hr_ci_lower'] <- exp(confint(cox_at_female))[1]
    hr_sex[2, 'female_hr_ci_upper'] <- exp(confint(cox_at_female))[2]
    
    hr_sex %<>% relocate(model)
    
    hr_sex$region <- region
    hr_sex$comparison <- comparison
    hr_sex$outcome <- outcome
    
    hr_sex %<>%
      mutate(across(where(is.numeric), ~ round(., 3)))
    
    write_xlsx(hr_sex, paste(path_res, outcome, 'hr_sex.xlsx', sep = '/'))
    rm(cox_itt_male, cox_at_male, cox_itt_female, cox_at_female)
  }
  
  ## c) hr_year
  cox_itt_2019 <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2019'
  )
  
  cox_at_2019 <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2019'
  )
  
  cox_itt_2020 <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2020'
  )
  
  cox_at_2020 <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2020'
  )
  
  cox_itt_2021 <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2021'
  )
  
  cox_at_2021 <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2021'
  )
  
  cox_itt_2022 <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2022'
  )
  
  cox_at_2022 <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = year == '2022'
  )
  
  hr_year <- data.frame(matrix(nrow = 2, ncol = 12))
  colnames(hr_year) <- c(
    'x2019_hr_estimate',
    'x2019_hr_ci_lower',
    'x2019_hr_ci_upper',
    'x2020_hr_estimate',
    'x2020_hr_ci_lower',
    'x2020_hr_ci_upper',
    'x2021_hr_estimate',
    'x2021_hr_ci_lower',
    'x2021_hr_ci_upper',
    'x2022_hr_estimate',
    'x2022_hr_ci_lower',
    'x2022_hr_ci_upper'
  )
  
  hr_year$model <- c('ITT', 'AT')
  
  hr_year[1, 'x2019_hr_estimate'] <- exp(cox_itt_2019$coef)
  hr_year[1, 'x2019_hr_ci_lower'] <- exp(confint(cox_itt_2019))[1]
  hr_year[1, 'x2019_hr_ci_upper'] <- exp(confint(cox_itt_2019))[2]
  
  hr_year[2, 'x2019_hr_estimate'] <- exp(cox_at_2019$coef)
  hr_year[2, 'x2019_hr_ci_lower'] <- exp(confint(cox_at_2019))[1]
  hr_year[2, 'x2019_hr_ci_upper'] <- exp(confint(cox_at_2019))[2]
  
  hr_year[1, 'x2020_hr_estimate'] <- exp(cox_itt_2020$coef)
  hr_year[1, 'x2020_hr_ci_lower'] <- exp(confint(cox_itt_2020))[1]
  hr_year[1, 'x2020_hr_ci_upper'] <- exp(confint(cox_itt_2020))[2]
  
  hr_year[2, 'x2020_hr_estimate'] <- exp(cox_at_2020$coef)
  hr_year[2, 'x2020_hr_ci_lower'] <- exp(confint(cox_at_2020))[1]
  hr_year[2, 'x2020_hr_ci_upper'] <- exp(confint(cox_at_2020))[2]
  
  hr_year[1, 'x2021_hr_estimate'] <- exp(cox_itt_2021$coef)
  hr_year[1, 'x2021_hr_ci_lower'] <- exp(confint(cox_itt_2021))[1]
  hr_year[1, 'x2021_hr_ci_upper'] <- exp(confint(cox_itt_2021))[2]
  
  hr_year[2, 'x2021_hr_estimate'] <- exp(cox_at_2021$coef)
  hr_year[2, 'x2021_hr_ci_lower'] <- exp(confint(cox_at_2021))[1]
  hr_year[2, 'x2021_hr_ci_upper'] <- exp(confint(cox_at_2021))[2]
  
  hr_year[1, 'x2022_hr_estimate'] <- exp(cox_itt_2022$coef)
  hr_year[1, 'x2022_hr_ci_lower'] <- exp(confint(cox_itt_2022))[1]
  hr_year[1, 'x2022_hr_ci_upper'] <- exp(confint(cox_itt_2022))[2]
  
  hr_year[2, 'x2022_hr_estimate'] <- exp(cox_at_2022$coef)
  hr_year[2, 'x2022_hr_ci_lower'] <- exp(confint(cox_at_2022))[1]
  hr_year[2, 'x2022_hr_ci_upper'] <- exp(confint(cox_at_2022))[2]
  
  hr_year %<>% relocate(model)
  
  hr_year$region <- region
  hr_year$comparison <- comparison
  hr_year$outcome <- outcome
  
  hr_year %<>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  write_xlsx(hr_year, paste(path_res, outcome, 'hr_year.xlsx', sep = '/'))
  rm(cox_itt_2019, cox_at_2019, cox_itt_2020, cox_at_2020, cox_itt_2021, cox_at_2021, cox_itt_2022, cox_at_2022)
  
  ## d) hr_age
  cox_itt_old <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = above_65 == 1
  )
  
  cox_at_old <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = above_65 == 1
  )
  
  cox_itt_young <- coxph(
    Surv(as.numeric(itt_exit_date - entry_date), itt_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = above_65 == 0
  )
  
  cox_at_young <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic,
    weights = iptw,
    cluster = id,
    robust = TRUE,
    subset = above_65 == 0
  )
  
  hr_age <- data.frame(matrix(nrow = 2, ncol = 6))
  colnames(hr_age) <- c(
    'old_hr_estimate',
    'old_hr_ci_lower',
    'old_hr_ci_upper',
    'young_hr_estimate',
    'young_hr_ci_lower',
    'young_hr_ci_upper'
  )
  
  hr_age$model <- c('ITT', 'AT')
  
  hr_age[1, 'old_hr_estimate'] <- exp(cox_itt_old$coef)
  hr_age[1, 'old_hr_ci_lower'] <- exp(confint(cox_itt_old))[1]
  hr_age[1, 'old_hr_ci_upper'] <- exp(confint(cox_itt_old))[2]
  
  hr_age[1, 'young_hr_estimate'] <- exp(cox_itt_young$coef)
  hr_age[1, 'young_hr_ci_lower'] <- exp(confint(cox_itt_young))[1]
  hr_age[1, 'young_hr_ci_upper'] <- exp(confint(cox_itt_young))[2]
  
  hr_age[2, 'old_hr_estimate'] <- exp(cox_at_old$coef)
  hr_age[2, 'old_hr_ci_lower'] <- exp(confint(cox_at_old))[1]
  hr_age[2, 'old_hr_ci_upper'] <- exp(confint(cox_at_old))[2]
  
  hr_age[2, 'young_hr_estimate'] <- exp(cox_at_young$coef)
  hr_age[2, 'young_hr_ci_lower'] <- exp(confint(cox_at_young))[1]
  hr_age[2, 'young_hr_ci_upper'] <- exp(confint(cox_at_young))[2]
  
  hr_age %<>% relocate(model)
  
  hr_age$region <- region
  hr_age$comparison <- comparison
  hr_age$outcome <- outcome
  
  hr_age %<>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  write_xlsx(hr_age, paste(path_res, outcome, 'hr_age.xlsx', sep = '/'))
  rm(cox_itt_old, cox_at_old, cox_itt_young, cox_at_young)
  
  ## e) hr_sens
  cohort_analytic_sens <- cohort
  
  cohort_analytic_sens <- switch(
    outcome,
    "breast_cancer_screen" = cohort_analytic_sens %<>% filter(male == 0), 
    cohort_analytic_sens
  )
  
  cohort_analytic_sens$event <- cohort_analytic_sens[[outcome]]
  cohort_analytic_sens$event_date <- cohort_analytic_sens[[paste(outcome, 'date', sep = '_')]]
  cohort_analytic_sens$itt_exit_date <- cohort_analytic$itt_exit_date
  
  # do not use switch date for antidiabetic cohort
  if (comparison %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
    cohort_analytic_sens$at_exit_date <- pmin(
      cohort_analytic_sens$itt_exit_date,
      cohort_analytic_sens$disc_date_sens,
      na.rm = TRUE
    )
  } else {
    cohort_analytic_sens$at_exit_date <- pmin(
      cohort_analytic_sens$itt_exit_date,
      cohort_analytic_sens$disc_date_sens,
      cohort_analytic_sens$switch_date,
      na.rm = TRUE
    )
  }
  
  cohort_analytic_sens %<>%
    mutate(
      at_event = if_else(!is.na(event_date) &
                           event_date <= at_exit_date, 1, 0),
      at_event_date = if_else(at_event == 1, event_date, NA)
    )
  
  cox_at_sens <- coxph(
    Surv(as.numeric(at_exit_date - entry_date), at_event) ~ trt,
    data = cohort_analytic_sens,
    weights = iptw,
    cluster = id,
    robust = TRUE
  )
  
  hr_sens <- data.frame(matrix(nrow = 1, ncol = 3))
  colnames(hr_sens) <- c('hr_estimate', 'hr_ci_lower', 'hr_ci_upper')
  hr_sens$model <- c('AT_sens')
  
  hr_sens[1, 'hr_estimate'] <- exp(cox_at_sens$coef)
  hr_sens[1, 'hr_ci_lower'] <- exp(confint(cox_at_sens))[1]
  hr_sens[1, 'hr_ci_upper'] <- exp(confint(cox_at_sens))[2]
  
  hr_sens %<>% relocate(model)
  
  hr_sens$region <- region
  hr_sens$comparison <- comparison
  hr_sens$outcome <- outcome
  
  hr_sens %<>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  write_xlsx(hr_sens,
             paste(path_res, outcome, 'hr_sens.xlsx', sep = '/'))
  rm(cox_at_sens, cohort_analytic_sens)
  
  ## f) marg_bias
  marg_bias <- data.frame(
    cov_name = NA,
    freq_exp0 = 0,
    freq_exp1 = 0,
    estimate = 0,
    risk_ratio = 0
  )
  
  for (cov in c(covariates)) {
    # get proportion of patients with covariate in each treatment group
    prop_with_cov <- cohort_analytic %>%
      group_by(!!sym(cov), trt) %>%
      summarize(num_patients = n())
    
    prop_with_cov %<>%
      group_by(trt) %>%
      mutate(num_patients_trt = sum(num_patients)) %>%
      ungroup() %>%
      mutate(freq = num_patients / num_patients_trt)
    
    # a) CATEGORICAL COV
    if (cov %in% cat_variables) {
      prop_with_cov %<>% select(!!sym(cov), trt, freq)
      
      # split into groups for each level of the covariate
      cov_groups <- split(prop_with_cov, prop_with_cov[[cov]])
      
      for (i in 1:length(cov_groups)) {
        prop_with_cov_i <- cov_groups[[i]]
        cov_i <- names(cov_groups[i])
        prop_with_cov_i %<>%
          pivot_wider(
            names_from = trt,
            values_from = freq,
            names_prefix = 'freq_exp'
          )
        
        prop_with_cov_i %<>%
          select(-!!sym(cov))
        
        # get probability of event associated with covariate
        model <- glm(
          reformulate(paste0(cov, "==", "'", cov_i, "'"), 'event'),
          data = cohort_analytic,
          family = binomial(link = 'logit')
        )
        
        risk_ratio_cov_i <- tidy(model)
        
        risk_ratio_cov_i %<>%
          filter(term != '(Intercept)') %>%
          mutate(risk_ratio = exp(estimate)) %>%
          mutate(cov_name = paste(cov, cov_i, sep = '_')) %>%
          select(cov_name, estimate, risk_ratio)
        
        # merge results
        marg_bias_cov_i <- bind_cols(prop_with_cov_i, risk_ratio_cov_i)
        marg_bias <- bind_rows(marg_bias, marg_bias_cov_i)
        rm(cov_i,
           prop_with_cov_i,
           model,
           risk_ratio_cov_i,
           marg_bias_cov_i)
      }
      
    }
    
    # b) BINARY COV
    if (!(cov %in% cat_variables)) {
      prop_with_cov %<>%
        filter(!!sym(cov) == 1) %>%
        select(trt, freq) %>%
        pivot_wider(
          names_from = trt,
          values_from = freq,
          names_prefix = 'freq_exp'
        )
      
      model <- glm(reformulate(cov, 'event'),
                   data = cohort_analytic,
                   family = binomial(link = 'logit'))
      
      risk_ratio_cov <- tidy(model) %>%
        filter(term != '(Intercept)') %>%
        mutate(risk_ratio = exp(estimate)) %>%
        mutate(cov_name = cov) %>%
        select(cov_name, estimate, risk_ratio)
      
      marg_bias_cov <- bind_cols(prop_with_cov, risk_ratio_cov)
      marg_bias <- bind_rows(marg_bias, marg_bias_cov)
      rm(risk_ratio_cov, model, prop_with_cov)
      
    }
  }
  
  marg_bias <- marg_bias[-1, ]
  
  # cov frequencies in groups where the cov never occurred
  # are marked as 'NA', so let us replace that with 0
  marg_bias <- marg_bias %>% replace(is.na(.), 0)
  
  marg_bias %<>%
    mutate(bias = if_else(
      estimate > 0,
      (freq_exp1 * (risk_ratio - 1) + 1) / (freq_exp0 * (risk_ratio -
                                                           1) + 1),
      (freq_exp1 * (1 / risk_ratio - 1) + 1) / freq_exp0 * (1 /
                                                              risk_ratio - 1) + 1
    ))
  
  # clean variable names
  marg_bias %<>%
    mutate(cov_name = gsub('_base', '', cov_name))
  
  marg_bias$region <- region
  marg_bias$comparison <- comparison
  marg_bias$outcome <- outcome
  
  marg_bias %<>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  write_xlsx(marg_bias,
             paste(path_res, outcome, 'marg_bias.xlsx', sep = '/'))
  rm(cov, i, marg_bias_cov)
  
  ## g) y_by_month
  cohort_analytic_exp0 <- cohort_analytic %>% filter(trt == 0)
  cohort_analytic_exp1 <- cohort_analytic %>% filter(trt == 1)
  
  month_list <- c()
  for (y in c('2019', '2020', '2021', '2022')) {
    for (m in 1:12) {
      month = paste(y, m, sep = '-')
      month_list = c(month_list, month)
    }
  }
  month_list
  rm(y, m, month)
  
  # create fx to get days of follow-up for each patient and month
  get_pt_per_month <- function(month, data) {
    data %<>% select(id, entry_date, itt_exit_date)
    
    month_start <- floor_date(ym(month), 'month')
    month_end <- ceiling_date(ym(month), 'month')
    month_col <- paste('pt', month, sep = '_')
    data[[month_col]] <- NA
    
    data[[month_col]] <- case_when(
      # patient entered after month ended OR entered and left after month started: 0 days
      data$entry_date > month_end |
        (
          data$entry_date < month_start &
            data$itt_exit_date < month_start
        ) ~ 0,
      
      # patient entered on/before month started and left on/after month ended: entire month
      data$entry_date <= month_start &
        data$itt_exit_date >= month_end ~ days_in_month(ym(month)),
      
      # patient entered on/before month started and left before month ended: days before left
      data$entry_date <= month_start &
        data$itt_exit_date < month_end ~ as.numeric(data$itt_exit_date - month_start),
      
      # patients entered after month started and left on/after month ended: days since entered
      data$entry_date > month_start &
        data$itt_exit_date >= month_end ~ as.numeric(month_end - data$entry_date),
      
      # patients entred after month started and left before month ended: days between entry and exit
      data$entry_date > month_start &
        data$itt_exit_date < month_end ~ as.numeric(data$itt_exit_date - data$entry_date),
      
      TRUE ~ data[[month_col]]
      
    )
    
    return(data)
    
  }
  
  ## (1) get IR overall 
  # apply fx and sum results
  pt_per_month <- lapply(month_list, function(month) {
    get_pt_per_month(month = month, data = cohort_analytic)
  })
  
  for (i in 1:length(pt_per_month)) {
    pt_month_i <- pt_per_month[[i]] %>%
      select(id, last_col()) # select id and person time (last col)
    cohort_analytic <- merge(cohort_analytic,
                             pt_month_i,
                             by = 'id',
                             all.x = TRUE)
  }
  
  sum_pt_per_month <- lapply(month_list, function(month) {
    cohort_analytic %>%
      summarize(pdays = sum(.data[[paste('pt', month, sep = '_')]])) %>%
      mutate(event_month = as.character(month), pyears = pdays / 365)
  })
  
  sum_pt_per_month <- bind_rows(sum_pt_per_month)
  
  # get number of events in each month
  events_per_month <- cohort_analytic %>%
    filter(itt_event == 1) %>%
    mutate(event_month = paste(year(itt_event_date), month(itt_event_date), sep = '-')) %>%
    group_by(event_month) %>%
    summarize(num_events = n())
  
  # get IR
  y_by_month <- data.frame(event_month = month_list)
  y_by_month <- merge(y_by_month, events_per_month, all.x = TRUE)
  y_by_month <- merge(y_by_month, sum_pt_per_month, all.x = TRUE)
  
  # months with 0 events or person-time have 'NA' for those columns
  # so let us replace that by 0
  y_by_month <- y_by_month %>% replace(is.na(.), 0)
  y_by_month %<>% mutate(IRper100 = num_events * 100 / pyears)
  
  
  ## (2) get IR for untreated
  # apply fx and sum results
  pt_per_month_exp0 <- lapply(month_list, function(month) {
    get_pt_per_month(month = month, data = cohort_analytic_exp0)
  })
  
  for (i in 1:length(pt_per_month_exp0)) {
    pt_month_i <- pt_per_month_exp0[[i]] %>%
      select(id, last_col()) # select id and person time (last col)
    cohort_analytic_exp0 <- merge(cohort_analytic_exp0,
                             pt_month_i,
                             by = 'id',
                             all.x = TRUE)
  }
  
  sum_pt_per_month_exp0 <- lapply(month_list, function(month) {
    cohort_analytic_exp0 %>%
      summarize(pdays_exp0 = sum(.data[[paste('pt', month, sep = '_')]])) %>%
      mutate(event_month = as.character(month), pyears_exp0 = pdays_exp0 / 365)
  })
  
  sum_pt_per_month_exp0 <- bind_rows(sum_pt_per_month_exp0)
  
  # get number of events in each month
  events_per_month_exp0 <- cohort_analytic_exp0 %>%
    filter(itt_event == 1) %>%
    mutate(event_month = paste(year(itt_event_date), month(itt_event_date), sep = '-')) %>%
    group_by(event_month) %>%
    summarize(num_events_exp0 = n())
  
  # get IR
  y_by_month_exp0 <- data.frame(event_month = month_list)
  y_by_month_exp0 <- merge(y_by_month_exp0, events_per_month_exp0, all.x = TRUE)
  y_by_month_exp0 <- merge(y_by_month_exp0, sum_pt_per_month_exp0, all.x = TRUE)
  
  # months with 0 events or person-time have 'NA' for those columns
  # so let us replace that by 0
  y_by_month_exp0 <- y_by_month_exp0 %>% replace(is.na(.), 0)
  y_by_month_exp0 %<>% mutate(IRper100_exp0 = num_events_exp0 * 100 / pyears_exp0)
  
  
  ## (3) get IR for treated
  # apply fx and sum results
  pt_per_month_exp1 <- lapply(month_list, function(month) {
    get_pt_per_month(month = month, data = cohort_analytic_exp1)
  })
  
  for (i in 1:length(pt_per_month_exp1)) {
    pt_month_i <- pt_per_month_exp1[[i]] %>%
      select(id, last_col()) # select id and person time (last col)
    cohort_analytic_exp1 <- merge(cohort_analytic_exp1,
                                  pt_month_i,
                                  by = 'id',
                                  all.x = TRUE)
  }
  
  sum_pt_per_month_exp1 <- lapply(month_list, function(month) {
    cohort_analytic_exp1 %>%
      summarize(pdays_exp1 = sum(.data[[paste('pt', month, sep = '_')]])) %>%
      mutate(event_month = as.character(month), pyears_exp1 = pdays_exp1 / 365)
  })
  
  sum_pt_per_month_exp1 <- bind_rows(sum_pt_per_month_exp1)
  
  # get number of events in each month
  events_per_month_exp1 <- cohort_analytic_exp1 %>%
    filter(itt_event == 1) %>%
    mutate(event_month = paste(year(itt_event_date), month(itt_event_date), sep = '-')) %>%
    group_by(event_month) %>%
    summarize(num_events_exp1 = n())
  
  # get IR
  y_by_month_exp1 <- data.frame(event_month = month_list)
  y_by_month_exp1 <- merge(y_by_month_exp1, events_per_month_exp1, all.x = TRUE)
  y_by_month_exp1 <- merge(y_by_month_exp1, sum_pt_per_month_exp1, all.x = TRUE)
  
  # months with 0 events or person-time have 'NA' for those columns
  # so let us replace that by 0
  y_by_month_exp1 <- y_by_month_exp1 %>% replace(is.na(.), 0)
  y_by_month_exp1 %<>% mutate(IRper100_exp1 = num_events_exp1 * 100 / pyears_exp1)
  
  
  ## (4) final data
  y_by_month_final <- merge(y_by_month, y_by_month_exp0, by = 'event_month')
  y_by_month_final <- merge(y_by_month_final, y_by_month_exp1, by = 'event_month')
  
  y_by_month_final$region <- region
  y_by_month_final$comparison <- comparison
  y_by_month_final$outcome <- outcome
  
  y_by_month_final %<>%
    mutate(across(where(is.numeric), ~ round(., 3)))
  
  write_xlsx(y_by_month_final,
             paste(path_res, outcome, 'y_by_month.xlsx', sep = '/'))
  rm(events_per_month,
     pt_per_month,
     pt_month_i,
     i,
     sum_pt_per_month,
     y_by_month_exp0, 
     y_by_month_exp1, 
     y_by_month)
  
}

