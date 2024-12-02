## ---------------------------
##
## Program: 3. Analyses
##
## Purposes: Generate descriptive analyses data for visualization tool.
## All data is tagged with the 'region' corresponding to the data partner, and 
## the final data will be merged across regions. The descriptive data is obtained
## in 2 steps: 
## 1. General descriptive data
## - a) ps_coef: coefficients of association between covariates and treatment
## - b) x_by_month: monthly number of patients in each treatment group
## - c) covs: number of patients with covariates
## - d) smd: standardized mean differences of covariates between treatment groups
## 2. Outcome-specific descriptive data
## - a) hr_main: hazard ratio for main analyses
## - b) hr_sex: hazard ratio in subgroups by sex (male, female)
## - c) hr_year: hazard ratio in subgroups by year of entry (2019-2022)
## - d) hr_age: hazard ratio in subgroups by age (<65, >=65)
## - e) marg_bias: marginal bias terms
## - f) y_by_month: crude monthly outcome incidence rate
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

# define analysis
analysis <- 'su_vs_dpp4'
region <- 'cprd'

# path results
path_cohort <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/', analysis)
path_temp <- paste0('Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/', analysis, '/temp')
path_codes <- 'Z:/EPI/Protocol 24_004042/Gwen - cprd_vis/codes'
path_cprd_event <- paste(path_codes, 'event', 'aurum_codes', sep = '/')
setwd(path_temp) # save intermediate results

# load packages
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)
library(writexl)
library(broom)
library(survival)
library(cobalt)

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

# set reference treatment and tag cohort with region
cohort <- readRDS(paste(path_temp, 'cohort_cens.rds', sep = '/'))

cohort %<>% mutate(
  trt = case_when(
    treatment == 'su' ~ 0,
    treatment == 'snri' ~ 0,
    treatment == 'arb' ~ 0,
    .default = 1
  )
)

cohort$region <- region
cohort$year_month <- cohort$month_year # remove later
cohort %<>% mutate(above_65 = if_else(age_at_entry >= 65, 1, 0)) # remove later

#### 1. GENERAL DESCRIPTIVE DATA ####

## a) ps_coef

# fit the IPTW model

# remove events that never occur in data
covariates <- covariates[!covariates %in% c('hypocalcemia_base', 'hypomagnesemia_base', 'suicidal_base')]
cov_events <- cov_events[!cov_events %in% c('hypocalcemia_base', 'hypomagnesemia_base', 'suicidal_base')]


summary(cohort[covariates])

iptw_model <- reformulate(covariates, 'trt')
iptw_fit <- glm(iptw_model, family= binomial, data = cohort)

cohort$prop_score <- if_else(cohort$trt == 0, 
                             1-predict(iptw_fit, type = 'response'),
                             predict(iptw_fit, type = 'response'))

cohort$iptw <- 1/cohort$prop_score
summary(cohort$iptw)
sd(cohort$iptw)

ps_coef <- tidy(iptw_fit)
ps_coef$region <- region

ps_coef %<>%
  filter(term != '(Intercept)') %>% 
  mutate(odds_ratio = exp(estimate)) %>% 
  rename(cov_name = term) %>% 
  select(cov_name, region, estimate, odds_ratio)

write_xlsx(ps_coef, paste(path_cohort, 'ps_coef.xlsx', sep = '/'))
rm(iptw_fit, iptw_model)

## b) x_by_month
x_by_month <- cohort %>% 
  group_by(region, year_month, trt) %>% 
  summarize(num_patients = n())

write_xlsx(x_by_month, paste(path_cohort, 'x_by_month.xlsx', sep = '/'))

## c) covs
covs <- cohort %>% 
  summarise(across(all_of(cov_events), ~sum(. == 1, na.rm = TRUE)))

covs %<>% pivot_longer(cols = -c(), names_to = 'cov_name', values_to = 'count')

# for treated only
covs_trt1 <- cohort %>% 
  filter(trt == 1) %>% 
  summarise(across(all_of(cov_events), ~sum(. == 1, na.rm = TRUE)))

covs_trt1 %<>% pivot_longer(cols = -c(), names_to = 'cov_name', values_to = 'count_trt1')

# for untreated only
covs_trt0 <- cohort %>% 
  filter(trt == 0) %>% 
  summarise(across(all_of(cov_events), ~sum(. == 1, na.rm = TRUE)))

covs_trt0 %<>% pivot_longer(cols = -c(), names_to = 'cov_name', values_to = 'count_trt0')

# merge for overall, treated, and untreated
covs <- merge(covs, covs_trt1, by = 'cov_name')
covs <- merge(covs, covs_trt0, by = 'cov_name')
covs <- cbind(region, covs, row.names = NULL)

write_xlsx(covs, paste(path_cohort, 'covs.xlsx', sep = '/'))
rm(covs_trt1, covs_trt0)

## d) smd
cohort_covs <- cohort %>% select(all_of(c(covariates)))
bal_tab <- bal.tab(
  trt ~ cohort_covs,
  data = cohort,
  s.d.denom = 'pooled'
)

smd <- bal_tab$Balance
smd$cov_name <- rownames(smd)
rownames(smd) <- NULL

smd %<>%
  select(Diff.Un, cov_name) %>% 
  rename(SMD = Diff.Un)

smd$region <- region
smd %<>% relocate(cov_name, region, SMD)

write_xlsx(smd, paste(path_cohort, 'smd.xlsx', sep = '/'))
rm(cohort_covs, bal_tab)

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
if (!analysis %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  outcome_list <- outcome_list[!outcome_list %in% c('hypoglycemia_hosp', 'amputation', 'retinopathy')]
} 

if (analysis %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
  outcome_list <- outcome_list[!outcome_list %in% c('diabetes')]
}

if (analysis == 'arb_vs_acei') {
  outcome_list <- outcome_list[!outcome_list %in% c('hypertension')]
}

## Define outcome

for (outcome in outcome_list) {
  outcome <- outcome
  
  cohort_analytic <- cohort
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
  
  cohort_analytic$at_exit_date <- pmin(
    cohort_analytic$itt_exit_date,
    cohort_analytic$disc_date,
    cohort_analytic$switch_date,
    na.rm = TRUE
  )
  
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
  write_xlsx(hr_main,
             paste(path_cohort, outcome, 'hr_main.xlsx', sep = '/'))
  rm(cox_itt, cox_at)
  
  
  ## b) hr_sex
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
  write_xlsx(hr_sex, paste(path_cohort, outcome, 'hr_sex.xlsx', sep = '/'))
  rm(cox_itt_male, cox_at_male, cox_itt_female, cox_at_female)
  
  
  ## c) hr_age
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
  write_xlsx(hr_age, paste(path_cohort, outcome, 'hr_age.xlsx', sep = '/'))
  rm(cox_itt_old, cox_at_old, cox_itt_young, cox_at_young)
  
  
  ## d) hr_sens
  cohort_analytic_sens <- cohort
  cohort_analytic_sens$event <- cohort_analytic_sens[[outcome]]
  cohort_analytic_sens$event_date <- cohort_analytic_sens[[paste(outcome, 'date', sep = '_')]]
  cohort_analytic_sens$itt_exit_date <- cohort_analytic$itt_exit_date
  
  cohort_analytic_sens$at_exit_date <- pmin(
    cohort_analytic_sens$itt_exit_date,
    cohort_analytic_sens$disc_date_sens,
    cohort_analytic_sens$switch_date,
    na.rm = TRUE
  )
  
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
  write_xlsx(hr_sens,
             paste(path_cohort, outcome, 'hr_sens.xlsx', sep = '/'))
  rm(cox_at_sens, cohort_analytic_sens)
  
  ## e) marg_bias
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
  
  write_xlsx(marg_bias,
             paste(path_cohort, outcome, 'marg_bias.xlsx', sep = '/'))
  rm(cov, i, marg_bias_cov)
  
  ## f) y_by_month
  
  month_list <- c()
  for (y in c('2019', '2020', '2021', '2022')) {
    for (m in 1:12) {
      month = paste(y, m, sep = '-')
      month_list = c(month_list, month)
    }
  }
  month_list
  rm(y, m, month)
  
  # 1) get person-time per month
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
  
  write_xlsx(y_by_month,
             paste(path_cohort, outcome, 'y_by_month.xlsx', sep = '/'))
  rm(events_per_month,
     pt_per_month,
     pt_month_i,
     i,
     sum_pt_per_month)
  
}

