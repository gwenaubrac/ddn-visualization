## ---------------------------
##
## Program: 3. Combine & Clean Data for Visualization App
##
## Purposes: Combine and format results from all data partners for the visualization tool
##
## Author: Gwen Aubrac
##
## Date Created: 2024-12-17
##
## ---------------------------

#### SET UP ####

# define path to folders containing the results
cprd_vis <- "Z:/EPI/Protocol 24_004042/Gwen - cprd_vis"
bc_vis <- ""
ontario_vis <- ""
setwd('Z:/EPI/Protocol 24_004042/Gwen  - vis_app')

outcome_list <- c(
  
  #event-based outcomes
  'death',
  'stroke',
  'hypoglycemia_hosp',
  'amputation',
  'mi',
  #'chf_hosp',
  'copd_exacerbation',
  #'suicidal_hosp',
  
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

# load packages
library(dplyr)
library(magrittr)
library(writexl)
library(readxl)

#### CREATE MERGED DATAFRAMES ####

# bind all rows together
# by iterating through each data partner's
# active comparator cohorts and outcomes
covs <- data.frame()
ps_coef <- data.frame()
ps_bal <- data.frame()
smd <- data.frame()
x_by_month <- data.frame()
hr_main <- data.frame()
hr_sex <- data.frame()
hr_year <- data.frame()
hr_age <- data.frame()
hr_sens <- data.frame()
y_by_month <- data.frame()
marg_bias <- data.frame()

for (path_region in c(cprd_vis, bc_vis, ontario_vis)) {
  path_region = cprd_vis # remove later
  for (cohort in c('arb_vs_acei',
                   'snri_vs_ssri',
                   'su_vs_dpp4',
                   'su_vs_glp1',
                   'su_vs_sglt2')) {
    covs_cohort <- read_excel(paste(path_region, cohort, 'covs.xlsx', sep = '/'))
    covs <- bind_rows(covs, covs_cohort)
    
    ps_coef_cohort <- read_excel(paste(path_region, cohort, 'ps_coef.xlsx', sep = '/'))
    ps_coef <- bind_rows(ps_coef, ps_coef_cohort)
    
    ps_bal_cohort <- read_excel(paste(path_region, cohort, 'ps_bal.xlsx', sep = '/'))
    ps_bal <- bind_rows(ps_bal, ps_bal_cohort)
    
    smd_cohort <- read_excel(paste(path_region, cohort, 'smd.xlsx', sep = '/'))
    smd <- bind_rows(smd, smd_cohort)
    
    x_by_month_cohort <- read_excel(paste(path_region, cohort, 'x_by_month.xlsx', sep = '/'))
    x_by_month <- bind_rows(x_by_month, x_by_month_cohort)
    
    rm(covs_cohort, ps_coef_cohort, ps_bal_cohort, smd_cohort, x_by_month_cohort)
    
    # remove events that are only for certain cohorts
    new_outcome_list <- outcome_list
    
    if (!cohort %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
      new_outcome_list <- new_outcome_list[!new_outcome_list %in% c('hypoglycemia_hosp', 'amputation', 'retinopathy')]
    } 
    
    if (cohort %in% c('su_vs_dpp4', 'su_vs_glp1', 'su_vs_sglt2')) {
      new_outcome_list <- new_outcome_list[!new_outcome_list %in% c('diabetes')]
    }
    
    if (cohort == 'arb_vs_acei') {
      new_outcome_list <- new_outcome_list[!new_outcome_list %in% c('hypertension')]
    }
    
    for (outcome in new_outcome_list) {
      
      y_by_month_cohort <- read_excel(paste(path_region, cohort, outcome, 'y_by_month.xlsx', sep = '/'))
      y_by_month <- bind_rows(y_by_month, y_by_month_cohort)
      
      marg_bias_cohort <- read_excel(paste(path_region, cohort, outcome, 'marg_bias.xlsx', sep = '/'))
      marg_bias <- bind_rows(marg_bias, marg_bias_cohort)
      
      hr_main_cohort <- read_excel(paste(path_region, cohort, outcome, 'hr_main.xlsx', sep = '/'))
      hr_main <- bind_rows(hr_main, hr_main_cohort)
      
      if (outcome != "breast_cancer_screen") {
        hr_sex_cohort <- read_excel(paste(path_region, cohort, outcome, 'hr_sex.xlsx', sep = '/'))
        hr_sex <- bind_rows(hr_sex, hr_sex_cohort)
      }
      
      hr_year_cohort <- read_excel(paste(path_region, cohort, outcome, 'hr_year.xlsx', sep = '/'))
      hr_year <- bind_rows(hr_year, hr_year_cohort)
      
      hr_age_cohort <- read_excel(paste(path_region, cohort, outcome, 'hr_age.xlsx', sep = '/'))
      hr_age <- bind_rows(hr_age, hr_age_cohort)
      
      hr_sens_cohort <- read_excel(paste(path_region, cohort, outcome, 'hr_sens.xlsx', sep = '/'))
      hr_sens <- bind_rows(hr_sens, hr_sens_cohort)
      
      rm(hr_main_cohort,
         hr_sex_cohort,
         hr_year_cohort,
         hr_age_cohort,
         hr_sens_cohort,
         marg_bias_cohort,
         y_by_month_cohort)
    }
  }
}

#### FORMAT AND SAVE DATA

# round all numeric variables to 2 decimals
covs <- covs %>% mutate_if(is.numeric, round, 3)
ps_coef <- ps_coef %>% mutate_if(is.numeric, round, 3)
ps_bal <- ps_bal %>% mutate_if(is.numeric, round, 3)
smd <- smd %>% mutate_if(is.numeric, round, 3)
x_by_month <- x_by_month %>% mutate_if(is.numeric, round, 3)
hr_main <- hr_main %>% mutate_if(is.numeric, round, 3)
hr_sex <- hr_sex %>% mutate_if(is.numeric, round, 3)
hr_year <- hr_year %>% mutate_if(is.numeric, round, 3)
hr_age <- hr_age %>% mutate_if(is.numeric, round, 3)
hr_sens <- hr_sens %>% mutate_if(is.numeric, round, 3)
marg_bias <- marg_bias %>% mutate_if(is.numeric, round, 3)
y_by_month <- y_by_month %>% mutate_if(is.numeric, round, 3)

# Clean covariate names
covs$cov_name <- gsub("_base", "", covs$cov_name)
smd$cov_name <- gsub("_base", "", smd$cov_name)
ps_coef$cov_name <- gsub("_base", "", ps_coef$cov_name)

write_xlsx(covs, 'covs.xlsx')
write_xlsx(ps_coef, 'ps_coef.xlsx')
saveRDS(ps_bal, 'ps_bal.rds')
write_xlsx(smd, 'smd.xlsx')
write_xlsx(x_by_month, 'x_by_month.xlsx')
write_xlsx(hr_main, 'hr_main.xlsx')
write_xlsx(hr_sex, 'hr_sex.xlsx')
write_xlsx(hr_year, 'hr_year.xlsx')
write_xlsx(hr_age, 'hr_age.xlsx')
write_xlsx(hr_sens, 'hr_sens.xlsx')
write_xlsx(marg_bias, 'marg_bias.xlsx')
write_xlsx(y_by_month, 'y_by_month.xlsx')
