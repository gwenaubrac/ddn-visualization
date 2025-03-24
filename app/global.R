## ---------------------------
##
## global.R
##
## Purpose: Set up everything you need to run your app. This is the only document which you need to edit. 
## First, in your folder, create three R files ui.R, server.R, and global.R. Copy paste the ones from this github project.
##
## Then, please read through this code and follow the instructions to replace all relevant elements
## with the ones for your project. In order, we will go over:
## 1. Loading the libraries (nothing to change)
## 2. Adding the path to your data
## 3. Adding details about your project
## 4. Naming your cohorts
## 5. Naming your outcomes
## 6. Writing the text for your home page
## 7. Loading the data (nothing to change)
## 8. Naming your regions and colors to plot them
## 9. Instructions to remove undesired plots (optional)
##
## When you're done, click the "run app" button or type runApp() in your console. Good luck!
##
## ---------------------------

#### 1. LOAD LIBRARIES ####
# INSTRUCTIONS: Nothing to change here

library(shiny)
library(shinydashboard)
library(highcharter)
library(plotly)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(shinyalert)
library(shinyWidgets)
library(webshot)
library(htmlwidgets)

#### 2. PATH TO YOUR DATA ####
# INSTRUCTIONS: Replace this by the folder path to your data.

path_data <- "./data"

#### 3. ADD DETAILS ABOUT YOUR PROJECT ####
# INSTRUCTIONS: Replace these by the information of your project.

tool_title <- "DDN Visualization"
github_link <- "https://github.com/gwenaubrac/ddn-visualization"
email_address <- "mailto:gwen.aubrac@mail.mcgill.ca"

#### 4. NAME YOUR COHORTS ####
# INSTRUCTIONS: Replace these by the names of your cohorts.

# a) Cohort name
# left-hand side: name that appears in the app (label) for sidebar
# right-hand side: how it is referenced in your code/data
# note: please indicate which one is your reference group!

cohort_choices = c(
  "SNRI (ref) vs SSRI" = "snri_vs_ssri", 
  "ARB (ref) vs ACEI" = "arb_vs_acei",
  "SU (ref) vs SGLT2" = "su_vs_sglt2",
  "SU (ref) vs GLP-1 RA" = "su_vs_glp1",
  "SU (ref) vs DPP-4" = "su_vs_dpp4"
)

# b) Comparator group name ("treated/trt1")
# left-hand side: how cohort is referenced in your code/data
# right-hand side: name that appears in the app (label) for plots
cohort_mapping_trt1 <- list(
  snri_vs_ssri = 'SSRI',
  arb_vs_acei = 'ACEI',
  su_vs_dpp4 = 'DPP-4',
  su_vs_sglt2 = 'SGLT2',
  su_vs_glp1 = 'GLP-1 RA'
)

# c) Reference group name ("untreated/trt0")
cohort_mapping_trt0 <- list(
  snri_vs_ssri = 'SNRI',
  arb_vs_acei = 'ARB',
  su_vs_dpp4 = 'SU',
  su_vs_sglt2 = 'SU',
  su_vs_glp1 = 'SU'
)

#### 5. NAME YOUR OUTCOMES ####
# INSTRUCTION: Specify which outcomes can be selected and plotted
# left-hand side: name that appears in app (label)
# right-hand side: how it is referenced in your code/data

all_outcomes <- c(
  "All-cause mortality" = "death", 
  "Stroke" = "stroke",
  "Hypoglycemia (hospitalization)" = "hypoglycemia_hosp",
  "Diabetic amputation" = "amputation",
  "Myocardial infarction" = "mi",
  "COPD exacerbation" = "copd_exacerbation",
  "Diabetes" = "diabetes", 
  "Hypertension" = "hypertension",
  "Depression" = "depression",
  "Hyperlipidemia" = "hyperlipidemia",
  "COPD" = "copd",
  "Congestive Heart Failure" = "chf",
  "End-stage renal disease" = "end_stage_renal",
  "Retinopathy" = "retinopathy",
  "Breast cancer screening" = "breast_cancer_screen",
  "Colon cancer screening" = "colon_cancer_screen"
)

# optional: if certain outcomes are specific to a cohort of medication users,
# define it here, otherwise comment out this section or delete it
cohort_outcomes <- list(
  snri_vs_ssri = all_outcomes[!all_outcomes %in% c("hypoglycemia_hosp", "amputation", "retinopathy")],
  arb_vs_acei = all_outcomes[!all_outcomes %in% c("hypoglycemia_hosp", "amputation", "hypertension", "retinopathy")],
  su_vs_sglt2 = all_outcomes[!all_outcomes %in% c("diabetes")],
  su_vs_glp1 = all_outcomes[!all_outcomes %in% c("diabetes")],
  su_vs_dpp4 = all_outcomes[!all_outcomes %in% c("diabetes")]
)

#### 6. HOME PAGE ####
# INSTRUCTIONS: Modify all elements of the home page as you wish

home_page <- tagList(
  
  tags$style(HTML("
    h1 {
      font-size: 36px;  /* Font size for h1 */
    }
    p {
      font-size: 18px;  /* Font size for paragraphs */
    }
    ul {
      font-size: 16px;  /* Font size for unordered list */
    }
  ")),
  
  h1("Distibuted Data Networks Visualization Tool"),
  p("Distributed data networks (DDNs) and multi-database studies offer new ways of conducting health research that is rapid, rigorous, and reproducible."),
  p("However, the study populations contained in each database may be heterogeneous in terms of event rates and confounding structures."),
  p("This tool visualizes data from the United Kingdom, British Columbia, and Ontario for three different new-user active comparator cohorts which you can select in the sidebar, namely:"),
  tags$ul(
    tags$li("New users of SNRIs and SSRIs (antidepressant medications)"),
    tags$li("New users of ARBs and ACEIs (antihypertensive medications)"),
    tags$li("New users of SUs and SGLT2s (antidiabetic medications)"),
    tags$li("New users of SUs and GLP-1 RAs (antidiabetic medications)"),
    tags$li("New users of SUs and DPP-4s (antidiabetic medications)")
  ),
  p("For more information, visit our ",
    tags$a(href = "https://github.com/gwenaubrac/ddn-visualization", "GitHub page", target = "_blank"),
    "!"
  )
)

#### 7. LOAD DATA ####
# INSTRUCTIONS: Nothing to change here if you formatted your tables correctly

x_by_month <- read_excel(paste(path_data, 'x_by_month.xlsx', sep = '/'))
covs <- read_excel(paste(path_data, 'covs.xlsx', sep = '/'))
covs %<>% mutate(prop = prop* 100,
                 prop_trt0 = prop_trt0 * 100,
                 prop_trt1 = prop_trt1 * 100)

ps_coef <- read_excel(paste(path_data, 'ps_coef.xlsx', sep = '/'))
#ps_bal <- readRDS(paste(path_data, 'ps_bal.rds', sep = '/'))
smd <- read_excel(paste(path_data, 'smd.xlsx', sep = '/'))
y_by_month <- read_excel(paste(path_data, 'y_by_month.xlsx', sep = '/'))
hr_main <- read_excel(paste(path_data, 'hr_main.xlsx', sep = '/'))
hr_sens <- read_excel(paste(path_data, 'hr_sens.xlsx', sep = '/'))
marg_bias <- read_excel(paste(path_data, 'marg_bias.xlsx', sep = '/'))
hr_age <- read_excel(paste(path_data, 'hr_age.xlsx', sep = '/'))
hr_sex <- read_excel(paste(path_data, 'hr_sex.xlsx', sep = '/'))
hr_year <- read_excel(paste(path_data, 'hr_year.xlsx', sep = '/'))

## test start ##
# artificially adding a region to test app
# delete later
x_by_month_test <- x_by_month %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(trt = if_else(trt == 2, 1, 0)) %>% 
  mutate(region = "test")

x_by_month <- rbind(x_by_month, x_by_month_test)
x_by_month %<>%
  mutate(num_patients = if_else(region == "test" & year_month == "2019-03-01", NA, num_patients))

covs_test <- covs %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test") 

covs <- rbind(covs, covs_test)

covs %<>%
  mutate(prop = if_else(region == "test" & cov_name == "acute_renal", NA, prop)) %>% 
  mutate(prop = if_else(region == "test" & cov_name == "arrhythmia", NA, prop))


ps_coef_test <- ps_coef %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test")

ps_coef <- rbind(ps_coef, ps_coef_test)

ps_coef$cov_name <- sub("1$", "", ps_coef$cov_name) # clean var name
ps_coef %<>%
  mutate(odds_ratio = if_else(region == "test" & cov_name == "acute_renal", NA, odds_ratio)) %>% 
  mutate(odds_ratio = if_else(region == "test" & cov_name == "arrhythmia", NA, odds_ratio))

smd_test <- smd %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test")

smd <- rbind(smd, smd_test)

smd %<>%
  mutate(SMD = if_else(region == "test" & cov_name == "acute_renal", NA, SMD)) %>% 
  mutate(SMD = if_else(region == "test" & cov_name == "arrhythmia", NA, SMD))

y_by_month_test <- y_by_month %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test") %>% 
  mutate(IRper100 = if_else(year_month == "2019-03-01", NA, IRper100))

y_by_month <- rbind(y_by_month, y_by_month_test)

hr_main_test <- hr_main %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test")

hr_main <- rbind(hr_main, hr_main_test) %>% 
  mutate(hr_estimate = if_else(region == "test" & comparison == "snri_vs_ssri", NA, hr_estimate)) %>% 
  mutate(hr_ci_lower = if_else(region == "test" & comparison == "snri_vs_ssri", NA, hr_ci_lower)) %>% 
  mutate(hr_ci_upper = if_else(region == "test" & comparison == "snri_vs_ssri", NA, hr_ci_upper))

hr_sens_test <- hr_sens %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test")

hr_sens <- rbind(hr_sens, hr_sens_test)

marg_bias_test <- marg_bias %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "test")

marg_bias <- rbind(marg_bias, marg_bias_test)

marg_bias %<>%
  mutate(bias = if_else(region == "test" & cov_name == "acute_renal", NA, bias)) %>% 
  mutate(bias = if_else(region == "test" & cov_name == "arrhythmia", NA, bias))

hr_age_test <- hr_age %>%
  mutate_if(is.numeric, ~ . * 2) %>%
  mutate(region = "test")

hr_age <- rbind(hr_age, hr_age_test) %>% 
  mutate(old_hr_estimate = if_else(region == "test" & comparison == "snri_vs_ssri", NA, old_hr_estimate)) %>% 
  mutate(old_hr_ci_lower = if_else(region == "test" & comparison == "snri_vs_ssri", NA, old_hr_ci_lower)) %>% 
  mutate(old_hr_ci_upper = if_else(region == "test" & comparison == "snri_vs_ssri", NA, old_hr_ci_upper))

hr_year_test <- hr_year %>%
  mutate_if(is.numeric, ~ . * 2) %>%
  mutate(region = "test")

hr_year <- rbind(hr_year, hr_year_test)

hr_sex_test <- hr_sex %>%
  mutate_if(is.numeric, ~ . * 2) %>%
  mutate(region = "test")

hr_sex <- rbind(hr_sex, hr_sex_test)

## test end ##

x_by_month_all <- x_by_month %>% 
  group_by(year_month, region, comparison) %>% 
  summarise(num_patients = sum(num_patients), .groups = 'drop') %>%
  mutate(trt = 3)

#### 8. DEFINE REGION NAMES AND COLORS ####
# INSTRUCTIONS: Replace with desired name and color for each region where indicated
# You can add or remove regions as needed. 

df_list <- list(x_by_month = x_by_month,
                x_by_month_all = x_by_month_all,
                covs = covs,
                ps_coef = ps_coef,
                smd = smd,
                y_by_month = y_by_month,
                hr_main = hr_main,
                hr_sens = hr_sens,
                marg_bias = marg_bias,
                hr_age = hr_age,
                hr_year = hr_year,
                hr_sex = hr_sex)

# left-hand side: how region is referenced in your code/data
# right-hand side: name that will be displayed for that region in the app (label)
df_list <- lapply(df_list, function(df) {
  df$region <- recode(df$region,
                      "cprd" = "United Kingdom", # replace these by the name of your region 1
                      "test" = "Test") # replace these by the name of your region 2
  return(df)
})

list2env(df_list, envir = .GlobalEnv)

region_colors <- c(
  "#003f5c", # replace by desired color for your region 1
  "#bc5090" # replace by desired color for your region 2
)

#### 9. REMOVE PLOTS FROM THE APP ####
# If there are plots in the app that you are not interested in or don't have the data for,
# please search for the name of the plot in the ui.R and server.R files and comment out or delete the relevant section. 
# You can use the control + F command on Windows (or command + F on Mac) to look for them.

# PLOT 1: x_by_month (number of new users)
# PLOT 2: covs (covariates)
# PLOT 3: ps_coef (propensity score coefficients)
# PLOT 4: smds (standardized mean differences)
# PLOT 5: y_by_month (incidence rates)
# PLOT 6: hr_itt + hr_at + hr_sens (hazard ratios)
# PLOT 7: marg_bias (marginal bias terms)
# PLOT 8: hr_age + hr_sex + hr_year (subgroup analyses)


