#### LOAD LIBRARIES ####

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

#### LOAD DATA ####

path_data <- "./data"
x_by_month <- read_excel(paste(path_data, 'x_by_month.xlsx', sep = '/'))
covs <- read_excel(paste(path_data, 'covs.xlsx', sep = '/'))
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
x_by_month_bc <- x_by_month %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(trt = if_else(trt == 2, 1, 0)) %>% 
  mutate(region = "bc")

x_by_month <- rbind(x_by_month, x_by_month_bc)
x_by_month %<>%
  mutate(num_patients = if_else(region == "bc" & year_month == "2019-03-01", NA, num_patients))

covs_bc <- covs %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc") 

covs <- rbind(covs, covs_bc)

covs %<>%
  mutate(prop = if_else(region == "bc" & cov_name == "acute_renal", NA, prop)) %>% 
  mutate(prop = if_else(region == "bc" & cov_name == "arrhythmia", NA, prop))


ps_coef_bc <- ps_coef %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc")

ps_coef <- rbind(ps_coef, ps_coef_bc)

ps_coef$cov_name <- sub("1$", "", ps_coef$cov_name) # clean var name
ps_coef %<>%
  mutate(odds_ratio = if_else(region == "bc" & cov_name == "acute_renal", NA, odds_ratio)) %>% 
  mutate(odds_ratio = if_else(region == "bc" & cov_name == "arrhythmia", NA, odds_ratio))

smd_bc <- smd %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc")

smd <- rbind(smd, smd_bc)

smd %<>%
  mutate(SMD = if_else(region == "bc" & cov_name == "acute_renal", NA, SMD)) %>% 
  mutate(SMD = if_else(region == "bc" & cov_name == "arrhythmia", NA, SMD))

y_by_month_bc <- y_by_month %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc") %>% 
  mutate(IRper100 = if_else(year_month == "2019-03-01", NA, IRper100))

y_by_month <- rbind(y_by_month, y_by_month_bc)

hr_main_bc <- hr_main %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc")

hr_main <- rbind(hr_main, hr_main_bc) %>% 
  mutate(hr_estimate = if_else(region == "bc" & comparison == "snri_vs_ssri", NA, hr_estimate)) %>% 
  mutate(hr_ci_lower = if_else(region == "bc" & comparison == "snri_vs_ssri", NA, hr_ci_lower)) %>% 
  mutate(hr_ci_upper = if_else(region == "bc" & comparison == "snri_vs_ssri", NA, hr_ci_upper))

hr_sens_bc <- hr_sens %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc")

hr_sens <- rbind(hr_sens, hr_sens_bc)

marg_bias_bc <- marg_bias %>% 
  mutate_if(is.numeric, ~ . * 2) %>% 
  mutate(region = "bc")

marg_bias <- rbind(marg_bias, marg_bias_bc)

marg_bias %<>%
  mutate(bias = if_else(region == "bc" & cov_name == "acute_renal", NA, bias)) %>% 
  mutate(bias = if_else(region == "bc" & cov_name == "arrhythmia", NA, bias))

hr_age_bc <- hr_age %>%
  mutate_if(is.numeric, ~ . * 2) %>%
  mutate(region = "bc")

hr_age <- rbind(hr_age, hr_age_bc) %>% 
  mutate(old_hr_estimate = if_else(region == "bc" & comparison == "snri_vs_ssri", NA, old_hr_estimate)) %>% 
  mutate(old_hr_ci_lower = if_else(region == "bc" & comparison == "snri_vs_ssri", NA, old_hr_ci_lower)) %>% 
  mutate(old_hr_ci_upper = if_else(region == "bc" & comparison == "snri_vs_ssri", NA, old_hr_ci_upper))

hr_year_bc <- hr_year %>%
  mutate_if(is.numeric, ~ . * 2) %>%
  mutate(region = "bc")

hr_year <- rbind(hr_year, hr_year_bc)

hr_sex_bc <- hr_sex %>%
  mutate_if(is.numeric, ~ . * 2) %>%
  mutate(region = "bc")

hr_sex <- rbind(hr_sex, hr_sex_bc)

## test end ##

x_by_month_all <- x_by_month %>% 
  group_by(year_month, region, comparison) %>% 
  summarise(num_patients = sum(num_patients), .groups = 'drop') %>%
  mutate(trt = 3)

# if want to specify names and colors for regions a certain way:
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

df_list <- lapply(df_list, function(df) {
  df$region <- recode(df$region,
                      "cprd" = "United Kingdom",
                      "bc" = "British Columbia")
  return(df)
})

# assign modified dataframes to their original names
list2env(df_list, envir = .GlobalEnv)

region_colors <- c(
  "#003f5c",
  "#bc5090",
  "#ffa600"
)
