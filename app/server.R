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

#### DEFINE SPECIFIC ELEMENTS FOR THE APP ####

# specify which outcomes can be selected and plotted
# left-hand side: name that appears in app (label)
# right-hand side: how it is referenced in the code/data

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

# if certain outcomes are specific to a cohort of medication users,
# define it here:
cohort_outcomes <- list(
  snri_vs_ssri = all_outcomes[!all_outcomes %in% c("hypoglycemia_hosp", "amputation", "retinopathy")],
  arb_vs_acei = all_outcomes[!all_outcomes %in% c("hypoglycemia_hosp", "amputation", "hypertension", "retinopathy")],
  su_vs_sglt2 = all_outcomes[!all_outcomes %in% c("diabetes")],
  su_vs_glp1 = all_outcomes[!all_outcomes %in% c("diabetes")],
  su_vs_dpp4 = all_outcomes[!all_outcomes %in% c("diabetes")]
)

# define function to determine range of data to plot
find_lim <- function(rd) {
  rd %<>% select(where(is.numeric))
  lims <- range(rd[, ])
  lower_limit <- round(min(lims) - (min(lims) * 0.15), 2)
  upper_limit <- round(max(lims) + (max(lims) * 0.15), 2)
  
  return(c(lower_limit, upper_limit))
}

region_colors <- c(
  "#003f5c",
  "#bc5090",
  "#ffa600"
)

#### APP SERVER ####
server <- function(input, output, session) {
  
  # Load data
  path_data <- "./data"
  x_by_month <- read_excel(paste(path_data, 'x_by_month.xlsx', sep = '/'))
  covs <- read_excel(paste(path_data, 'covs.xlsx', sep = '/'))
  ps_coef <- read_excel(paste(path_data, 'ps_coef.xlsx', sep = '/'))
  ps_bal <- readRDS(paste(path_data, 'ps_bal.rds', sep = '/'))
  smd <- read_excel(paste(path_data, 'smd.xlsx', sep = '/'))
  y_by_month <- read_excel(paste(path_data, 'y_by_month.xlsx', sep = '/'))
  hr_main <- read_excel(paste(path_data, 'hr_main.xlsx', sep = '/'))
  hr_sens <- read_excel(paste(path_data, 'hr_sens.xlsx', sep = '/'))
  marg_bias <- read_excel(paste(path_data, 'marg_bias.xlsx', sep = '/'))
  hr_age <- read_excel(paste(path_data, 'hr_age.xlsx', sep = '/'))
  hr_sex <- read_excel(paste(path_data, 'hr_sex.xlsx', sep = '/'))
  hr_year <- read_excel(paste(path_data, 'hr_year.xlsx', sep = '/'))
  
  #### TESTING START ####
  x_by_month_bc <- x_by_month %>% 
    mutate_if(is.numeric, ~ . * 2) %>% 
    mutate(trt = if_else(trt == 2, 1, 0)) %>% 
    mutate(region = "bc")
  
  x_by_month <- rbind(x_by_month, x_by_month_bc)
  x_by_month %<>%
    mutate(num_patients = if_else(region == "bc" & year_month == "2019-03", NA, num_patients))
  
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
    mutate(IRper100 = if_else(event_month == "2019-2", NA, IRper100))
  
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
  
  #### TESTING END ####
  
  x_by_month_all <- x_by_month %>% 
    group_by(year_month, region, comparison) %>% 
    summarise(num_patients = sum(num_patients), .groups = 'drop') %>%
    mutate(trt = 3)
  
  output$messageMenu <- renderMenu({
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Github",
                   message = "Documentation, Source, Citation",
                   icon = icon("github"),
                   href = "https://github.com/gwenaubrac/ddn-visualization"),
                 messageItem(
                   from = "Issues",
                   message = "Report Issues",
                   icon = icon("exclamation"),
                   href = "https://github.com/gwenaubrac/ddn-visualization/issues"),
                 messageItem(
                   from = "Contact",
                   message = "Contact Us",
                   icon = icon("envelope"),
                   href = "mailto:gwen.aubrac@mail.mcgill.ca"),
                 badgeStatus = NULL,
                 headerText = "App Information",
                 icon = icon("info-circle") 
    )
  })
  
  # Update list of outcomes user can select based on cohort selected
  observeEvent(input$cohort, {
    freezeReactiveValue(input, "outcome")  
    updateSelectInput(inputId = "outcome",
                      choices = cohort_outcomes[[input$cohort]])
  })
  
  # Update tab names for exposed/unexposed based on cohort selected
  output$exp1_panel_x <- renderText({
    switch(
      input$cohort,
      'snri_vs_ssri' = 'SSRI',
      'arb_vs_acei' = 'ACEI',
      'su_vs_dpp4' = 'DPP-4',
      'su_vs_sglt2' = 'SGLT2',
      'su_vs_glp1' = 'GLP-1 RA'
    )
  })
  
  output$exp0_panel_x <- renderText({
    switch(
      input$cohort,
      'snri_vs_ssri' = 'SNRI',
      'arb_vs_acei' = 'ARB',
      'su_vs_dpp4' = 'SU',
      'su_vs_sglt2' = 'SU',
      'su_vs_glp1' = 'SU'
    )
  })
  
  output$exp1_panel_covs <- renderText({
    switch(
      input$cohort,
      'snri_vs_ssri' = 'SSRI',
      'arb_vs_acei' = 'ACEI',
      'su_vs_dpp4' = 'DPP-4',
      'su_vs_sglt2' = 'SGLT2',
      'su_vs_glp1' = 'GLP-1 RA'
    )
  })
  
  output$exp0_panel_covs <- renderText({
    switch(
      input$cohort,
      'snri_vs_ssri' = 'SNRI',
      'arb_vs_acei' = 'ARB',
      'su_vs_dpp4' = 'SU',
      'su_vs_sglt2' = 'SU',
      'su_vs_glp1' = 'SU'
    )
  })
  
  ## Generate "PATIENTS" plots
  
  # Incidence rates
  
  # all patients
  output$x_by_month_plot <- renderHighchart({
    
    # only keep patients in data that are in selected cohort
    # ("comparison" = which treatments are being contrasted in cohort)
    filtered_data <- x_by_month_all %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = year_month, y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users")) %>% 
      hc_colors(region_colors)
  })
  
  # exposed patients only
  output$x_by_month_plot_exp1 <- renderHighchart({
    x_by_month_exp1 <- x_by_month %>% filter(trt == 1)
    
    filtered_data <- x_by_month_exp1 %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = year_month, y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users")) %>%
      hc_colors(region_colors)
    
  })
  
  # unexposed patients only
  output$x_by_month_plot_exp0 <- renderHighchart({
    x_by_month_exp0 <- x_by_month %>% filter(trt == 0)
    
    filtered_data <- x_by_month_exp0 %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = year_month, y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users")) %>% 
      hc_colors(region_colors)
  })
  
  # plot info
  observeEvent(input$info_x_by_month, {
    shinyalert(
      "Number of New Users",
      "This plot shows the number of new patients who received a prescription for the drug in each month.",
      type = "info"
    )
  })
  
  # Covariates
  
  # all patients
  output$covs_plot <- renderHighchart({
    filtered_data <- covs %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    hchart(filtered_data,
           'scatter',
           hcaes(
             y = prop,
             x = cov_name,
             name = region,
             group = region,
           ))  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Pct: {point.proportion}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Proportion (%)")) %>% 
      hc_colors(region_colors)
    
  })
  
  # exposed patients only
  output$covs_plot_exp1 <- renderHighchart({
    filtered_data <- covs %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    hchart(
      filtered_data,
      'scatter',
      hcaes(
        y = prop_trt1,
        x = cov_name,
        name = region,
        group = region
      )
    )  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Pct: {point.prop_trt1}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Proportion (%)")) %>% 
      hc_colors(region_colors)
  })
  
  # unexposed patients only
  output$covs_plot_exp0 <- renderHighchart({
    filtered_data <- covs %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    hchart(
      filtered_data,
      'scatter',
      hcaes(
        y = prop_trt0,
        x = cov_name,
        name = region,
        group = region
      )
    )  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Pct: {point.prop_trt0}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Proportion (%)")) %>% 
      hc_colors(region_colors)
  })
  
  # plot info
  observeEvent(input$info_covs, {
    shinyalert(
      "Proportion with Covariates",
      "This plot shows the proportion of patients with certain covariate values.",
      type = "info"
    )
  })
  
  # Propensity score coefficients
  output$ps_coef_plot <- renderHighchart({
    
    filtered_data <- ps_coef %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    hchart(
      filtered_data,
      'scatter',
      hcaes(
        y = odds_ratio,
        x = cov_name,
        name = region,
        group = region
      )
    )  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>OR: {point.odds_ratio}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Odds Ratio")) %>% 
      hc_colors(region_colors)
  })
  
  # plot info
  observeEvent(input$info_ps_coef, {
    shinyalert(
      "Propensity Score Coefficient",
      "This plot shows the coefficient of each covariate in the propensity score model. This coefficient describes the strenght of the association between each covariate and the treatment.",
      type = "info"
    )
  })
  
  # output$ps_bal_plot_unweighted <- renderHighchart({
  #   filtered_data <- ps_bal %>%
  #     filter(comparison == input$cohort) 
  #   
  #   trt1_name <- 
  #     switch(
  #       input$cohort,
  #       'snri_vs_ssri' = 'SSRI',
  #       'arb_vs_acei' = 'ACEI',
  #       'su_vs_dpp4' = 'DPP-4',
  #       'su_vs_sglt2' = 'SGLT2',
  #       'su_vs_glp1' = 'GLP-1 RA'
  #     )
  #   
  #   trt0_name <- 
  #     switch(
  #       input$cohort,
  #       'snri_vs_ssri' = 'SNRI',
  #       'arb_vs_acei' = 'ARB',
  #       'su_vs_dpp4' = 'SU',
  #       'su_vs_sglt2' = 'SU',
  #       'su_vs_glp1' = 'SU'
  #     )
  #   
  #   data_trt0 <- filtered_data %>% filter(trt == 0)
  #   data_trt1 <- filtered_data %>% filter(trt == 1)
  #   
  #   p <- hchart(
  #     density(data_trt0$prop_score), 
  #     type = "area", 
  #     color = "steelblue", 
  #     name = trt0_name
  #   ) %>%
  #     hc_add_series(
  #       density(data_trt1$prop_score), type = "area",
  #       color = "#B71C1C", 
  #       name = trt1_name
  #     )
  #   
  #   # unweighted PS distribution
  #   p %<>%
  #     hc_xAxis(
  #       labels = list(format = "{value:.2f}"),
  #       title = list(text = "Propensity Score")
  #     ) %>%
  #     hc_yAxis(
  #       title = list(text = "Density")
  #     ) %>% 
  #     hc_tooltip(
  #       shared = FALSE, 
  #       useHTML = TRUE, 
  #       headerFormat = "", 
  #       pointFormat = "ps: {point.x:.3f} <br> density: {point.y:.3f}"
  #     ) %>%
  #     hc_legend(
  #       layout = "vertical",
  #       align = "right",
  #       verticalAlign = "middle"
  #     )
  #   
  #   p
  #   
  # })
  # 
  # output$ps_bal_plot_weighted <- renderHighchart({
  #   filtered_data <- ps_bal %>%
  #     filter(comparison == input$cohort) 
  #   
  #   trt1_name <- 
  #     switch(
  #       input$cohort,
  #       'snri_vs_ssri' = 'SSRI',
  #       'arb_vs_acei' = 'ACEI',
  #       'su_vs_dpp4' = 'DPP-4',
  #       'su_vs_sglt2' = 'SGLT2',
  #       'su_vs_glp1' = 'GLP-1 RA'
  #     )
  #   
  #   trt0_name <- 
  #     switch(
  #       input$cohort,
  #       'snri_vs_ssri' = 'SNRI',
  #       'arb_vs_acei' = 'ARB',
  #       'su_vs_dpp4' = 'SU',
  #       'su_vs_sglt2' = 'SU',
  #       'su_vs_glp1' = 'SU'
  #     )
  #   
  #   data_trt0 <- filtered_data %>% filter(trt == 0)
  #   data_trt1 <- filtered_data %>% filter(trt == 1)
  #   
  #   # weighted PS distribution
  #   p <- hchart(
  #     density(data_trt0$prop_score, 
  #             weights = data_trt0$iptw/sum(data_trt0$iptw)), 
  #     type = "area", 
  #     color = "steelblue", 
  #     name = trt0_name
  #   ) %>%
  #     hc_add_series(
  #       density(data_trt1$prop_score, 
  #               weights = data_trt1$iptw/sum(data_trt1$iptw)), 
  #       type = "area",
  #       color = "#B71C1C", 
  #       name = trt1_name
  #     )
  #   
  #   p %<>%
  #     hc_xAxis(
  #       labels = list(format = "{value:.2f}"),
  #       title = list(text = "Propensity Score")
  #     ) %>%
  #     hc_yAxis(
  #       title = list(text = "Weighted Density")
  #     ) %>% 
  #     hc_tooltip(
  #       shared = FALSE, 
  #       useHTML = TRUE, 
  #       headerFormat = "", 
  #       pointFormat = "ps: {point.x:.3f} <br> density: {point.y:.3f}"
  #     ) %>%
  #     hc_legend(
  #       layout = "vertical",
  #       align = "right",
  #       verticalAlign = "middle"
  #     )
  #   
  #   p
  #   
  # })
  
  # Standardized mean differences
  output$smd_plot <- renderHighchart({
    filtered_data <- smd %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    hchart(filtered_data,
           "line",
           hcaes(
             x = cov_name,
             y = round(SMD, 2),
             group = region
           )) %>%
      hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "SMD"),
               plotLines = list(
                 list(
                   value = 0.1,
                   color = "black",
                   dashStyle = "Dash",
                   width = 2,
                   zIndex = 5,
                   label = list(
                     text = "SMD Threshold",
                     align = "right",
                     x = 0,
                     y = -5
                   )
                 ),
                 list(
                   value = -0.1,
                   color = "black",
                   dashStyle = "Dash",
                   width = 2,
                   zIndex = 5,
                   label = list(
                     align = "right",
                     x = 0,
                     y = -5
                   )
                 )
               )) %>% 
      hc_colors(region_colors)
  })
  
  # plot info
  observeEvent(input$info_smd, {
    shinyalert(
      "Standardized Mean Differences",
      "This plot shows the crude standardized mean difference in the covariate distribution between treated and untreated patients. A high SMD indicates imbalance between both groups with regards to the covariate. Typically, an SMD below 0.1 is desired to achieve balance.",
      type = "info"
    )
  })
  
  ## Generate "OUTCOMES" plots
  
  # Incidence rates
  output$y_by_month_plot <- renderHighchart({
    filtered_data <- y_by_month %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(
             x = event_month,
             y = round(IRper100, 2),
             group = region
           )) %>%
      hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "IR per 100 years")) %>% 
      hc_colors(region_colors)
  })
  
  # plot info
  observeEvent(input$info_y_by_month, {
    shinyalert(
      "Incidence Rates",
      "This plot shows the crude incidence rate of the event in each month. Incidence rates are calculated as the number of events over the person-time contributed by patients in the cohort.",
      type = "info"
    )
  })
  
  # Hazard ratios
  # ITT (30-days grace period)
  output$hr_itt_plot <- renderPlotly({
    filtered_data <- hr_main %>%
      filter(outcome == input$outcome &
               comparison == input$cohort &
               model == "ITT")
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = hr_estimate,
          y = region,
          color = region
        )
      ) + geom_point() + 
        theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region"
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
    
  })
  
  # AT (30-day grace period)
  output$hr_at_plot <- renderPlotly({
    filtered_data <- hr_main %>%
      filter(outcome == input$outcome &
               comparison == input$cohort &
               model == "AT")
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = hr_estimate,
          y = region,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
    
  })
  
  # AT (90-day grace period)
  output$hr_sens_plot <- renderPlotly({
    filtered_data <- hr_sens %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = hr_estimate,
          y = region,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # plot info
  observeEvent(input$info_hr, {
    shinyalert(
      "Hazard Ratio",
      "These forest plots show the estimated IPTW-weighted hazard ratio of the event and corresponding 95% confidence intervals. You can view results for the intention-to-treat anlaysis (ITT) and the as-treated analysis (AT) using different grace periods to define treatment discontinuation.",
      type = "info"
    )
  })
  
  # ITT vs AT
  output$itt_vs_at_plot <- renderPlotly({
    
    filtered_data <- hr_main %>% 
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    itt_data <- filtered_data %>% filter(model == "ITT") %>% select(-model)
    at_data <- filtered_data %>% filter(model == "AT") %>% select(-model)
    plot_data <- merge(itt_data, at_data, by = c("region", "comparison", "outcome"), suffixes = c("_ITT", "_AT"))
    
    p <- ggplotly(
      ggplot(
        data = plot_data,
        aes(
          x = hr_estimate_ITT,
          y = hr_estimate_AT,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbar(aes(ymin = hr_ci_lower_AT, ymax = hr_ci_upper_AT, width = 0.05)) +
        geom_errorbarh(aes(xmin = hr_ci_lower_ITT, xmax = hr_ci_upper_ITT, height = 0.05)) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(plot_data)) + ylim(find_lim(plot_data)) +
        labs(
          fill = 'Region',
          x = "ITT",
          y = "AT",
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # plot info
  observeEvent(input$info_itt_vs_at, {
    shinyalert(
      "Intention-to-Treat vs As-Treated",
      "This plot compares the IPTW-weighted estimates (and confidence intervals) for the intention-to-treat (ITT) and the as-treated analysis (AT), in which patients are censored when they discontinue their treatment for 30 days or more.",
      type = "info"
    )
  })
  
  # Marginal bias terms
  output$marg_bias_plot <- renderHighchart({
    filtered_data <- marg_bias %>%
      filter(outcome == input$outcome &
               comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar_bias)
    
    hchart(filtered_data,
           'scatter',
           hcaes(
             y = bias,
             x = cov_name,
             name = region,
             group = region
           ))  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Bias: {point.bias}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Bias")) %>% 
      hc_colors(region_colors)
  })
  
  # plot info
  observeEvent(input$info_marg_bias, {
    shinyalert(
      "Marginal Bias Terms",
      "This plot shows the marginal bias terms.",
      type = "info"
    )
  })
  
  # Subgroup analyses
  
  # by age
  output$hr_age_plot <- renderPlotly({
    
    req(input$model_subgroup, input$outcome, input$cohort)
    
    filtered_data <- hr_age %>%
      filter(outcome == input$outcome &
               comparison == input$cohort &
               model == input$model_subgroup)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = old_hr_estimate,
          y = young_hr_estimate,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbar(aes(ymin = young_hr_ci_lower, ymax = young_hr_ci_upper, width = 0.05)) +
        geom_errorbarh(aes(xmin = old_hr_ci_lower, xmax = old_hr_ci_upper, height = 0.05)) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(filtered_data)) + ylim(find_lim(filtered_data)) +
        labs(
          fill = 'Region',
          x = "≥65",
          y = "<65",
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # by sex
  output$hr_sex_plot <- renderPlotly({
    filtered_data <- hr_sex %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    filtered_data <- filtered_data %>%
      filter(model == input$model_subgroup)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = female_hr_estimate,
          y = male_hr_estimate,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbar(aes(ymin = male_hr_ci_lower, ymax = male_hr_ci_upper, width = 0.05)) +
        geom_errorbarh(aes(xmin = female_hr_ci_lower, xmax = female_hr_ci_upper, height = 0.05)) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(filtered_data)) + ylim(find_lim(filtered_data)) +
        labs(
          fill = 'Region',
          x = "Female",
          y = "Male",
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # by year (2020 vs 2019)
  output$hr_2020_plot <- renderPlotly({
    filtered_data <- hr_year %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    filtered_data <- filtered_data %>%
      filter(model == input$model_subgroup)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = x2020_hr_estimate,
          y = x2019_hr_estimate,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbar(aes(ymin = x2019_hr_ci_lower, ymax = x2019_hr_ci_upper), width = 0.05) +
        geom_errorbarh(aes(xmin = x2020_hr_ci_lower, xmax = x2020_hr_ci_upper), height = 0.05) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(filtered_data)) + ylim(find_lim(filtered_data)) +
        labs(
          fill = 'Region',
          x = "2020",
          y = "2019",
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # by year (2021 vs 2019)
  output$hr_2021_plot <- renderPlotly({
    filtered_data <- hr_year %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    filtered_data <- filtered_data %>%
      filter(model == input$model_subgroup)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = x2021_hr_estimate,
          y = x2019_hr_estimate,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbar(aes(ymin = x2019_hr_ci_lower, ymax = x2019_hr_ci_upper, width = 0.05)) +
        geom_errorbarh(aes(xmin = x2021_hr_ci_lower, xmax = x2021_hr_ci_upper, height = 0.05)) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(filtered_data)) + ylim(find_lim(filtered_data)) +
        labs(
          fill = 'Region',
          x = "2021",
          y = "2019",
        ) +
        scale_color_manual(values = region_colors)
    ) 
    
    p
  })
  
  # by year (2022 vs 2019)
  output$hr_2022_plot <- renderPlotly({
    filtered_data <- hr_year %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    filtered_data <- filtered_data %>%
      filter(model == input$model_subgroup)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = x2022_hr_estimate,
          y = x2019_hr_estimate,
          color = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbar(aes(ymin = x2019_hr_ci_lower, ymax = x2019_hr_ci_upper, width = 0.05)) +
        geom_errorbarh(aes(xmin = x2022_hr_ci_lower, xmax = x2022_hr_ci_upper, height = 0.05)) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(filtered_data)) + ylim(find_lim(filtered_data)) +
        labs(
          fill = 'Region',
          x = "2022",
          y = "2019",
        ) + 
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # plot info
  observeEvent(input$info_subgroup, {
    shinyalert(
      "Subgroup Analyses",
      "These plots show the IPTW-weighted hazard ratios comparing different subgroups. You can select whether to view results for the intention-to-treat (ITT) or as-treated (AT) analyses.",
      type = "info"
    )
  })
}