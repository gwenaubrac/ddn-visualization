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

#### DEFINE SPECIFIC ELEMENTS ####

all_outcomes <- c(
  "All-cause mortality" = "death", 
  "Stroke" = "stroke",
  "Hypoglycemia (hospitalization)" = "hypoglycemia_hosp",
  "Diabetic amputation" = "amputation",
  "Myocardial infarction" = "mi",
  #"Congestive heart failure (hospitalization)" = "chf_hosp",
  "COPD exacerbation" = "copd_exacerbation",
  #"Suicidal ideation (hospitalization)" = "suicidal_hosp",
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

cohort_outcomes <- list(
  snri_vs_ssri = all_outcomes[!all_outcomes %in% c("hypoglycemia_hosp", "amputation", "retinopathy")],
  arb_vs_acei = all_outcomes[!all_outcomes %in% c("hypoglycemia_hosp", "amputation", "hypertension", "retinopathy")],
  su_vs_sglt2 = all_outcomes[!all_outcomes %in% c("diabetes")],
  su_vs_glp1 = all_outcomes[!all_outcomes %in% c("diabetes")],
  su_vs_dpp4 = all_outcomes[!all_outcomes %in% c("diabetes")]
)

#### APP SERVER ####
server <- function(input, output, session) {
  
  # Load data
  path_data <- "./data"
  x_by_month <- read_excel(paste(path_data, 'x_by_month.xlsx', sep = '/'))
  
  x_by_month_all <- x_by_month %>% 
    group_by(year_month, region, comparison) %>% 
    summarise(num_patients = sum(num_patients), .groups = 'drop') %>%
    mutate(trt = 3)
  
  covs <- read_excel(paste(path_data, 'covs.xlsx', sep = '/'))
  ps_coef <- read_excel(paste(path_data, 'ps_coef.xlsx', sep = '/'))
  ps_bal <- read_excel(paste(path_data, 'ps_bal.xlsx', sep = '/'))
  smd <- read_excel(paste(path_data, 'smd.xlsx', sep = '/'))
  hr_main <- read_excel(paste(path_data, 'hr_main.xlsx', sep = '/'))
  hr_sens <- read_excel(paste(path_data, 'hr_sens.xlsx', sep = '/'))
  y_by_month <- read_excel(paste(path_data, 'y_by_month.xlsx', sep = '/'))
  marg_bias <- read_excel(paste(path_data, 'marg_bias.xlsx', sep = '/'))
  hr_age <- read_excel(paste(path_data, 'hr_age.xlsx', sep = '/'))
  hr_sex <- read_excel(paste(path_data, 'hr_sex.xlsx', sep = '/'))
  hr_year <- read_excel(paste(path_data, 'hr_year.xlsx', sep = '/'))
  
  # Define functions
  find_lim <- function(rd) {
    rd %<>% select(where(is.numeric))
    lims <- range(rd[, ])
    lower_limit <- round(min(lims) - (min(lims) * 0.15), 2)
    upper_limit <- round(max(lims) + (max(lims) * 0.15), 2)
    
    return(c(lower_limit, upper_limit))
  }
  
  # Update list of outcomes based on cohort selected
  observeEvent(input$cohort, {
    freezeReactiveValue(input, "outcome")  
    updateSelectInput(inputId = "outcome",
                      choices = cohort_outcomes[[input$cohort]])
  })
  
  # Update tab names based on cohort
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
  
  # Generate "PATIENTS" plots
  output$x_by_month_plot <- renderHighchart({
    filtered_data <- x_by_month_all %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = year_month, y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users"))
  })
  
  output$x_by_month_plot_exp1 <- renderHighchart({
    x_by_month_exp1 <- x_by_month %>% filter(trt == 1)
    
    filtered_data <- x_by_month_exp1 %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = year_month, y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users"))
  })
  
  output$x_by_month_plot_exp0 <- renderHighchart({
    x_by_month_exp0 <- x_by_month %>% filter(trt == 0)
    
    filtered_data <- x_by_month_exp0 %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = year_month, y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users"))
  })
  
  
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
             group = region
           ))  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Pct: {point.proportion}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Proportion (%)"))
  })
  
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
      hc_yAxis(title = list(text = "Proportion (%)"))
  })
  
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
      hc_yAxis(title = list(text = "Proportion (%)"))
  })
  
  output$ps_coef_plot <- renderHighchart({
    filtered_data <- ps_coef %>%
      filter(comparison == input$cohort)
    
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
      hc_yAxis(title = list(text = "Odds Ratio"))
  })
  
  output$ps_bal_plot_unweighted <- renderHighchart({
    filtered_data <- ps_bal %>%
      filter(comparison == input$cohort) 
    
    trt1_name <- 
      switch(
        input$cohort,
        'snri_vs_ssri' = 'SSRI',
        'arb_vs_acei' = 'ACEI',
        'su_vs_dpp4' = 'DPP-4',
        'su_vs_sglt2' = 'SGLT2',
        'su_vs_glp1' = 'GLP-1 RA'
      )
    
    trt0_name <- 
      switch(
        input$cohort,
        'snri_vs_ssri' = 'SNRI',
        'arb_vs_acei' = 'ARB',
        'su_vs_dpp4' = 'SU',
        'su_vs_sglt2' = 'SU',
        'su_vs_glp1' = 'SU'
      )
    
    data_trt0 <- filtered_data %>% filter(trt == 0)
    data_trt1 <- filtered_data %>% filter(trt == 1)
    
    p <- hchart(
      density(data_trt0$prop_score), 
      type = "area", 
      color = "steelblue", 
      name = trt0_name
    ) %>%
      hc_add_series(
        density(data_trt1$prop_score), type = "area",
        color = "#B71C1C", 
        name = trt1_name
      )
    
    # unweighted PS distribution
    p %<>%
      hc_xAxis(
        labels = list(format = "{value:.2f}"),
        title = list(text = "Propensity Score")
      ) %>%
      hc_yAxis(
        title = list(text = "Density")
      ) %>% 
      hc_tooltip(
        shared = FALSE, 
        useHTML = TRUE, 
        headerFormat = "", 
        pointFormat = "ps: {point.x:.3f} <br> density: {point.y:.3f}"
      ) %>%
      hc_legend(
        layout = "vertical",
        align = "right",
        verticalAlign = "middle"
      )
    
    p
    
  })
  
  output$ps_bal_plot_weighted <- renderHighchart({
    filtered_data <- ps_bal %>%
      filter(comparison == input$cohort) 
    
    trt1_name <- 
      switch(
        input$cohort,
        'snri_vs_ssri' = 'SSRI',
        'arb_vs_acei' = 'ACEI',
        'su_vs_dpp4' = 'DPP-4',
        'su_vs_sglt2' = 'SGLT2',
        'su_vs_glp1' = 'GLP-1 RA'
      )
    
    trt0_name <- 
      switch(
        input$cohort,
        'snri_vs_ssri' = 'SNRI',
        'arb_vs_acei' = 'ARB',
        'su_vs_dpp4' = 'SU',
        'su_vs_sglt2' = 'SU',
        'su_vs_glp1' = 'SU'
      )
    
    data_trt0 <- filtered_data %>% filter(trt == 0)
    data_trt1 <- filtered_data %>% filter(trt == 1)
    
    # weighted PS distribution
    p <- hchart(
      density(data_trt0$prop_score, 
              weights = data_trt0$iptw/sum(data_trt0$iptw)), 
      type = "area", 
      color = "steelblue", 
      name = trt0_name
    ) %>%
      hc_add_series(
        density(data_trt1$prop_score, 
                weights = data_trt1$iptw/sum(data_trt1$iptw)), 
        type = "area",
        color = "#B71C1C", 
        name = trt1_name
      )
    
    p %<>%
      hc_xAxis(
        labels = list(format = "{value:.2f}"),
        title = list(text = "Propensity Score")
      ) %>%
      hc_yAxis(
        title = list(text = "Weighted Density")
      ) %>% 
      hc_tooltip(
        shared = FALSE, 
        useHTML = TRUE, 
        headerFormat = "", 
        pointFormat = "ps: {point.x:.3f} <br> density: {point.y:.3f}"
      ) %>%
      hc_legend(
        layout = "vertical",
        align = "right",
        verticalAlign = "middle"
      )
    
    p
    
  })
  
  output$smd_plot <- renderHighchart({
    filtered_data <- smd %>%
      filter(comparison == input$cohort)
    
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
               ))
  })
  
  # Generate "OUTCOMES" plots
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
      hc_title(text = "Incidence Rate by Year")
  })
  
  output$hr_main_plot <- renderPlotly({
    filtered_data <- hr_main %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    filtered_data <- filtered_data %>%
      filter(model == input$model)
    
    # ggplot(data = filtered_data,
    #        aes(
    #          x = region,
    #          y = hr_estimate,
    #          ymin = hr_ci_lower,
    #          ymax = hr_ci_upper
    #        )) +
    #   geom_pointrange() +
    #   geom_hline(yintercept = 1, lty = 2) +
    #   coord_flip() +
    #   xlab("Model") + ylab("HR (95% CI)") +
    #   theme(
    #     panel.background = element_blank(),
    #     plot.background = element_blank(),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank()
    #   ) +
    #   scale_y_log10()
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = hr_estimate,
          y = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
        ) 
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
    
  })
  
  output$hr_sens_plot <- renderPlotly({
    filtered_data <- hr_sens %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    # ggplot(data = filtered_data,
    #        aes(
    #          x = model,
    #          y = hr_estimate,
    #          ymin = hr_ci_lower,
    #          ymax = hr_ci_upper
    #        )) +
    #   geom_pointrange() +
    #   geom_hline(yintercept = 1, lty = 2) +
    #   coord_flip() +
    #   xlab("Model") + ylab("HR (95% CI)") +
    #   theme(
    #     panel.background = element_blank(),
    #     plot.background = element_blank(),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank()
    #   ) +
    #   scale_y_log10()
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = hr_estimate,
          y = region
        )
      ) + geom_point() + theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
        ) 
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
  
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
          colour = factor(region)
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
          color = "Region"
        )
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
  
  output$marg_bias_plot <- renderHighchart({
    filtered_data <- marg_bias %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
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
      hc_title(text = "Marginal Bias of Covariates")
  })
  
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
          colour = factor(region)
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
          color = "Region"
        )
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
  
  output$hr_age_plot <- renderPlotly({
    filtered_data <- hr_age %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    filtered_data <- filtered_data %>%
      filter(model == input$model_subgroup)
    
    p <- ggplotly(
      ggplot(
        data = filtered_data,
        aes(
          x = old_hr_estimate,
          y = young_hr_estimate,
          colour = factor(region)
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
          color = "Region"
        )
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
  
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
          colour = factor(region)
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
          color = "Region"
        )
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
  
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
          colour = factor(region)
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
          color = "Region"
        )
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
  
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
          colour = factor(region)
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
          color = "Region"
        )
    )
    
    p <- plotly::layout(
      p,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      legend = list(bgcolor = "rgba(255,255,255,0)"),
      dragmode = "false",
      title = ""
    )
    
    p
  })
}