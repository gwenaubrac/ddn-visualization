#### DEFINE OUTCOMES THAT CAN BE SELECTED ####

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

#### APP SERVER ####
server <- function(input, output, session) {
  
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
           hcaes(x = as.character(year_month), y = num_patients, group = region)) %>%
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
           hcaes(x = as.character(year_month), y = num_patients, group = region)) %>%
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
           hcaes(x = as.character(year_month), y = num_patients, group = region)) %>%
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
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Pct: {point.prop}") %>%
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
      "This plot shows the exponentiated coefficient (odds ratio) of each covariate in the propensity score model. This coefficient describes the strength of the association between each covariate and the treatment.",
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
      "This plot shows the crude standardized mean difference in the covariate distribution between treated and untreated patients. A high SMD indicates imbalance between both groups with regards to the covariate. Typically, an absolute SMD below 0.1 (indicated by the dashed line) is desired to achieve balance.",
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
             x = as.character(year_month),
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
      "This plot shows the crude incidence rate of the event per 100 years for each month. Incidence rates are calculated as the number of events over the person-years contributed by patients in the cohort in each month, multiplied by 100.",
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
      ) + 
        geom_point() + 
        theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
          color = "Region"
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
      ) + 
        geom_point() + 
        theme_bw() +
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
          color = "Region"
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
          color = "Region"
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
      ) + 
        geom_point() + 
        theme_bw() +
        geom_errorbar(aes(ymin = hr_ci_lower_AT, ymax = hr_ci_upper_AT, width = 0.05)) +
        geom_errorbarh(aes(xmin = hr_ci_lower_ITT, xmax = hr_ci_upper_ITT, height = 0.05)) +
        geom_abline(
          intercept = 0,
          slope = 1,
          linetype = 'dashed'
        ) +
        xlim(find_lim(plot_data)) + ylim(find_lim(plot_data)) +
        labs(
          x = "ITT",
          y = "AT",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # plot info
  observeEvent(input$info_itt_vs_at, {
    shinyalert(
      "Intention-to-Treat vs As-Treated",
      "This plot compares the IPTW-weighted estimates (and confidence intervals) for the intention-to-treat (ITT) and the as-treated analysis (AT), in which patients are censored when they discontinue their treatment for 30 days or more. The dotted line represents the identity line, along which ITT and AT estimates are equal.",
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
      ) + 
        geom_point() + 
        theme_bw() +
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
      ) + 
        geom_point() +
        theme_bw() +
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
      ) + 
        geom_point() + 
        theme_bw() +
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
      ) + 
        geom_point() + 
        theme_bw() +
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
      ) + 
        geom_point() + 
        theme_bw() +
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
        ) + 
        scale_color_manual(values = region_colors)
    )
    
    p
  })
  
  # plot info
  observeEvent(input$info_subgroup, {
    shinyalert(
      "Subgroup Analyses",
      "These plots show the IPTW-weighted hazard ratios comparing different subgroups. You can select whether to view results for the intention-to-treat (ITT) or as-treated (AT) analyses. The dotted line represents the identity line, along which estimates in both subgroups being compared are equal.",
      type = "info"
    )
  })
}