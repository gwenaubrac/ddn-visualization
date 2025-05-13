## ---------------------------
##
## server.R
##
## Purpose: Link the UI elements to the data and specify other back-end elements. 
##
## ---------------------------

#### APP SERVER ####
server <- function(input, output, session) {
  
  output$messageMenu <- renderMenu({
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Github",
                   message = "Documentation, Source, Citation",
                   icon = icon("github"),
                   href = github_link),
                 messageItem(
                   from = "Issues",
                   message = "Report Issues",
                   icon = icon("exclamation"),
                   href = paste0(github_link, "/issues")),
                 messageItem(
                   from = "Contact",
                   message = "Contact Us",
                   icon = icon("envelope"),
                   href = email_address),
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
  
  ## Generate "PATIENTS" plots
  
  ## PLOT 1: x_by_month (number of new users)
  
  # define "exposed" group
  output$exp1_panel_x <- renderText({
    cohort_selected <- input$cohort
    cohort_label <- cohort_mapping_trt1[[cohort_selected]]
    cohort_label
  })
  
  # define "unexposed" group
  output$exp0_panel_x <- renderText({
    cohort_selected <- input$cohort
    cohort_label <- cohort_mapping_trt0[[cohort_selected]]
    cohort_label
  })
  
  # all patients
  output$x_by_month_plot <- renderHighchart({
    
    cohort_selected <- input$cohort
    
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
      hc_title(text = "<strong>Number of New Users in Each Month</strong>") %>%
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the number of new patients who received a prescription for a ", cohort_mapping_trt1[[cohort_selected]], " or ", cohort_mapping_trt0[[cohort_selected]], " in each month."))
  })
  
  # exposed patients only
  output$x_by_month_plot_exp1 <- renderHighchart({
    x_by_month_exp1 <- x_by_month %>% filter(trt == 1)
    
    cohort_selected <- input$cohort
    
    filtered_data <- x_by_month_exp1 %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = as.character(year_month), y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users")) %>%
      hc_title(text = "<strong>Number of New Users in Each Month</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the number of new patients who received a prescription for a ", cohort_mapping_trt1[[cohort_selected]], " in each month."))
    
  })
  
  # unexposed patients only
  output$x_by_month_plot_exp0 <- renderHighchart({
    x_by_month_exp0 <- x_by_month %>% filter(trt == 0)
    
    cohort_selected <- input$cohort
    
    filtered_data <- x_by_month_exp0 %>%
      filter(comparison == input$cohort)
    
    hchart(filtered_data,
           "line",
           hcaes(x = as.character(year_month), y = num_patients, group = region)) %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Month")) %>%
      hc_yAxis(title = list(text = "Number of New Users")) %>% 
      hc_title(text = "<strong>Number of New Users in Each Month</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the number of new patients who received a prescription for a ", cohort_mapping_trt0[[cohort_selected]], " in each month."))
    
  })
  
  ## PLOT 2: covs (covariates)
  
  # all patients
  output$covs_plot <- renderHighchart({
    
    filtered_data <- covs %>%
      filter(comparison == input$cohort) %>%
      filter(cov_name %in% input$select_covar)

    cohort_selected <- input$cohort
    
    hchart(filtered_data,
           'column',
           hcaes(
             y = prop,
             x = cov_name,
             name = region,
             group = region
           ))  %>%
      hc_tooltip(pointFormat = "<b>{point.cov_name}</b><br>Pct: {point.prop}") %>%
      hc_legend(title = list(text = "Region")) %>%
      hc_xAxis(title = list(text = "Covariate")) %>%
      hc_yAxis(title = list(text = "Proportion (%)")) %>%
      hc_title(text = "<strong>Proportion of Patients with Covariates</strong>") %>%
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the proportion of patients with certain covariate values amongst ", cohort_mapping_trt1[[cohort_selected]], " and ", cohort_mapping_trt0[[cohort_selected]], " users."))
  })
  
  # exposed patients only
  output$covs_plot_exp1 <- renderHighchart({
    filtered_data <- covs %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    cohort_selected <- input$cohort
    
    hchart(
      filtered_data,
      'column',
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
      hc_title(text = "<strong>Proportion of Patients with Covariates</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the proportion of patients with certain covariate values amongst ", cohort_mapping_trt1[[cohort_selected]], " users."))
  })
  
  # unexposed patients only
  output$covs_plot_exp0 <- renderHighchart({
    filtered_data <- covs %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    cohort_selected <- input$cohort
    
    hchart(
      filtered_data,
      'column',
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
      hc_title(text = "<strong>Proportion of Patients with Covariates</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the proportion of patients with certain covariate values amongst ", cohort_mapping_trt0[[cohort_selected]], " users."))
  })
  
  ## PLOT 3: ps_coef (propensity score coefficients)
  
  # define "exposed" group
  output$exp1_panel_covs <- renderText({
    cohort_selected <- input$cohort
    cohort_label <- cohort_mapping_trt1[[cohort_selected]]
    cohort_label
  })
  
  # define "unexposed" group
  output$exp0_panel_covs <- renderText({
    cohort_selected <- input$cohort
    cohort_label <- cohort_mapping_trt0[[cohort_selected]]
    cohort_label
  })
  
  output$ps_coef_plot <- renderHighchart({
    
    filtered_data <- ps_coef %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    cohort_selected <- input$cohort
    
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
      hc_yAxis(title = list(text = "Odds Ratio"),
               plotLines = list(
                 list(
                   value = 1,
                   color = "black",
                   dashStyle = "Dash",
                   width = 2,
                   zIndex = 5,
                   label = list(
                     text = "No Association",
                     align = "right",
                     x = 0,
                     y = -5
                   )
                 ))) %>% 
      hc_title(text = "<strong>Propensity Score Coefficients</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the exponentiated coefficient (odds ratio) of each covariate in the propensity score model for initiating a ", cohort_mapping_trt1[[cohort_selected]], " compared to a ", cohort_mapping_trt0[[cohort_selected]], ". This coefficient describes the strength of the association between each covariate and the treatment compared to the reference."))
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
  
  ## PLOT 4: smds (standardized mean differences)
  
  output$smd_plot <- renderHighchart({
    
    filtered_data <- smd %>%
      filter(comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar)
    
    cohort_selected <- input$cohort
    
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
      hc_title(text = "<strong>Standardized Mean Differences</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the crude standardized mean difference in the covariate distribution between treatment groups. A high SMD indicates imbalance between both groups with regards to the covariate. Typically, an absolute SMD below 0.1 (indicated by the dashed line) is desired to achieve balance. The reference group is ", cohort_mapping_trt0[[cohort_selected]], "."))
  })
  
  ## Generate "OUTCOMES" plots
  
  ## PLOT 5: y_by_month (incidence rates)
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
      hc_title(text = "<strong>Incidence Rate</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the crude incidence rate of the event per 100 years for each month. Incidence rates are calculated as the number of events over the person-years contributed by patients in the cohort in each month, multiplied by 100."))
    
  })
  
  ## PLOT 6: hr_itt + hr_at + hr_sens (hazard ratios)
  # ITT (no patients censored for discontinuing treatment)
  
  output$hr_itt_plot <- renderPlotly({
    
    filtered_data <- hr_main %>%
      filter(outcome == input$outcome &
               comparison == input$cohort &
               model == "ITT")
    
    cohort_selected <- input$cohort
    
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
        geom_errorbarh(aes(xmin = hr_ci_lower, xmax = hr_ci_upper, height = 0.05)) + # add CIs
        geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) + # add HR of 1
        labs(
          x = "HR (95% CI)",
          y = "Region",
          color = "Region",
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio (ITT)") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
      ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio (ITT)</b><br>',
            '<span style="font-size:11px">',
            'This forest plot shows the IPTW-weighted hazard ratio and corresponding 95% confidence intervals using an ITT approach.',
            '<br>The x-axis is on a log-scale. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # the default quality of plots downloaded using plotly (from the browser) is poor
    # so i configured the download options
    # and also increase font size and add margins around the plot
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_itt',
                  height = 800, # after testing this was the min resolution needed to maintain plot quality
                  width = 1600,
                  scale = 2
                )
    )
    
    p
    
  })
  
  # AT (with 30-day grace period to define treatment discontinuation)
  output$hr_at_plot <- renderPlotly({
    
    filtered_data <- hr_main %>%
      filter(outcome == input$outcome &
               comparison == input$cohort &
               model == "AT")
    
    cohort_selected <- input$cohort
    
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
        geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) + 
        labs(
          x = "HR (95% CI)",
          y = "Region",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio (AT with 30-day grace period)") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio (AT with 30-day grace period)</b><br>',
            '<span style="font-size:11px">',
            'This forest plot shows the IPTW-weighted hazard ratio and corresponding 95% confidence intervals using an AT approach ',
            '<br> with a 30-day grace period. The x-axis is on a log-scale. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_at_30_days',
                  height = 800,
                  width = 1600,
                  scale = 2
                )
    )
    
    p
    
  })
  
  # AT (sensitivity analysis with 90-day grace period to define treatment discontinuation)
  output$hr_sens_plot <- renderPlotly({
    
    filtered_data <- hr_sens %>%
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    cohort_selected <- input$cohort
    
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
        geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
        labs(
          x = "HR (95% CI)",
          y = "Region",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio (AT with 90-day grace period)") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio (AT with 90-day grace period)</b><br>',
            '<span style="font-size:11px">',
            'This forest plot shows the IPTW-weighted hazard ratio and corresponding 95% confidence intervals using an AT approach ',
            '<br> with a 90-day grace period. The x-axis is on a log-scale. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_at_90_days',
                  height = 800,
                  width = 1600,
                  scale = 2
                )
    )
    
    p
  })
  
  # ITT vs AT
  output$itt_vs_at_plot <- renderPlotly({
    
    filtered_data <- hr_main %>% 
      filter(outcome == input$outcome &
               comparison == input$cohort)
    
    cohort_selected <- input$cohort
    
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
        labs(
          x = "ITT",
          y = "AT",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Intention-to-Treat vs As Treated Hazard Ratio") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Intention-to-Treat vs As Treated Hazard Ratio</b><br>',
            '<span style="font-size:11px">',
            'This plot compares the IPTW-weighted estimates (and confidence intervals) for the intention-to-treat and the as-treated analyses. <br>',
            'The dotted line represents the identity line, along which ITT and AT estimates are equal. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_itt_vs_at',
                  height = 800,
                  width = 1200,
                  scale = 2
                )
    )
    
    p
  })
  
  ## PLOT 7: marg_bias (marginal bias terms)
  output$marg_bias_plot <- renderHighchart({
    
    filtered_data <- marg_bias %>%
      filter(outcome == input$outcome &
               comparison == input$cohort) %>% 
      filter(cov_name %in% input$select_covar_bias)
    
    cohort_selected <- input$cohort
    
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
      hc_title(text = "<strong>Marginal Bias Terms</strong>") %>% 
      hc_colors(region_colors) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_subtitle(text = paste0("This plot shows the marginal bias terms of each covariate with the outcome, which is the possible amount of confounding the covariate could adjust for in a multiplicative model given a binary exposure and outcome after adjusting for demographic variables."))
    
  })
  
  ## PLOT 8: hr_age + hr_sex + hr_year (subgroup analyses)
  
  # by age
  output$hr_age_plot <- renderPlotly({
    
    filtered_data <- hr_age %>%
      filter(outcome == input$outcome &
               comparison == input$cohort &
               model == input$model_subgroup)
    
    cohort_selected <- input$cohort
    
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
        labs(
          fill = 'Region',
          x = "≥65",
          y = "<65",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio in Subgroups by Age") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio in Subgroups by Age</b><br>',
            '<span style="font-size:11px">',
            'This plot shows the IPTW-weighted hazard ratios comparing subgroups by age. You can view results for intention-to-treat (ITT) or as-treated (AT)<br>',
            'on the right. The dotted line represents the identity line, along which estimates in both subgroups are equal. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_young_vs_old',
                  height = 800,
                  width = 1200,
                  scale = 2
                )
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
    
    cohort_selected <- input$cohort
    
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
        labs(
          fill = 'Region',
          x = "Female",
          y = "Male",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio in Subgroups by Sex") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio in Subgroups by Sex</b><br>',
            '<span style="font-size:11px">',
            'This plot shows the IPTW-weighted hazard ratios comparing subgroups by sex. You can view results for intention-to-treat (ITT) or as-treated (AT)<br>',
            'on the right. The dotted line represents the identity line, along which estimates in both subgroups are equal. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_male_vs_female',
                  height = 800,
                  width = 1200,
                  scale = 2
                )
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
    
    cohort_selected <- input$cohort
    
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
        labs(
          fill = 'Region',
          x = "2020",
          y = "2019",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio in Subgroups by Year of Cohort Entry") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio in Subgroups by Year of Cohort Entry</b><br>',
            '<span style="font-size:11px">',
            'This plot shows the IPTW-weighted hazard ratios comparing subgroups by year of cohort entry. You can view results for intention-to-treat (ITT) or as-treated (AT)<br>',
            'on the right. The dotted line represents the identity line, along which estimates in both subgroups are equal. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_2020_vs_2019',
                  height = 800,
                  width = 1200,
                  scale = 2
                )
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
    
    cohort_selected <- input$cohort
    
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
        labs(
          fill = 'Region',
          x = "2021",
          y = "2019",
          color = "Region"
        ) +
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio in Subgroups by Year of Cohort Entry") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio in Subgroups by Year of Cohort Entry</b><br>',
            '<span style="font-size:11px">',
            'This plot shows the IPTW-weighted hazard ratios comparing subgroups by year of cohort entry. You can view results for intention-to-treat (ITT) or as-treated (AT)<br>',
            'on the right. The dotted line represents the identity line, along which estimates in both subgroups are equal. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_2021_vs_2019',
                  height = 800,
                  width = 1200,
                  scale = 2
                )
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
    
    cohort_selected <- input$cohort
    
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
        labs(
          fill = 'Region',
          x = "2022",
          y = "2019",
          color = "Region"
        ) + 
        scale_color_manual(values = region_colors) +
        ggtitle("Hazard Ratio in Subgroups by Year of Cohort Entry") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
        ) +
        scale_x_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01)) # plot on a log scale with 2 decimals
    ) %>% 
      layout(
        title = list(
          text = paste0(
            '<b>Hazard Ratio in Subgroups by Year of Cohort Entry</b><br>',
            '<span style="font-size:11px">',
            'This plot shows the IPTW-weighted hazard ratios comparing subgroups by year of cohort entry. You can view results for intention-to-treat (ITT) or as-treated (AT)<br>',
            'on the right. The dotted line represents the identity line, along which estimates in both subgroups are equal. The reference group is ', cohort_mapping_trt0[[cohort_selected]], '.',
            '</span>'
          ),
          x = 0.5,
          xanchor = "center"
        )
      )
    
    # add margins around plot
    p <- p %>% layout(
      margin = list(
        t = 100,
        b = 100,
        l = 100,
        r = 100
      )
    )
    
    # configure download options
    p <- config(p, 
                toImageButtonOptions = list(
                  format = 'png', 
                  filename = 'hazard_ratio_2022_vs_2019',
                  height = 800,
                  width = 1200,
                  scale = 2
                )
    )
    
    p
  })
  
}