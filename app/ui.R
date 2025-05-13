## ---------------------------
##
## ui.R
##
## Purpose: Define the user interface for the app, including the homepage, sidebar, and tabs for the plots. 
##
## ---------------------------


#### DEFINE PAGES AND TABS FOR THE APP ####

patient_tab <- tabPanel(
  "PATIENTS",
  id = "patients_tab",

  ## description
  fluidRow(
    box(
      title = "About",
      status = "primary",
      solidHeader = FALSE,
      width = 12,
      "This page describes the characteristics of patients in the cohort. You can select the cohort to visualize in the sidebar. You can also click on a region to see data for that region only. For more information, click on the information icon on the right. To download the plot, click on the download icon on the right."
    )
  ),
  
  ## PLOT 1: x_by_month (number of new users)
  fluidRow(
    column(11,
    tabBox(
      selected = "All",
      width = 12,
      tabPanel("All", highchartOutput("x_by_month_plot")),
      tabPanel(title = uiOutput("exp1_panel_x"), highchartOutput("x_by_month_plot_exp1")),
      tabPanel(title = uiOutput("exp0_panel_x"), highchartOutput("x_by_month_plot_exp0")),
      side = "right"
    ),
    
    ),
    column(1,
      # buttons to download
      actionButton("download_x_by_month_plot", label = NULL, icon = icon("circle-down"), 
                   onclick = "downloadHighchart('x_by_month_plot', 'x_by_month_plot'); return false;")
    ),
    
    # javascript wrapper to download highchart plots as png
    # only to place this somewhere in code once (will apply to all downloadHighchart calls)
    tags$head(
      tags$script(
        "
    function downloadHighchart(id, filename) {
      var chart = $('#' + id).highcharts();
      
      // Configure parameters for the chart
      if (chart) {
        chart.update({
          exporting: {
            chartOptions: {
              chart: {
                width: 1200,
                height: 800
              },
              title: {
                style: {
                  fontSize: '24px'
                }
              },
              xAxis: {
                labels: {
                  style: {
                    fontSize: '16px'
                  }
                },
                title: {
                  style: {
                    fontSize: '18px'
                  }
                }
              },
              yAxis: {
                labels: {
                  style: {
                    fontSize: '16px'
                  }
                },
                title: {
                  style: {
                    fontSize: '18px'
                  }
                }
              },
              legend: {
                itemStyle: {
                  fontSize: '14px'
                }
              }
            }
          }
        });
        
        // Export the chart
        chart.exportChart({
          type: 'image/png',
          filename: filename || 'chart',
          scale: 3
        });
      }
    }
    "
      )
    )
    
  ),

  ## PLOT 2: covs (covariates)
  fluidRow(
    column(11,
           # user can select which covariate to plot
           pickerInput(
             "select_covar",
             "Select Covariates to Plot:",
             choices = unique(covs$cov_name),
             selected = unique(covs$cov_name),
             multiple = TRUE,
             options = list(
               `live-search` = TRUE,
               `actions-box` = TRUE,
               `deselect-all-text` = "Select None",
               `select-all-text` = 'Select All'
             )
           ),
           tabBox(
             selected = "All",
             width = 12,
             tabPanel("All", highchartOutput("covs_plot")),
             tabPanel(title = uiOutput("exp1_panel_covs"), highchartOutput("covs_plot_exp1")),
             tabPanel(title = uiOutput("exp0_panel_covs"), highchartOutput("covs_plot_exp0")),
             side = "right"
           )
           ),
    column(1,
           actionButton("download_covs_plot", label = NULL, icon = icon("circle-down"), 
                        onclick = "downloadHighchart('covs_plot', 'covs_plot'); return false;")
           )
  ),
  
  ## PLOT 3: ps_coef (propensity score coefficients)
  fluidRow(
    column(11, 
           box(
             highchartOutput("ps_coef_plot"),
             width = 12
           )
           ),
    column(1,
           actionButton("download_ps_coef_plot", label = NULL, icon = icon("circle-down"), 
                        onclick = "downloadHighchart('ps_coef_plot', 'ps_coef_plot'); return false;")
           )
  ),
  
  # # ps balance (might be deleted)
  # fluidRow(
  #   box(
  #     title = "PS Balance Before IPTW",
  #     highchartOutput("ps_bal_plot_unweighted"),
  #     width = 6
  #   ),
  #   box(
  #     title = "PS Balance After IPTW",
  #     highchartOutput("ps_bal_plot_weighted"),
  #     width = 6
  #   )
  # ),
  
  ## PLOT 4: smds (standardized mean differences)
  fluidRow(
    column(width = 11,
           box(
             highchartOutput("smd_plot"),
             width = 12
           )
           ),
    column(width = 1,
           actionButton("download_smd_plot", label = NULL, icon = icon("circle-down"), 
                        onclick = "downloadHighchart('smd_plot', 'smd_plot'); return false;")
           )

  )
)

outcome_tab <- tabPanel(
  "OUTCOMES",
  id = "outcomes_tab",
  
  ## description
  fluidRow(
    box(
      title = "About",
      status = "primary",
      solidHeader = FALSE,
      width = 12,
      "This page describes the incidence and risk of different outcomes for the cohort. You can select different outcomes below. For more information, click on the information icon on the right. To download the plot, click on the download icon on the right or on the save button on the plot (depending on the plot).",
      selectInput(
        "outcome",
        label = "Select Outcome",
        choices = NULL
      )
    )
  ),
  
  ## PLOT 5: y_by_month (incidence rates)
  fluidRow(
    column(
      width = 11,
      box(
        highchartOutput("y_by_month_plot"),
        width = 12
      )
    ),
    column(
      width = 1,
      actionButton("download_y_by_month_plot", label = NULL, icon = icon("circle-down"), 
                   onclick = "downloadHighchart('y_by_month_plot', 'y_by_month_plot'); return false;")
    )
  ),
  
  # PLOT 6: hr_itt + hr_at + hr_sens (hazard ratios)
  fluidRow(
    column(
      width = 11,
      tabBox(
        width = 12,
        selected = "ITT",
        tabPanel("ITT", plotlyOutput("hr_itt_plot", height = "600px")),
        tabPanel("AT (30-day grace period)", plotlyOutput("hr_at_plot", height = "600px")),
        tabPanel("AT (90-day grace period)", plotlyOutput("hr_sens_plot", height = "600px")),
        side = "right"
      )
    ),
    column(
      width = 1,
      actionButton("info_hr", label = NULL, icon = icon("info"))
    )
  ),
  
  # hr_itt vs hr_at
  fluidRow(
    column(
      width = 11,
      box(
        plotlyOutput("itt_vs_at_plot", height = "600px"),
        width = 12,
      )
    ),
    column(
      width = 1,
      actionButton("info_itt_vs_at", label = NULL, icon = icon("info"))
    )
  ),
  
  ## PLOT 7: marg_bias (marginal bias terms)
  fluidRow(
    column(
      width = 11,
      pickerInput(
        "select_covar_bias",
        "Select Covariates to Plot:",
        choices = unique(marg_bias$cov_name),
        selected = unique(marg_bias$cov_name),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `deselect-all-text` = "Select None",
          `select-all-text` = 'Select All'
        )
      ),
      box(
        highchartOutput("marg_bias_plot"),
        width = 12
      )
    ),
    column(
      width = 1,
      actionButton("download_marg_bias_plot", label = NULL, icon = icon("circle-down"), 
                   onclick = "downloadHighchart('marg_bias_plot', 'marg_bias_plot'); return false;")
    )

  ),
  
  # PLOT 8: hr_age + hr_sex + hr_year (subgroup analyses)
  fluidRow(
    column(
      width=11,
      tabBox(
        selected = "Age",
        width = 12,
        tabPanel("Age", plotlyOutput("hr_age_plot", height = "600px")),
        tabPanel("Sex", plotlyOutput("hr_sex_plot", height = "600px")),
        tabPanel("2020", plotlyOutput("hr_2020_plot", height = "600px")),
        tabPanel("2021", plotlyOutput("hr_2021_plot", height = "600px")),
        tabPanel("2022", plotlyOutput("hr_2022_plot", height = "600px")),
        side = "right"
      ) 
    ),
    column(
      width = 1,
      radioButtons("model_subgroup", "Model", 
                   choices = c("ITT", "AT"), selected = "ITT")
    )
  )
)

#### APP UI ####
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = tool_title, 
                  tags$li(class = "dropdown", actionButton("browser", "browser"),
                          tags$script("$('#browser').hide();")),
                  dropdownMenuOutput("messageMenu")),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Results", tabName = "results", icon = icon("chart-simple")),
      
      ## display cohort buttons only when "results" tab is selected
      conditionalPanel(
        condition = "input.sidebarmenu == 'results'",
        radioButtons(
          "cohort",
          label = "Select cohort",
          choices = cohort_choices,
          selected = cohort_choices[1]
        )
      )
    )
  ),
  
  # Body
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .main-header {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          z-index: 1001;
          background-color: #3c8dbc;
        }
        .main-sidebar {
          position: fixed;
          top: 0px;
          left: 0;
          bottom: 0;
          width: 230px; 
          z-index: 1000;
        }
        .content-wrapper {
          margin-left: 230;
          margin-top: 50px;
        }
        .main-footer {
          position: fixed;
          bottom: 0;
          width: 100%;
        }
        .content {
          padding-top: 20px;
        }
      "))
    ),
    
    # change style for tab names
    tags$style(HTML("
    .nav-tabs > li > a {
      font-size: 18px;
    }
  ")),
    
    # change style for sidebar items
    tags$style(HTML("
    .sidebar-menu > li > a {
      font-size: 18px; 
    }
  ")),
    
    tabItems(
      tabItem(
        tabName = "home",
        home_page
      ),
      tabItem(
        tabName = "results",
        tabsetPanel(
          id = "results_tabs",
          patient_tab,
          outcome_tab
        )
      )
    )
  )
)