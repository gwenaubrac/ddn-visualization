#### LOAD LIBRARIES ####

library(shiny)
library(shinydashboard)
library(highcharter)
library(plotly)
library(readxl)
library(dplyr)
library(magrittr)
library(shinyWidgets)
library(shinyalert)

path_data <- "./data"
covs <- read_excel(paste(path_data, 'covs.xlsx', sep = '/'))

#### DEFINE PAGES AND TABS FOR THE APP ####

home_page <- tagList(
  
  tags$style(HTML("
    h1 {
      font-size: 36px;  /* Increase font size for h1 */
    }
    p {
      font-size: 18px;  /* Increase font size for paragraphs */
    }
    ul {
      font-size: 16px;  /* Increase font size for unordered list */
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
      "This page describes the characteristics of patients in the cohort. For more information on each plot, click on the icon on the right."
    )
  ),
  
  ## x_by_month
  fluidRow(
    column(11,
    tabBox(
      title = HTML("<strong>Number of New Users</strong>"),
      selected = "All",
      width = 12,
      tabPanel("All", highchartOutput("x_by_month_plot")),
      tabPanel(title = uiOutput("exp1_panel_x"), highchartOutput("x_by_month_plot_exp1")),
      tabPanel(title = uiOutput("exp0_panel_x"), highchartOutput("x_by_month_plot_exp0")),
      side = "right"
    )
    ),
    column(1,
      actionButton("info_x_by_month", label = NULL, icon = icon("info"))
    )
  ),
  
  # covs
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
             title = HTML("<strong>Proportion with Covariates</strong>"),
             selected = "All",
             width = 12,
             tabPanel("All", highchartOutput("covs_plot")),
             tabPanel(title = uiOutput("exp1_panel_covs"), highchartOutput("covs_plot_exp1")),
             tabPanel(title = uiOutput("exp0_panel_covs"), highchartOutput("covs_plot_exp0")),
             side = "right"
           )
           ),
    column(1,
           actionButton("info_covs", label = NULL, icon = icon("info"))
           )
    
  ),
  
  # ps_coef
  fluidRow(
    column(11, 
           box(
             title = HTML("<strong>Propensity Score Coefficients</strong>"),
             highchartOutput("ps_coef_plot"),
             width = 12
           )
           ),
    column(1,
           actionButton("info_ps_coef", label = NULL, icon = icon("info"))
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
  
  # smds
  fluidRow(
    column(width = 11,
           box(
             title = HTML("<strong>Standardized Mean Differences</strong>"),
             highchartOutput("smd_plot"),
             width = 12
           )
           ),
    column(width = 1,
           actionButton("info_smd", label = NULL, icon = icon("info"))
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
      "This page describes the incidence and risk of different outcomes for the cohort. You can select different outcomes below. For more information on each plot, click on the icon on the right.",
      selectInput(
        "outcome",
        label = "Select Outcome",
        choices = NULL
      )
    )
  ),
  
  # y_by_month 
  fluidRow(
    column(
      width = 11,
      box(
        title = HTML("<strong>Incidence Rate</strong>"),
        highchartOutput("y_by_month_plot"),
        width = 12
      )
    ),
    column(
      width = 1,
      actionButton("info_y_by_month", label = NULL, icon = icon("info"))
    )
  ),
  
  # hr_itt + hr_at + hr_sens (forest plots)
  fluidRow(
    column(
      width = 11,
      tabBox(
        title = HTML("<strong>Hazard Ratio</strong>"),
        width = 12,
        selected = "30-day grace period (ITT)",
        tabPanel("30-day grace period (ITT)", plotlyOutput("hr_itt_plot")),
        tabPanel("30-day grace period (AT)", plotlyOutput("hr_at_plot")),
        tabPanel("90-day grace period (AT)", plotlyOutput("hr_sens_plot")),
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
        title = HTML("<strong>Intention-to-Treat vs As-Treated</strong>"),
        plotlyOutput("itt_vs_at_plot"),
        width = 12
      )
    ),
    column(
      width = 1,
      actionButton("info_itt_vs_at", label = NULL, icon = icon("info"))
    )
  ),
  
  # marg_bias
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
        title = HTML("<strong>Marginal Bias Terms</strong>"),
        highchartOutput("marg_bias_plot"),
        width = 12
      )
    ),
    column(
      width = 1,
      actionButton("info_marg_bias", label = NULL, icon = icon("info"))
    )

  ),
  
  # hr_age + hr_sex + hr_year
  fluidRow(
    column(
      width=11,
      tabBox(
        title = HTML("<strong>Hazard Ratio in Subgroups</strong"),
        selected = "Age",
        width = 12,
        tabPanel("Age", plotlyOutput("hr_age_plot")),
        tabPanel("Sex", plotlyOutput("hr_sex_plot")),
        tabPanel("2020", plotlyOutput("hr_2020_plot")),
        tabPanel("2021", plotlyOutput("hr_2021_plot")),
        tabPanel("2022", plotlyOutput("hr_2022_plot")),
        side = "right"
      ) 
    ),
    column(
      width = 1,
      actionButton("info_subgroup", label = NULL, icon = icon("info")),
      radioButtons("model_subgroup", "Model", 
                   choices = c("ITT", "AT"), selected = "ITT")
    ),
  )
)

#### APP UI ####
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "DDN Visualization", 
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
          choices = c(
            "SNRI vs SSRI" = "snri_vs_ssri", 
            "ARB vs ACEI" = "arb_vs_acei",
            "SU vs SGLT2" = "su_vs_sglt2",
            "SU vs GLP-1 RA" = "su_vs_glp1",
            "SU vs DPP-4" = "su_vs_dpp4"
          ),
          selected = "snri_vs_ssri"
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