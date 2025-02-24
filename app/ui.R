#### LOAD LIBRARIES ####

library(shiny)
library(shinydashboard)
library(highcharter)
library(plotly)
library(readxl)
library(dplyr)
library(magrittr)
library(shinyWidgets)

path_data <- "./data"
covs <- read_excel(paste(path_data, 'covs.xlsx', sep = '/'))

#### DEFINE SPECIFIC ELEMENTS ####

about_page <- tagList(
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
  
  ## x_by_month
  fluidRow(
    tabBox(
      title = "Number of New Users",
      selected = "All",
      width = 12,
      tabPanel("All", highchartOutput("x_by_month_plot")),
      tabPanel(title = uiOutput("exp1_panel_x"), highchartOutput("x_by_month_plot_exp1")),
      tabPanel(title = uiOutput("exp0_panel_x"), highchartOutput("x_by_month_plot_exp0")),
      side = "right"
    ),
  ),
  
  # covs
  tabItem(
    tabName = "Covariate Selector",
    fluidRow(
      box(
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        pickerInput("select_covar", "Select Covariates to Plot:", 
                    choices = unique(covs$cov_name), 
                    selected = unique(covs$cov_name), 
                    multiple = TRUE, 
                    options = list(`live-search` = TRUE, 
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Select None",
                                   `select-all-text` = 'Select All'))
      )
    )
  ),
  
  fluidRow(
    tabBox(
      title = "Covariates",
      selected = "All",
      width = 12,
      tabPanel("All", highchartOutput("covs_plot")),
      tabPanel(title = uiOutput("exp1_panel_covs"), highchartOutput("covs_plot_exp1")),
      tabPanel(title = uiOutput("exp0_panel_covs"), highchartOutput("covs_plot_exp0")),
      side = "right"
    )
  ),
  
  # ps_coef
  fluidRow(
    box(
      title = "PS Coefficients",
      highchartOutput("ps_coef_plot"),
      width = 12
    )
  ),
  
  # ps balance
  fluidRow(
    box(
      title = "PS Balance Before IPTW",
      highchartOutput("ps_bal_plot_unweighted"),
      width = 6
    ),
    box(
      title = "PS Balance After IPTW",
      highchartOutput("ps_bal_plot_weighted"),
      width = 6
    )
  ),
  
  # smds
  fluidRow(
    box(
      title = "Standardized Mean Differences",
      highchartOutput("smd_plot"),
      width = 12
    )
  )
)

outcome_tab <- tabPanel(
  "OUTCOMES",
  
  fluidRow(
    tabBox(
      selectInput(
        "outcome",
        label = "Select Outcome",
        choices = NULL
      )
    )
  ),
  
  # hr_main + hr_sens (forest plot)
  fluidRow(
    column(
      width = 10,
      tabBox(
        title = "Forest Plot",
        width = 12,
        selected = "Main",
        tabPanel("30-day grace period", plotlyOutput("hr_main_plot")),
        tabPanel("90-day grace period", plotlyOutput("hr_sens_plot")),
        side = "left"
      )
    ),
    column(
      width = 1,
      radioButtons("model", "Model", 
                   choices = c("ITT", "AT"), selected = "ITT")
    )
  ),
  
  # hr_main (ITT vs AT)
  fluidRow(
    box(
      title = "Intention-to-Treat vs As-Treated",
      plotlyOutput("itt_vs_at_plot"),
      width = 12
    )
  ),
  
  # y_by_month
  tabItem(
    tabName = "y_by_month",
    fluidRow(
      box(
        title = "Incidence Rate",
        status = "primary",
        solidHeader = TRUE,
        highchartOutput("y_by_month_plot"),
        width = 12
      ),
    ),
    fluidRow(
      box(
        title = "Description",
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        # background = "light-blue",
        'Crude number of events over time.'
      )
    )
  ),
  
  # marg_bias
  fluidRow(
    box(
      title = "Marginal Bias Terms",
      highchartOutput("marg_bias_plot"),
      width = 12
    ),
  ),
  
  # hr_age + hr_sex + hr_year
  fluidRow(
    column(
      width=10,
      tabBox(
        title = "Subgroup Analyses",
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
      radioButtons("model_subgroup", "Model", 
                   choices = c("ITT", "AT"), selected = "ITT")
    )
  )
)

#### APP UI ####
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "DDN Visualization"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("About", tabName = "about", icon = icon("circle-info")),
      menuItem("Results", tabName = "results", icon = icon("chart-simple")),
      
      ## display cohort buttons only when results is selected
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
    
    tabItems(
      tabItem(
        tabName = "about",
        about_page
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