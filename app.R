# ==============================================================================
# STUDENT PLACEMENT ANALYTICS - INTERACTIVE SHINY DASHBOARD
# ==============================================================================
# Department of Computer Science & Engineering
# Dr. B. R. Ambedkar National Institute of Technology (NIT), Jalandhar
#
# Authors:
#   - Kavi Prakash Ranjan (Roll No. 24103076)
#   - Kartik Saini (Roll No. 24103075)
# Faculty Supervisor:
#   - Dr. Jagdeep Kaur, Department of CSE, NIT Jalandhar
# Academic Session: 2026–2027
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(janitor)
library(scales)

# Source Modules
source("R/data_loader.R")
source("R/mod_overview.R")
source("R/mod_exploration.R")
source("R/mod_visualizations.R")
source("R/mod_insights.R")
source("R/mod_clustering.R")
source("R/mod_association_rules.R")
source("R/mod_calculator.R")

# Modern Executive Bootstrap 5 Theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1e3a8a",
  secondary = "#475569",
  success = "#059669",
  warning = "#d97706",
  danger = "#e11d48",
  info = "#0284c7",
  base_font = font_google("Plus Jakarta Sans"),
  code_font = font_google("JetBrains Mono")
)

# UI Definition
ui <- page_navbar(
  title = tagList(
    span(class = "brand-badge", "NITJ CSE"),
    span(class = "fw-bold ms-1", "Placement Analytics")
  ),
  theme = app_theme,
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    )
  ),
  
  # Tab 1: Executive Overview
  nav_panel(
    title = tagList(icon("gauge-high"), " Overview"),
    value = "tab_overview",
    div(class = "container-fluid py-3", mod_overview_ui("overview"))
  ),
  
  # Tab 2: Data Exploration & Quality
  nav_panel(
    title = tagList(icon("table"), " Data Explorer"),
    value = "tab_exploration",
    div(class = "container-fluid py-3", mod_exploration_ui("exploration"))
  ),
  
  # Tab 3: Empirical Visualizations
  nav_panel(
    title = tagList(icon("chart-column"), " Visualizations"),
    value = "tab_visualizations",
    div(class = "container-fluid py-3", mod_visualizations_ui("visualizations"))
  ),
  
  # Tab 4: Deep Dive Insights & Roadmap
  nav_panel(
    title = tagList(icon("lightbulb"), " Insights & Blueprint"),
    value = "tab_insights",
    div(class = "container-fluid py-3", mod_insights_ui("insights"))
  ),
  
  # Tab 5: Machine Learning Clustering & PCA
  nav_panel(
    title = tagList(icon("diagram-project"), " ML Clustering"),
    value = "tab_clustering",
    div(class = "container-fluid py-3", mod_clustering_ui("clustering"))
  ),
  
  # Tab 6: Association Rule Mining (Apriori)
  nav_panel(
    title = tagList(icon("link"), " Association Rules"),
    value = "tab_rules",
    div(class = "container-fluid py-3", mod_association_rules_ui("rules"))
  ),
  
  # Tab 7: Readiness Scorecard Calculator
  nav_panel(
    title = tagList(icon("calculator"), " Readiness Scorecard"),
    value = "tab_calculator",
    div(class = "container-fluid py-3", mod_calculator_ui("calculator"))
  ),
  
  # Global Application Footer
  footer = div(
    class = "app-footer container-fluid",
    div(
      class = "d-flex justify-content-between align-items-center flex-wrap px-3 gap-2",
      div(
        strong("Student Placement Analytics & Employability Insights Dashboard"),
        " • NIT Jalandhar Department of CSE"
      ),
      div(
        "Authors: ",
        strong("Kavi Prakash Ranjan"), " (24103076) & ",
        strong("Kartik Saini"), " (24103075) • ",
        "Supervisor: ", strong("Dr. Jagdeep Kaur")
      ),
      div(
        span(class = "badge bg-secondary", "R Shiny v1.10+"),
        span(class = "badge bg-primary ms-1", "Bootstrap 5"),
        span(class = "badge bg-success ms-1", "Plotly & ggplot2")
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Load master dataset
  placement_data <- load_placement_data()
  data_r <- reactiveVal(placement_data)
  
  # Initialize module servers
  mod_overview_server("overview", data_r)
  mod_exploration_server("exploration", data_r)
  mod_visualizations_server("visualizations", data_r)
  mod_insights_server("insights", data_r)
  mod_clustering_server("clustering", data_r)
  mod_association_rules_server("rules")
  mod_calculator_server("calculator")
}

# Run Shiny Application
shinyApp(ui = ui, server = server)
