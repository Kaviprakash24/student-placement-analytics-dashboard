# ==============================================================================
# MODULE: EXECUTIVE OVERVIEW
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)

mod_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Executive Header Banner
    div(
      class = "executive-banner",
      div(
        class = "d-flex justify-content-between align-items-start flex-wrap gap-2",
        div(
          h2(class = "banner-title", "Student Placement Analytics & Employability Insights"),
          p(
            class = "banner-subtitle",
            "An empirical study investigating undergraduate placement readiness, core technical differentiators, and salary determinants across engineering cohorts at Dr. B. R. Ambedkar National Institute of Technology (NIT) Jalandhar."
          )
        ),
        div(
          span(class = "badge bg-primary px-3 py-2 fs-6", "NIT Jalandhar • CSE Dept")
        )
      ),
      div(
        class = "banner-meta-bar",
        div(class = "banner-meta-item", strong("Researchers:"), "Kavi Prakash Ranjan (24103076) & Kartik Saini (24103075)"),
        div(class = "banner-meta-item", strong("Faculty Supervisor:"), "Dr. Jagdeep Kaur"),
        div(class = "banner-meta-item", strong("Dataset:"), "100 Survey Records • 32 Variables"),
        div(class = "banner-meta-item", strong("Academic Session:"), "2026–2027")
      )
    ),
    
    # High-Impact KPI Metric Cards
    div(
      class = "kpi-container",
      # KPI 1: Cohort Size
      div(
        class = "kpi-card primary",
        div(
          class = "kpi-header",
          span(class = "kpi-title", "Total Cohort Analyzed"),
          div(class = "kpi-icon-wrapper", icon("users"))
        ),
        div(class = "kpi-value", "100"),
        div(class = "kpi-subtitle", "Undergraduate Engineering Students"),
        span(class = "kpi-pill neutral", "6 Academic Branches")
      ),
      # KPI 2: Full-Time Placement Rate
      div(
        class = "kpi-card success",
        div(
          class = "kpi-header",
          span(class = "kpi-title", "Full-Time Placement Rate"),
          div(class = "kpi-icon-wrapper", icon("briefcase"))
        ),
        div(class = "kpi-value", "17.0%"),
        div(class = "kpi-subtitle", "17 Students Received Full-Time Offers"),
        span(class = "kpi-pill positive", "+1.37 DSA Margin Over Unplaced")
      ),
      # KPI 3: Key Technical Differentiator
      div(
        class = "kpi-card primary",
        div(
          class = "kpi-header",
          span(class = "kpi-title", "Core Technical Differentiator"),
          div(class = "kpi-icon-wrapper", icon("code"))
        ),
        div(class = "kpi-value", "3.94 vs 2.57"),
        div(class = "kpi-subtitle", "DSA Rating (Placed vs Unplaced)"),
        span(class = "kpi-pill positive", "+53.3% Superiority in DSA")
      ),
      # KPI 4: 12+ LPA Threshold
      div(
        class = "kpi-card warning",
        div(
          class = "kpi-header",
          span(class = "kpi-title", "Premium Salary (12+ LPA)"),
          div(class = "kpi-icon-wrapper", icon("chart-line"))
        ),
        div(class = "kpi-value", "4.33 / 5.0"),
        div(class = "kpi-subtitle", "Mean DSA Score for Top Packages"),
        span(class = "kpi-pill warning", "Monotonic Salary Staircase")
      ),
      # KPI 5: Primary Bottleneck
      div(
        class = "kpi-card danger",
        div(
          class = "kpi-header",
          span(class = "kpi-title", "Primary Preparation Gap"),
          div(class = "kpi-icon-wrapper", icon("triangle-exclamation"))
        ),
        div(class = "kpi-value", "18%"),
        div(class = "kpi-subtitle", "Experience Void (Projects / Internships)"),
        span(class = "kpi-pill warning", "Aptitude Tests Second (16%)")
      )
    ),
    
    # Executive Takeaways Callout
    div(
      class = "insight-callout primary mb-4",
      h5(class = "fw-bold mb-1", icon("lightbulb"), " Executive Takeaways & Empirical Law:"),
      p(
        class = "mb-0",
        "The empirical analysis establishes that while soft skills (Communication = 3.35 vs 3.13) create an essential entry baseline, ",
        strong("technical mastery in Data Structures & Algorithms (3.94 vs 2.57) and Object-Oriented Programming (4.00 vs 2.63) constitutes the true statistical differentiator"),
        " between placed and unplaced students. Furthermore, mock interviews eliminate false confidence, and hands-on project building directly fuels algorithmic proficiency."
      )
    ),
    
    # Visual Summaries Row 1
    fluidRow(
      column(
        width = 6,
        div(
          class = "card h-100",
          div(class = "card-header", "Cohort Demographics by Academic Year & Branch"),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_demographics"), height = "320px")
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card h-100",
          div(class = "card-header", "Placement Status Distribution"),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_placement_status"), height = "320px")
          )
        )
      )
    ),
    
    # Visual Summaries Row 2
    fluidRow(
      class = "mt-4",
      column(
        width = 7,
        div(
          class = "card h-100",
          div(class = "card-header", "Placed vs. Unplaced Competency Blueprint (1–5 Scale)"),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_gap_snapshot"), height = "320px")
          )
        )
      ),
      column(
        width = 5,
        div(
          class = "card h-100",
          div(class = "card-header", "Offer Compensation Tiers (LPA)"),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_compensation_tiers"), height = "320px")
          )
        )
      )
    )
  )
}

mod_overview_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Plot 1: Demographics by Academic Year & Branch
    output$plot_demographics <- renderPlotly({
      df <- data_r()
      
      demo_summary <- df %>%
        count(academic_year, branch, name = "count")
      
      p <- ggplot(demo_summary, aes(x = academic_year, y = count, fill = branch)) +
        geom_col(position = "stack", alpha = 0.9) +
        scale_fill_brewer(palette = "Set2") +
        labs(
          title = NULL,
          x = "Academic Year",
          y = "Number of Students",
          fill = "Branch"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 5, 5, 5)
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          legend = list(orientation = "v", x = 1.02, y = 0.5),
          margin = list(t = 20, r = 10, b = 40, l = 50)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Plot 2: Placement Status Distribution
    output$plot_placement_status <- renderPlotly({
      df <- data_r()
      
      status_summary <- df %>%
        count(placement_status, name = "count") %>%
        mutate(
          pct = round(count / sum(count) * 100, 1),
          label = paste0(count, " (", pct, "%)")
        ) %>%
        arrange(desc(count))
      
      # Clean labels for axis
      status_summary <- status_summary %>%
        mutate(
          short_status = case_when(
            grepl("Received a full-time", placement_status) ~ "Placed (Offer)",
            grepl("Participated in placement", placement_status) ~ "Participated (No Offer)",
            grepl("Haven't participated", placement_status) ~ "Not Participated Yet",
            grepl("Not eligible", placement_status) ~ "Not Eligible",
            TRUE ~ as.character(placement_status)
          )
        )
      
      p <- ggplot(status_summary, aes(x = reorder(short_status, count), y = count, fill = short_status)) +
        geom_col(show.legend = FALSE, width = 0.6) +
        geom_text(aes(label = paste0(count, " (", pct, "%)")), hjust = -0.1, size = 3.6, fontface = "bold") +
        coord_flip() +
        scale_fill_manual(values = c(
          "Placed (Offer)" = "#059669",
          "Participated (No Offer)" = "#e11d48",
          "Not Participated Yet" = "#0284c7",
          "Not Eligible" = "#64748b"
        )) +
        scale_y_continuous(limits = c(0, max(status_summary$count) * 1.25)) +
        labs(x = NULL, y = "Number of Students") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 5, 5, 5)
        )
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 20, r = 20, b = 40, l = 140)) %>%
        config(displayModeBar = FALSE)
    })
    
    # Plot 3: Placed vs Unplaced Competency Snapshot
    output$plot_gap_snapshot <- renderPlotly({
      df <- data_r()
      
      placed_comp <- df %>%
        group_by(placement_category) %>%
        summarise(
          DSA = round(mean(dsa_rating, na.rm = TRUE), 2),
          OOP = round(mean(oop_rating, na.rm = TRUE), 2),
          DBMS = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
          OS = round(mean(operating_systems_rating, na.rm = TRUE), 2),
          CN = round(mean(computer_networks_rating, na.rm = TRUE), 2),
          Communication = round(mean(communication_interview_rating, na.rm = TRUE), 2),
          Aptitude = round(mean(aptitude_logical_reasoning_rating, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols = -placement_category,
          names_to = "Skill",
          values_to = "Rating"
        )
      
      p <- ggplot(placed_comp, aes(x = Skill, y = Rating, fill = placement_category)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6) +
        scale_fill_manual(
          values = c("Placed" = "#059669", "Unplaced" = "#ef4444"),
          name = "Outcome"
        ) +
        scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
        labs(x = NULL, y = "Average Rating (1–5 Scale)") +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 5, 5, 5)
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          legend = list(orientation = "h", x = 0.35, y = 1.15),
          margin = list(t = 30, r = 10, b = 40, l = 50)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # Plot 4: Offer Compensation Tiers
    output$plot_compensation_tiers <- renderPlotly({
      df <- data_r()
      
      sal_df <- df %>%
        filter(
          !highest_full_time_offer_compensation %in% c("Not applicable / No offer yet", "No full-time offer", "Prefer not to say"),
          !is.na(highest_full_time_offer_compensation)
        ) %>%
        count(highest_full_time_offer_compensation, name = "count") %>%
        mutate(
          highest_full_time_offer_compensation = factor(
            highest_full_time_offer_compensation,
            levels = c("Below 5 LPA", "5 to 8 LPA", "8 to 12 LPA", "Above 12 LPA")
          )
        ) %>%
        arrange(highest_full_time_offer_compensation)
      
      if (nrow(sal_df) == 0) {
        return(plotly_empty(type = "scatter", mode = "markers"))
      }
      
      p <- ggplot(sal_df, aes(x = highest_full_time_offer_compensation, y = count, fill = highest_full_time_offer_compensation)) +
        geom_col(width = 0.55, show.legend = FALSE) +
        geom_text(aes(label = count), vjust = -0.4, fontface = "bold", size = 4) +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(limits = c(0, max(sal_df$count) * 1.3)) +
        labs(x = "Compensation Bracket", y = "Students Placed") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 5, 5, 5)
        )
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 20, r = 10, b = 40, l = 50)) %>%
        config(displayModeBar = FALSE)
    })
    
  })
}
