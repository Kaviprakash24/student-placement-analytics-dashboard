# ==============================================================================
# MODULE: DEEP-DIVE ANALYSIS & INSTITUTIONAL INSIGHTS
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)

mod_insights_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "mb-3",
      h4(class = "fw-bold text-dark mb-1", "Empirical Findings, Behavioral Disconnects & Institutional Roadmap"),
      p(class = "text-muted mb-0", "Comprehensive synthesis of analytical insights and evidence-based interventions from the NIT Jalandhar placement study.")
    ),
    
    navset_card_tab(
      # -------------------------------------------------------------
      # TAB 1: The Placed Student Blueprint
      # -------------------------------------------------------------
      nav_panel(
        "The Placed Blueprint",
        div(
          class = "p-3",
          div(
            class = "insight-callout success mb-3",
            h5(class = "fw-bold mb-1", icon("award"), " What Separates Placed Candidates?"),
            p(
              class = "mb-0",
              "Analysis of 17 placed vs. 83 unplaced/in-process candidates reveals that ",
              strong("DSA proficiency (+1.37 gap) and OOP competency (+1.37 gap) dominate as hiring gates"),
              ", while self-reported communication skills show virtually no divergence. Placed candidates typically build 3+ projects, solve 200+ coding problems, and attend rigorous mock interviews."
            )
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light h-100",
                h6(class = "fw-bold text-dark mb-3", "Comparative Competency Matrix (Placed vs. Unplaced)"),
                DTOutput(ns("table_blueprint"))
              )
            ),
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light h-100",
                h6(class = "fw-bold text-dark mb-3", "Signature Divergence in Competency Radar"),
                plotlyOutput(ns("plot_blueprint_radar"), height = "360px")
              )
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 2: The Mock Interview Effect & Confidence Collapse
      # -------------------------------------------------------------
      nav_panel(
        "The Mock Interview Effect",
        div(
          class = "p-3",
          div(
            class = "insight-callout warning mb-3",
            h5(class = "fw-bold mb-1", icon("triangle-exclamation"), " The 'Mock Wakeup' Phenomenon:"),
            p(
              class = "mb-0",
              "Students completing ", strong("zero mock interviews"), " exhibit an alarming ",
              strong("80.0% false resume readiness confidence"),
              ". However, following participation in just 1–2 mock interviews, reported readiness confidence crashes to ",
              strong("12%–34%"),
              "—proving that interview simulations immediately expose unaddressed technical gaps and puncture overconfidence."
            )
          ),
          fluidRow(
            column(
              width = 7,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Resume Readiness Confidence vs. Mock Interview Volume"),
                plotlyOutput(ns("plot_mock_collapse"), height = "350px")
              )
            ),
            column(
              width = 5,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Mock Participation Breakdown"),
                DTOutput(ns("table_mock_stats"))
              )
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 3: Resume Reality Disconnect
      # -------------------------------------------------------------
      nav_panel(
        "Resume Disconnect Audit",
        div(
          class = "p-3",
          div(
            class = "insight-callout danger mb-3",
            h5(class = "fw-bold mb-1", icon("circle-exclamation"), " The Superficial Formatting Trap:"),
            p(
              class = "mb-0",
              "Among 39 students claiming their resumes were 'Fully updated and reviewed', an astonishing ",
              strong("7.7% had completed ZERO technical projects"),
              ", and over 25% had inactive or missing GitHub repositories. Students frequently mistake cosmetic resume templates for genuine technical portfolio depth."
            )
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "GitHub Activity Among 'Fully Ready' Resumes"),
                plotlyOutput(ns("plot_resume_github"), height = "330px")
              )
            ),
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Project Counts Among 'Fully Ready' Resumes"),
                plotlyOutput(ns("plot_resume_projects"), height = "330px")
              )
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 4: Departmental Competency Matrix
      # -------------------------------------------------------------
      nav_panel(
        "Branch Competency Matrix",
        div(
          class = "p-3",
          div(
            class = "insight-callout primary mb-3",
            strong("Cross-Disciplinary Uniformity: "),
            "Baseline DSA proficiency varies narrowly across engineering branches (2.71 to 2.88), proving that technical preparation challenges affect both computing and non-computing departments equally. Institutional intervention must therefore be campus-wide."
          ),
          DTOutput(ns("table_branch_matrix"))
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 5: Strategic Recommendations & Action Plan
      # -------------------------------------------------------------
      nav_panel(
        "Strategic Recommendations",
        div(
          class = "p-3",
          div(
            class = "insight-callout primary mb-4",
            h5(class = "fw-bold mb-1", icon("graduation-cap"), " NIT Jalandhar Institutional Placement Improvement Framework:"),
            p(class = "mb-0", "Derived directly from Table 11.1 of the research report, outlining concrete, measurable actions across curriculum, project mentoring, and mock evaluations.")
          ),
          div(
            class = "table-responsive",
            tags$table(
              class = "table table-hover table-bordered align-middle bg-white",
              tags$thead(
                class = "table-light",
                tags$tr(
                  tags$th(style = "width: 25%", "Strategic Initiative"),
                  tags$th(style = "width: 20%", "Proposed Frequency"),
                  tags$th(style = "width: 55%", "Expected Analytical Outcome & Impact")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong(icon("code"), " DSA & Algorithmic Training")),
                  tags$td(span(class = "badge bg-primary", "3–4 Sessions / Week")),
                  tags$td("Elevate median cohort DSA rating from 2.80 to target 3.80+ threshold, directly satisfying Tier-1 campus recruitment benchmarks.")
                ),
                tags$tr(
                  tags$td(strong(icon("server"), " Core CS Intensive Workshops")),
                  tags$td(span(class = "badge bg-info text-dark", "Weekly")),
                  tags$td("Reinforce deep conceptual understanding in Operating Systems, DBMS/SQL, and Computer Networks, eliminating early technical elimination.")
                ),
                tags$tr(
                  tags$td(strong(icon("brain"), " Quantitative Aptitude Practice")),
                  tags$td(span(class = "badge bg-warning text-dark", "2–3 Sessions / Week")),
                  tags$td("Directly mitigate the #2 recruitment barrier (16 students blocked at initial screening tests via timed problem-solving sets).")
                ),
                tags$tr(
                  tags$td(strong(icon("laptop-code"), " Project Mentorship Milestones")),
                  tags$td(span(class = "badge bg-success", "Every Semester")),
                  tags$td("Mandate 2–3 full-stack/systems projects with active GitHub commits, preventing zero-project candidates from entering placement drives.")
                ),
                tags$tr(
                  tags$td(strong(icon("comments"), " Mandatory Mock Interviews")),
                  tags$td(span(class = "badge bg-danger", "Twice Monthly")),
                  tags$td("Dismantle false resume overconfidence early, providing structured feedback rubrics on technical coding and behavioral responses.")
                ),
                tags$tr(
                  tags$td(strong(icon("file-lines"), " Technical Portfolio Audit")),
                  tags$td(span(class = "badge bg-secondary", "Monthly")),
                  tags$td("Audit student resumes for substantiated GitHub repositories and live deployments rather than superficial formatting.")
                ),
                tags$tr(
                  tags$td(strong(icon("chart-line"), " Longitudinal Placement Tracking")),
                  tags$td(span(class = "badge bg-dark", "Monthly Cohort Review")),
                  tags$td("Monitor individual and departmental competency progression over time to dynamically deploy targeted academic support.")
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_insights_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Blueprint Table
    output$table_blueprint <- renderDT({
      df <- data_r()
      
      bp_df <- df %>%
        group_by(placement_category) %>%
        summarise(
          `DSA (1–5)` = round(mean(dsa_rating, na.rm = TRUE), 2),
          `OOP (1–5)` = round(mean(oop_rating, na.rm = TRUE), 2),
          `DBMS / SQL (1–5)` = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
          `OS (1–5)` = round(mean(operating_systems_rating, na.rm = TRUE), 2),
          `Networks (1–5)` = round(mean(computer_networks_rating, na.rm = TRUE), 2),
          `Communication (1–5)` = round(mean(communication_interview_rating, na.rm = TRUE), 2),
          `Aptitude (1–5)` = round(mean(aptitude_logical_reasoning_rating, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Transpose for clean comparative presentation
      bp_long <- bp_df %>%
        pivot_longer(cols = -placement_category, names_to = "Dimension", values_to = "Score") %>%
        pivot_wider(names_from = placement_category, values_from = Score) %>%
        mutate(
          `Delta (+/-)` = round(Placed - Unplaced, 2),
          `Impact` = case_when(
            `Delta (+/-)` >= 1.0 ~ "Critical Differentiator",
            `Delta (+/-)` >= 0.5 ~ "Secondary Factor",
            TRUE ~ "Baseline Parity"
          )
        )
      
      datatable(
        bp_long,
        options = list(dom = "t", pageLength = 10),
        class = "compact stripe hover",
        rownames = FALSE
      ) %>%
        formatStyle(
          "Delta (+/-)",
          color = styleInterval(c(0.3, 0.99), c("#64748b", "#0284c7", "#059669")),
          fontWeight = "bold"
        )
    })
    
    # 2. Blueprint Radar Plot
    output$plot_blueprint_radar <- renderPlotly({
      df <- data_r()
      
      bp_df <- df %>%
        group_by(placement_category) %>%
        summarise(
          DSA = mean(dsa_rating, na.rm = TRUE),
          OOP = mean(oop_rating, na.rm = TRUE),
          DBMS = mean(dbms_sql_rating, na.rm = TRUE),
          OS = mean(operating_systems_rating, na.rm = TRUE),
          CN = mean(computer_networks_rating, na.rm = TRUE),
          Comm = mean(communication_interview_rating, na.rm = TRUE),
          Apt = mean(aptitude_logical_reasoning_rating, na.rm = TRUE),
          .groups = "drop"
        )
      
      skills <- c("DSA", "OOP", "DBMS", "OS", "CN", "Comm", "Apt")
      placed_vals <- as.numeric(bp_df %>% filter(placement_category == "Placed") %>% select(-placement_category))
      unplaced_vals <- as.numeric(bp_df %>% filter(placement_category == "Unplaced") %>% select(-placement_category))
      
      # Close polygon loop
      skills_loop <- c(skills, skills[1])
      placed_loop <- c(placed_vals, placed_vals[1])
      unplaced_loop <- c(unplaced_vals, unplaced_vals[1])
      
      plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
        add_trace(
          r = placed_loop,
          theta = skills_loop,
          name = 'Placed Students',
          fill = 'toself',
          fillcolor = 'rgba(5, 150, 105, 0.25)',
          line = list(color = '#059669', width = 2.5)
        ) %>%
        add_trace(
          r = unplaced_loop,
          theta = skills_loop,
          name = 'Unplaced Students',
          fill = 'toself',
          fillcolor = 'rgba(239, 68, 68, 0.15)',
          line = list(color = '#ef4444', width = 2, dash = 'dot')
        ) %>%
        layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 5))
          ),
          legend = list(orientation = "h", x = 0.2, y = 1.15),
          margin = list(t = 25, b = 25, l = 25, r = 25)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 3. Mock Collapse Plot
    output$plot_mock_collapse <- renderPlotly({
      df <- data_r()
      
      mock_eff <- df %>%
        filter(!is.na(mock_interviews_completed)) %>%
        mutate(
          mock_tier = factor(
            case_when(
              grepl("None|0", mock_interviews_completed) ~ "0 (None)",
              grepl("1.*2", mock_interviews_completed) ~ "1-2 Mocks",
              grepl("3.*5", mock_interviews_completed) ~ "3-5 Mocks",
              TRUE ~ "6+ Mocks"
            ),
            levels = c("0 (None)", "1-2 Mocks", "3-5 Mocks", "6+ Mocks")
          )
        ) %>%
        group_by(mock_tier) %>%
        summarise(
          Students = n(),
          Pct_Overconfident = round(
            mean(grepl("Fully updated|4|5", as.character(self_reported_resume_readiness)), na.rm = TRUE) * 100,
            1
          ),
          Avg_Comm = round(mean(communication_interview_rating, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      p <- ggplot(mock_eff, aes(x = mock_tier, y = Pct_Overconfident, group = 1)) +
        geom_col(fill = "#d97706", width = 0.5, alpha = 0.85) +
        geom_text(aes(label = paste0(Pct_Overconfident, "%")), vjust = -0.5, fontface = "bold", size = 4.2) +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
        labs(
          x = "Mock Interview Participation Tier",
          y = "% Reporting 'Fully Ready' Resume Confidence"
        ) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 25, r = 10, b = 40, l = 50)) %>%
        config(displayModeBar = FALSE)
    })
    
    # 4. Mock Stats DT Table
    output$table_mock_stats <- renderDT({
      df <- data_r()
      
      mock_summary <- df %>%
        filter(!is.na(mock_interviews_completed)) %>%
        group_by(mock_interviews_completed) %>%
        summarise(
          Count = n(),
          `Avg Comm Rating` = round(mean(communication_interview_rating, na.rm = TRUE), 2),
          `Avg DSA Rating` = round(mean(dsa_rating, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        arrange(desc(Count))
      
      datatable(
        mock_summary,
        options = list(dom = "t", pageLength = 5),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })
    
    # 5A. Resume vs GitHub Plot
    output$plot_resume_github <- renderPlotly({
      df <- data_r() %>%
        filter(grepl("Fully updated|4|5", as.character(self_reported_resume_readiness))) %>%
        count(github_project_usage, name = "count") %>%
        mutate(pct = round(count / sum(count) * 100, 1))
      
      p <- ggplot(df, aes(x = reorder(github_project_usage, count), y = count, fill = github_project_usage)) +
        geom_col(show.legend = FALSE, width = 0.6) +
        geom_text(aes(label = paste0(count, " (", pct, "%)")), hjust = -0.1, fontface = "bold", size = 3.8) +
        coord_flip() +
        scale_fill_brewer(palette = "Reds") +
        scale_y_continuous(limits = c(0, max(df$count) * 1.35)) +
        labs(x = NULL, y = "Students Claiming 'Ready' Resume") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 20, r = 25, b = 35, l = 160)) %>%
        config(displayModeBar = FALSE)
    })
    
    # 5B. Resume vs Projects Plot
    output$plot_resume_projects <- renderPlotly({
      df <- data_r() %>%
        filter(grepl("Fully updated|4|5", as.character(self_reported_resume_readiness))) %>%
        count(projects_completed_range, name = "count") %>%
        mutate(pct = round(count / sum(count) * 100, 1))
      
      p <- ggplot(df, aes(x = projects_completed_range, y = count, fill = projects_completed_range)) +
        geom_col(show.legend = FALSE, width = 0.6) +
        geom_text(aes(label = paste0(count, " (", pct, "%)")), vjust = -0.4, fontface = "bold", size = 3.8) +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(limits = c(0, max(df$count) * 1.3)) +
        labs(x = "Projects Completed", y = "Students Claiming 'Ready' Resume") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 20, r = 10, b = 35, l = 40)) %>%
        config(displayModeBar = FALSE)
    })
    
    # 6. Branch Matrix Table
    output$table_branch_matrix <- renderDT({
      df <- data_r()
      
      branch_summary <- df %>%
        group_by(Branch = branch) %>%
        summarise(
          `Students` = n(),
          `Avg DSA` = round(mean(dsa_rating, na.rm = TRUE), 2),
          `Avg OOP` = round(mean(oop_rating, na.rm = TRUE), 2),
          `Avg DBMS` = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
          `Avg Aptitude` = round(mean(aptitude_logical_reasoning_rating, na.rm = TRUE), 2),
          `Avg Comm` = round(mean(communication_interview_rating, na.rm = TRUE), 2),
          `% Doing Internship` = paste0(round(mean(grepl("Yes|Currently|Completed", as.character(technical_internship_status)), na.rm = TRUE) * 100, 1), "%"),
          .groups = "drop"
        ) %>%
        arrange(desc(Students))
      
      datatable(
        branch_summary,
        options = list(dom = "t", pageLength = 10),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })
    
  })
}
