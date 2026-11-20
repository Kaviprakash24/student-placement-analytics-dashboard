# ==============================================================================
# MODULE: DATA VISUALIZATION SUITE
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)

mod_visualizations_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "mb-3",
      h4(class = "fw-bold text-dark mb-1", "Empirical Data Visualizations & Statistical Graphs"),
      p(class = "text-muted mb-0", "Interactive charts derived from the cohort survey analysis, comparing competencies, ROI, and compensation drivers.")
    ),
    
    navset_card_pill(
      id = ns("viz_tabs"),
      
      # -------------------------------------------------------------
      # TAB 1: Core Competency Baseline
      # -------------------------------------------------------------
      nav_panel(
        "1. Competency Baseline",
        div(
          class = "p-3",
          div(
            class = "insight-callout primary mb-3",
            strong("Cohort Baseline Insight: "),
            "Soft skills currently lead the cohort averages, with Communication scoring highest (3.17 / 5.0) followed by Aptitude (3.15). Technical domains show a slight lag across the whole cohort, particularly Web Development which drops to the lowest average rating (2.74)."
          ),
          plotlyOutput(ns("plot_core_skills"), height = "450px")
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 2: The Placement Gap (True Differentiator)
      # -------------------------------------------------------------
      nav_panel(
        "2. The Placement Gap",
        div(
          class = "p-3",
          div(
            class = "insight-callout success mb-3",
            strong("The True Differentiator: "),
            "Placed students dominate in Data Structures & Algorithms (3.94 vs 2.57, +1.37 gap) and OOP (4.00 vs 2.63, +1.37 gap). In sharp contrast, Communication scores remain almost identical (3.35 vs 3.13, difference of only 0.22), proving technical skills are the true differentiator in recruitment."
          ),
          plotlyOutput(ns("plot_placement_gap"), height = "450px")
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 3: Projects & Practice ROI
      # -------------------------------------------------------------
      nav_panel(
        "3. Projects & Coding ROI",
        div(
          class = "p-3",
          div(
            class = "insight-callout primary mb-3",
            strong("Building Drives Algorithmic Logic: "),
            "There is a clear positive relationship between completed technical projects and DSA proficiency. Students with zero projects exhibit the lowest median DSA ratings (1–2), while students with 5+ projects reach median ratings of 4.0."
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "DSA Proficiency by Completed Projects"),
                plotlyOutput(ns("plot_projects_roi"), height = "360px")
              )
            ),
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "DSA Proficiency by Coding Problems Solved"),
                plotlyOutput(ns("plot_coding_roi"), height = "360px")
              )
            )
          ),
          fluidRow(
            class = "mt-3",
            column(
              width = 12,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Weekly Preparation Hours vs. DSA Rating (Spearman r = 0.52)"),
                plotlyOutput(ns("plot_prephours_scatter"), height = "340px")
              )
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 4: The Salary Staircase
      # -------------------------------------------------------------
      nav_panel(
        "4. The Salary Staircase",
        div(
          class = "p-3",
          div(
            class = "insight-callout warning mb-3",
            strong("Monotonic Compensation Driver: "),
            "Breaking the 12+ LPA salary ceiling demands near-perfect DSA scores (4.33+ average). Students securing 8–12 LPA average 4.00 in DSA and 4.10 in Core Systems (OS & DBMS), while 5–8 LPA offers require a baseline of 3.50."
          ),
          fluidRow(
            column(
              width = 7,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "DSA Rating Progression Across Offer Ranges"),
                plotlyOutput(ns("plot_salary_staircase"), height = "380px")
              )
            ),
            column(
              width = 5,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Multi-Skill Comparison Across Offer Tiers"),
                plotlyOutput(ns("plot_salary_multiskill"), height = "380px")
              )
            )
          )
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 5: Placement Bottlenecks
      # -------------------------------------------------------------
      nav_panel(
        "5. Primary Bottlenecks",
        div(
          class = "p-3",
          div(
            class = "insight-callout danger mb-3",
            strong("Where Students Struggle Most: "),
            "Lack of practical project/internship experience is the single largest self-reported roadblock (18 students), followed closely by Aptitude test hurdles (16 students), and lack of time or proper guidance (13 students)."
          ),
          plotlyOutput(ns("plot_bottlenecks"), height = "450px")
        )
      ),
      
      # -------------------------------------------------------------
      # TAB 6: Company Priorities vs Student Focus
      # -------------------------------------------------------------
      nav_panel(
        "6. Industry vs. Student Focus",
        div(
          class = "p-3",
          div(
            class = "insight-callout primary mb-3",
            strong("Perception Alignment: "),
            "Analysis of multi-select responses comparing what skills students believe companies prioritize vs. what skills students are actively spending time improving."
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Skills Believed Prioritized by Companies"),
                plotlyOutput(ns("plot_company_priorities"), height = "380px")
              )
            ),
            column(
              width = 6,
              div(
                class = "card p-3 border-0 bg-light",
                h6(class = "fw-bold text-dark", "Skills Students Are Currently Improving"),
                plotlyOutput(ns("plot_skills_improving"), height = "380px")
              )
            )
          )
        )
      )
    )
  )
}

mod_visualizations_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Core Skills Baseline (Horizontal Bar)
    output$plot_core_skills <- renderPlotly({
      df <- data_r()
      
      skill_df <- df %>%
        summarise(
          Communication = mean(communication_interview_rating, na.rm = TRUE),
          Aptitude = mean(aptitude_logical_reasoning_rating, na.rm = TRUE),
          `Operating Systems` = mean(operating_systems_rating, na.rm = TRUE),
          `OOP (Java/C++)` = mean(oop_rating, na.rm = TRUE),
          `Data Science / ML` = mean(data_science_ml_rating, na.rm = TRUE),
          `DBMS / SQL` = mean(dbms_sql_rating, na.rm = TRUE),
          `Computer Networks` = mean(computer_networks_rating, na.rm = TRUE),
          `Data Structures & Algo` = mean(dsa_rating, na.rm = TRUE),
          `Web Development` = mean(web_development_rating, na.rm = TRUE)
        ) %>%
        pivot_longer(cols = everything(), names_to = "Skill", values_to = "Rating") %>%
        mutate(Rating = round(Rating, 2)) %>%
        arrange(Rating)
      
      # Grouping into categories for color
      skill_df <- skill_df %>%
        mutate(
          Domain = case_when(
            Skill %in% c("Communication", "Aptitude") ~ "Soft / Cognitive",
            Skill %in% c("Web Development", "Data Science / ML") ~ "Applied Tech",
            TRUE ~ "Core Computer Science"
          )
        )
      
      p <- ggplot(skill_df, aes(x = reorder(Skill, Rating), y = Rating, fill = Domain)) +
        geom_col(width = 0.65, alpha = 0.9) +
        geom_text(aes(label = sprintf("%.2f", Rating)), hjust = -0.15, fontface = "bold", size = 4) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
        scale_fill_manual(values = c(
          "Soft / Cognitive" = "#0284c7",
          "Core Computer Science" = "#0f172a",
          "Applied Tech" = "#059669"
        )) +
        labs(
          title = NULL,
          x = NULL,
          y = "Average Self-Reported Rating (1–5 Scale)",
          fill = "Skill Domain"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 20, 10, 10)
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          legend = list(orientation = "h", x = 0.2, y = 1.1),
          margin = list(t = 30, r = 30, b = 40, l = 160)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 2. The Placement Gap (Grouped Bar)
    output$plot_placement_gap <- renderPlotly({
      df <- data_r()
      
      gap_df <- df %>%
        group_by(placement_category) %>%
        summarise(
          `DSA` = round(mean(dsa_rating, na.rm = TRUE), 2),
          `OOP` = round(mean(oop_rating, na.rm = TRUE), 2),
          `DBMS / SQL` = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
          `Operating Systems` = round(mean(operating_systems_rating, na.rm = TRUE), 2),
          `Computer Networks` = round(mean(computer_networks_rating, na.rm = TRUE), 2),
          `Communication` = round(mean(communication_interview_rating, na.rm = TRUE), 2),
          `Aptitude` = round(mean(aptitude_logical_reasoning_rating, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = -placement_category, names_to = "Skill", values_to = "Score")
      
      p <- ggplot(gap_df, aes(x = Skill, y = Score, fill = placement_category)) +
        geom_col(position = position_dodge(width = 0.65), width = 0.55) +
        geom_text(
          aes(label = sprintf("%.2f", Score)),
          position = position_dodge(width = 0.65),
          vjust = -0.4,
          fontface = "bold",
          size = 3.8
        ) +
        scale_fill_manual(values = c("Placed" = "#059669", "Unplaced" = "#ef4444"), name = "Status") +
        scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
        labs(
          x = NULL,
          y = "Average Rating (1–5 Scale)",
          title = NULL
        ) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          legend = list(orientation = "h", x = 0.4, y = 1.1),
          margin = list(t = 30, r = 10, b = 40, l = 50)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 3A. Projects ROI Boxplot
    output$plot_projects_roi <- renderPlotly({
      df <- data_r() %>%
        filter(!is.na(projects_completed_range), !is.na(dsa_rating))
      
      p <- ggplot(df, aes(x = projects_completed_range, y = dsa_rating, fill = projects_completed_range)) +
        geom_boxplot(alpha = 0.7, outlier.colour = "#ef4444", show.legend = FALSE) +
        geom_jitter(width = 0.15, alpha = 0.4, size = 1.8, show.legend = FALSE) +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
        labs(x = "Technical Projects Completed", y = "DSA Rating (1–5)") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = FALSE)
    })
    
    # 3B. Coding Problems ROI Boxplot
    output$plot_coding_roi <- renderPlotly({
      df <- data_r() %>%
        filter(!is.na(coding_problems_solved_range), !is.na(dsa_rating))
      
      p <- ggplot(df, aes(x = coding_problems_solved_range, y = dsa_rating, fill = coding_problems_solved_range)) +
        geom_boxplot(alpha = 0.7, show.legend = FALSE) +
        geom_jitter(width = 0.15, alpha = 0.4, size = 1.8, show.legend = FALSE) +
        scale_fill_brewer(palette = "Oranges") +
        scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
        labs(x = "Coding Problems Solved", y = "DSA Rating (1–5)") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 1)
        )
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = FALSE)
    })
    
    # 3C. Prep Hours vs DSA Scatter
    output$plot_prephours_scatter <- renderPlotly({
      df <- data_r()
      
      # Handle numeric approx if column exists, else map factors
      if (!"weekly_prep_hours_numeric_approx" %in% names(df)) {
        df$weekly_prep_hours_numeric_approx <- case_when(
          grepl("1 to 3", df$weekly_placement_prep_hours) ~ 2,
          grepl("4 to 6", df$weekly_placement_prep_hours) ~ 5,
          grepl("7 to 10", df$weekly_placement_prep_hours) ~ 8.5,
          grepl("More than 10", df$weekly_placement_prep_hours) ~ 12,
          TRUE ~ 5
        )
      }
      
      p <- ggplot(df, aes(x = weekly_prep_hours_numeric_approx, y = dsa_rating, color = placement_category)) +
        geom_jitter(width = 0.3, height = 0.15, size = 2.5, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "#2563eb", fill = "#bfdbfe") +
        scale_color_manual(values = c("Placed" = "#059669", "Unplaced" = "#64748b"), name = "Status") +
        scale_y_continuous(limits = c(0.8, 5.2), breaks = 1:5) +
        labs(
          x = "Approximate Weekly Preparation Hours",
          y = "DSA Proficiency Rating",
          title = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank()
        )
      
      ggplotly(p, tooltip = c("x", "y", "color")) %>%
        layout(
          legend = list(orientation = "h", x = 0.4, y = 1.1),
          margin = list(t = 25, r = 10, b = 40, l = 50)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 4A. Salary Staircase
    output$plot_salary_staircase <- renderPlotly({
      df <- data_r()
      
      tiers <- df %>%
        filter(
          !highest_full_time_offer_compensation %in% c("Not applicable / No offer yet", "No full-time offer", "Prefer not to say"),
          !is.na(highest_full_time_offer_compensation)
        ) %>%
        mutate(
          offer_tier = factor(
            highest_full_time_offer_compensation,
            levels = c("Below 5 LPA", "5 to 8 LPA", "8 to 12 LPA", "Above 12 LPA")
          )
        ) %>%
        group_by(offer_tier) %>%
        summarise(
          Avg_DSA = round(mean(dsa_rating, na.rm = TRUE), 2),
          Students = n(),
          .groups = "drop"
        )
      
      if (nrow(tiers) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
      
      p <- ggplot(tiers, aes(x = offer_tier, y = Avg_DSA, group = 1)) +
        geom_line(color = "#2563eb", linewidth = 1.8) +
        geom_point(color = "#e11d48", size = 5) +
        geom_text(aes(label = sprintf("%.2f", Avg_DSA)), vjust = -1.2, fontface = "bold", size = 4.2) +
        scale_y_continuous(limits = c(2.5, 5), breaks = seq(2.5, 5, 0.5)) +
        labs(
          x = "Full-Time Offer Compensation Bracket",
          y = "Average DSA Rating (1–5)",
          title = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 25, r = 10, b = 40, l = 50)) %>%
        config(displayModeBar = FALSE)
    })
    
    # 4B. Multi-Skill Compensation Tiers
    output$plot_salary_multiskill <- renderPlotly({
      df <- data_r()
      
      tiers_multi <- df %>%
        filter(
          !highest_full_time_offer_compensation %in% c("Not applicable / No offer yet", "No full-time offer", "Prefer not to say"),
          !is.na(highest_full_time_offer_compensation)
        ) %>%
        mutate(
          offer_tier = factor(
            highest_full_time_offer_compensation,
            levels = c("5 to 8 LPA", "8 to 12 LPA", "Above 12 LPA")
          )
        ) %>%
        group_by(offer_tier) %>%
        summarise(
          DSA = round(mean(dsa_rating, na.rm = TRUE), 2),
          OOP = round(mean(oop_rating, na.rm = TRUE), 2),
          DBMS = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
          OS = round(mean(operating_systems_rating, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = -offer_tier, names_to = "Skill", values_to = "Rating")
      
      if (nrow(tiers_multi) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
      
      p <- ggplot(tiers_multi, aes(x = offer_tier, y = Rating, fill = Skill)) +
        geom_col(position = "dodge", width = 0.6) +
        scale_fill_brewer(palette = "Blues") +
        scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
        labs(x = "Compensation Bracket", y = "Average Rating", fill = "Skill") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top", panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          legend = list(orientation = "h", x = 0.1, y = 1.15),
          margin = list(t = 30, r = 10, b = 40, l = 40)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 5. Placement Bottlenecks Pareto
    output$plot_bottlenecks <- renderPlotly({
      df <- data_r()
      
      bottlenecks <- df %>%
        count(biggest_placement_preparation_challenge, sort = TRUE, name = "count") %>%
        filter(!is.na(biggest_placement_preparation_challenge), biggest_placement_preparation_challenge != "") %>%
        mutate(
          pct = round(count / sum(count) * 100, 1),
          label = paste0(count, " (", pct, "%)")
        )
      
      p <- ggplot(bottlenecks, aes(x = reorder(biggest_placement_preparation_challenge, count), y = count)) +
        geom_col(fill = "#d97706", width = 0.65, alpha = 0.9) +
        geom_text(aes(label = label), hjust = -0.1, fontface = "bold", size = 3.8) +
        coord_flip() +
        scale_y_continuous(limits = c(0, max(bottlenecks$count) * 1.3)) +
        labs(
          x = NULL,
          y = "Number of Students Reporting Challenge",
          title = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 20, 10, 10)
        )
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 20, r = 30, b = 40, l = 200)) %>%
        config(displayModeBar = FALSE)
    })
    
    # 6A. Company Priorities
    output$plot_company_priorities <- renderPlotly({
      df <- data_r()
      
      if (!"skills_believed_prioritized_by_companies" %in% names(df)) return(plotly_empty(type = "scatter", mode = "markers"))
      
      comp_skills <- df %>%
        select(skills_believed_prioritized_by_companies) %>%
        separate_rows(skills_believed_prioritized_by_companies, sep = ";|,") %>%
        mutate(skill = str_trim(skills_believed_prioritized_by_companies)) %>%
        filter(!is.na(skill), skill != "") %>%
        count(skill, sort = TRUE, name = "count") %>%
        head(8)
      
      p <- ggplot(comp_skills, aes(x = reorder(skill, count), y = count)) +
        geom_col(fill = "#1e3a8a", width = 0.6) +
        geom_text(aes(label = count), hjust = -0.2, fontface = "bold", size = 3.6) +
        coord_flip() +
        scale_y_continuous(limits = c(0, max(comp_skills$count) * 1.25)) +
        labs(x = NULL, y = "Selections by Cohort") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 15, r = 20, b = 35, l = 160)) %>%
        config(displayModeBar = FALSE)
    })
    
    # 6B. Skills Currently Improving
    output$plot_skills_improving <- renderPlotly({
      df <- data_r()
      
      if (!"skills_currently_improving" %in% names(df)) return(plotly_empty(type = "scatter", mode = "markers"))
      
      improving_skills <- df %>%
        select(skills_currently_improving) %>%
        separate_rows(skills_currently_improving, sep = ";|,") %>%
        mutate(skill = str_trim(skills_currently_improving)) %>%
        filter(!is.na(skill), skill != "") %>%
        count(skill, sort = TRUE, name = "count") %>%
        head(8)
      
      p <- ggplot(improving_skills, aes(x = reorder(skill, count), y = count)) +
        geom_col(fill = "#059669", width = 0.6) +
        geom_text(aes(label = count), hjust = -0.2, fontface = "bold", size = 3.6) +
        coord_flip() +
        scale_y_continuous(limits = c(0, max(improving_skills$count) * 1.25)) +
        labs(x = NULL, y = "Selections by Cohort") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 15, r = 20, b = 35, l = 160)) %>%
        config(displayModeBar = FALSE)
    })
    
  })
}
