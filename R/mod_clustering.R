# ==============================================================================
# MODULE: MACHINE LEARNING - K-MEANS CLUSTERING & PCA SEGMENTATION
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)

mod_clustering_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "mb-3",
      h4(class = "fw-bold text-dark mb-1", "Cohort Segmentation: K-Means Clustering & PCA Analysis"),
      p(class = "text-muted mb-0", "Unsupervised machine learning model identifying natural student competency clusters using dimensional reduction.")
    ),
    
    div(
      class = "insight-callout primary mb-3",
      h5(class = "fw-bold mb-1", icon("diagram-project"), " K-Means Segmentation (K = 2 Clusters):"),
      p(
        class = "mb-0",
        "Following Elbow Method optimization on the cohort's technical and academic dimensions, students cleanly bifurcate into two distinct profiles: ",
        strong("Cluster 1: Foundational Learners (52% of cohort)"), " and ",
        strong("Cluster 2: High-Capability Technologists (48% of cohort)"),
        ". Cluster 2 exhibits systematically higher DSA (3.25 vs 2.40), DBMS (3.31 vs 2.40), Systems (3.40 vs 2.37), and prep hours (6.82 vs 5.41 hrs/wk)."
      )
    ),
    
    fluidRow(
      column(
        width = 7,
        div(
          class = "card h-100",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            span("Principal Component Analysis (PCA) 2D Projection"),
            span(class = "badge bg-primary", "Dim 1 + Dim 2 Projection")
          ),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_pca_clusters"), height = "400px")
          )
        )
      ),
      column(
        width = 5,
        div(
          class = "card h-100",
          div(class = "card-header", "Cluster Profiles Summary"),
          div(
            class = "card-body",
            p(class = "text-muted small", "Statistical means across core dimensions for each identified cluster segment."),
            DTOutput(ns("table_cluster_profiles"))
          )
        )
      )
    ),
    
    fluidRow(
      class = "mt-4",
      column(
        width = 6,
        div(
          class = "card h-100",
          div(class = "card-header", "Competency Rating Comparison by Cluster"),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_cluster_comparison"), height = "360px")
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card h-100",
          div(class = "card-header", "Elbow Curve & Variance Optimization Context"),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_elbow_curve"), height = "360px")
          )
        )
      )
    )
  )
}

mod_clustering_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Precompute PCA
    pca_data <- reactive({
      df <- data_r()
      
      skill_cols <- c(
        "dsa_rating", "dbms_sql_rating", "operating_systems_rating",
        "computer_networks_rating", "oop_rating", "web_development_rating",
        "data_science_ml_rating", "aptitude_logical_reasoning_rating"
      )
      
      valid_cols <- intersect(skill_cols, names(df))
      mat <- df[, valid_cols]
      
      # Impute median if any NA
      mat <- apply(mat, 2, function(x) {
        ifelse(is.na(x), median(x, na.rm = TRUE), x)
      })
      
      pca_res <- prcomp(mat, scale. = TRUE)
      
      var_exp <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 1)
      
      pca_df <- data.frame(
        PC1 = pca_res$x[, 1],
        PC2 = pca_res$x[, 2],
        ID = df$synthetic_record_id,
        Year = df$academic_year,
        Branch = df$branch,
        CGPA = df$cgpa_range,
        DSA = df$dsa_rating,
        Outcome = df$placement_category,
        Cluster = df$cluster_label
      )
      
      list(df = pca_df, var_exp = var_exp)
    })
    
    # 1. PCA 2D Scatter Plot
    output$plot_pca_clusters <- renderPlotly({
      res <- pca_data()
      df <- res$df
      var_exp <- res$var_exp
      
      p <- ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
        geom_point(aes(shape = Outcome), size = 3.5, alpha = 0.85) +
        stat_ellipse(geom = "polygon", aes(fill = Cluster), alpha = 0.1, show.legend = FALSE) +
        scale_color_manual(values = c(
          "Cluster 1: Foundational Learners" = "#2563eb",
          "Cluster 2: High-Capability Technologists" = "#059669"
        )) +
        scale_fill_manual(values = c(
          "Cluster 1: Foundational Learners" = "#2563eb",
          "Cluster 2: High-Capability Technologists" = "#059669"
        )) +
        labs(
          x = paste0("Principal Component 1 (", var_exp[1], "% Variance)"),
          y = paste0("Principal Component 2 (", var_exp[2], "% Variance)"),
          color = "Cluster",
          shape = "Outcome"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank()
        )
      
      ggplotly(p, tooltip = c("x", "y", "color", "shape")) %>%
        layout(
          legend = list(orientation = "h", x = 0.1, y = 1.15),
          margin = list(t = 30, r = 10, b = 40, l = 50)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 2. Cluster Profiles Table
    output$table_cluster_profiles <- renderDT({
      profiles <- load_cluster_profiles()
      
      if (is.null(profiles)) {
        # Fallback to computing dynamically
        df <- data_r()
        profiles <- df %>%
          group_by(cluster) %>%
          summarise(
            student_count = n(),
            avg_cgpa = round(mean(cgpa_numeric_approx, na.rm = TRUE), 2),
            avg_prep_hours = round(mean(weekly_prep_hours_numeric_approx, na.rm = TRUE), 1),
            avg_dsa = round(mean(dsa_rating, na.rm = TRUE), 2),
            avg_dbms = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
            avg_os = round(mean(operating_systems_rating, na.rm = TRUE), 2),
            avg_oop = round(mean(oop_rating, na.rm = TRUE), 2),
            avg_aptitude = round(mean(aptitude_logical_reasoning_rating, na.rm = TRUE), 2),
            avg_communication = round(mean(communication_interview_rating, na.rm = TRUE), 2),
            .groups = "drop"
          )
      }
      
      display_prof <- profiles %>%
        mutate(
          Cluster = ifelse(cluster == 1, "Cluster 1 (Foundational)", "Cluster 2 (High-Capability)"),
          `Count` = student_count,
          `DSA` = round(avg_dsa, 2),
          `DBMS` = round(avg_dbms, 2),
          `OS` = round(avg_os, 2),
          `OOP` = round(avg_oop, 2),
          `Prep (Hrs/Wk)` = round(avg_prep_hours, 1)
        ) %>%
        select(Cluster, Count, DSA, DBMS, OS, OOP, `Prep (Hrs/Wk)`)
      
      datatable(
        display_prof,
        options = list(dom = "t"),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })
    
    # 3. Competency Comparison by Cluster Bar Plot
    output$plot_cluster_comparison <- renderPlotly({
      df <- data_r()
      
      cl_comp <- df %>%
        group_by(cluster_label) %>%
        summarise(
          DSA = round(mean(dsa_rating, na.rm = TRUE), 2),
          OOP = round(mean(oop_rating, na.rm = TRUE), 2),
          DBMS = round(mean(dbms_sql_rating, na.rm = TRUE), 2),
          OS = round(mean(operating_systems_rating, na.rm = TRUE), 2),
          Networks = round(mean(computer_networks_rating, na.rm = TRUE), 2),
          Web = round(mean(web_development_rating, na.rm = TRUE), 2),
          `AI / ML` = round(mean(data_science_ml_rating, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = -cluster_label, names_to = "Skill", values_to = "Rating")
      
      p <- ggplot(cl_comp, aes(x = Skill, y = Rating, fill = cluster_label)) +
        geom_col(position = position_dodge(width = 0.65), width = 0.55) +
        scale_fill_manual(
          values = c(
            "Cluster 1: Foundational Learners" = "#2563eb",
            "Cluster 2: High-Capability Technologists" = "#059669"
          ),
          name = NULL
        ) +
        scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
        labs(x = NULL, y = "Mean Competency Rating (1–5)") +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "top",
          panel.grid.minor = element_blank()
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          legend = list(orientation = "h", x = 0.1, y = 1.15),
          margin = list(t = 25, r = 10, b = 35, l = 40)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    # 4. Elbow Curve Plot
    output$plot_elbow_curve <- renderPlotly({
      # Precomputed WCSS values from the project
      elbow_df <- data.frame(
        Clusters = 1:6,
        WCSS = c(642, 428, 362, 315, 278, 245)
      )
      
      p <- ggplot(elbow_df, aes(x = Clusters, y = WCSS)) +
        geom_line(color = "#0f172a", linewidth = 1.5) +
        geom_point(color = "#2563eb", size = 4.5) +
        geom_vline(xintercept = 2, linetype = "dashed", color = "#e11d48", linewidth = 1) +
        annotate("text", x = 2.4, y = 500, label = "Optimal Elbow at K = 2", color = "#e11d48", fontface = "bold") +
        scale_x_continuous(breaks = 1:6) +
        labs(
          x = "Number of Clusters (K)",
          y = "Within-Cluster Sum of Squares (WCSS)"
        ) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(margin = list(t = 20, r = 10, b = 35, l = 50)) %>%
        config(displayModeBar = FALSE)
    })
    
  })
}
