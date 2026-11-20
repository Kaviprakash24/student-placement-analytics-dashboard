# ==============================================================================
# MODULE: DATA EXPLORATION & AUDIT
# ==============================================================================

library(shiny)
library(DT)
library(dplyr)
library(tibble)

mod_exploration_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header note
    div(
      class = "d-flex justify-content-between align-items-center mb-3",
      div(
        h4(class = "fw-bold text-dark mb-1", "Cohort Dataset Explorer & Data Quality Audit"),
        p(class = "text-muted mb-0", "Filter, inspect, export, and review the statistical distributions of the student survey cohort.")
      ),
      div(
        downloadButton(ns("download_data"), "Export Filtered CSV", class = "btn-outline-primary btn-sm")
      )
    ),
    
    # Filter Bar
    div(
      class = "card p-3 mb-3 bg-white",
      div(
        class = "row g-2",
        div(
          class = "col-md-3",
          selectInput(
            ns("filter_year"),
            "Academic Year:",
            choices = c("All Years", "1st Year", "2nd Year", "3rd Year", "4th Year"),
            selected = "All Years"
          )
        ),
        div(
          class = "col-md-3",
          selectInput(
            ns("filter_branch"),
            "Branch:",
            choices = c("All Branches"),
            selected = "All Branches"
          )
        ),
        div(
          class = "col-md-3",
          selectInput(
            ns("filter_status"),
            "Placement Status:",
            choices = c("All Outcomes", "Placed", "Unplaced"),
            selected = "All Outcomes"
          )
        ),
        div(
          class = "col-md-3",
          selectInput(
            ns("filter_projects"),
            "Technical Projects:",
            choices = c("All Ranges", "0", "1 to 2", "3 to 4", "5 or more"),
            selected = "All Ranges"
          )
        )
      )
    ),
    
    # Exploration Tabs
    navset_card_tab(
      title = "Exploration Views",
      # Tab 1: Interactive Data Table
      nav_panel(
        "Interactive Records Table",
        div(
          class = "p-2",
          p(class = "text-muted small", "Showing real-time filtered cohort records. Use column filters or global search to inspect individual student profiles."),
          DTOutput(ns("table_records"))
        )
      ),
      
      # Tab 2: Variable Dictionary & Data Types
      nav_panel(
        "Data Dictionary & Types",
        div(
          class = "p-2",
          p(class = "text-muted small", "Formal schema definition, scale descriptions, and variable categorizations used across the analytical pipeline."),
          DTOutput(ns("table_metadata"))
        )
      ),
      
      # Tab 3: Data Quality & Missingness Audit
      nav_panel(
        "Data Quality & Completeness",
        div(
          class = "p-3",
          fluidRow(
            column(
              width = 4,
              div(
                class = "card p-3 border-0 bg-light text-center",
                h6(class = "text-muted fw-bold", "Total Records"),
                h3(class = "fw-bold text-primary", textOutput(ns("audit_total_rows"))),
                p(class = "small text-muted mb-0", "Anonymous student responses")
              )
            ),
            column(
              width = 4,
              div(
                class = "card p-3 border-0 bg-light text-center",
                h6(class = "text-muted fw-bold", "Exact Duplicate Rows"),
                h3(class = "fw-bold text-success", textOutput(ns("audit_duplicates"))),
                p(class = "small text-muted mb-0", "Zero duplicate entries found")
              )
            ),
            column(
              width = 4,
              div(
                class = "card p-3 border-0 bg-light text-center",
                h6(class = "text-muted fw-bold", "Total Missing Values"),
                h3(class = "fw-bold text-info", textOutput(ns("audit_missing_total"))),
                p(class = "small text-muted mb-0", "High data completeness")
              )
            )
          ),
          h5(class = "fw-bold text-dark mt-4 mb-2", "Missing Values by Column Audit"),
          DTOutput(ns("table_missing"))
        )
      ),
      
      # Tab 4: Summary Statistics
      nav_panel(
        "Parametric & Ordinal Summary",
        div(
          class = "p-2",
          p(class = "text-muted small", "Five-number summary, mean, and standard deviation across all 1–5 competency rating domains."),
          DTOutput(ns("table_summary_stats"))
        )
      )
    )
  )
}

mod_exploration_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    
    # Populate branch choices dynamically
    observe({
      df <- data_r()
      branches <- c("All Branches", sort(as.character(unique(df$branch))))
      updateSelectInput(session, "filter_branch", choices = branches, selected = "All Branches")
    })
    
    # Reactive filtered dataset
    filtered_df <- reactive({
      df <- data_r()
      
      if (input$filter_year != "All Years") {
        df <- df %>% filter(academic_year == input$filter_year)
      }
      if (input$filter_branch != "All Branches") {
        df <- df %>% filter(branch == input$filter_branch)
      }
      if (input$filter_status != "All Outcomes") {
        df <- df %>% filter(placement_category == input$filter_status)
      }
      if (input$filter_projects != "All Ranges") {
        df <- df %>% filter(grepl(input$filter_projects, as.character(projects_completed_range)))
      }
      
      df
    })
    
    # Download handler for filtered CSV
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("student_placement_filtered_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(filtered_df(), file, row.names = FALSE)
      }
    )
    
    # 1. Main Records DT Table
    output$table_records <- renderDT({
      df <- filtered_df() %>%
        select(
          ID = synthetic_record_id,
          Year = academic_year,
          Branch = branch,
          CGPA = cgpa_range,
          DSA = dsa_rating,
          OOP = oop_rating,
          DBMS = dbms_sql_rating,
          OS = operating_systems_rating,
          CN = computer_networks_rating,
          Comm = communication_interview_rating,
          Aptitude = aptitude_logical_reasoning_rating,
          Projects = projects_completed_range,
          Outcome = placement_status,
          Offer = highest_full_time_offer_compensation,
          Cluster = cluster_label
        )
      
      datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          autoWidth = FALSE
        ),
        class = "compact stripe hover",
        rownames = FALSE
      ) %>%
        formatStyle(
          "Outcome",
          target = "row",
          backgroundColor = styleEqual(
            c("Received a full-time job offer"),
            c("#ecfdf5")
          )
        )
    })
    
    # 2. Metadata Dictionary DT
    output$table_metadata <- renderDT({
      meta_df <- get_column_metadata()
      datatable(
        meta_df,
        options = list(pageLength = 15, dom = "ftp"),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })
    
    # 3. Quality Metrics
    output$audit_total_rows <- renderText({
      nrow(filtered_df())
    })
    
    output$audit_duplicates <- renderText({
      sum(duplicated(filtered_df()))
    })
    
    output$audit_missing_total <- renderText({
      sum(is.na(filtered_df()))
    })
    
    output$table_missing <- renderDT({
      df <- filtered_df()
      missing_df <- tibble(
        Column = names(df),
        Missing_Count = sapply(df, function(x) sum(is.na(x))),
        Missing_Percentage = round(sapply(df, function(x) sum(is.na(x))) / nrow(df) * 100, 2),
        Distinct_Values = sapply(df, n_distinct)
      ) %>%
        filter(Missing_Count > 0 | Column %in% c("dsa_rating", "academic_year", "placement_status")) %>%
        arrange(desc(Missing_Count))
      
      datatable(
        missing_df,
        options = list(pageLength = 8, dom = "tp"),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })
    
    # 4. Parametric Summary Stats
    output$table_summary_stats <- renderDT({
      df <- filtered_df()
      
      skill_cols <- c(
        "dsa_rating", "oop_rating", "dbms_sql_rating", "operating_systems_rating",
        "computer_networks_rating", "web_development_rating", "data_science_ml_rating",
        "aptitude_logical_reasoning_rating", "communication_interview_rating"
      )
      
      skill_names <- c(
        "DSA", "OOP", "DBMS & SQL", "Operating Systems",
        "Computer Networks", "Web Development", "Data Science & ML",
        "Aptitude & Logic", "Communication & Interview"
      )
      
      stats_list <- lapply(seq_along(skill_cols), function(i) {
        v <- df[[skill_cols[i]]]
        v <- v[!is.na(v)]
        if (length(v) == 0) return(NULL)
        
        tibble(
          Skill = skill_names[i],
          Mean = round(mean(v), 2),
          SD = round(sd(v), 2),
          Median = median(v),
          Q1 = quantile(v, 0.25),
          Q3 = quantile(v, 0.75),
          Min = min(v),
          Max = max(v)
        )
      })
      
      stats_df <- bind_rows(stats_list) %>%
        arrange(desc(Mean))
      
      datatable(
        stats_df,
        options = list(pageLength = 10, dom = "tp"),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })
    
  })
}
