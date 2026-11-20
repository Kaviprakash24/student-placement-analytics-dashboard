# ==============================================================================
# MODULE: DATA MINING - ASSOCIATION RULE MINING (APRIORI)
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)

mod_association_rules_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "mb-3",
      h4(class = "fw-bold text-dark mb-1", "Association Rule Mining (Apriori Algorithm)"),
      p(class = "text-muted mb-0", "Uncovering transactional patterns and itemset correlations linking academic status, coding practice, and placement eligibility.")
    ),
    
    div(
      class = "insight-callout primary mb-3",
      h5(class = "fw-bold mb-1", icon("link"), " Frequent Itemset Mining & Rule Metrics:"),
      p(
        class = "mb-0",
        "Mined using the ", code("arules"), " library with parameters: ",
        code("support >= 0.05, confidence >= 0.50, minlen = 2, maxlen = 3"),
        ". Generating 2,938 total rules with lifts up to 3.68. Rules uncover critical milestone sequences between academic seniority, internship acquisitions, and coding problem milestones."
      )
    ),
    
    # Filter Controls Card
    div(
      class = "card p-3 mb-3 bg-white",
      fluidRow(
        column(
          width = 4,
          sliderInput(
            ns("min_lift"),
            "Minimum Lift Threshold:",
            min = 1.0,
            max = 3.6,
            value = 2.0,
            step = 0.1
          )
        ),
        column(
          width = 4,
          sliderInput(
            ns("min_confidence"),
            "Minimum Confidence Threshold:",
            min = 0.5,
            max = 1.0,
            value = 0.6,
            step = 0.05
          )
        ),
        column(
          width = 4,
          sliderInput(
            ns("min_support"),
            "Minimum Support Threshold:",
            min = 0.05,
            max = 0.25,
            value = 0.05,
            step = 0.01
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        div(
          class = "card h-100",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            span("Support vs. Confidence Scatter (Colored by Lift)"),
            span(class = "badge bg-info text-dark", textOutput(ns("rule_count_badge"), inline = TRUE))
          ),
          div(
            class = "card-body",
            plotlyOutput(ns("plot_rules_scatter"), height = "400px")
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card h-100",
          div(class = "card-header", "Top Discovered Behavioral Patterns"),
          div(
            class = "card-body",
            div(
              class = "list-group list-group-flush small",
              div(
                class = "list-group-item px-0",
                strong(icon("arrow-trend-up", class = "text-success"), " Rule #1 (Lift = 3.68): "),
                br(),
                code("{Placement Status = Not Eligible, Resume = Draft Ready}"), " => ", code("{Academic Year = 2nd Year}"),
                p(class = "text-muted mb-0 mt-1", "Sophomore students actively construct resumes prior to formal placement eligibility, representing an ideal intervention window.")
              ),
              div(
                class = "list-group-item px-0",
                strong(icon("arrow-trend-up", class = "text-success"), " Rule #2 (Lift = 3.62): "),
                br(),
                code("{Academic Year = 4th Year, GitHub = Rarely}"), " => ", code("{Internship Status = Yes}"),
                p(class = "text-muted mb-0 mt-1", "Seniors with corporate internships often maintain internal proprietary codebases rather than public personal repositories.")
              ),
              div(
                class = "list-group-item px-0",
                strong(icon("arrow-trend-up", class = "text-success"), " Rule #3 (Lift = 3.53): "),
                br(),
                code("{Branch = CSE, Projects Completed = None}"), " => ", code("{Academic Year = 2nd Year}"),
                p(class = "text-muted mb-0 mt-1", "Project void is heavily concentrated in the early 2nd year curriculum before capstone and software engineering courses commence.")
              ),
              div(
                class = "list-group-item px-0",
                strong(icon("arrow-trend-up", class = "text-success"), " Rule #4 (Lift = 3.33): "),
                br(),
                code("{GitHub = Sometimes, Placement Status = Haven't participated}"), " => ", code("{Coding Probs = >200}"),
                p(class = "text-muted mb-0 mt-1", "Pre-placement 3rd years actively solve algorithmic problems on LeetCode/HackerRank in parallel with repository maintenance.")
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      class = "mt-4",
      column(
        width = 12,
        div(
          class = "card",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            span("Mined Association Rules Catalog"),
            span(class = "small text-muted", "Sortable by Lift, Support, or Confidence")
          ),
          div(
            class = "card-body",
            DTOutput(ns("table_rules"))
          )
        )
      )
    )
  )
}

mod_association_rules_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load rules once
    all_rules <- reactive({
      rules <- load_association_rules(top_only = FALSE)
      if (is.null(rules)) {
        # Fallback sample rules
        rules <- tibble(
          rule = c(
            "{placement_status=Not eligible,resume=3} => {academic_year=2nd Year}",
            "{academic_year=4th Year,github=Rarely} => {internship_status=Yes}",
            "{branch=CSE,projects=None} => {academic_year=2nd Year}",
            "{projects=None,internship=No} => {academic_year=2nd Year}",
            "{github=Sometimes,status=Haven't participated} => {coding_probs=>200}"
          ),
          support = c(0.05, 0.05, 0.06, 0.06, 0.05),
          confidence = c(0.625, 0.833, 0.600, 0.600, 1.000),
          lift = c(3.68, 3.62, 3.53, 3.53, 3.33)
        )
      }
      rules
    })
    
    # Filter rules reactively
    filtered_rules <- reactive({
      df <- all_rules()
      df %>%
        filter(
          lift >= input$min_lift,
          confidence >= input$min_confidence,
          support >= input$min_support
        ) %>%
        arrange(desc(lift))
    })
    
    # Badge count
    output$rule_count_badge <- renderText({
      paste(nrow(filtered_rules()), "Matching Rules")
    })
    
    # Scatter Plot
    output$plot_rules_scatter <- renderPlotly({
      df <- filtered_rules()
      
      if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers"))
      
      # Sample if too many for fast rendering
      plot_df <- if (nrow(df) > 300) head(df, 300) else df
      
      p <- ggplot(plot_df, aes(x = support, y = confidence, color = lift, text = rule)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_viridis_c(option = "plasma", name = "Lift") +
        labs(
          x = "Support",
          y = "Confidence",
          title = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("text", "x", "y", "color")) %>%
        layout(margin = list(t = 25, r = 10, b = 40, l = 50)) %>%
        config(displayModeBar = FALSE)
    })
    
    # Rules DT Table
    output$table_rules <- renderDT({
      df <- filtered_rules() %>%
        mutate(
          Support = round(support, 3),
          Confidence = round(confidence, 3),
          Lift = round(lift, 3)
        ) %>%
        select(Association_Rule = rule, Support, Confidence, Lift)
      
      datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip"
        ),
        class = "compact stripe hover",
        rownames = FALSE
      ) %>%
        formatStyle(
          "Lift",
          background = styleColorBar(range(df$Lift, na.rm = TRUE), "#dbeafe"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
    
  })
}
