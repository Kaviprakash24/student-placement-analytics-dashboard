# ==============================================================================
# MODULE: INTERACTIVE PLACEMENT READINESS CALCULATOR & SCORECARD
# ==============================================================================

library(shiny)
library(bslib)
library(plotly)

mod_calculator_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "mb-3",
      h4(class = "fw-bold text-dark mb-1", "Interactive Placement Readiness Scorecard"),
      p(class = "text-muted mb-0", "Evaluate your personal profile against the empirical benchmarks of placed NIT Jalandhar engineering cohorts.")
    ),
    
    fluidRow(
      # Left Column: User Input Form
      column(
        width = 5,
        div(
          class = "card p-3 bg-white",
          h5(class = "fw-bold text-primary mb-3", icon("sliders"), " Your Academic & Technical Profile"),
          
          # Academic Year & Branch
          fluidRow(
            column(
              width = 6,
              selectInput(
                ns("calc_year"),
                "Academic Year:",
                choices = c("1st Year", "2nd Year", "3rd Year", "4th Year"),
                selected = "3rd Year"
              )
            ),
            column(
              width = 6,
              selectInput(
                ns("calc_cgpa"),
                "CGPA Bracket:",
                choices = c("Below 6.0", "6.0 to 6.99", "7.0 to 7.99", "8.0 to 8.99", "9.0 and above"),
                selected = "8.0 to 8.99"
              )
            )
          ),
          
          # Core Technical Ratings
          h6(class = "fw-bold text-secondary mt-2 mb-2", "Core Technical Competencies (1–5 Scale)"),
          sliderInput(ns("calc_dsa"), "Data Structures & Algorithms (DSA):", min = 1, max = 5, value = 3, step = 1),
          sliderInput(ns("calc_oop"), "Object-Oriented Programming (OOP):", min = 1, max = 5, value = 3, step = 1),
          sliderInput(ns("calc_systems"), "Operating Systems & DBMS / SQL:", min = 1, max = 5, value = 3, step = 1),
          sliderInput(ns("calc_comm"), "Communication & Interview Confidence:", min = 1, max = 5, value = 3, step = 1),
          
          # Practical Experience
          h6(class = "fw-bold text-secondary mt-3 mb-2", "Practical Experience & Interview Discipline"),
          selectInput(
            ns("calc_projects"),
            "Completed Technical Projects:",
            choices = c("0", "1 to 2", "3 to 4", "5 or more"),
            selected = "1 to 2"
          ),
          selectInput(
            ns("calc_coding"),
            "Coding Problems Solved (LeetCode / Codeforces):",
            choices = c("0 to 50", "51 to 150", "151 to 300", "More than 300"),
            selected = "51 to 150"
          ),
          selectInput(
            ns("calc_internship"),
            "Technical Internship Experience:",
            choices = c(
              "Completed at least one technical internship",
              "Currently doing a technical internship",
              "Applied / looking, but no internship yet",
              "Have not applied for internships yet"
            ),
            selected = "Applied / looking, but no internship yet"
          ),
          selectInput(
            ns("calc_mocks"),
            "Mock Interviews Completed:",
            choices = c("None", "1 to 2", "3 to 5", "More than 5"),
            selected = "None"
          )
        )
      ),
      
      # Right Column: Dynamic Scorecard & Action Plan
      column(
        width = 7,
        div(
          class = "calculator-score-card mb-4",
          h5(class = "fw-bold text-secondary text-uppercase letter-spacing-1 mb-3", "Placement Readiness Score"),
          
          # Circular Score Dial
          div(
            class = "score-circle",
            span(class = "score-number", textOutput(ns("score_value"))),
            span(class = "score-label", "/ 100 PTS")
          ),
          
          # Placement Tier Badge
          uiOutput(ns("tier_badge_ui")),
          
          # Target Salary Projection
          div(
            class = "mt-2",
            span(class = "text-muted", "Estimated Target Salary Bracket: "),
            strong(textOutput(ns("salary_target"), inline = TRUE))
          )
        ),
        
        # Personalized Gap Analysis Card
        div(
          class = "card p-3",
          h5(class = "fw-bold text-dark mb-3", icon("clipboard-check"), " Personalized Empirical Gap Analysis"),
          uiOutput(ns("recommendations_ui"))
        )
      )
    )
  )
}

mod_calculator_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate empirical readiness score (0-100)
    calculated_score <- reactive({
      score <- 0
      
      # 1. DSA Rating (Max 25 pts) - Strongest single differentiator
      # Placed average is 3.94 / 5.0
      score <- score + (input$calc_dsa / 5) * 25
      
      # 2. OOP Rating (Max 15 pts) - Placed average is 4.00
      score <- score + (input$calc_oop / 5) * 15
      
      # 3. Core Systems (OS + DBMS) (Max 15 pts)
      score <- score + (input$calc_systems / 5) * 15
      
      # 4. Communication (Max 10 pts) - Baseline requirement
      score <- score + (input$calc_comm / 5) * 10
      
      # 5. Projects (Max 12 pts)
      proj_score <- switch(
        input$calc_projects,
        "0" = 0,
        "1 to 2" = 5,
        "3 to 4" = 9,
        "5 or more" = 12,
        0
      )
      score <- score + proj_score
      
      # 6. Coding Problems (Max 10 pts)
      coding_score <- switch(
        input$calc_coding,
        "0 to 50" = 2,
        "51 to 150" = 5,
        "151 to 300" = 8,
        "More than 300" = 10,
        0
      )
      score <- score + coding_score
      
      # 7. Internship (Max 8 pts)
      intern_score <- switch(
        input$calc_internship,
        "Completed at least one technical internship" = 8,
        "Currently doing a technical internship" = 6,
        "Applied / looking, but no internship yet" = 2,
        "Have not applied for internships yet" = 0,
        0
      )
      score <- score + intern_score
      
      # 8. Mock Interviews (Max 5 pts)
      mock_score <- switch(
        input$calc_mocks,
        "None" = 0,
        "1 to 2" = 2,
        "3 to 5" = 4,
        "More than 5" = 5,
        0
      )
      score <- score + mock_score
      
      round(min(100, max(0, score)), 0)
    })
    
    # Render score value
    output$score_value <- renderText({
      calculated_score()
    })
    
    # Dynamic Tier Badge
    output$tier_badge_ui <- renderUI({
      score <- calculated_score()
      if (score >= 80) {
        div(class = "tier-badge tier-1", icon("crown"), " Tier 1: Placement Ready • Premium Target (>12 LPA)")
      } else if (score >= 60) {
        div(class = "tier-badge tier-2", icon("check-circle"), " Tier 2: Competitive Candidate • Core Target (6–10 LPA)")
      } else {
        div(class = "tier-badge tier-3", icon("triangle-exclamation"), " Tier 3: Foundation Building Required • At Risk (<6 LPA)")
      }
    })
    
    # Target Salary Prediction
    output$salary_target <- renderText({
      score <- calculated_score()
      dsa <- input$calc_dsa
      
      if (score >= 85 && dsa >= 4) {
        "Above 12 LPA (Top Product / High-Growth FinTech Bracket)"
      } else if (score >= 70 && dsa >= 3) {
        "8 to 12 LPA (Enterprise Software & System Engineering)"
      } else if (score >= 55) {
        "5 to 8 LPA (Standard Campus Placement Bracket)"
      } else {
        "Below 5 LPA / Additional Preparation Required Prior to Placement"
      }
    })
    
    # Dynamic Personalized Recommendations
    output$recommendations_ui <- renderUI({
      recs <- list()
      
      # Check DSA
      if (input$calc_dsa < 4) {
        recs[[length(recs) + 1]] <- div(
          class = "alert alert-warning p-2 small mb-2",
          strong(icon("code"), " DSA Bottleneck: "),
          "Your self-reported DSA rating is ", input$calc_dsa, "/5. Placed candidates average 3.94, and 12+ LPA offers require 4.33+. Prioritize dynamic programming, graphs, and LeetCode medium/hard patterns."
        )
      }
      
      # Check Mock Interviews
      if (input$calc_mocks %in% c("None", "1 to 2")) {
        recs[[length(recs) + 1]] <- div(
          class = "alert alert-danger p-2 small mb-2",
          strong(icon("comments"), " Mock Wakeup Warning: "),
          "Candidates with 0 mocks have an 80% false confidence rate that collapses under live technical grilling. Schedule at least 3 live peer/mentor mock interviews immediately."
        )
      }
      
      # Check Projects
      if (input$calc_projects %in% c("0", "1 to 2")) {
        recs[[length(recs) + 1]] <- div(
          class = "alert alert-info p-2 small mb-2",
          strong(icon("laptop-code"), " Project Depth Gap: "),
          "The cohort study demonstrates a strong link between building systems and algorithmic logic. Complete at least 2 full-stack or systems projects with active GitHub commit histories."
        )
      }
      
      # Check Internship
      if (grepl("not applied|Applied", input$calc_internship)) {
        recs[[length(recs) + 1]] <- div(
          class = "alert alert-secondary p-2 small mb-2",
          strong(icon("briefcase"), " Experience Void: "),
          "18 students cited lack of internships as their #1 roadblock. Actively apply to off-campus developer internships and open-source fellowship contributions."
        )
      }
      
      if (length(recs) == 0) {
        recs[[1]] <- div(
          class = "alert alert-success p-2 small mb-0",
          strong(icon("award"), " Outstanding Profile: "),
          "Your technical metrics, practical projects, and interview preparation match the top 10% benchmark of placed candidates. Maintain consistency and focus on company-specific system design."
        )
      }
      
      tagList(recs)
    })
    
  })
}
