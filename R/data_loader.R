# ==============================================================================
# STUDENT PLACEMENT ANALYTICS - DATA LOADER & PREPROCESSING ENGINE
# ==============================================================================
# Preserves the original research data and analytical definitions from:
# Department of CSE, Dr. B. R. Ambedkar National Institute of Technology Jalandhar
# Authors: Kavi Prakash Ranjan (24103076) & Kartik Saini (24103075)
# Supervisor: Dr. Jagdeep Kaur
# ==============================================================================

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)

#' Find data file with fallback paths
find_file <- function(filename) {
  candidates <- c(
    file.path("24103076 Code", filename),
    file.path("24103076 Code", "data", filename),
    file.path("data", filename),
    filename
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  return(NULL)
}

#' Load and standardize the primary student placement dataset
load_placement_data <- function() {
  # Prefer the clustered dataset which has unified column definitions and cluster labels
  clustered_path <- find_file("student_placement_clustered.csv")
  primary_path <- find_file("student_placement.csv")
  
  if (!is.null(clustered_path)) {
    df <- read_csv(clustered_path, show_col_types = FALSE)
  } else if (!is.null(primary_path)) {
    df <- read_csv(primary_path, show_col_types = FALSE)
    df <- clean_names(df)
  } else {
    stop("Placement dataset not found in workspace.")
  }
  
  # Standardize column naming if reading raw survey
  if (!"dsa_rating" %in% names(df) && "dsa" %in% names(df)) {
    df <- df %>%
      rename(
        dsa_rating = dsa,
        dbms_sql_rating = dbms,
        operating_systems_rating = os,
        computer_networks_rating = cn,
        oop_rating = oop,
        web_development_rating = web,
        data_science_ml_rating = ds_ml,
        aptitude_logical_reasoning_rating = aptitude,
        communication_interview_rating = comm,
        projects_completed_range = projects,
        technical_internship_status = internship_status,
        coding_problems_solved_range = coding_probs,
        weekly_placement_prep_hours = prep_hours,
        highest_full_time_offer_compensation = offer_range,
        biggest_placement_preparation_challenge = biggest_challenge
      )
  }
  
  # Ensure synthetic_record_id exists
  if (!"synthetic_record_id" %in% names(df)) {
    df$synthetic_record_id <- sprintf("STU%03d", seq_len(nrow(df)))
  }
  
  # Ensure cluster label exists
  if (!"cluster" %in% names(df)) {
    # If not present, assign based on K-means k=2 on technical skills
    skill_cols <- c("dsa_rating", "dbms_sql_rating", "operating_systems_rating",
                    "computer_networks_rating", "oop_rating", "web_development_rating")
    valid_skill_cols <- intersect(skill_cols, names(df))
    if (length(valid_skill_cols) >= 3) {
      set.seed(123)
      km <- kmeans(scale(df[, valid_skill_cols]), centers = 2)
      df$cluster <- km$cluster
    } else {
      df$cluster <- 1
    }
  }
  
  # Factor leveling for consistent categorical display
  df <- df %>%
    mutate(
      academic_year = factor(
        academic_year,
        levels = c("1st Year", "2nd Year", "3rd Year", "4th Year")
      ),
      branch = factor(branch),
      placement_status = factor(
        placement_status,
        levels = c(
          "Received a full-time job offer",
          "Participated in placement processes but haven't received an offer",
          "Haven't participated yet",
          "Not eligible for placements"
        )
      ),
      placement_category = ifelse(
        placement_status == "Received a full-time job offer",
        "Placed",
        "Unplaced"
      ),
      placement_category = factor(placement_category, levels = c("Placed", "Unplaced")),
      highest_full_time_offer_compensation = factor(
        highest_full_time_offer_compensation,
        levels = c("Below 5 LPA", "5 to 8 LPA", "8 to 12 LPA", "Above 12 LPA", "Not applicable / No offer yet", "No full-time offer", "Prefer not to say")
      ),
      projects_completed_range = factor(
        projects_completed_range,
        levels = c("0", "None", "1 to 2", "1", "2", "3 to 4", "3 or more", "5 or more")
      ),
      coding_problems_solved_range = factor(
        coding_problems_solved_range,
        levels = c("0 to 50", "1 to 50", "51 to 100", "51 to 150", "101 to 200", "151 to 300", "More than 200", "More than 300")
      ),
      cluster_label = factor(
        ifelse(cluster == 1, "Cluster 1: Foundational Learners", "Cluster 2: High-Capability Technologists"),
        levels = c("Cluster 1: Foundational Learners", "Cluster 2: High-Capability Technologists")
      )
    )
  
  return(df)
}

#' Load precomputed cluster profiles
load_cluster_profiles <- function() {
  p <- find_file("student_placement_cluster_profiles.csv")
  if (!is.null(p)) {
    read_csv(p, show_col_types = FALSE)
  } else {
    NULL
  }
}

#' Load association rules mined with Apriori
load_association_rules <- function(top_only = FALSE) {
  fname <- if (top_only) "top_association_rules.csv" else "student_placement_association_rules.csv"
  p <- find_file(fname)
  if (is.null(p)) p <- find_file("student_placement_association_rules.csv")
  if (is.null(p)) p <- find_file("top_association_rules.csv")
  
  if (!is.null(p)) {
    read_csv(p, show_col_types = FALSE)
  } else {
    NULL
  }
}

#' Get column metadata dictionary for Data Exploration
get_column_metadata <- function() {
  tibble(
    Field_Name = c(
      "synthetic_record_id", "academic_year", "branch", "cgpa_range",
      "dsa_rating", "dbms_sql_rating", "operating_systems_rating", "computer_networks_rating",
      "oop_rating", "web_development_rating", "data_science_ml_rating",
      "aptitude_logical_reasoning_rating", "communication_interview_rating",
      "projects_completed_range", "technical_internship_status", "coding_problems_solved_range",
      "weekly_placement_prep_hours", "mock_interviews_completed", "github_project_usage",
      "self_reported_resume_readiness", "placement_status", "highest_full_time_offer_compensation",
      "biggest_placement_preparation_challenge", "career_interest", "cluster"
    ),
    Category = c(
      "Identifier", "Demographics", "Demographics", "Academic",
      "Technical Skill", "Technical Skill", "Technical Skill", "Technical Skill",
      "Technical Skill", "Technical Skill", "Technical Skill",
      "Soft / Cognitive", "Soft / Cognitive",
      "Practical Experience", "Practical Experience", "Practical Experience",
      "Preparation Effort", "Placement Readiness", "Placement Readiness",
      "Placement Readiness", "Career Outcome", "Career Outcome",
      "Perception & Bottlenecks", "Career Goals", "Machine Learning"
    ),
    Data_Type = c(
      "Character", "Categorical (Ordinal)", "Categorical (Nominal)", "Categorical (Ordinal)",
      "Integer (1-5 Scale)", "Integer (1-5 Scale)", "Integer (1-5 Scale)", "Integer (1-5 Scale)",
      "Integer (1-5 Scale)", "Integer (1-5 Scale)", "Integer (1-5 Scale)",
      "Integer (1-5 Scale)", "Integer (1-5 Scale)",
      "Categorical (Ordinal)", "Categorical (Nominal)", "Categorical (Ordinal)",
      "Categorical (Ordinal)", "Categorical (Ordinal)", "Categorical (Ordinal)",
      "Categorical / Likert", "Categorical (Nominal)", "Categorical (Ordinal)",
      "Categorical (Nominal)", "Categorical (Nominal)", "Cluster Label (1 or 2)"
    ),
    Description = c(
      "Unique anonymous student record identifier",
      "Academic year of study (1st, 2nd, 3rd, 4th Year)",
      "Engineering branch (CSE, IT, ECE, EEE, Mechanical, Civil, etc.)",
      "Cumulative Grade Point Average interval bracket",
      "Data Structures & Algorithms competency rating (1 to 5)",
      "Database Management Systems & SQL competency rating (1 to 5)",
      "Operating Systems core principles rating (1 to 5)",
      "Computer Networks protocols & architecture rating (1 to 5)",
      "Object-Oriented Programming (Java/C++) rating (1 to 5)",
      "Full-stack web technologies & frameworks rating (1 to 5)",
      "Machine learning algorithms & data science rating (1 to 5)",
      "Quantitative aptitude & logical problem-solving rating (1 to 5)",
      "Verbal communication & interview articulation rating (1 to 5)",
      "Count of hands-on technical software projects completed",
      "Status of industrial/corporate technical internship",
      "Volume of LeetCode / HackerRank coding problems solved",
      "Hours spent weekly on placement and technical preparation",
      "Number of technical & HR mock interview sessions attended",
      "Frequency of GitHub version control and project commits",
      "Self-assessed resume completion and audit readiness",
      "Current placement result (Placed with offer, Unplaced, etc.)",
      "Compensation package bracket for secured full-time offers (LPA)",
      "Primary roadblock identified in placement preparation journey",
      "Preferred target career role and domain specialization",
      "K-Means cluster segment (Cluster 1: Foundational, Cluster 2: High-Capability)"
    )
  )
}
