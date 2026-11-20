library(tidyverse)

# Assuming your dataset is loaded as 'students' and column names are formatted 
# in lowercase with underscores (e.g., Q1 = academic_year, Q2 = engineering_branch, etc.)

# ==============================================================================
# 1. THE "PLACED STUDENT" BLUEPRINT (OUTCOME ANALYSIS)
# ==============================================================================

# 1A. The Compensation Driver
# Isolates students with actual salary data to see which skills map to top pay tiers
compensation_drivers <- students %>%
  filter(
    !highest_full_time_offer_compensation %in% c("Not applicable / No offer yet", "Prefer not to say"),
    !is.na(highest_full_time_offer_compensation)
  ) %>%
  group_by(highest_full_time_offer_compensation) %>%
  summarise(
    student_count = n(),
    avg_dsa = mean(dsa_rating, na.rm = TRUE),
    avg_oop = mean(oop_rating, na.rm = TRUE),
    avg_dbms = mean(dbms_sql_rating, na.rm = TRUE),
    avg_os = mean(operating_systems_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Reorder to match the survey's ordinal tier progression
  mutate(
    highest_full_time_offer_compensation = factor(
      highest_full_time_offer_compensation,
      levels = c("Below 5 LPA", "5 to 8 LPA", "8 to 12 LPA", "Above 12 LPA")
    )
  ) %>%
  arrange(highest_full_time_offer_compensation)

print(compensation_drivers)

# 1B. The Baseline for Placement
# Compares the experience levels of placed vs. unplaced students
baseline_profile <- students %>%
  mutate(
    placement_category = ifelse(
      placement_status == "Received a full-time job offer", 
      "Placed", 
      "Unplaced"
    )
  ) %>%
  group_by(placement_category) %>%
  summarise(
    total_students = n(),
    # Identifies the most frequently selected option for categorical variables
    typical_projects = names(which.max(table(projects_completed_range))),
    typical_coding_problems = names(which.max(table(coding_problems_solved_range))),
    typical_mock_interviews = names(which.max(table(mock_interviews_completed))),
    avg_dsa = mean(dsa_rating, na.rm = TRUE),
    avg_communication = mean(communication_interview_rating, na.rm = TRUE),
    .groups = "drop"
  )

print(baseline_profile)


# ==============================================================================
# 2. THE ROI OF PREPARATION (EFFORT TRACKING)
# ==============================================================================

# 2A. The Mock Interview Effect
# Checks if mock interviews actually boost self-reported communication and resume readiness
mock_interview_roi <- students %>%
  group_by(mock_interviews_completed) %>%
  summarise(
    student_count = n(),
    avg_communication_rating = mean(communication_interview_rating, na.rm = TRUE),
    # Calculate percentage of students who claim their resume is fully ready
    percent_resume_fully_ready = mean(
      resume_readiness == "Fully updated and reviewed", 
      na.rm = TRUE
    ) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    mock_interviews_completed = factor(
      mock_interviews_completed,
      levels = c("None", "1 to 2", "3 to 5", "More than 5")
    )
  ) %>%
  arrange(mock_interviews_completed)

print(mock_interview_roi)


# ==============================================================================
# 3. VALIDATING STUDENT SELF-AWARENESS (BOTTLENECK CHECKS)
# ==============================================================================

# 3A. Imposter Syndrome vs. True Weakness
# Filters students who claim CS fundamentals are their biggest challenge and checks actual ratings
core_cs_awareness <- students %>%
  filter(biggest_placement_preparation_challenge == "Weak core CS fundamentals") %>%
  summarise(
    student_count = n(),
    avg_os_rating = mean(operating_systems_rating, na.rm = TRUE),
    avg_cn_rating = mean(computer_networks_rating, na.rm = TRUE),
    avg_dbms_rating = mean(dbms_sql_rating, na.rm = TRUE)
  )

print(core_cs_awareness)

# 3B. The Resume Disconnect
# Analyzes the actual GitHub and project habits of students claiming "Fully updated" resumes
resume_reality_check <- students %>%
  filter(resume_readiness == "Fully updated and reviewed") %>%
  summarise(
    total_claiming_ready = n(),
    percent_no_github = mean(
      github_usage == "Do not have a GitHub account", 
      na.rm = TRUE
    ) * 100,
    percent_zero_projects = mean(
      projects_completed_range == "0", 
      na.rm = TRUE
    ) * 100
  )

print(resume_reality_check)


# ==============================================================================
# 4. CAREER INTEREST VS. SKILL ALIGNMENT
# ==============================================================================

# Group by stated career interest and check if their technical strengths map properly
career_skill_alignment <- students %>%
  group_by(primary_career_interest) %>%
  summarise(
    student_count = n(),
    avg_web_dev = mean(web_development_rating, na.rm = TRUE),
    avg_ds_ml = mean(data_science_ml_rating, na.rm = TRUE),
    avg_cn = mean(computer_networks_rating, na.rm = TRUE),
    avg_os = mean(operating_systems_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(student_count))

print(career_skill_alignment)


# ==============================================================================
# 5. BRANCH-LEVEL COMPETENCY MAPPING
# ==============================================================================

# Find technical curriculum gaps by engineering department
branch_competency <- students %>%
  group_by(engineering_branch) %>%
  summarise(
    student_count = n(),
    avg_dsa = mean(dsa_rating, na.rm = TRUE),
    avg_oop = mean(oop_rating, na.rm = TRUE),
    avg_aptitude = mean(aptitude_logical_reasoning_rating, na.rm = TRUE),
    percent_doing_internship = mean(
      technical_internship_status %in% c(
        "Completed at least one technical internship", 
        "Currently doing a technical internship"
      ), 
      na.rm = TRUE
    ) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(student_count))

print(branch_competency)