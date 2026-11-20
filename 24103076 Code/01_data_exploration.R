# ==========================================
# STUDENT PLACEMENT ANALYTICS
# Step 1: Import and Explore Data
# ==========================================

# Install packages if not already installed
# Run this line only once:
 install.packages(c("tidyverse", "janitor"))

library(tidyverse)
library(janitor)

# Import dataset
students <- read_csv(
  "data/student_placement_synthetic_100.csv"
)

# Clean column names
students <- clean_names(students)

# Display first 6 rows
head(students)

# Display dataset structure
str(students)

# Number of rows and columns
dim(students)

# Column names
names(students)

# ==========================================
# Step 2: Data Quality Checks
# ==========================================

# Count missing values in each column
colSums(is.na(students))

# Count completely duplicated rows
sum(duplicated(students))

# Count distinct values in each column
sapply(students, n_distinct)

# Summary of all columns
summary(students)

# ==========================================
# PHASE 2: EXPLORATORY DATA ANALYSIS
# Analysis 1: Student Distribution
# ==========================================

library(tidyverse)

# Count students in each academic year
year_distribution <- students %>%
  count(academic_year, name = "student_count")

# Display the frequency table
print(year_distribution)

# Calculate percentages
year_distribution <- year_distribution %>%
  mutate(
    percentage = round(
      student_count / sum(student_count) * 100,
      2
    )
  )

print(year_distribution)

# Bar chart of students by academic year

ggplot(
  year_distribution,
  aes(x = academic_year, y = student_count)
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Distribution of Students by Academic Year",
    x = "Academic Year",
    y = "Number of Students"
  ) +
  theme_minimal()

ggplot(
  year_distribution,
  aes(x = academic_year, y = student_count)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(
      label = paste0(
        student_count,
        " (",
        percentage,
        "%)"
      )
    ),
    vjust = -0.5
  ) +
  labs(
    title = "Student Distribution by Academic Year",
    x = "Academic Year",
    y = "Number of Students"
  ) +
  theme_minimal()

# ==========================================
# EDA - STEP 2: CGPA DISTRIBUTION
# ==========================================

library(tidyverse)

# Count students in each CGPA range
cgpa_distribution <- students %>%
  count(cgpa_range, name = "student_count")

# Calculate percentages
cgpa_distribution <- cgpa_distribution %>%
  mutate(
    percentage = round(
      student_count / sum(student_count) * 100,
      2
    )
  )

# Display the results
print(cgpa_distribution)

ggplot(
  cgpa_distribution,
  aes(x = cgpa_range, y = student_count)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = student_count),
    vjust = -0.5
  ) +
  labs(
    title = "Distribution of Students by CGPA Range",
    x = "CGPA Range",
    y = "Number of Students"
  ) +
  theme_minimal()


ggplot(
  students,
  aes(x = academic_year, fill = cgpa_range)
) +
  geom_bar(position = "dodge") +
  labs(
    title = "CGPA Distribution Across Academic Years",
    x = "Academic Year",
    y = "Number of Students",
    fill = "CGPA Range"
  ) +
  theme_minimal()

# ==========================================
# EDA - STEP 3: TECHNICAL SKILLS ANALYSIS
# ==========================================

library(tidyverse)

# Select the seven technical skill columns
skill_data <- students %>%
  select(
    dsa_rating,
    dbms_sql_rating,
    operating_systems_rating,
    computer_networks_rating,
    oop_rating,
    web_development_rating,
    data_science_ml_rating
  )

# Calculate average rating for each skill
skill_summary <- skill_data %>%
  summarise(
    across(
      everything(),
      ~ mean(.x, na.rm = TRUE)
    )
  )

# Convert the result into a long format
skill_summary <- skill_summary %>%
  pivot_longer(
    cols = everything(),
    names_to = "skill",
    values_to = "average_rating"
  ) %>%
  mutate(
    average_rating = round(average_rating, 2)
  ) %>%
  arrange(desc(average_rating))

# Display the results
print(skill_summary)

ggplot(
  skill_summary,
  aes(
    x = reorder(skill, average_rating),
    y = average_rating
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = round(average_rating, 2)),
    hjust = -0.2
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 5),
    breaks = 0:5
  ) +
  labs(
    title = "Average Student Rating Across Technical Skills",
    x = "Technical Skill",
    y = "Average Rating (1–5)"
  ) +
  theme_minimal()

# Highest-rated skill
strongest_skill <- skill_summary %>%
  slice_max(
    average_rating,
    n = 1,
    with_ties = TRUE
  )

# Lowest-rated skill
weakest_skill <- skill_summary %>%
  slice_min(
    average_rating,
    n = 1,
    with_ties = TRUE
  )

cat("Highest-rated skill:\n")
print(strongest_skill)

cat("\nLowest-rated skill:\n")
print(weakest_skill)

# ==========================================
# EDA - STEP 4A: PROJECT EXPERIENCE
# ==========================================

library(tidyverse)

# Count students by project experience
project_summary <- students %>%
  count(projects_completed_range, name = "student_count")

print(project_summary)

ggplot(
  project_summary,
  aes(
    x = projects_completed_range,
    y = student_count
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = student_count),
    vjust = -0.5
  ) +
  labs(
    title = "Technical Projects Completed by Students",
    x = "Number of Projects",
    y = "Number of Students"
  ) +
  theme_minimal()

# ==========================================
# EDA - STEP 4B: INTERNSHIP EXPERIENCE
# ==========================================

internship_summary <- students %>%
  count(technical_internship_status, name = "student_count") %>%
  mutate(
    percentage = round(
      student_count / sum(student_count) * 100,
      2
    )
  )

print(internship_summary)

ggplot(
  internship_summary,
  aes(
    x = technical_internship_status,
    y = student_count
  )
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = student_count),
    vjust = -0.5
  ) +
  labs(
    title = "Internship Experience Among Students",
    x = "Internship Status",
    y = "Number of Students"
  ) +
  theme_minimal()

# ==========================================
# EDA - STEP 4C: CODING PRACTICE
# ==========================================

coding_summary <- students %>%
  count(coding_problems_solved_range, name = "student_count")

print(coding_summary)

ggplot(
  coding_summary,
  aes(
    x = coding_problems_solved_range,
    y = student_count
  )
) +
  geom_col(fill = "orange") +
  geom_text(
    aes(label = student_count),
    vjust = -0.5
  ) +
  labs(
    title = "Coding Problems Solved by Students",
    x = "Coding Problems Solved",
    y = "Number of Students"
  ) +
  theme_minimal()

# ==========================================
# EDA - STEP 4D: PROJECTS VS DSA
# ==========================================

ggplot(
  students,
  aes(
    x = projects_completed_range,
    y = dsa_rating
  )
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "DSA Ratings by Project Experience",
    x = "Projects Completed",
    y = "Self-Reported DSA Rating"
  ) +
  theme_minimal()

# Count students by placement status
placement_summary <- students %>%
  count(placement_status, name = "student_count") %>%
  mutate(
    percentage = round(student_count / sum(student_count) * 100, 2)
  )

# Display the summary
print(placement_summary)

ggplot(
  placement_summary,
  aes(x = placement_status, y = student_count)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = student_count),
    vjust = -0.5
  ) +
  labs(
    title = "Student Placement Status Distribution",
    x = "Placement Status",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

compensation_summary <- students %>%
  count(
    highest_full_time_offer_compensation,
    name = "student_count"
  )

print(compensation_summary)

ggplot(
  compensation_summary,
  aes(
    x = highest_full_time_offer_compensation,
    y = student_count
  )
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = student_count),
    vjust = -0.5
  ) +
  labs(
    title = "Distribution of Highest Full-Time Offer Compensation",
    x = "Compensation Range",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

library(tidyverse)

skills_summary <- students %>%
  select(skills_currently_improving) %>%
  separate_rows(
    skills_currently_improving,
    sep = ","
  ) %>%
  mutate(
    skills_currently_improving =
      str_trim(skills_currently_improving)
  ) %>%
  filter(
    !is.na(skills_currently_improving),
    skills_currently_improving != ""
  ) %>%
  count(
    skills_currently_improving,
    sort = TRUE,
    name = "student_count"
  )

print(skills_summary)

ggplot(
  skills_summary,
  aes(
    x = reorder(
      skills_currently_improving,
      student_count
    ),
    y = student_count
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = student_count),
    hjust = -0.2
  ) +
  coord_flip() +
  labs(
    title = "Skills Students Are Currently Improving",
    x = "Skill",
    y = "Number of Selections"
  ) +
  theme_minimal()

factors_summary <- students %>%
  select(factors_believed_important_for_placement) %>%
  separate_rows(
    factors_believed_important_for_placement,
    sep = ","
  ) %>%
  mutate(
    factors_believed_important_for_placement =
      str_trim(factors_believed_important_for_placement)
  ) %>%
  filter(
    !is.na(factors_believed_important_for_placement),
    factors_believed_important_for_placement != ""
  ) %>%
  count(
    factors_believed_important_for_placement,
    sort = TRUE,
    name = "student_count"
  )

print(factors_summary)

ggplot(
  factors_summary,
  aes(
    x = reorder(
      factors_believed_important_for_placement,
      student_count
    ),
    y = student_count
  )
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = student_count),
    hjust = -0.2
  ) +
  coord_flip() +
  labs(
    title = "Factors Students Consider Important for Placement",
    x = "Placement Factor",
    y = "Number of Students"
  ) +
  theme_minimal()

company_skills_summary <- students %>%
  select(skills_believed_prioritized_by_companies) %>%
  separate_rows(
    skills_believed_prioritized_by_companies,
    sep = ","
  ) %>%
  mutate(
    skills_believed_prioritized_by_companies =
      str_trim(skills_believed_prioritized_by_companies)
  ) %>%
  filter(
    !is.na(skills_believed_prioritized_by_companies),
    skills_believed_prioritized_by_companies != ""
  ) %>%
  count(
    skills_believed_prioritized_by_companies,
    sort = TRUE,
    name = "student_count"
  )

print(company_skills_summary)

ggplot(
  company_skills_summary,
  aes(
    x = reorder(
      skills_believed_prioritized_by_companies,
      student_count
    ),
    y = student_count
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = student_count),
    hjust = -0.2
  ) +
  coord_flip() +
  labs(
    title = "Skills Students Believe Companies Prioritize",
    x = "Skill",
    y = "Number of Students Selecting the Skill"
  ) +
  theme_minimal()

challenge_summary <- students %>%
  count(
    biggest_placement_preparation_challenge,
    sort = TRUE,
    name = "student_count"
  ) %>%
  mutate(
    percentage = round(
      student_count / sum(student_count) * 100,
      2
    )
  )

print(challenge_summary)

ggplot(
  challenge_summary,
  aes(
    x = reorder(
      biggest_placement_preparation_challenge,
      student_count
    ),
    y = student_count
  )
) +
  geom_col(fill = "tomato") +
  geom_text(
    aes(label = student_count),
    hjust = -0.2
  ) +
  coord_flip() +
  labs(
    title = "Biggest Placement Preparation Challenges",
    x = "Challenge",
    y = "Number of Students"
  ) +
  theme_minimal()

ggplot(
  students,
  aes(
    x = cgpa_numeric_approx,
    y = dsa_rating
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0.08,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red"
  ) +
  labs(
    title = "Relationship Between CGPA and DSA Rating",
    x = "Approximate CGPA",
    y = "Self-Reported DSA Rating"
  ) +
  theme_minimal()

cor(
  students$cgpa_numeric_approx,
  students$dsa_rating,
  use = "complete.obs",
  method = "spearman"
)

ggplot(
  students,
  aes(
    x = projects_completed_range,
    y = dsa_rating
  )
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "DSA Rating by Number of Projects Completed",
    x = "Projects Completed",
    y = "Self-Reported DSA Rating"
  ) +
  theme_minimal()

project_dsa_summary <- students %>%
  group_by(projects_completed_range) %>%
  summarise(
    student_count = n(),
    median_dsa_rating = median(
      dsa_rating,
      na.rm = TRUE
    ),
    mean_dsa_rating = mean(
      dsa_rating,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(project_dsa_summary)

ggplot(
  students,
  aes(
    x = technical_internship_status,
    y = dsa_rating
  )
) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "DSA Rating by Technical Internship Status",
    x = "Technical Internship Status",
    y = "Self-Reported DSA Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

internship_dsa_summary <- students %>%
  group_by(technical_internship_status) %>%
  summarise(
    student_count = n(),
    median_dsa_rating = median(
      dsa_rating,
      na.rm = TRUE
    ),
    mean_dsa_rating = mean(
      dsa_rating,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(internship_dsa_summary)

ggplot(
  students,
  aes(
    x = weekly_prep_hours_numeric_approx,
    y = dsa_rating
  )
) +
  geom_jitter(
    width = 0.15,
    height = 0.08,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red"
  ) +
  labs(
    title = "Weekly Preparation Hours vs DSA Rating",
    x = "Approximate Weekly Preparation Hours",
    y = "Self-Reported DSA Rating"
  ) +
  theme_minimal()

cor(
  students$weekly_prep_hours_numeric_approx,
  students$dsa_rating,
  use = "complete.obs",
  method = "spearman"
)

cgpa_placement_summary <- students %>%
  count(
    cgpa_range,
    placement_status,
    name = "student_count"
  )

print(cgpa_placement_summary)

ggplot(
  students,
  aes(
    x = cgpa_range,
    fill = placement_status
  )
) +
  geom_bar(position = "fill") +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Placement Status Distribution Across CGPA Categories",
    x = "CGPA Range",
    y = "Percentage of Students",
    fill = "Placement Status"
  ) +
  theme_minimal()

ggplot(
  students,
  aes(
    x = coding_problems_solved_range,
    y = dsa_rating
  )
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "DSA Rating by Coding Practice",
    x = "Coding Problems Solved",
    y = "Self-Reported DSA Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

coding_dsa_summary <- students %>%
  group_by(coding_problems_solved_range) %>%
  summarise(
    student_count = n(),
    median_dsa_rating = median(
      dsa_rating,
      na.rm = TRUE
    ),
    mean_dsa_rating = mean(
      dsa_rating,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(coding_dsa_summary)

# Count missing values in every column
missing_summary <- students %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column_name",
    values_to = "missing_count"
  ) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

print(missing_summary)

# Check whether ratings fall within the expected 1–5 range
rating_check <- students %>%
  summarise(
    across(
      c(
        dsa_rating,
        dbms_sql_rating,
        operating_systems_rating,
        computer_networks_rating,
        oop_rating,
        web_development_rating,
        data_science_ml_rating
      ),
      ~ sum(!is.na(.x) & !(.x %in% 1:5))
    )
  )

print(rating_check)

# Dataset dimensions
cat("Total Students:", nrow(students), "\n")
cat("Total Columns:", ncol(students), "\n")

# Dataset structure
glimpse(students)

# Summary of all variables
summary(students)

skill_summary <- students %>%
  summarise(
    across(
      c(
        dsa_rating,
        dbms_sql_rating,
        operating_systems_rating,
        computer_networks_rating,
        oop_rating,
        web_development_rating,
        data_science_ml_rating
      ),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "skill",
    values_to = "average_rating"
  ) %>%
  arrange(desc(average_rating))

print(skill_summary)

correlation_summary <- tibble(
  relationship = c(
    "CGPA vs DSA Rating",
    "Weekly Preparation Hours vs DSA Rating"
  ),
  spearman_correlation = c(
    cor(
      students$cgpa_numeric_approx,
      students$dsa_rating,
      use = "complete.obs",
      method = "spearman"
    ),
    cor(
      students$weekly_prep_hours_numeric_approx,
      students$dsa_rating,
      use = "complete.obs",
      method = "spearman"
    )
  )
)

print(correlation_summary)

data_quality_summary <- tibble(
  total_rows = nrow(students),
  total_columns = ncol(students),
  exact_duplicate_rows = sum(duplicated(students)),
  missing_cells = sum(is.na(students)),
  unique_record_ids = n_distinct(students$synthetic_record_id)
)

print(data_quality_summary)