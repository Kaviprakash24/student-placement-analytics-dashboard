# ==============================================================================
# STUDENT PLACEMENT ANALYTICS: PRESENTATION VISUALIZATION SCRIPT
# ==============================================================================

# Install packages if you don't have them
# install.packages(c("tidyverse", "janitor"))

library(tidyverse)
library(janitor)

# 1. LOAD AND CLEAN THE DATASET
# Adjust the file name to match exactly what is on your computer
students <- read_csv("data/student_placement.csv", show_col_types = FALSE)
students <- clean_names(students)

# Rename a few columns temporarily just to make the plot labels cleaner automatically
students_clean <- students %>%
  rename(
    DSA = dsa,
    OOP = oop,
    DBMS = dbms,
    OS = os,
    CN = cn,
    Web_Dev = web,
    DS_ML = ds_ml,
    Aptitude = aptitude,
    Communication = comm
  )

# ==============================================================================
# SECTION 1: THE CORE TECHNICAL PROFILE (Overall Class Strength)
# PPT Use Case: "Current Cohort Technical Baseline"
# ==============================================================================

# Calculate average rating for every technical skill across the whole class
skill_averages <- students_clean %>%
  summarise(
    across(c(DSA, OOP, DBMS, OS, CN, Web_Dev, DS_ML, Aptitude, Communication), 
           ~ mean(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Skill", values_to = "Average_Rating") %>%
  arrange(Average_Rating)

plot_1_skills <- ggplot(skill_averages, aes(x = reorder(Skill, Average_Rating), y = Average_Rating)) +
  geom_col(fill = "#2c3e50") +
  geom_text(aes(label = round(Average_Rating, 2)), hjust = -0.2, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Average Student Competency Across Core Skills",
    subtitle = "Self-Reported Ratings on a 1-5 Scale",
    x = "Technical / Soft Skill",
    y = "Average Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_1_skills)


# ==============================================================================
# SECTION 2: PLACEMENT OUTCOME BLUEPRINT (What separates winners?)
# PPT Use Case: "The Gap Between Placed and Unplaced Students"
# ==============================================================================

# Group by placement status and compare core technical skills
placement_gap <- students_clean %>%
  mutate(
    Status = ifelse(placement_status == "Received a full-time job offer", "Placed", "Not Placed")
  ) %>%
  group_by(Status) %>%
  summarise(
    DSA = mean(DSA, na.rm = TRUE),
    OOP = mean(OOP, na.rm = TRUE),
    Communication = mean(Communication, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(DSA, OOP, Communication), names_to = "Skill", values_to = "Score")

plot_2_gap <- ggplot(placement_gap, aes(x = Skill, y = Score, fill = Status)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = round(Score, 2)), position = position_dodge(width = 0.6), vjust = -0.5) +
  scale_fill_manual(values = c("Placed" = "#27ae60", "Not Placed" = "#e74c3c")) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "The True Differentiator: Technical Skills vs. Soft Skills",
    subtitle = "Placed students dominate in DSA & OOP, but tie in Communication",
    x = "Skill Domain",
    y = "Average Competency (1-5)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

print(plot_2_gap)


# ==============================================================================
# SECTION 3: THE ROI OF PROJECTS (Does building things matter?)
# PPT Use Case: "Why We Need to Push Project-Based Learning"
# ==============================================================================

# Order the factor logically from 0 to 5+
students_clean <- students_clean %>%
  mutate(projects = factor(projects, levels = c("0", "1 to 2", "3 to 4", "5 or more")))

plot_3_projects <- ggplot(students_clean, aes(x = projects, y = DSA, fill = projects)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Do Builders Make Better Coders?",
    subtitle = "Relationship between Completed Projects and DSA Proficiency",
    x = "Total Technical Projects Completed",
    y = "DSA Proficiency Rating (1-5)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

print(plot_3_projects)


# ==============================================================================
# SECTION 4: IDENTIFYING THE BOTTLENECKS
# PPT Use Case: "Where Students Are Struggling Most"
# ==============================================================================

# Count the frequency of each stated challenge
bottlenecks <- students_clean %>%
  count(biggest_challenge, sort = TRUE) %>%
  drop_na(biggest_challenge)

plot_4_bottlenecks <- ggplot(bottlenecks, aes(x = reorder(biggest_challenge, n), y = n)) +
  geom_col(fill = "#d35400") +
  geom_text(aes(label = n), hjust = -0.2, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Primary Placement Bottlenecks Identified by Students",
    subtitle = "What is holding our cohort back from securing offers?",
    x = "Stated Challenge",
    y = "Number of Students"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_4_bottlenecks)


# ==============================================================================
# SECTION 5: COMPENSATION DRIVERS (What gets the highest pay?)
# PPT Use Case: "The Blueprint for Top-Tier Offers (12+ LPA)"
# ==============================================================================

# Filter out N/A and map skills to salary tiers
salary_tiers <- students_clean %>%
  filter(!offer_range %in% c("Not applicable / No offer yet", "Prefer not to say"), !is.na(offer_range)) %>%
  mutate(offer_range = factor(offer_range, levels = c("Below 5 LPA", "5 to 8 LPA", "8 to 12 LPA", "Above 12 LPA"))) %>%
  group_by(offer_range) %>%
  summarise(Avg_DSA = mean(DSA, na.rm = TRUE), .groups = "drop")

plot_5_salary <- ggplot(salary_tiers, aes(x = offer_range, y = Avg_DSA, group = 1)) +
  geom_line(color = "#2980b9", size = 2) +
  geom_point(color = "#c0392b", size = 5) +
  geom_text(aes(label = round(Avg_DSA, 2)), vjust = -1.5, fontface = "bold", size = 5) +
  scale_y_continuous(limits = c(2, 5)) +
  labs(
    title = "The Salary Staircase: DSA Proficiency vs. Offer Range",
    subtitle = "Breaking the 12+ LPA ceiling requires near-perfect (4.33+) DSA scores",
    x = "Full-Time Offer Compensation Range",
    y = "Average DSA Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

print(plot_5_salary)


# ==============================================================================
# SECTION 6: AUTOMATIC EXPORT FOR POWERPOINT
# ==============================================================================

# This will automatically save high-resolution PNGs of all 5 plots to your working directory
ggsave("Slide_1_Core_Skills.png", plot = plot_1_skills, width = 10, height = 6, dpi = 300)
ggsave("Slide_2_Placement_Gap.png", plot = plot_2_gap, width = 10, height = 6, dpi = 300)
ggsave("Slide_3_Projects_ROI.png", plot = plot_3_projects, width = 10, height = 6, dpi = 300)
ggsave("Slide_4_Bottlenecks.png", plot = plot_4_bottlenecks, width = 10, height = 6, dpi = 300)
ggsave("Slide_5_Salary_Drivers.png", plot = plot_5_salary, width = 10, height = 6, dpi = 300)

print("All charts have been successfully exported as PNG files for your PowerPoint!")
