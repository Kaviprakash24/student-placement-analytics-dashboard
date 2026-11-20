install.packages("arules")

library(tidyverse)
library(arules)

# Load the synthetic dataset
students <- read_csv(
  "data/student_placement_synthetic_100.csv"
)

# Check the dataset
glimpse(students)

# Check number of records
nrow(students)

# Step 2: Select categorical features

association_data <- students %>%
  select(
    academic_year,
    branch,
    cgpa_range,
    coding_problems_solved_range,
    projects_completed_range,
    technical_internship_status,
    placement_status,
    career_interest,
    mock_interviews_completed,
    github_project_usage,
    self_reported_resume_readiness
  )

# Inspect selected features
glimpse(association_data)

# Check missing values
colSums(is.na(association_data))

# Step 3: Convert each student into a transaction

transaction_list <- lapply(
  seq_len(nrow(association_data)),
  function(i) {
    
    row_values <- as.character(
      unlist(association_data[i, ], use.names = FALSE)
    )
    
    feature_names <- names(association_data)
    
    # Create feature=value items
    items <- paste(
      feature_names,
      row_values,
      sep = "="
    )
    
    # Remove missing and empty values
    valid_items <- !is.na(row_values) &
      trimws(row_values) != ""
    
    items[valid_items]
  }
)

# Convert lists into arules transactions
student_transactions <- as(
  transaction_list,
  "transactions"
)

# Inspect transactions
summary(student_transactions)

# Display first five transactions
inspect(student_transactions[1:5])

# Step 5: Apply Apriori algorithm

set.seed(123)

placement_rules <- apriori(
  student_transactions,
  parameter = list(
    support = 0.05,
    confidence = 0.50,
    minlen = 2,
    maxlen = 3
  )
)

# Display summary
summary(placement_rules)

# Count the rules generated
length(placement_rules)

# Display the 10 rules with highest lift

if (length(placement_rules) > 0) {
  
  top_rules <- sort(
    placement_rules,
    by = "lift",
    decreasing = TRUE
  )
  
  inspect(head(top_rules, 10))
  
} else {
  
  cat("No rules found. We need to adjust the thresholds.\n")
  
}

# Step 7: Evaluate association rules

if (length(placement_rules) > 0) {
  
  rules_quality <- quality(placement_rules)
  
  # Add rule metrics to a summary table
  rules_table <- tibble(
    rule = labels(placement_rules),
    support = rules_quality$support,
    confidence = rules_quality$confidence,
    lift = rules_quality$lift
  ) %>%
    arrange(desc(lift))
  
  print(rules_table)
  
} else {
  
  cat("No rules available to evaluate.\n")
  
}

# Step 8: Export association rules

if (exists("rules_table") && nrow(rules_table) > 0) {
  
  write_csv(
    rules_table,
    "student_placement_association_rules.csv"
  )
  
  cat("Association rules exported successfully!\n")
  
} else {
  
  cat("No rules to export yet.\n")
  
}

getwd()

install.packages("arulesViz")
library(arulesViz)

# Step 9: Visualize association rules

if (length(placement_rules) > 0) {
  
  # Select the 10 rules with highest lift
  top_rules <- head(
    sort(placement_rules, by = "lift", decreasing = TRUE),
    10
  )
  
  # Create scatterplot
  plot(
    top_rules,
    method = "scatterplot",
    measure = c("support", "confidence"),
    shading = "lift"
  )
  
} else {
  
  cat("No rules available to visualize.\n")
  
}

# Step 10: Save association rule visualization

if (length(placement_rules) > 0) {
  
  top_rules <- head(
    sort(placement_rules, by = "lift", decreasing = TRUE),
    10
  )
  
  png(
    filename = "association_rules_plot.png",
    width = 1200,
    height = 900,
    res = 120
  )
  
  plot(
    top_rules,
    method = "scatterplot",
    measure = c("support", "confidence"),
    shading = "lift"
  )
  
  dev.off()
  
  cat("Association rule plot saved successfully!\n")
  
} else {
  
  cat("No rules available to save.\n")
  
}

getwd()

# Step 10.2: Export top association rules

if (length(placement_rules) > 0) {
  
  top_rules <- head(
    sort(placement_rules, by = "lift", decreasing = TRUE),
    10
  )
  
  top_rules_quality <- quality(top_rules)
  
  top_rules_table <- tibble(
    rule = labels(top_rules),
    support = top_rules_quality$support,
    confidence = top_rules_quality$confidence,
    lift = top_rules_quality$lift
  )
  
  print(top_rules_table)
  
  write_csv(
    top_rules_table,
    "top_association_rules.csv"
  )
  
  cat("Top association rules exported successfully!\n")
  
} else {
  
  cat("No rules available to export.\n")
  
}