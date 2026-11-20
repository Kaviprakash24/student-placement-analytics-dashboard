library(tidyverse)

# Load the dataset
students <- read_csv(
  "data/student_placement_synthetic_100.csv"
)

# Count placement categories
placement_summary <- students %>%
  count(placement_status, name = "student_count") %>%
  mutate(
    percentage = round(
      student_count / sum(student_count) * 100,
      2
    )
  ) %>%
  arrange(desc(student_count))

print(placement_summary)

ggplot(
  placement_summary,
  aes(
    x = reorder(placement_status, student_count),
    y = student_count
  )
) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = student_count),
    hjust = -0.2
  ) +
  labs(
    title = "Distribution of Student Placement Status",
    x = "Placement Status",
    y = "Number of Students"
  ) +
  theme_minimal()

ggplot(
  placement_summary,
  aes(
    x = reorder(placement_status, student_count),
    y = student_count
  )
) +
  geom_col() +
  geom_text(
    aes(label = paste0(student_count, " (", percentage, "%)")),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 42),
    breaks = seq(0, 40, 10)
  ) +
  labs(
    title = "Distribution of Student Placement Status",
    subtitle = "Based on 100 synthetic student records",
    x = "Placement Status",
    y = "Number of Students"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "placement_status_distribution.png",
  width = 11,
  height = 7,
  dpi = 300
)

install.packages("shiny")
library(shiny)