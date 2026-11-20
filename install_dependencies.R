# ==============================================================================
# STUDENT PLACEMENT ANALYTICS - DEPENDENCY INSTALLATION SCRIPT
# ==============================================================================
# Run this script to install all required R packages for the Shiny Dashboard:
# Rscript install_dependencies.R
# ==============================================================================

required_packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "plotly",
  "DT",
  "dplyr",
  "readr",
  "tidyr",
  "stringr",
  "janitor",
  "scales",
  "viridis"
)

cat("Checking and installing dependencies for Student Placement Analytics...\n")

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  cat(sprintf("Installing %d missing packages: %s\n", length(new_packages), paste(new_packages, collapse = ", ")))
  install.packages(new_packages, repos = "https://cloud.r-project.org/")
} else {
  cat("All dependencies are already installed and up to date!\n")
}

cat("Setup complete! You can now launch the dashboard by running:\n")
cat("  shiny::runApp('.')\n")
