# ============================================================================
# MASTER SCRIPT: Run Complete Analysis
# ============================================================================

# Set working directory
setwd("c:/Users/JJ/OneDrive - epfl.ch/Documents/SMT/RA/risk_analytics/final_project")

cat("\n")
cat("================================================================================\n")
cat("  RISK ANALYTICS PROJECT: ENERGY DEMAND & TEMPERATURE EXTREMES ANALYSIS\n")
cat("================================================================================\n")
cat("\n")
cat("Starting complete analysis pipeline...\n")
cat("This will take approximately 10-20 minutes.\n\n")

# Record start time
start_time <- Sys.time()

# Run Step 1-2: Data Preparation & EDA
cat("\n")
cat("################################################################################\n")
cat("# PHASE 1: DATA PREPARATION & EXPLORATORY DATA ANALYSIS (Steps 1-2)\n")
cat("################################################################################\n")
source("03_main_analysis.R")

# Run Step 3-5: Extreme Value Analysis
cat("\n")
cat("################################################################################\n")
cat("# PHASE 2: EXTREME VALUE ANALYSIS & TIME SERIES MODELING (Steps 3-5)\n")
cat("################################################################################\n")
source("04_advanced_analysis.R")

# Run Step 6-8: Dependence & Risk Analysis
cat("\n")
cat("################################################################################\n")
cat("# PHASE 3: DEPENDENCE ANALYSIS & RISK METRICS (Steps 6-8)\n")
cat("################################################################################\n")
source("05_final_analysis.R")

# Calculate total time
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

cat("\n")
cat("================================================================================\n")
cat("  ANALYSIS COMPLETE!\n")
cat("================================================================================\n")
cat("\n")
cat("Total execution time:", round(total_time, 2), "minutes\n\n")
cat("All outputs have been saved:\n")
cat("  - Figures: output_figures/ (23 figures)\n")
cat("  - Tables: output_tables/ (CSV files)\n")
cat("  - Report content: analysis_report_snippets.md\n\n")
cat("Next steps:\n")
cat("  1. Review all figures in output_figures/\n")
cat("  2. Review all tables in output_tables/\n")
cat("  3. Read analysis_report_snippets.md for report content\n")
cat("  4. Write your 3-page report using the provided structure\n\n")
cat("================================================================================\n")
