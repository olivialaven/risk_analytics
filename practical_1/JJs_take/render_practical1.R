# Risk Analytics Practical 1 - Rendering Script (Updated for Actual Assignment)
# This script renders the R Markdown reports to multiple formats

# Set working directory to ensure relative paths work
if(!endsWith(getwd(), "JJs_take")) {
  if(dir.exists("practical_1/JJs_take")) {
    setwd("practical_1/JJs_take")
  } else if(dir.exists("JJs_take")) {
    setwd("JJs_take")
  }
}

cat("Current working directory:", getwd(), "\n")

# Check if required files exist
required_files <- c(
  "Combined_Report.Rmd",
  "Assignment_Summary.Rmd",
  "Practical1_FullReport.Rmd",
  "../River_and_precip_Neuchatel.csv"
)

missing_files <- required_files[!sapply(required_files, file.exists)]
if(length(missing_files) > 0) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "))
}

# Install and load required packages
required_packages <- c('rmarkdown', 'knitr', 'readr', 'ggplot2', 'dplyr')
missing_packages <- required_packages[!required_packages %in% installed.packages()[,1]]
if(length(missing_packages) > 0) {
  install.packages(missing_packages, repos = 'https://cloud.r-project.org')
}

library(rmarkdown)

# Create output directory
output_dir <- getwd()
cat("Output directory:", output_dir, "\n")

# Render the ASSIGNMENT report (MAIN)
cat("\n=== Rendering Assignment Report ===\n")
tryCatch({
  # HTML version
  rmarkdown::render("Assignment_Summary.Rmd",
                   output_format = "html_document",
                   output_file = "Assignment_Summary.html",
                   output_dir = output_dir)
  cat("✓ Assignment HTML report rendered successfully\n")
  
  # Word version
  rmarkdown::render("Assignment_Summary.Rmd",
                   output_format = "word_document", 
                   output_file = "Assignment_Summary.docx",
                   output_dir = output_dir)
  cat("✓ Assignment Word report rendered successfully\n")
  
  # Markdown version for GitHub
  rmarkdown::render("Assignment_Summary.Rmd",
                   output_format = "github_document",
                   output_file = "Assignment_Summary.md", 
                   output_dir = output_dir)
  cat("✓ Assignment GitHub Markdown report rendered successfully\n")
  
}, error = function(e) {
  cat("Error rendering Assignment_Summary.Rmd:", e$message, "\n")
})

# Render the detailed combined report (OPTIONAL)
cat("\n=== Rendering Detailed Combined Report ===\n")
tryCatch({
  # HTML version
  rmarkdown::render("Combined_Report.Rmd",
                   output_format = "html_document",
                   output_file = "Combined_Report.html",
                   output_dir = output_dir)
  cat("✓ Detailed HTML report rendered successfully\n")
  
  # Word version
  rmarkdown::render("Combined_Report.Rmd",
                   output_format = "word_document", 
                   output_file = "Combined_Report.docx",
                   output_dir = output_dir)
  cat("✓ Detailed Word report rendered successfully\n")
  
}, error = function(e) {
  cat("Error rendering Combined_Report.Rmd:", e$message, "\n")
})

# Render the FULL SUBMISSION report (MAIN)
cat("\n=== Rendering Full Submission Report ===\n")
tryCatch({
  # Word version (PRIMARY)
  rmarkdown::render("Practical1_FullReport.Rmd",
                   output_format = "word_document", 
                   output_file = "Practical1_FullReport.docx",
                   output_dir = output_dir)
  cat("✓ Full Report Word document rendered successfully\n")
  
  # HTML version (REFERENCE)
  rmarkdown::render("Practical1_FullReport.Rmd",
                   output_format = "html_document",
                   output_file = "Practical1_FullReport.html",
                   output_dir = output_dir)
  cat("✓ Full Report HTML rendered successfully\n")
  
}, error = function(e) {
  cat("Error rendering Practical1_FullReport.Rmd:", e$message, "\n")
})

# List output files
cat("\n=== Rendered Files ===\n")
output_files <- c(
  "Assignment_Summary.html",
  "Assignment_Summary.docx", 
  "Assignment_Summary.md",
  "Combined_Report.html",
  "Combined_Report.docx",
  "Practical1_FullReport.docx",
  "Practical1_FullReport.html"
)

for(file in output_files) {
  if(file.exists(file)) {
    file_size <- round(file.size(file) / 1024, 1)
    cat("✓", file, "(", file_size, "KB )\n")
  } else {
    cat("✗", file, "(not found)\n")
  }
}

cat("\nRendering complete!\n")
cat("\n=== MAIN DELIVERABLES ===\n")
cat("The assignment solutions are in:\n")
cat("- Practical1_FullReport.docx (SUBMISSION VERSION)\n")
cat("- Assignment_Summary.html\n") 
cat("- Assignment_Summary.docx\n")
cat("- Assignment_Summary.md\n")
cat("\nThese follow the assignment requirements (distribution testing, CCF, extremograms, Granger causality, ARIMA/GARCH)\n")