# Runner script for JJ's_take (PowerShell)
# Run this from the project root (so paths remain correct):
#   .\practical_1\JJs_take\run_from_JJs_take.ps1

$ErrorActionPreference = 'Stop'
$projectRoot = (Resolve-Path .).Path
Write-Host "Project root: $projectRoot"
# Convert to forward-slash path for safe R usage
$projectRootFs = $projectRoot -replace '\\','/'
Write-Host "Using path for R: $projectRootFs"

# Full path to Rscript - adjust if your R install differs
$Rscript = 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe'

# Run the solutions script (quick run)
& $Rscript -e "setwd('$projectRootFs'); Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/Pandoc'); source('practical_1/JJs_take/Practical1_solutions.R')"

# Render the RMarkdown report to Markdown and Word (optional)
& $Rscript -e "setwd('$projectRootFs'); Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/Pandoc'); rmarkdown::render('practical_1/JJs_take/Practical1_report.Rmd', output_format = rmarkdown::md_document(), output_file='practical_1/JJs_take/Practical1_report_v2.md', knit_root_dir = normalizePath(getwd()))"
& $Rscript -e "setwd('$projectRootFs'); Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/Pandoc'); rmarkdown::render('practical_1/JJs_take/Practical1_report.Rmd', output_format='word_document', output_file='practical_1/JJs_take/Practical1_report_v2.docx', knit_root_dir = normalizePath(getwd()))"

Write-Host 'Done. Outputs will be saved in the parent practical_1/ directory (figures, RDS) and rendered files in practical_1/JJs_take/'.
