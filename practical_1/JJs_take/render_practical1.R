# Render script for Practical1 (JJ's_take)
# This script assumes it is executed from the project root (setwd('.') or current working dir is project root).

# Use project root as working dir
proj_root <- normalizePath(getwd())
setwd(proj_root)

# Ensure rmarkdown and pandoc env
Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/Pandoc')

# Create output directories if they don't exist
jj_dir <- file.path(proj_root, 'practical_1', 'JJs_take')
if (!dir.exists(jj_dir)) {
  dir.create(jj_dir, recursive = TRUE)
  cat('Created directory:', jj_dir, '\n')
}

# Use absolute paths for all files
rmd_in <- file.path(proj_root, 'practical_1', 'JJs_take', 'Practical_ALL_Combined_Report.Rmd')
md_out <- file.path(proj_root, 'practical_1', 'JJs_take', 'Combined_Report.md')
docx_out <- file.path(proj_root, 'practical_1', 'JJs_take', 'Combined_Report.docx')

# Verify input file exists
if (!file.exists(rmd_in)) {
  stop('Input file does not exist: ', rmd_in)
}

cat('Rendering MD ->', md_out, '\n')
tryCatch({
  rmarkdown::render(rmd_in, output_format = rmarkdown::md_document(), output_file = basename(md_out), output_dir = dirname(md_out), knit_root_dir = proj_root)
}, error = function(e){
  message('MD render error: ', e$message)
  stop(e)
})

cat('Rendering DOCX ->', docx_out, '\n')
tryCatch({
  rmarkdown::render(rmd_in, output_format = 'word_document', output_file = basename(docx_out), output_dir = dirname(docx_out), knit_root_dir = proj_root)
}, error = function(e){
  message('DOCX render error: ', e$message)
  stop(e)
})

cat('Render script finished.\n')
