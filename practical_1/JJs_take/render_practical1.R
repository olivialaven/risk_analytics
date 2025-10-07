# Render script for Practical1 (JJ's_take)
# This script assumes it is executed from the project root (setwd('.') or current working dir is project root).

# Use project root as working dir
proj_root <- normalizePath(getwd())
setwd(proj_root)

# Ensure rmarkdown and pandoc env
Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/Pandoc')

rmd_in <- 'practical_1/JJs_take/Practical1_report.Rmd'
md_out <- 'practical_1/JJs_take/Practical1_report_v2.md'
docx_out <- 'practical_1/JJs_take/Practical1_report_v2.docx'

cat('Rendering MD ->', md_out, '\n')
tryCatch({
  rmarkdown::render(rmd_in, output_format = rmarkdown::md_document(), output_file = md_out, knit_root_dir = normalizePath(getwd()))
}, error = function(e){
  message('MD render error: ', e$message)
  stop(e)
})

cat('Rendering DOCX ->', docx_out, '\n')
tryCatch({
  rmarkdown::render(rmd_in, output_format = 'word_document', output_file = docx_out, knit_root_dir = normalizePath(getwd()))
}, error = function(e){
  message('DOCX render error: ', e$message)
  stop(e)
})

cat('Render script finished.\n')
