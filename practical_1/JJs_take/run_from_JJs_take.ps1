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
# Ensure the JJ folder exists (create if missing)
$jjPath = Join-Path -Path $projectRoot -ChildPath 'practical_1\JJs_take'
if(-not (Test-Path -Path $jjPath)){
	Write-Host "Directory '$jjPath' not found — creating it now..."
	try{
		New-Item -ItemType Directory -Path $jjPath -Force | Out-Null
		Write-Host "Created: $jjPath"
	} catch{
		Write-Error "Failed to create directory $jjPath : $_"
		exit 1
	}
} else {
	Write-Host "Using existing directory: $jjPath"
}

# Build absolute forward-slash paths for R (avoid backslash escaping issues)
$jjPathFs = ($jjPath -replace '\\','/')
$projectRootFs = $projectRoot -replace '\\','/'
$rmdAbs = "$jjPathFs/Practical1_report.Rmd"
$mdOut = "$jjPathFs/Practical1_report_v2.md"
$docxOut = "$jjPathFs/Practical1_report_v2.docx"

# Run the solutions script (quick run)
Write-Host "Running solution script via Rscript..."
try{
	& $Rscript -e "setwd('$projectRootFs'); Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/Pandoc'); source('practical_1/JJs_take/Practical1_solutions.R')"
	Write-Host "Solution script completed."
} catch{
	Write-Error "Solution script failed: $_"
	exit 1
}

# Render the RMarkdown report to Markdown and Word using a temporary R script (avoids PowerShell parsing issues)
Write-Host "Preparing R render script in: $jjPath"
$renderScript = Join-Path -Path $jjPath -ChildPath 'render_practical1.R'
$rLines = @()
$rLines += "setwd('$projectRootFs')"
$rLines += "Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/Pandoc')"
$rLines += "cat('Rendering MD to: $mdOut\\n')"
$rLines += "rmarkdown::render(\"$rmdAbs\", output_format = rmarkdown::md_document(), output_file = \"$mdOut\", knit_root_dir = normalizePath(getwd()))"
$rLines += "cat('Rendering DOCX to: $docxOut\\n')"
$rLines += "rmarkdown::render(\"$rmdAbs\", output_format = 'word_document', output_file = \"$docxOut\", knit_root_dir = normalizePath(getwd()))"

# Write the R render script (UTF8)
try{
	$rLines | Out-File -FilePath $renderScript -Encoding UTF8 -Force
	Write-Host "Wrote render script: $renderScript"
} catch{
	Write-Error "Failed to write render script: $_"
}

Write-Host "Running R render script via Rscript..."
try{
	& $Rscript $renderScript
	Write-Host "R render script completed; outputs (md/docx) should be in: $jjPath"
} catch{
	Write-Error "R render script failed: $_"
}

Write-Host 'Done. Outputs will be saved in the parent practical_1/ directory (figures, RDS) and rendered files in practical_1/JJs_take/'.
