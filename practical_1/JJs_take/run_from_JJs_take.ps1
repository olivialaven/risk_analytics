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

# Render the RMarkdown report using the static R script shipped in this folder
Write-Host "Running prebuilt R render script (render_practical1.R) via Rscript..."
$renderScript = Join-Path -Path $jjPath -ChildPath 'render_practical1.R'
if(-not (Test-Path -Path $renderScript)){
	Write-Error "Render script not found: $renderScript. Ensure render_practical1.R exists in $jjPath"
} else {
	try{
		& $Rscript $renderScript
		Write-Host "R render script completed; check for outputs in: $jjPath"
	} catch{
		Write-Error "R render script failed: $_"
	}
}

Write-Host 'Done. Outputs will be saved in the parent practical_1/ directory (figures, RDS) and rendered files in practical_1/JJs_take/'.
