# Cleaned runner script
$ErrorActionPreference = 'Stop'
$projectRoot = (Resolve-Path .).Path
Write-Host "Project root: $projectRoot"
# Forward-slash path for R
$projectRootFs = $projectRoot -replace '\\','/'
Write-Host "Using path for R: $projectRootFs"

# Path to Rscript (x64)
$Rscript = 'C:\Program Files\R\R-4.5.1\bin\x64\Rscript.exe'

# JJ folder
$jjPath = Join-Path -Path $projectRoot -ChildPath 'practical_1\JJs_take'
if(-not (Test-Path $jjPath)){
    New-Item -ItemType Directory -Path $jjPath -Force | Out-Null
    Write-Host "Created: $jjPath"
} else { Write-Host "Using existing directory: $jjPath" }

# Create figures directory if needed
$figuresPath = Join-Path -Path $projectRoot -ChildPath 'practical_1\figures'
if(-not (Test-Path $figuresPath)){
    New-Item -ItemType Directory -Path $figuresPath -Force | Out-Null
    Write-Host "Created: $figuresPath"
} else { Write-Host "Using existing directory: $figuresPath" }

# Ensure all required files exist
$requiredFiles = @(
    (Join-Path -Path $projectRoot -ChildPath 'practical_1\River_and_precip_Neuchatel.csv'),
    (Join-Path -Path $projectRoot -ChildPath 'practical_1\JuroExtremes.R'),
    (Join-Path -Path $jjPath -ChildPath 'Practical1_solutions.R'),
    (Join-Path -Path $jjPath -ChildPath 'Practical1_report.Rmd')
)

foreach($file in $requiredFiles){
    if(-not (Test-Path $file)){
        Write-Warning "Required file not found: $file"
    } else {
        Write-Host "Found: $file"
    }
}

# Paths used by the scripts
$jjPathFs = $jjPath -replace '\\','/'
$rmdAbs = "$jjPathFs/Practical1_report.Rmd"

Write-Host "Running solution script via Rscript..."
try{
    $tmpR = Join-Path -Path $jjPath -ChildPath 'run_jj_solution_debug.R'
    $rContent = @"
setwd('$projectRootFs')
Sys.setenv(RSTUDIO_PANDOC='C:/Program Files/Pandoc')
source('practical_1/JJs_take/Practical1_solutions.R')
"@
    [System.IO.File]::WriteAllBytes($tmpR, [System.Text.Encoding]::UTF8.GetBytes($rContent))
    & $Rscript $tmpR
    if($LASTEXITCODE -ne 0){ Write-Error "Solution Rscript failed with exit code $LASTEXITCODE"; exit $LASTEXITCODE }
    Write-Host "Solution script completed. (debug wrapper: $tmpR)"
} catch{
    Write-Error "Solution step failed: $_"
    exit 1
}

Write-Host "Running render script via Rscript..."
try{
    $tmpRender = Join-Path -Path $jjPath -ChildPath 'run_jj_render_debug.R'
    $renderContent = @"
setwd('$projectRootFs')
source('practical_1/JJs_take/render_practical1.R')
"@
    [System.IO.File]::WriteAllBytes($tmpRender, [System.Text.Encoding]::UTF8.GetBytes($renderContent))
    & $Rscript $tmpRender
    if($LASTEXITCODE -ne 0){ Write-Error "Render Rscript failed with exit code $LASTEXITCODE"; exit $LASTEXITCODE }
    Write-Host "R render script completed; check outputs in: $jjPath (render wrapper: $tmpRender)"
} catch{
    Write-Error "Render step failed: $_"
}

Write-Host 'Done.'
