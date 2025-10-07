# PowerShell script to run the ACTUAL Risk Analytics Practical 1 assignment
# File: run_actual_assignment.ps1

Write-Host "=============================================" -ForegroundColor Green
Write-Host "Risk Analytics Practical 1 - Assignment Solutions" -ForegroundColor Green
Write-Host "Running solutions for the assignment requirements" -ForegroundColor Green
Write-Host "=============================================" -ForegroundColor Green

# Set execution policy for current session
Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope Process -Force

# Change to the practical folder
$practicalPath = "practical_1\JJs_take"
if (Test-Path $practicalPath) {
    Set-Location $practicalPath
    Write-Host "Changed to directory: $practicalPath" -ForegroundColor Yellow
} else {
    Write-Host "Directory not found: $practicalPath" -ForegroundColor Red
    exit 1
}

# Check if required files exist
$requiredFiles = @(
    "Practical1_solutions.R",
    "Assignment_Summary.Rmd",
    "render_practical1.R",
    "..\River_and_precip_Neuchatel.csv"
)

$missingFiles = @()
foreach ($file in $requiredFiles) {
    if (-not (Test-Path $file)) {
        $missingFiles += $file
    }
}

if ($missingFiles.Count -gt 0) {
    Write-Host "Missing required files:" -ForegroundColor Red
    foreach ($file in $missingFiles) {
        Write-Host "  - $file" -ForegroundColor Red
    }
    exit 1
}

Write-Host "All required files found ✓" -ForegroundColor Green

# Step 1: Run the assignment R analysis
Write-Host "`n=== STEP 1: Running Assignment Analysis ===" -ForegroundColor Cyan
Write-Host "Executing: Practical1_solutions.R" -ForegroundColor Yellow

try {
    # Use UTF-8 encoding to avoid parsing issues
    $env:LC_ALL = "en_US.UTF-8"
    
    # Run the R script
    Rscript.exe Practical1_solutions.R
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Assignment analysis completed successfully" -ForegroundColor Green
    } else {
        Write-Host "✗ R script execution failed with exit code: $LASTEXITCODE" -ForegroundColor Red
        Write-Host "Check the output above for error details" -ForegroundColor Yellow
    }
}
catch {
    Write-Host "✗ Error running R script: $($_.Exception.Message)" -ForegroundColor Red
}

# Step 2: Render the assignment reports
Write-Host "`n=== STEP 2: Rendering Assignment Reports ===" -ForegroundColor Cyan
Write-Host "Executing: render_practical1.R" -ForegroundColor Yellow

try {
    # Run the rendering script
    Rscript.exe render_practical1.R
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ Report rendering completed successfully" -ForegroundColor Green
    } else {
        Write-Host "✗ Report rendering failed with exit code: $LASTEXITCODE" -ForegroundColor Red
        Write-Host "Check the output above for error details" -ForegroundColor Yellow
    }
}
catch {
    Write-Host "✗ Error rendering reports: $($_.Exception.Message)" -ForegroundColor Red
}

# Step 3: Check output files
Write-Host "`n=== STEP 3: Checking Output Files ===" -ForegroundColor Cyan

$outputFiles = @(
    "Assignment_Summary.html",
    "Assignment_Summary.docx", 
    "Assignment_Summary.md",
    "..\practical1_actual_results.rds"
)

$figureFiles = @(
    "..\figures\part1a_discharge_histogram.png",
    "..\figures\part1a_discharge_qqplot.png",
    "..\figures\part2b_ccf.png",
    "..\figures\part3a_acf_raw.png"
)

Write-Host "Report files:" -ForegroundColor Yellow
foreach ($file in $outputFiles) {
    if (Test-Path $file) {
        $size = [math]::Round((Get-Item $file).Length / 1KB, 1)
        Write-Host "  ✓ $file ($size KB)" -ForegroundColor Green
    } else {
        Write-Host "  ✗ $file (not found)" -ForegroundColor Red
    }
}

Write-Host "`nFigure files:" -ForegroundColor Yellow
foreach ($file in $figureFiles) {
    if (Test-Path $file) {
        Write-Host "  ✓ $file" -ForegroundColor Green
    } else {
        Write-Host "  ✗ $file (not found)" -ForegroundColor Red
    }
}

# Summary
Write-Host "`n=============================================" -ForegroundColor Green
Write-Host "ASSIGNMENT EXECUTION SUMMARY" -ForegroundColor Green
Write-Host "=============================================" -ForegroundColor Green

if (Test-Path "Assignment_Summary.html") {
    Write-Host "✓ SUCCESS: Assignment solutions completed!" -ForegroundColor Green
    Write-Host "" 
    Write-Host "Key deliverables:" -ForegroundColor Yellow
    Write-Host "  • Part 1: Distribution testing & Anderson-Darling test" -ForegroundColor White
    Write-Host "  • Part 2: CCF, extremograms, Granger causality tests" -ForegroundColor White  
    Write-Host "  • Part 3: ARIMA/GARCH time series modeling" -ForegroundColor White
    Write-Host ""
    Write-Host "Main report file: Assignment_Summary.html" -ForegroundColor Cyan
    Write-Host "Word version: Assignment_Summary.docx" -ForegroundColor Cyan
    Write-Host "Markdown version: Assignment_Summary.md" -ForegroundColor Cyan
} else {
    Write-Host "✗ FAILED: Could not generate assignment reports" -ForegroundColor Red
    Write-Host "Please check the error messages above" -ForegroundColor Yellow
}

Write-Host "`nExecution completed." -ForegroundColor White