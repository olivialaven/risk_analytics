# Risk Analytics Practical 1 - Assignment Solutions

## 🎯 Assignment Implementation

This folder contains the **correct solutions for the assignment requirements**, focusing on distribution testing, causality analysis, and time series modeling rather than traditional extreme value analysis. 

## 📋 What Changed

After discovering the real assignment requirements from `Practical1.md`, I completely rewrote the analysis to focus on:

### Part 1: Statistical assumptions for modeling extremes
- ✅ Visual distribution assessment (histograms, Q-Q plots)
- ✅ Anderson-Darling test for normality
- ✅ Alternative distribution fitting (gamma, log-normal, Weibull, exponential)
- ✅ Tail probability comparison and interpretation

### Part 2: Correlation versus causation  
- ✅ Pearson correlation test
- ✅ Cross-correlation function (CCF) analysis
- ✅ Extremograms for extreme event clustering
- ✅ Granger causality tests (both directions)
- ✅ Extreme causality testing with JuroExtremes
- ✅ Predictive relationship insights

### Part 3: Time series modeling, heteroscedasticity, and weather-driven volatility
- ✅ Autocorrelation pattern analysis (ACF/PACF)
- ✅ Ljung-Box test for serial dependence
- ✅ ARIMA modeling with automatic selection
- ✅ GARCH modeling (Normal and Student-t)
- ✅ Two-step ARIMA+GARCH approach
- ✅ Model comparison and conclusions

## 📁 Key Files

### Analysis Scripts
- **`Practical1_solutions.R`** - Complete R analysis implementing all three parts
- **`render_practical1.R`** - Report rendering script
- **`run_assignment.ps1`** - PowerShell automation script

### Reports
- **`Assignment_Summary.html`** - Executive summary with key findings (756 KB)
- **`Assignment_Summary.docx`** - Word version for submission (13 KB)
- **`Combined_Report.Rmd`** - Detailed technical report (source)

### Supporting Files
- **`Assignment_Summary.Rmd`** - Source for summary report
- **`README.md`** - This file

## 🔬 Key Findings

### Statistical Distribution
- River discharge data **strongly rejects normality** (Anderson-Darling p < 2.2e-16)
- **Log-normal distribution** provides best fit (AIC = 44,315)
- Tail probabilities differ by 7% between normal and log-normal assumptions

### Causality Analysis
- Very weak linear correlation (r = 0.012, p = 0.295)
- **Strong lagged relationship**: precipitation leads discharge by 2 days (r = 0.41)
- **Granger causality confirmed**: precipitation → discharge (p < 2.2e-16)
- No reverse causality: discharge ↛ precipitation (p = 0.98)

### Time Series Modeling
- Strong serial dependence in raw series, reduced after differencing
- ARIMA models capture linear dependencies
- GARCH models reveal time-varying volatility patterns
- Combined ARIMA+GARCH provides comprehensive framework

## 🏃‍♂️ How to Run

### Option 1: PowerShell (Full Automation)
```powershell
# From the JJs_take directory:
powershell -ExecutionPolicy Bypass -File run_assignment.ps1
```

### Option 2: R Scripts Directly
```powershell
# Run analysis
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" Practical1_solutions.R

# Render reports  
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" render_practical1.R
```

### Option 3: Summary Report Only
```powershell
# Quick summary report
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "rmarkdown::render('Assignment_Summary.Rmd')"
```

## 📊 Generated Outputs

### Figures (in `../figures/`)
- `part1a_discharge_histogram.png` - Distribution histogram
- `part1a_discharge_qqplot.png` - Normal Q-Q plot
- `part1c_qq_comparison.png` - Distribution comparison
- `part1d_density_comparison.png` - Density overlays
- `part2b_ccf.png` - Cross-correlation function
- `part2c_extremograms.png` - Extreme event clustering
- `part3a_acf_raw.png` - Raw series autocorrelation
- `part3a_acf_diff.png` - Differenced series autocorrelation
- `part3c_pacf_diff.png` - Partial autocorrelation
- `part3c_arima_residuals.png` - ARIMA residual plots

### Data Files
- `../practical1_actual_results.rds` - Complete R analysis results

## ✅ Validation

This implementation correctly addresses the **assignment requirements**:

1. **NOT** traditional extreme value analysis (GEV/POT models)
2. **YES** distribution testing with Anderson-Darling
3. **YES** correlation vs causation analysis with CCF and Granger tests
4. **YES** time series modeling with ARIMA/GARCH approaches

## 📋 Assignment Correspondence

| Assignment Section | Implementation | Status |
|-------------------|----------------|---------|
| Part 0: Data exploration | `Practical1_solutions.R` lines 60-95 | ✅ Complete |
| Part 1: Distribution testing | Lines 97-257 | ✅ Complete |
| Part 2: Correlation/causation | Lines 259-453 | ✅ Complete |
| Part 3: Time series modeling | Lines 455-617 | ✅ Complete |

## 🎯 Bottom Line

**The assignment has been correctly implemented.** The analysis focuses on distribution testing, causality analysis, and time series modeling as specified in the requirements.

**Main deliverable**: `Assignment_Summary.html` - Contains executive summary with all key findings and technical details.

---
*Updated: December 2024 - JJ's Take*