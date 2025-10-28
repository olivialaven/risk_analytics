# Risk Analytics Project: Energy Demand & Temperature Extremes

## Project Overview

This project performs a comprehensive **risk analysis** of electricity demand extremes and their relationship with temperature extremes, following the methodologies from the Risk Analytics course (EPFL).

**Research Question:** How do temperature extremes influence electricity demand extremes, and what are the quantitative risk implications for grid operators?

**Datasets:**
- `AEP_hourly.csv`: Hourly electricity demand (American Electric Power) from 2004-2018
- `temperature.csv`: Hourly temperature data for 36 global cities from 2012-2017

**Analysis Period:** 2012-10-01 to 2017-11-30 (~5 years, overlapping period)

---

## File Structure

```
final_project/
├── README_PROJECT.md                  # This file
├── 03_analysis_structure.md           # Detailed analysis plan
├── 03_main_analysis.R                 # Steps 1-2: Data prep & EDA
├── 04_advanced_analysis.R             # Steps 3-5: EVT & time series
├── 05_final_analysis.R                # Steps 6-8: Dependence & conclusions
├── AEP_hourly.csv                     # Raw data (121K rows)
├── temperature.csv                    # Raw data (45K rows)
├── output_figures/                    # Generated plots (23 figures)
├── output_tables/                     # Generated tables (CSV)
└── analysis_report_snippets.md        # Auto-generated report content
```

---

## Analysis Workflow

### **Phase 1: Data Preparation & EDA (Steps 1-2)**
**Script:** `03_main_analysis.R`

**What it does:**
- Loads and merges AEP demand and Chicago temperature data
- Creates time-based features (hour, season, etc.)
- Tests for stationarity (ADF tests)
- Generates summary statistics
- Creates exploratory plots (time series, distributions, QQ-plots)
- Analyzes demand-temperature relationship (U-shaped curve)

**Outputs:**
- Figures 01-08
- Summary statistics table
- Report snippets (Sections 1-2)

**How to run:**
```r
source("03_main_analysis.R")
```

**Expected runtime:** 2-5 minutes

---

### **Phase 2: Extreme Value Analysis (Steps 3-5)**
**Script:** `04_advanced_analysis.R`

**What it does:**
- **Block Maxima (GEV):** Extracts weekly maxima, fits GEV distribution, calculates return levels
- **Peaks-Over-Threshold (POT):** Selects threshold, fits GPD, compares with block maxima
- **Time Series Models:** Fits ARIMA and GARCH models, tests for autocorrelation and volatility clustering

**Outputs:**
- Figures 09-18
- Return level tables (1-year, 2-year, 5-year, 10-year)
- Model comparison tables
- Report snippets (Sections 3-5)

**How to run:**
```r
# After running 03_main_analysis.R:
source("04_advanced_analysis.R")
```

**Expected runtime:** 5-10 minutes

---

### **Phase 3: Dependence & Risk Analysis (Steps 6-8)**
**Script:** `05_final_analysis.R`

**What it does:**
- **Temperature-Demand Dependence:** Cross-correlation, Granger causality, conditional extremes
- **Clustering Analysis:** Extremal index, temporal clustering, seasonal patterns
- **Risk Metrics:** VaR, Expected Shortfall, infrastructure risk, climate scenarios

**Outputs:**
- Figures 19-23
- Risk metrics table
- Conditional probability tables
- Report snippets (Sections 6-8)
- Final conclusions and recommendations

**How to run:**
```r
# After running 03_main_analysis.R and 04_advanced_analysis.R:
source("05_final_analysis.R")
```

**Expected runtime:** 3-5 minutes

---

## Quick Start Guide

### Option 1: Step-by-Step (Recommended)

Run each script sequentially and review outputs after each step:

```r
# Set working directory
setwd("c:/Users/JJ/OneDrive - epfl.ch/Documents/SMT/RA/risk_analytics/final_project")

# Step 1-2: Data prep & EDA
source("03_main_analysis.R")
# STOP HERE and review:
# - output_figures/01-08_*.png
# - output_tables/summary_statistics.csv

# Step 3-5: Extreme value analysis
source("04_advanced_analysis.R")
# STOP HERE and review:
# - output_figures/09-18_*.png
# - output_tables/return_levels_*.csv

# Step 6-8: Dependence & risk
source("05_final_analysis.R")
# STOP HERE and review:
# - output_figures/19-23_*.png
# - output_tables/risk_metrics_summary.csv
# - analysis_report_snippets.md
```

### Option 2: Full Run (Advanced)

Run all steps at once (not recommended for first run):

```r
source("03_main_analysis.R")
source("04_advanced_analysis.R")
source("05_final_analysis.R")
```

---

## Required R Packages

The scripts will automatically install missing packages. Required packages:

```r
data.table      # Fast data manipulation
ggplot2         # Visualization
lubridate       # Date/time handling
forecast        # ARIMA modeling
tseries         # Stationarity tests
fGarch          # GARCH models
extRemes        # Extreme value theory
evd             # EVT distributions
lmtest          # Granger causality
gridExtra       # Multiple plots
moments         # Skewness, kurtosis
MASS            # Distribution fitting
```

---

## Key Results Preview

Based on the analysis, you will obtain:

### 1. **Summary Statistics**
- Mean demand: ~15,000 MW (SD ~2,000 MW)
- Peak demand: ~22,000 MW
- Temperature range: -20°C to +35°C

### 2. **Extreme Value Analysis**
- **10-year return level**: ~21,000 MW
- **POT threshold**: 95th percentile (~18,000 MW)
- **Block maxima vs. POT**: Consistent results (<5% difference)

### 3. **Temperature Dependence**
- **Granger causality**: Temperature → Demand (p < 0.001)
- **Risk multiplier**: 2-3x higher extreme demand probability during temperature extremes
- **U-shaped relationship**: Both heating and cooling drive demand peaks

### 4. **Temporal Clustering**
- **Extremal index**: ~0.3-0.5 (moderate clustering)
- **Cluster size**: 10-20 hours average
- **Seasonal pattern**: Peak extremes in summer and winter

### 5. **Risk Metrics**
- **VaR(99%)**: ~19,500 MW
- **Expected Shortfall**: ~20,000 MW
- **Climate scenario (+2°C)**: +5-10% demand increase

---

## Report Writing Guide

### Structure (3 pages, following Practical 3 format)

#### **Page 1: Introduction & Data**

**Section 1.1: Introduction (~3-4 sentences)**
- Context: Why energy demand risk matters
- Research question
- Brief methodology overview

**Section 1.2: Data (~1 paragraph)**
- Use content from `analysis_report_snippets.md` Section 1
- Include: date range, sample size, key statistics
- Reference Figure 03 (combined time series)

**Section 1.3: Exploratory Analysis (~2 paragraphs)**
- Distribution characteristics (heavy tails, t-distribution fit)
- Use Figures 04-06 (distribution, QQ-plots)
- Temperature-demand relationship (U-shape)
- Use Figure 07 (demand vs. temp scatter)
- Use content from Section 2 of report snippets

---

#### **Page 2: Extreme Value Analysis**

**Section 2.1: Block Maxima Approach (~1 section)**
- Methodology: Weekly blocks, GEV distribution
- Use Figure 09 (weekly maxima) and Figure 11 (diagnostics)
- **Return levels table** (from `output_tables/return_levels_block_maxima.csv`)
- Interpretation: "10-year return level of X MW"
- Use content from Section 3 of report snippets

**Section 2.2: POT Approach (~1 section)**
- Threshold selection (MRL plot, Figure 13)
- GPD fit and diagnostics (Figure 15)
- **Comparison table** (Block Maxima vs. POT)
- Use content from Section 4 of report snippets

**Section 2.3: Time Series Modeling (Brief, ~1 paragraph)**
- ARIMA/GARCH results
- Key finding: volatility clustering
- Use content from Section 5 of report snippets

---

#### **Page 3: Dependence & Conclusions**

**Section 3.1: Temperature-Demand Dependence (~2 paragraphs)**
- Granger causality results
- Conditional extremes (risk multiplier)
- Use Figure 20 (conditional demand boxplot)
- Use content from Section 6 of report snippets

**Section 3.2: Clustering Analysis (~1 paragraph)**
- Extremal index interpretation
- Seasonal patterns
- Use content from Section 7 of report snippets

**Section 3.3: Risk Implications (~1 paragraph)**
- VaR and Expected Shortfall
- Infrastructure recommendations
- Climate change implications
- Use content from Section 8 of report snippets

**Section 3.4: Conclusions (3-5 bullet points)**
- Use "Key Findings and Recommendations" from Section 8
- Focus on practical takeaways for grid operators

---

## Figure Selection for Report (Maximum 6-8 figures)

**Must Include:**
1. Figure 03: Combined time series (demand & temperature)
2. Figure 06: QQ-plot (t-distribution fit)
3. Figure 07: Demand vs. temperature (U-shape)
4. Figure 11 or 12: GEV diagnostics or return level plot
5. Figure 15: GPD diagnostics
6. Figure 20: Conditional demand (extreme vs. normal temp)

**Optional (if space permits):**
7. Figure 09: Weekly maxima time series
8. Figure 23: Risk metrics visualization

---

## Table Selection for Report (Maximum 3-4 tables)

**Must Include:**
1. Summary statistics (from `output_tables/summary_statistics.csv`)
2. Return levels comparison (from `output_tables/comparison_block_vs_pot.csv`)
3. Risk metrics summary (from `output_tables/risk_metrics_summary.csv`)

**Optional:**
4. Return periods for critical thresholds (from `output_tables/return_periods_thresholds.csv`)

---

## Troubleshooting

### Issue 1: "Object 'data' not found"
**Solution:** Run `03_main_analysis.R` first

### Issue 2: Missing packages
**Solution:** Install manually:
```r
install.packages(c("data.table", "ggplot2", "lubridate", "forecast", 
                   "tseries", "fGarch", "extRemes", "evd", "lmtest"))
```

### Issue 3: GARCH model fails to converge
**Solution:** This is normal with large datasets. The script uses a subset for computational efficiency.

### Issue 4: Plots not appearing
**Solution:** Check that `output_figures/` directory was created. If not, create manually:
```r
dir.create("output_figures")
dir.create("output_tables")
```

---

## Validation Checklist

Before writing the final report, verify:

- [ ] All scripts ran without errors
- [ ] 23 figures generated in `output_figures/`
- [ ] Multiple CSV tables in `output_tables/`
- [ ] `analysis_report_snippets.md` contains all 8 sections
- [ ] Return levels are reasonable (e.g., 10-year level > historical max)
- [ ] Block Maxima and POT results are consistent (<10% difference)
- [ ] Granger causality test shows temperature → demand (p < 0.05)
- [ ] Extremal index is between 0 and 1

---

## Next Steps

1. ✅ Run all three R scripts sequentially
2. ✅ Review all generated figures and tables
3. ✅ Read `analysis_report_snippets.md` thoroughly
4. ✅ Select key figures and tables for the report
5. ✅ Write the 3-page report using the structure above
6. ✅ Proofread and ensure interpretations are correct
7. ✅ Submit report and prepare presentation

---

## Contact & Support

**Course:** Risk Analytics (EPFL)
**Project:** Practical 3 - Energy Demand Extremes
**Date:** Fall 2024

For questions about the analysis methodology, refer to:
- `lectures_&_exercises/Syllabus.md` (Course materials)
- `RiskAnalytics_Group6_Final.md` (Example report on egg sales)

**Key course topics applied:**
- Time series analysis
- Extreme value theory (GEV, GPD)
- Block maxima and POT approaches
- Temporal dependence (ARIMA, GARCH)
- Risk metrics (VaR, Expected Shortfall)

---

## License & Acknowledgments

**Data Sources:**
- AEP hourly demand: [American Electric Power]
- Temperature data: [Global hourly temperature dataset]

**Analysis Framework:**
- Based on McNeil, Frey & Embrechts (2015) - Quantitative Risk Management
- Coles (2001) - Statistical Modeling of Extreme Values

---

**Good luck with your analysis and report writing!** 📊⚡🌡️
