# Risk Analytics Project: Energy Demand & Temperature Extremes Analysis

## Analysis Structure (Based on Course Syllabus & Practical 3 Format)

---

## Overview
This analysis examines extreme events in electricity demand (AEP) and their relationship with temperature extremes. The goal is to quantify risks associated with extreme energy demand events, which are critical for:
- Grid infrastructure planning
- Risk management for utilities
- Understanding climate-energy nexus
- Capacity planning and investment decisions

---

## Data Description

### Available Data:
1. **AEP_hourly.csv**: Hourly electricity demand (MW) from 2004-12-31 to 2018-01-02
2. **temperature.csv**: Hourly temperature (Kelvin) for 36 global cities from 2012-10-01 to 2017-11-30

### Data Selection Strategy:
- **Time window**: 2012-10-01 to 2017-11-30 (overlapping period, ~5 years)
- **Primary city**: Chicago (central US location, representative of AEP service area)
- **Focus**: Extreme DEMAND events (high load) and their relationship with temperature

---

## Analysis Workflow (Step-by-Step)

### **STEP 1: Data Preparation & Exploration**
**Objective**: Load, clean, and understand the data structure

**Tasks**:
1.1. Load both datasets and merge on datetime
1.2. Convert temperature from Kelvin to Celsius
1.3. Handle missing values
1.4. Create time-based features (hour, day of week, month, season)
1.5. Test for stationarity (ADF test)
1.6. Create transformed variables if needed (e.g., demand changes, detrending)

**Output for Report**:
- Summary statistics table
- Time range confirmation
- Missing data summary
- Basic time series plots (demand over time, temperature over time)

---

### **STEP 2: Exploratory Data Analysis (EDA)**
**Objective**: Visualize patterns, trends, and identify extreme events

**Tasks**:
2.1. Plot full time series for demand and temperature
2.2. Seasonal decomposition (trend, seasonality, residuals)
2.3. Identify peak demand periods
2.4. Distribution analysis (histograms, QQ-plots)
2.5. Test for normality vs. heavy tails (t-distribution fit)
2.6. Correlation analysis (demand vs. temperature)

**Output for Report**:
- Time series plots with annotations
- Seasonal patterns identification
- Distribution comparison (normal vs. t-distribution)
- Scatter plot (demand vs. temperature)
- Key statistics (mean, std, skewness, kurtosis)

---

### **STEP 3: Extreme Value Analysis - Block Maxima Approach**
**Objective**: Model extreme demand using Generalized Extreme Value (GEV) distribution

**Tasks**:
3.1. Extract block maxima (monthly or weekly)
3.2. Fit GEV distribution (constant and time-varying parameters)
3.3. Model comparison (AIC, BIC)
3.4. Diagnostic plots (QQ-plot, return level plot)
3.5. Calculate return levels (1-year, 2-year, 5-year, 10-year)
3.6. Calculate return periods for specific demand thresholds

**Output for Report**:
- Block maxima time series plot
- GEV diagnostic plots
- Table: Return levels for different periods
- Table: Return periods for critical thresholds (e.g., 20,000 MW, 22,000 MW)
- Interpretation: "A demand exceeding X MW is expected once every Y years"

---

### **STEP 4: Extreme Value Analysis - Peaks-Over-Threshold (POT) Approach**
**Objective**: Model exceedances over a high threshold using Generalized Pareto Distribution (GPD)

**Tasks**:
4.1. Mean Residual Life plot to select threshold
4.2. Extract exceedances above threshold
4.3. Fit GPD model
4.4. Diagnostic plots (QQ-plot, probability plot, density plot)
4.5. Calculate return levels and periods
4.6. Compare with Block Maxima results

**Output for Report**:
- Mean Residual Life plot with chosen threshold
- Exceedances plot (highlighting extreme events)
- GPD diagnostic plots
- Table: Return levels comparison (Block Maxima vs. POT)
- Discussion: Consistency between methods

---

### **STEP 5: Temporal Dependence & Time Series Modeling**
**Objective**: Analyze autocorrelation, volatility clustering, and model temporal structure

**Tasks**:
5.1. ACF and PACF plots
5.2. Test for autocorrelation (Ljung-Box test)
5.3. Fit ARIMA model (auto.arima for automatic selection)
5.4. Test for heteroscedasticity (ARCH effects)
5.5. Fit GARCH model for volatility clustering
5.6. Compare ARIMA, GARCH, and ARIMA-GARCH models
5.7. Residual diagnostics (normality, autocorrelation)

**Output for Report**:
- ACF/PACF plots
- Table: Model comparison (AIC, BIC, log-likelihood)
- Residual diagnostic plots
- Interpretation: "Demand shows volatility clustering, better captured by GARCH"

---

### **STEP 6: Temperature-Demand Dependence Analysis**
**Objective**: Quantify the relationship between temperature extremes and demand extremes

**Tasks**:
6.1. Scatter plot with loess smoother (demand vs. temperature)
6.2. Identify non-linear relationship (U-shaped or V-shaped)
6.3. Granger causality test (temperature → demand)
6.4. Cross-correlation function
6.5. Conditional extreme analysis (demand|extreme temperature)
6.6. Calculate joint probabilities

**Output for Report**:
- Scatter plot showing U-shaped relationship (heating & cooling demands)
- Cross-correlation plot
- Granger causality test results
- Table: Extreme demand probability conditional on extreme temperature
- Key finding: "Temperature extremes (both hot and cold) significantly predict demand extremes"

---

### **STEP 7: Clustering of Extreme Events**
**Objective**: Assess whether extreme demand events occur in clusters

**Tasks**:
7.1. Calculate extremal index
7.2. Identify clusters of exceedances
7.3. Decluster the data
7.4. Compare clustered vs. declustered results
7.5. Analyze seasonal clustering (e.g., summer vs. winter)

**Output for Report**:
- Extremal index interpretation
- Average cluster size
- Probability of consecutive extreme events
- Seasonal analysis (if relevant)
- Declustered diagnostic plots

---

### **STEP 8: Risk Metrics & Practical Implications**
**Objective**: Translate statistical results into actionable risk insights

**Tasks**:
8.1. Calculate Value-at-Risk (VaR) for demand
8.2. Expected shortfall (conditional VaR)
8.3. Probability of exceeding critical infrastructure thresholds
8.4. Impact of climate scenarios (what if temperatures increase by 2°C?)
8.5. Cost-benefit analysis framework (reserves vs. blackout costs)

**Output for Report**:
- Table: Risk metrics (VaR, Expected Shortfall)
- Practical interpretation for grid operators
- Policy recommendations
- Climate change implications

---

## Report Structure (3 pages, following Practical 3 format)

### Page 1: Introduction & Data
- Context: Why energy demand risk matters
- Data description (1 paragraph)
- EDA highlights (2-3 key plots)
- Distribution analysis (normal vs. t-distribution)

### Page 2: Extreme Value Analysis
- Block Maxima approach (GEV) - 1 section
  - Return level table
  - Key plot
- POT approach (GPD) - 1 section
  - Threshold selection
  - Return level comparison
- Brief model comparison

### Page 3: Dependence & Conclusions
- Time series modeling (ARIMA/GARCH) - brief
- Temperature-demand relationship (Granger causality, cross-correlation)
- Clustering analysis
- Key findings and risk implications (3-4 bullet points)
- Practical recommendations

---

## Required R Packages
```r
library(data.table)      # Fast data manipulation
library(ggplot2)         # Visualization
library(lubridate)       # Date handling
library(forecast)        # ARIMA modeling
library(tseries)         # Time series tests
library(fGarch)          # GARCH models
library(extRemes)        # Extreme value analysis
library(evd)             # EVT distributions
library(lmtest)          # Granger causality
library(gridExtra)       # Multiple plots
```

---

## Key Questions to Answer
1. What is the return level for extreme demand events (e.g., 5-year, 10-year)?
2. How often should we expect demand to exceed critical thresholds?
3. Does extreme temperature predict extreme demand (lead-lag relationship)?
4. Do extreme demand events cluster in time?
5. What are the risk implications for grid operators?

---

## Success Criteria
✓ All steps produce valid outputs
✓ Results are consistent across methods (Block Maxima vs. POT)
✓ Models pass diagnostic tests
✓ Report snippets are ready for each section
✓ Visualizations are publication-quality
✓ Interpretations are clear and actionable

