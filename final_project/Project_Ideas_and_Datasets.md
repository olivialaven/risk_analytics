# Risk Analytics Final Project Ideas & Datasets

## Overview
Based on your Practical 1 experience with river discharge modeling (ARIMA, GARCH, extreme value analysis, causality testing), here are several project ideas that apply similar risk analytics tools to different domains.

---

## Project Idea 1: Energy Demand Risk and Extreme Temperature Events ⚡

### Topic
Modeling electricity demand volatility and extreme consumption events during heatwaves/cold snaps.

### Datasets (Kaggle)
1. **Hourly Energy Consumption Data**
   - URL: `https://www.kaggle.com/datasets/robikscube/hourly-energy-consumption`
   - Contains: Hourly electricity consumption for multiple US regions (2004-2018)
   - Size: ~50,000 hourly observations per region

2. **Weather Data (combine with energy)**
   - URL: `https://www.kaggle.com/datasets/selfishgene/historical-hourly-weather-data`
   - Contains: Temperature, humidity, pressure, wind speed
   - Can link temperature extremes to energy consumption spikes

### Risk Analytics Methods to Apply
1. **Time Series Analysis:**
   - ARIMA modeling for baseline consumption patterns
   - Seasonal decomposition (daily, weekly, yearly cycles)
   - ACF/PACF analysis

2. **Volatility Modeling:**
   - GARCH(1,1) for consumption volatility
   - Compare Normal vs Student-t distributions
   - Two-step ARIMA+GARCH

3. **Extreme Value Analysis:**
   - POT (Peaks Over Threshold) for consumption spikes
   - Return period estimation for grid stress events
   - GEV modeling for annual maximum demand

4. **Causality Testing:**
   - Granger causality: temperature → electricity demand
   - Extremogram analysis: extreme heat → extreme demand
   - Lag structure identification

### Research Questions
- How does electricity demand volatility change with temperature extremes?
- What is the return period for grid-stress events (demand > 95th percentile)?
- Do extreme temperature events have persistent effects on demand volatility?
- Can we forecast peak demand during heatwaves for grid management?

### Practical Applications
- Grid capacity planning and risk management
- Energy derivative pricing (weather-linked contracts)
- Emergency response planning for utilities
- Infrastructure investment decisions

---

## Project Idea 2: Cryptocurrency Volatility and Crash Risk 💰

### Topic
Modeling Bitcoin/Ethereum price volatility, extreme losses, and contagion between cryptocurrencies.

### Datasets (Kaggle)
1. **Cryptocurrency Historical Prices**
   - URL: `https://www.kaggle.com/datasets/sudalairajkumar/cryptocurrencypricehistory`
   - Contains: Daily/hourly prices for 10+ cryptocurrencies
   - Period: 2013-present

2. **Bitcoin Historical Data**
   - URL: `https://www.kaggle.com/datasets/mczielinski/bitcoin-historical-data`
   - Minute-level Bitcoin data
   - Can aggregate to hourly/daily

### Risk Analytics Methods to Apply
1. **Volatility Clustering:**
   - GARCH(1,1) with Student-t innovations
   - EGARCH (asymmetric effects: crashes vs rallies)
   - Persistence analysis (α + β)

2. **Extreme Value Theory:**
   - VaR (Value at Risk) estimation
   - Expected Shortfall (CVaR) for tail risk
   - POT modeling for extreme losses

3. **Contagion Analysis:**
   - Bivariate extremogram between BTC and ETH
   - Copula modeling for tail dependence
   - Does a Bitcoin crash predict Ethereum crashes?

4. **Two-Step Modeling:**
   - ARIMA+GARCH for log returns
   - Residual diagnostics and model comparison
   - Heavy-tailed distributions essential

### Research Questions
- Is cryptocurrency volatility more persistent than traditional assets?
- What is the 1-day 99% VaR for Bitcoin?
- Do crashes in Bitcoin cause crashes in altcoins (extremogram)?
- Are crypto returns Student-t distributed, or even heavier-tailed?

### Practical Applications
- Portfolio risk management for crypto investors
- Margin requirement setting for exchanges
- Regulatory capital requirements
- Insurance/hedging products for crypto exposure

---

## Project Idea 3: Earthquake Risk and Aftershock Clustering 🌍

### Topic
Modeling earthquake magnitude distributions, aftershock sequences, and temporal clustering.

### Datasets (Kaggle)
1. **USGS Earthquake Database**
   - URL: `https://www.kaggle.com/datasets/usgs/earthquake-database`
   - Contains: Global earthquakes (1965-2016), magnitude, location, depth
   - ~23,000 significant earthquakes

2. **Alternative: Real-time USGS Data**
   - Can download recent data from USGS API
   - More current than Kaggle dataset

### Risk Analytics Methods to Apply
1. **Extreme Value Analysis:**
   - GEV fitting for annual maximum magnitude
   - Return period estimation (100-year earthquake)
   - Gutenberg-Richter law (frequency-magnitude)

2. **Temporal Clustering:**
   - Extremogram for aftershock sequences
   - Hawkes process for self-exciting events
   - ACF/PACF of earthquake counts

3. **Spatial-Temporal Models:**
   - ETAS (Epidemic Type Aftershock Sequence) model
   - Time-varying seismicity rates
   - Volatility clustering in seismic activity

4. **Threshold Modeling:**
   - POT for earthquakes > magnitude 5.0
   - Exceedance probability estimation
   - Conditional probability of aftershocks given mainshock

### Research Questions
- What is the 100-year return period earthquake magnitude for California?
- Do large earthquakes trigger clustering in subsequent seismicity?
- Can we model aftershock decay rates using ARIMA or exponential models?
- Are earthquake magnitudes heavy-tailed beyond the exponential (GR law)?

### Practical Applications
- Building code design (return period magnitudes)
- Insurance pricing for earthquake coverage
- Emergency response planning
- Infrastructure resilience assessment

---

## Project Idea 4: Wildfire Risk and Climate Extremes 🔥

### Topic
Modeling wildfire occurrence, burned area, and relationship to temperature/drought extremes.

### Datasets (Kaggle/Public Sources)
1. **US Wildfires (1992-2015)**
   - URL: `https://www.kaggle.com/datasets/rtatman/188-million-us-wildfires`
   - Contains: 1.88M wildfires, location, size, duration
   - Can aggregate to monthly/annual burned area

2. **Climate Data (NOAA/Kaggle)**
   - Temperature, precipitation, drought indices
   - Can link to wildfire temporal patterns

### Risk Analytics Methods to Apply
1. **Count Data Modeling:**
   - Poisson/Negative Binomial for monthly wildfire counts
   - ARIMA for time series of fire occurrence
   - Seasonal patterns (summer fire season)

2. **Extreme Value Analysis:**
   - GEV for annual maximum burned area
   - POT for large fire events (> 1000 acres)
   - Return period estimation for catastrophic fires

3. **Causality:**
   - Extremogram: extreme temperature → extreme fire activity
   - Granger causality with drought indices
   - Lag effects (drought today → fires in 1-3 months)

4. **Volatility Modeling:**
   - GARCH for variance in monthly burned area
   - Persistence of high-risk periods
   - Time-varying fire risk

### Research Questions
- What is the 50-year return period for annual burned area?
- Do extreme temperature/drought events predict fire occurrence?
- Is wildfire risk increasing over time (trend analysis)?
- Can we model fire season intensity with GARCH?

### Practical Applications
- Insurance pricing for wildfire coverage
- Forest management and prescribed burn planning
- Evacuation planning and resource allocation
- Climate change adaptation strategies

---

## Project Idea 5: Stock Market Crash Risk and VIX Volatility 📉

### Topic
Modeling equity index volatility, crash risk, and the VIX (fear index).

### Datasets (Kaggle)
1. **S&P 500 Historical Data**
   - URL: `https://www.kaggle.com/datasets/camnugent/sandp500`
   - Daily prices for S&P 500 (1986-2018)

2. **VIX (Volatility Index)**
   - URL: Various sources on Kaggle or Yahoo Finance
   - Measure of market fear/volatility

### Risk Analytics Methods to Apply
1. **GARCH Modeling:**
   - EGARCH for asymmetric volatility (leverage effect)
   - GJR-GARCH (crashes increase volatility more than rallies)
   - Student-t vs normal innovations

2. **Extreme Value Analysis:**
   - VaR and CVaR for portfolio risk
   - POT for extreme losses (< -3% daily returns)
   - Black Monday 1987, 2008 crisis, COVID crash

3. **Volatility Forecasting:**
   - ARIMA+GARCH for log returns
   - Realized volatility models
   - Compare forecasts to VIX

4. **Tail Risk:**
   - Heavy-tailed distributions (Student-t, GED)
   - Kurtosis and skewness analysis
   - Crisis vs normal period comparison

### Research Questions
- What is the 1-day 99% VaR for the S&P 500?
- Are crash days predictable from volatility clustering?
- How much heavier are equity return tails vs normal?
- Does the VIX predict future realized volatility?

### Practical Applications
- Portfolio risk management
- Options pricing (volatility surface)
- Hedge fund strategy (volatility trading)
- Regulatory capital (Basel III)

---

## Project Idea 6: Pandemic Risk and Hospital Surge Capacity 🏥

### Topic
Modeling COVID-19 case surges, hospital capacity risk, and predictive modeling.

### Datasets (Kaggle)
1. **COVID-19 Data Repository**
   - URL: `https://www.kaggle.com/datasets/sudalairajkumar/novel-corona-virus-2019-dataset`
   - Daily cases, deaths, hospitalizations by region

2. **Our World in Data COVID-19**
   - URL: `https://www.kaggle.com/datasets/datasets/covid-19-pandemic-data`
   - Comprehensive global data with vaccination, testing

### Risk Analytics Methods to Apply
1. **Time Series Analysis:**
   - ARIMA for case counts/growth rates
   - Seasonal patterns (winter waves)
   - Trend analysis

2. **Extreme Value Analysis:**
   - POT for hospital surge events
   - Return period for ICU capacity exceedance
   - Wave peak modeling (GEV)

3. **Volatility Clustering:**
   - GARCH for case count variability
   - Persistence in epidemic activity
   - Post-intervention effect (lockdowns)

4. **Change Point Detection:**
   - Structural breaks (variant emergence)
   - Policy intervention effects
   - Vaccination impact

### Research Questions
- What is the 90-day return period for ICU capacity exceedance?
- Do case surges show volatility clustering?
- Can we forecast the next wave using ARIMA+GARCH?
- How do interventions affect case volatility?

### Practical Applications
- Hospital resource planning
- Public health policy (when to implement restrictions)
- Vaccine allocation strategies
- Pandemic insurance and preparedness

---

## Recommended Project Structure (Similar to Practical 1)

### Part 1: Exploratory Data Analysis
- Time series plots
- Summary statistics
- ACF/PACF plots
- Identification of extreme events

### Part 2: Extreme Value Analysis
- Threshold selection (POT) or block maxima (GEV)
- Return period estimation
- Tail behavior analysis
- Extremogram (if applicable for causality)

### Part 3: Time Series Modeling
- **Section 3a:** Data transformation and differencing
- **Section 3b:** ACF/PACF analysis
- **Section 3c:** ARIMA modeling
- **Section 3d:** GARCH modeling (Normal vs Student-t)
- **Section 3e:** Two-step ARIMA+GARCH
- **Section 3f:** Model comparison and conclusion

### Part 4: Risk Metrics and Applications
- VaR/CVaR estimation (if financial)
- Return periods for extreme events
- Forecasting and uncertainty quantification
- Policy/management recommendations

### Part 5: Conclusion
- Key findings
- Practical implications
- Model limitations
- Future research directions

---

## Dataset Selection Criteria

### Good Datasets Should Have:
1. **Sufficient length:** At least 1000+ observations for ARIMA/GARCH
2. **Temporal structure:** Regular intervals (daily, hourly, monthly)
3. **Extreme events:** Clear outliers/extremes to analyze
4. **Volatility clustering:** Periods of high/low variability
5. **Practical relevance:** Real-world risk management applications

### Red Flags to Avoid:
- Too short (< 500 observations)
- Irregular intervals or missing data
- No clear extremes or risk events
- Already heavily analyzed (be original)

---

## My Top 3 Recommendations

### 1. **Energy Demand + Temperature** (Easiest to implement)
   - Very similar structure to your river discharge project
   - Clear extreme events (heatwaves)
   - Strong causality story
   - Excellent data quality

### 2. **Cryptocurrency Volatility** (Most relevant for finance students)
   - High volatility and extreme events
   - GARCH is essential (like your project)
   - Cutting-edge topic
   - Good for portfolios/interviews

### 3. **Earthquake Risk** (Most novel/unique)
   - Strong extreme value story
   - Temporal clustering (extremogram)
   - Practical importance
   - Less commonly done by students

---

## Next Steps

1. **Choose a dataset** from the options above
2. **Download and explore** the data (descriptive stats, plots)
3. **Define research questions** (2-3 specific questions)
4. **Adapt your Practical 1 code** to the new dataset
5. **Write up results** in report format (like your Section 3d/3e/3f docs)

Let me know which project idea interests you most, and I can help you:
- Download and prepare the data
- Adapt your R code from Practical 1
- Define specific research questions
- Structure the analysis and report

---

*Document created: October 27, 2025*
*For: Risk Analytics Final Project*
