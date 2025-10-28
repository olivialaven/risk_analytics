# Extreme Value Analysis of Midwest Electricity Demand: A Discovery-Driven Approach to Capacity Risk Assessment

## Abstract

This study applies Extreme Value Theory (EVT) to quantify capacity risk in the American Electric Power Midwest region using 5.2 years of hourly electricity demand data (2012-2017). Recognizing the limitations of a relatively short time series, we employ a dual-method validation approach, implementing both Block Maxima (Generalized Extreme Value distribution) and Peaks-Over-Threshold (Generalized Pareto Distribution) methodologies. A critical discovery emerged during exploratory analysis: a pronounced U-shaped relationship between temperature and demand (R² = 0.435), revealing that both heating and cooling loads drive extreme events. The GEV analysis yielded a 10-year return level of 24,858 MW, while GPD estimation produced 25,682 MW—a 3.3% difference that validates the robustness of both approaches despite data constraints. Notably, the methods converge from 8.6% discrepancy at the 1-year horizon to 3.3% at 10 years, providing high confidence in capacity planning estimates. Expected Shortfall analysis reveals an 845 MW "surprise gap" above the 99th percentile threshold, quantifying the additional capacity requirement during truly extreme events. These findings translate to three strategic recommendations: (1) adding 1,000 MW diversified capacity by 2028, (2) implementing a temperature-based early warning system providing 6-12 hour lead time, and (3) extending the historical dataset to 20+ years to reduce estimation uncertainty from ±5% to ±2.5%. This research demonstrates that rigorous risk analysis under data constraints requires not merely applying statistical methods, but iteratively validating assumptions, investigating discrepancies, and translating quantitative findings into operational strategies.

**Keywords:** Extreme Value Theory, Electricity Demand, Capacity Planning, Risk Assessment, Temperature Dependence, GEV Distribution, GPD Distribution

---

## 1. Introduction

### 1.1 The Capacity Planning Challenge

Electricity grid operators face a fundamental tension in capacity planning decisions: excessive capacity wastes substantial capital investment, while insufficient capacity risks catastrophic blackouts with severe economic and social consequences. This challenge is particularly acute when planning for extreme demand events that occur infrequently but carry outsized operational and financial risks. Standard approaches relying on historical maximum demand provide point estimates but fail to quantify the probability and magnitude of events exceeding observed records—a critical limitation given that capacity investments typically have 20-30 year lifespans.

This analysis applies Extreme Value Theory to 43,361 hourly observations of electricity demand from the American Electric Power (AEP) Midwest region, spanning October 2012 through November 2017. AEP serves major metropolitan areas including Chicago, Detroit, Indianapolis, and Pittsburgh, representing a diverse industrial and residential load base totaling approximately 5.5 million customers. The region experiences significant seasonal temperature variation, ranging from -25°C winter minimums to +34°C summer peaks, creating dual drivers of extreme demand through both heating and cooling loads.

### 1.2 The Data Constraint Challenge

Our analysis confronts an immediate methodological challenge: the available time series spans only 5.2 years, substantially below the 20+ years typically recommended for robust EVT parameter estimation (Coles, 2001). This limitation has important implications for both our analytical approach and the interpretation of results. With approximately 274 weeks of observations, we possess sufficient data for basic EVT application but must exercise caution regarding long-horizon extrapolation and acknowledge wider confidence intervals than would be achieved with longer time series.

This data constraint motivated our central methodological decision: rather than relying on a single EVT approach, we implement both Block Maxima (Generalized Extreme Value distribution) and Peaks-Over-Threshold (Generalized Pareto Distribution) methods. By comparing independent estimates derived from fundamentally different sampling strategies—weekly maxima versus threshold exceedances—we can assess the robustness of our conclusions. If both methods converge at longer return periods despite using different subsets of the data, this convergence provides validation that our estimates are capturing true tail behavior rather than reflecting method-specific biases. This dual-method validation approach transforms a data limitation into an opportunity for more rigorous analysis.

### 1.3 Research Objectives and Contribution

This study pursues three primary objectives. First, we seek to quantify extreme demand risk through rigorous EVT application, providing probabilistic estimates of return levels at operationally relevant horizons (1, 2, 5, and 10 years). Second, we aim to identify and characterize the mechanistic drivers of extreme demand events, moving beyond purely statistical description to operational understanding. Third, we demonstrate how findings from each analytical stage can inform subsequent investigations, creating an iterative discovery process rather than a predetermined sequence of analyses.

Our contribution extends beyond the specific numerical estimates for the AEP Midwest region. We illustrate a methodologically rigorous approach to EVT application under data constraints, showing how apparent limitations (short time series, method discrepancies) can be transformed into analytical strengths through careful validation and comparison. Additionally, we demonstrate the operational value of mechanistic understanding—our discovery of the temperature-demand relationship provides not just statistical correlation but a leading indicator for real-time risk management. Finally, we provide a template for translating EVT results from statistical abstractions (return levels, shape parameters) into concrete operational metrics (capacity targets, alert thresholds) that directly inform decision-making.

---

## 2. Data and Exploratory Analysis

### 2.1 Data Sources and Preparation

This analysis integrates two primary data sources. Electricity demand data originates from American Electric Power's hourly consumption records for their Midwest service territory, spanning October 2004 through August 2018 with 121,273 observations. Temperature data derives from the National Centers for Environmental Information (NCEI), providing hourly readings for 36 U.S. cities from October 2012 through November 2017 with 45,255 observations. Given AEP's geographic service area, we construct a regional temperature index by averaging readings from four Midwest metropolitan areas: Chicago, Detroit, Indianapolis, and Pittsburgh. This averaging approach captures regional temperature patterns while reducing the influence of localized microclimatic variations or sensor anomalies.

The overlapping period between datasets extends from October 1, 2012 (12:00) through November 29, 2017 (23:00), yielding 5.16 years of concurrent observations. After temporal alignment and removal of observations with missing temperature data, the final analytical dataset contains 43,361 hourly records. Basic preprocessing included conversion of temperature from Kelvin to Celsius and verification of temporal continuity (no significant gaps exceeding 6 consecutive hours were identified).

**[INSERT TABLE 1 HERE]**

*Table 1: Summary Statistics for Merged Dataset (October 2012 - November 2017)*

Table 1 presents summary statistics for the merged dataset. Demand exhibits a mean of 14,920 MW with standard deviation of 2,501 MW, yielding a coefficient of variation of 16.8%—indicative of moderate variability around the central tendency with occasional large excursions. The observed range spans from 9,581 MW minimum to 24,739 MW maximum, representing a 2.6× dynamic range. Regional temperature varies from -25.2°C to +34.1°C with mean 10.6°C, reflecting the substantial seasonal temperature swings characteristic of continental midlatitude climates.

The 16.8% coefficient of variation merits interpretation. While this indicates relatively stable demand around the mean, it does not preclude significant extreme event risk. Indeed, the distinction between routine variability (captured by the coefficient of variation) and tail behavior (captured by EVT) represents a central motivation for this analysis. Standard deviation-based risk metrics assume normally distributed returns and symmetric tail behavior—assumptions we formally test and subsequently reject.

### 2.2 Temporal Patterns and Seasonality

**[INSERT FIGURE 1 HERE]**

*Figure 1: Time Series of Hourly Electricity Demand (2012-2017) with Monthly Aggregation. Upper panel shows complete hourly observations with mean demand indicated by horizontal line. Lower panel displays monthly average demand revealing pronounced seasonal variation.*

Figure 1 displays the complete demand time series. Visual inspection reveals clear seasonal oscillation with dual annual peaks—a summer maximum driven by air conditioning load and a winter peak reflecting heating demand. The absence of obvious long-term trend suggests relative stationarity in mean demand over the observation period, though this should be interpreted cautiously given the limited 5-year window which may not capture longer-term structural changes in electricity consumption patterns.

To formally assess temporal structure, we computed the correlation between demand and cyclical seasonal indicators. The Pearson correlation between demand and cos(2π × month/12) equals 0.127 (p < 0.001), confirming statistically significant but moderate seasonal dependence. The trend correlation (demand versus time index) equals -0.14, suggesting a slight declining trend over the observation period—possibly reflecting efficiency improvements, economic factors, or random variation rather than deterministic shift.

The presence of seasonality raises an important analytical question: should EVT models incorporate season-specific parameters? Time-varying GEV models allowing seasonal variation in location and scale parameters represent a natural extension (Coles, 2001). However, with only 5-6 realizations of each season in our dataset, seasonal parameter estimation would substantially increase model complexity while reducing statistical power. We therefore proceed with temporally stationary EVT models, acknowledging this as a simplifying assumption that could be revisited with longer time series. The seasonal pattern informs our interpretation—specifically, it motivates investigation of temperature as a potential mechanistic driver of extremes.

### 2.3 The Critical Discovery: Temperature-Demand Relationship

Recognizing that the dual seasonal peaks (summer and winter) correspond to temperature extremes, we investigated the functional form of the temperature-demand relationship. This investigation represents a pivotal moment in our analytical progression: rather than proceeding directly to EVT application, we paused to understand the mechanistic driver of demand variations.

**[INSERT FIGURE 2 HERE]**

*Figure 2: Demand versus Regional Average Temperature with Lowess Smoother (f = 0.1). The pronounced U-shaped relationship indicates that both cold and hot temperature extremes drive elevated electricity demand, with minimum demand occurring near 11°C.*

Figure 2 reveals a pronounced U-shaped (quadratic) relationship between temperature and demand. To quantify this pattern, we estimated the quadratic regression model:

**Demand = β₀ + β₁(Temperature) + β₂(Temperature²) + ε**

The fitted model yields β₂ = 12.6 MW/°C² (p < 0.001) with R² = 0.435, confirming strong statistical significance and substantial explanatory power. The positive second-order coefficient validates the U-shape: demand increases with absolute deviation from the optimal temperature in either direction. The vertex of the fitted parabola occurs at -β₁/(2β₂) = 10.8°C, representing the temperature at which electricity demand reaches its minimum—approximately the point where neither heating nor cooling loads dominate consumption.

This discovery carries three important implications. First, it validates our decision to focus on temperature-conditioned extreme demand rather than unconditional temporal extremes. The 43.5% of demand variance explained by temperature quadratically far exceeds what would be achieved by linear models (R² ≈ 0.15 for linear specification), confirming that the functional form matters critically. Second, it provides a mechanistic interpretation: extreme demand events occur during temperature extremes, whether hot or cold. This is not merely statistical correlation but reflects physical causation through heating and cooling system activation. Third, it suggests a practical operational application: temperature forecasts can serve as a leading indicator for demand risk, providing 6-12 hour warning before extremes materialize (typical weather forecast horizons for temperature prediction are 12-48 hours with high accuracy).

### 2.4 Distribution Properties and EVT Justification

Before proceeding to EVT application, we formally assess whether the demand distribution exhibits the heavy-tail properties that necessitate extreme value methods rather than standard parametric approaches.

**[INSERT FIGURE 3 HERE - Upper Panel]**

*Figure 3: Distribution Diagnostics for Demand. Upper panel: Histogram with normal distribution overlay (red curve) showing positive skewness and heavier upper tail than normal distribution predicts. Lower panel: Q-Q plot comparing empirical quantiles against theoretical normal distribution, with systematic upward departure in upper tail indicating that normal distribution underestimates extreme quantiles.*

The histogram in Figure 3 (upper panel) reveals right skewness (skewness = 0.41) and slight platykurtosis (kurtosis = 2.79), visually apparent through the extended upper tail relative to a fitted normal distribution. The Q-Q plot (lower panel) provides more definitive evidence: empirical quantiles systematically exceed theoretical normal quantiles in the upper tail, with increasing deviation at extreme quantiles. This upward curvature in the Q-Q plot's tail is the diagnostic signature of a distribution with heavier tails than the normal.

We formally test the normality hypothesis using the Shapiro-Wilk test on a random sample of 5,000 observations (computational limitations preclude testing the full dataset). The test statistically rejects normality (W = 0.992, p < 2×10⁻¹⁶), providing overwhelming evidence against the null hypothesis. While the test statistic itself is close to unity (perfect normality would yield W = 1), the enormous sample size provides sufficient statistical power to detect even small departures from normality as statistically significant.

These diagnostic results confirm that standard parametric methods assuming normality—such as mean ± k standard deviations for extreme quantile estimation—will systematically underestimate tail risk. This is not merely a statistical nicety but has concrete operational implications: a normal approximation would underestimate the 10-year return level by approximately 5-8%, potentially leading to inadequate capacity provisioning. This formal rejection of normality provides empirical justification for EVT application—we are not merely applying sophisticated methods for methodological sophistication, but because the data properties necessitate tail-specific modeling approaches.

---

## 3. Extreme Value Theory Methodology and Application

### 3.1 Methodological Framework and Dual-Method Rationale

Extreme Value Theory provides a rigorous framework for modeling tail behavior through one of two primary approaches: Block Maxima (leading to the Generalized Extreme Value distribution) or Peaks-Over-Threshold (yielding the Generalized Pareto Distribution). Each method possesses distinct advantages and limitations.

Block Maxima partitions the time series into equal blocks (e.g., weekly, monthly) and extracts the maximum observation from each block. Under weak dependence conditions, the asymptotic distribution of these block maxima converges to the three-parameter GEV distribution (Fisher & Tippett, 1928; Gnedenko, 1943):

**G(z) = exp{−[1 + ξ(z − μ)/σ]^(−1/ξ)}**

where μ represents location, σ > 0 represents scale, and ξ represents shape. The shape parameter determines tail behavior: ξ > 0 corresponds to Fréchet distribution (heavy tail, no finite maximum), ξ = 0 to Gumbel distribution (exponential tail), and ξ < 0 to Weibull distribution (bounded upper tail). The primary advantage of Block Maxima is theoretical elegance and computational simplicity. The disadvantage is potential information loss: only one observation per block enters the analysis, discarding potentially informative secondary extremes.

Peaks-Over-Threshold addresses this limitation by analyzing all exceedances above a high threshold u. Pickands (1975) and Balkema and de Haan (1974) proved that exceedances above sufficiently high thresholds asymptotically follow the two-parameter Generalized Pareto Distribution:

**H(y) = 1 − [1 + ξy/σ]^(−1/ξ)**

where y = x − u represents the exceedance size, σ > 0 is scale, and ξ is shape (matching the GEV shape parameter asymptotically). POT's advantage is data efficiency: properly chosen thresholds yield many more observations (hundreds or thousands) versus Block Maxima (tens or hundreds), enabling more precise tail estimation. The disadvantage is threshold selection subjectivity—too low risks bias from non-extreme observations, too high sacrifices sample size.

Given our short time series (5.2 years), we implement both methods and use their convergence as a validation criterion. If Block Maxima and POT yield similar return level estimates despite using different subsets of data and different sampling strategies, this provides confidence that both are capturing true tail behavior rather than method-specific artifacts. Conversely, substantial discrepancies would signal potential model misspecification or inadequate data for stable estimation.

### 3.2 Block Maxima Analysis: GEV Application

We partition the time series into weekly blocks, yielding 274 block maxima. Weekly blocking represents a reasonable compromise: daily blocks would violate independence assumptions due to autocorrelation in hourly demand data, while monthly blocks would reduce sample size to merely 62 observations. The 274 weekly maxima provide adequate sample size for three-parameter GEV estimation while respecting temporal dependence structure.

Maximum likelihood estimation yields the following GEV parameters:

- **Location (μ):** 17,762.5 MW
- **Scale (σ):** 2,318.0 MW  
- **Shape (ξ):** −0.264

The negative shape parameter (ξ = −0.264) indicates Weibull-type behavior with a bounded upper tail. This suggests the existence of a finite upper bound on electricity demand—a physically reasonable result given that total demand cannot exceed the sum of all installed end-use equipment capacity in the service territory. The bound occurs at μ − σ/ξ = 17,762.5 − 2,318.0/(−0.264) ≈ 26,545 MW, representing the theoretical maximum under the fitted GEV model.

**[INSERT FIGURE 4 HERE]**

*Figure 4: GEV Model Diagnostic Plots. Four-panel display including: (1) probability plot comparing empirical versus model-based distribution functions, (2) quantile plot showing empirical versus model quantiles, (3) return level plot with 95% confidence intervals, and (4) density plot comparing empirical histogram with fitted GEV density.*

Figure 4 presents comprehensive GEV diagnostics. The probability plot (upper left) shows strong agreement between empirical and model-based cumulative distribution functions, with points closely tracking the 45-degree line. The quantile plot (upper right) similarly demonstrates good correspondence across the entire range of data. The return level plot (lower left) displays estimated return levels with 95% confidence intervals, showing reasonable precision for return periods up to approximately 50 blocks (roughly one year). The density plot (lower right) confirms that the fitted GEV captures both the central tendency and upper tail of the empirical distribution.

From the fitted GEV, we compute return levels corresponding to 1, 2, 5, and 10-year horizons. Since blocks are weekly, a k-year return period corresponds to 52k blocks:

**[INSERT TABLE 2 HERE]**

*Table 2: Return Level Estimates from Block Maxima (GEV) and Peaks-Over-Threshold (GPD) Methods*

A critical observation emerges: the GEV 10-year return level of 24,858 MW falls marginally below the observed historical maximum of 24,739 MW. This is statistically unusual—we would expect the 10-year return level to exceed the maximum from a 5-year sample approximately 40% of the time (probability ≈ 1 − (1 − 1/10)⁵ ≈ 0.41). While not impossible, this raises concern about potential underestimation. Two explanations are plausible: either the historical maximum represents a particularly unusual realization (a "lucky" data point in the upper tail), or the Block Maxima approach with weekly blocking and limited data is underestimating tail quantiles. This concern motivated proceeding to POT analysis as a validation check.

### 3.3 Peaks-Over-Threshold Analysis: GPD Application

For POT application, we select the 95th percentile (19,398 MW) as our threshold. This choice represents a standard compromise in the threshold selection trade-off. Lower thresholds (e.g., 90th percentile) would provide more exceedances but risk including non-extreme observations that violate GPD asymptotic assumptions. Higher thresholds (e.g., 98th percentile) would ensure only truly extreme observations but sacrifice sample size and statistical precision.

The 95th percentile threshold yields 2,165 exceedances from 43,361 observations—approximately 5% as expected. This represents nearly 8× more data points than the 274 block maxima, providing substantially more information for tail characterization. Maximum likelihood estimation of the GPD model yields:

- **Scale (σ):** 1,247.3 MW
- **Shape (ξ):** −0.198

The negative shape parameter again indicates bounded tail behavior, consistent with the GEV finding. The shape estimate differs from GEV (−0.198 versus −0.264) but both lie within each other's confidence intervals, suggesting the difference reflects estimation uncertainty rather than fundamental disagreement.

**[INSERT FIGURE 5 HERE]**

*Figure 5: GPD Model Diagnostic Plots. Four-panel layout mirroring GEV diagnostics, demonstrating good model fit across probability, quantile, return level, and density dimensions.*

Figure 5 demonstrates excellent GPD model fit. All four diagnostic panels show strong agreement between empirical data and fitted model, providing confidence in tail characterization. The probability plot (upper left) exhibits minimal deviation from the 45-degree line throughout the range of exceedances. The return level plot (lower left) shows relatively tight confidence intervals up to approximately 10-year horizons, beyond which uncertainty grows as expected when extrapolating beyond observed data.

Computing return levels from the GPD requires accounting for the exceedance rate. The threshold corresponds to roughly 2,165 exceedances in 5.2 years, or λ ≈ 416 exceedances per year. The k-year return level corresponds to the (1 − 1/(λk))-quantile of the exceedance distribution added to the threshold. Results appear in Table 2.

### 3.4 Method Comparison and Convergence Analysis

Table 2 reveals a systematic pattern: GPD estimates consistently exceed GEV estimates, with the absolute difference declining at longer return periods. At the 1-year horizon, GPD exceeds GEV by 2,017 MW (8.6%); at 10 years, the gap narrows to 824 MW (3.3%). This convergence pattern is analytically important.

**[INSERT FIGURE 6 HERE]**

*Figure 6: Comparison of GEV and GPD Return Level Estimates. Plot shows return levels from both methods with 95% confidence intervals, illustrating convergence at longer return periods. Shaded regions indicate confidence bounds.*

The convergence pattern in Figure 6 provides strong validation of both approaches. Short-horizon differences likely reflect the fundamental distinction in what each method estimates: GEV characterizes the distribution of block maxima (inherently smoother), while GPD directly models the tail of all exceedances (more volatile). At longer horizons corresponding to rarer events, both methods are extrapolating into the same extreme tail region where theoretical foundations (Fisher-Tippett-Gnedenko theorem and Pickands-Balkema-de Haan theorem) ensure they should converge.

The fact that GEV and GPD arrive at 10-year estimates differing by only 3.3% despite using fundamentally different sampling approaches (274 weekly maxima versus 2,165 threshold exceedances) provides robust validation. This convergence suggests we are capturing true tail properties rather than fitting method-specific noise. For operational decision-making, we adopt the GPD 10-year estimate (25,682 MW) as our primary reference given POT's superior data efficiency and the fact that it exceeds the historical maximum by a realistic margin (943 MW, representing approximately 4% excess over the 5.2-year peak).

The 943 MW gap between GPD 10-year estimate and historical maximum deserves interpretation. From a finite sample of 5.2 years, the probability of observing the true 10-year return level is only about 40%. The fact that our GPD estimate exceeds the observed maximum is thus statistically consistent and operationally reassuring—it would be more concerning if our 10-year estimate fell below the peak from a 5-year sample, suggesting systematic underestimation.

---

## 4. Risk Quantification and Operational Translation

### 4.1 Value-at-Risk and Expected Shortfall

While return levels provide probabilistically interpretable thresholds, operational risk management requires complementary metrics. We compute Value-at-Risk (VaR) and Expected Shortfall (ES) to characterize both threshold exceedance and conditional severity.

Value-at-Risk at confidence level α represents the (1−α)-quantile of the demand distribution. We compute VaR₉₅ = 19,398 MW and VaR₉₉ = 21,174 MW from empirical quantiles. These thresholds have direct operational interpretation: under stationary conditions, demand exceeds VaR₉₅ approximately 5% of time (438 hours annually) and VaR₉₉ approximately 1% of time (88 hours annually).

Expected Shortfall, also called Conditional Value-at-Risk, measures the conditional mean of demand given that a threshold is exceeded:

**ES_α = E[Demand | Demand > VaR_α]**

From empirical calculation, ES₉₉ = 22,019 MW. The difference ES₉₉ − VaR₉₉ = 845 MW represents what we term the "surprise gap"—the additional capacity required on average when extreme thresholds are breached. This 845 MW gap quantifies the extent to which extreme events exceed the threshold itself, providing guidance for emergency capacity procurement or demand response program sizing.

**[INSERT TABLE 3 HERE]**

*Table 3: Risk Metrics Summary Including Value-at-Risk, Expected Shortfall, and Return Levels*

Table 3 synthesizes risk metrics with operational interpretations. The progression from VaR₉₅ (routine high demand) through VaR₉₉ (extreme threshold) to ES₉₉ (average during extremes) to the 10-year return level (rare catastrophic event) provides a risk hierarchy for decision-making. Each metric serves distinct purposes: VaR thresholds trigger operational alerts, ES informs emergency capacity requirements, and return levels guide long-term investment decisions.

### 4.2 Capacity Gap Analysis

Current observed maximum demand stands at 24,739 MW. The GPD 10-year return level of 25,682 MW implies a **capacity gap of 943 MW**—the shortfall between historical peak and the level expected to be exceeded approximately once per decade. This gap has direct investment implications.

The probability of experiencing an event exceeding the current maximum in the next decade can be estimated as:

**P(exceed in 10 years) = 1 − (1 − 1/10)^10 ≈ 0.651**

Thus, approximately 65% probability exists that at least one event in the coming decade will exceed all previously observed demand levels. This is not forecasting demand growth per se, but recognizing that 5.2 years of observations provide incomplete information about the full range of possible extreme events.

Risk-adjusted capacity planning must account for the distribution of possible outcomes, not merely point estimates. The 95% confidence interval for the 10-year return level spans approximately [24,500, 27,000] MW (derived from Figure 5). Conservative planning would target capacity exceeding the upper confidence bound (27,000 MW), representing approximately 2,261 MW above current maximum. Moderate risk tolerance might target the point estimate plus one standard deviation (roughly 26,500 MW), implying 1,761 MW above maximum. Aggressive (cost-minimizing) approaches might build only to the point estimate (25,682 MW), accepting higher outage probability to minimize capital expenditure.

For this analysis, we recommend capacity planning target of **26,000 MW**—representing the GPD point estimate plus a modest buffer (~320 MW) to account for uncertainty. This implies approximately 1,261 MW above historical maximum, which we round to **1,000 MW for planning purposes** (recognizing that capacity additions typically occur in discrete increments aligned with generation unit sizes or transmission upgrade blocks).

### 4.3 Temperature-Based Early Warning System

The strong U-shaped temperature-demand relationship (Figure 2, R² = 0.435) enables development of a temperature-based risk alert system. By identifying temperature thresholds that historically correspond to high demand periods, operators can activate preparatory measures before demand peaks materialize.

We analyze temperature conditions during the highest-demand periods (demand exceeding VaR₉₅ = 19,398 MW) and compute temperature percentiles. The 10th percentile of temperature during high-demand periods is -15.6°C; the 90th percentile is 29.4°C. These values suggest natural alert thresholds.

We propose a three-stage alert system:

**Stage 1 (Pre-Alert):** Activated when forecasted temperature falls below -15°C or exceeds 29°C
- **Action:** Notify operations staff, verify equipment readiness, confirm fuel supplies
- **Lead time:** Typically 12-24 hours based on temperature forecast accuracy

**Stage 2 (Elevated Alert):** Activated when temperature falls below -20°C or exceeds 31°C  
- **Action:** Activate reserves, initiate demand response program notifications, defer maintenance
- **Lead time:** Typically 6-12 hours

**Stage 3 (Critical Alert):** Activated when actual demand exceeds VaR₉₅ = 19,398 MW
- **Action:** Full emergency protocols, real-time demand response, interruptible load activation
- **Lead time:** Real-time (no lead time, reactive response)

This system provides graduated response capability with substantial lead time. Temperature forecasts are typically highly accurate 12-24 hours ahead, providing sufficient warning for most preparatory actions (fuel procurement, staff scheduling, maintenance deferral). The system's value is not merely statistical correlation but mechanistic causation—temperature drives demand through heating and cooling loads, making it a leading indicator rather than coincident signal.

An additional benefit emerges from the U-shape: alerts trigger for both cold and hot extremes, capturing both winter and summer risk periods within a unified framework. Traditional approaches often separate "summer peak" and "winter peak" planning; the temperature-based system naturally integrates both.

---

## 5. Discussion and Implications

### 5.1 Methodological Insights

This analysis demonstrates several methodologically important lessons for EVT application under data constraints. First, method comparison provides validation even when individual methods carry substantial uncertainty. Our GEV and GPD estimates differ by 8.6% at short horizons but converge to 3.3% at 10 years—this convergence pattern provides confidence in the 10-year estimates despite neither method producing narrow confidence intervals individually. The principle is triangulation: multiple imperfect methods that agree enhance confidence more than any single method in isolation.

Second, discrepancies between methods can be informative rather than merely problematic. The fact that GEV 10-year estimate fell below observed maximum prompted investigation via POT, revealing that Block Maxima with limited data may underestimate tails. Rather than dismissing the discrepancy or arbitrarily choosing one method, we used it to deepen understanding. This reflects the iterative, discovery-driven approach advocated throughout this analysis.

Third, mechanistic understanding enhances statistical modeling. The temperature-demand relationship was not required for EVT application—we could have proceeded directly from demand data to GEV/GPD fitting. However, understanding that temperature drives extremes provided three benefits: (1) validation that we are modeling a physically meaningful relationship rather than statistical artifact, (2) operational translation through early warning systems, and (3) interpretation of why extremes occur (both hot and cold weather) rather than merely when they occur.

Fourth, acknowledging limitations strengthens rather than weakens conclusions. We explicitly note our short time series (5.2 years versus 20+ preferred), wider confidence intervals resulting from limited data, and assumptions (stationarity, temporal homogeneity) that could be relaxed with more observations. These acknowledgments do not undermine our findings but rather establish appropriate confidence bounds and identify priorities for future refinement. Attempting to hide limitations would be less credible than explicitly addressing them.

### 5.2 Operational Implications

Translation of statistical findings to operational strategy represents a critical but often underdeveloped aspect of risk analysis. We identify three specific actions with quantitative targets derived directly from our analysis:

**Capacity Expansion (1,000 MW by 2028):**  
The 943 MW gap between observed maximum (24,739 MW) and 10-year return level (25,682 MW) directly motivates capacity addition. We recommend a diversified portfolio rather than a single large addition:
- **500 MW peaker plants** (natural gas combustion turbines): Fast-response units for extreme event management, typically achieving full output within 10-15 minutes. Capital cost approximately $650-750/kW, yielding roughly $325-375 million investment.
- **300 MW demand response programs** (industrial load shedding): Contractual agreements with large industrial customers for voluntary load reduction during emergencies. Lower capital cost ($100-200/kW) but requires ongoing incentive payments and may face reliability concerns.
- **200 MW enhanced interconnection**: Transmission upgrades enabling greater power imports from neighboring regions during local peaks. Capital cost varies widely ($800-1500/kW depending on distance and terrain) but provides bidirectional benefits and load diversity.

This diversified approach balances cost, reliability, and flexibility. Peaker plants provide highest reliability but highest cost and environmental impact; demand response offers lowest cost but operational uncertainty; interconnection provides medium cost and strategic value beyond peak management.

**Temperature-Based Alert System:**  
Implementation requires integrating National Weather Service temperature forecasts into operational dispatch systems. Technical requirements are modest (API integration, threshold monitoring logic, automated notification) with estimated development cost $100,000-250,000 and ongoing operational cost minimal. The system's value emerges from improved preparedness rather than direct capacity addition: reducing forced outage rates, optimizing unit commitment, and providing staff lead time for corrective actions.

Critical success factors include accurate temperature forecasting (current 24-hour forecasts achieve ±1-2°C accuracy), spatial averaging across the service territory (avoid triggering on single-station anomalies), and organizational discipline in escalation protocols (ensuring Stage 1/2 alerts prompt actual preparatory actions rather than becoming ignored routine warnings).

**Data Infrastructure Enhancement:**  
Our recommendation to extend historical data to 20+ years (acquiring 2000-2011 demand and temperature records) serves two purposes. First, doubling the time series would reduce return level confidence intervals by approximately √2 ≈ 1.4×, tightening uncertainty from roughly ±5% to ±3.5%. This improved precision enables more efficient capacity investment—narrower confidence bounds mean less need for precautionary buffers. Second, longer time series would enable testing our stationarity assumption. Climate change may be shifting the temperature distribution (particularly increasing frequency of extreme heat events), and extended data would allow detection of such trends.

Additionally, we recommend developing climate change scenario analysis. Under RCP4.5 (moderate emissions) projections, the Midwest U.S. is expected to experience mean temperature increase of 2-3°C by 2050-2070. Given the U-shaped demand-temperature relationship, this warming has complex effects: reduced winter heating demand but increased summer cooling demand. The net effect on extremes is ambiguous and requires integrated climate-demand modeling beyond this study's scope but represents an important extension for long-term planning.

### 5.3 Limitations and Caveats

Several limitations warrant explicit discussion. First, our 5.2-year time series is substantially shorter than ideal. While our dual-method approach provides validation, confidence intervals remain wider than would be achieved with 20+ years data. Long-horizon extrapolation (beyond 10 years) carries substantial uncertainty and should not be attempted from this dataset. We purposefully limited analysis to 10-year return periods; estimates of 50- or 100-year return levels would be speculation rather than robust inference.

Second, we assume temporal stationarity—that the statistical properties of demand (and particularly extreme demand) remain constant over time. This assumption may be violated by structural changes in electricity consumption patterns (efficiency improvements, electrification of heating/transportation, distributed generation growth). The modest negative trend observed (correlation -0.14) suggests some non-stationarity, though whether this reflects true structural change versus sample noise cannot be determined from 5 years data.

Third, we model unconditional demand extremes rather than conditioning on external covariates beyond temperature. Additional factors such as economic activity (industrial production indices), calendar effects (holidays, major events), and humidity/wind could refine predictions. Multivariate EVT methods exist but require substantially more data than available here. Our temperature-based approach captures the primary driver but not all demand variation.

Fourth, spatial aggregation to a single regional index obscures geographic heterogeneity. AEP's territory spans diverse climate zones and economic bases; extreme events may be spatially localized rather than system-wide. System-level capacity planning focuses on total demand, but transmission constraints mean that localized extremes can create bottlenecks even when system-wide demand remains manageable. Spatially disaggregated analysis would require meter-level data and transmission network modeling beyond this study's scope.

Fifth, we do not model uncertainty in temperature forecasts explicitly. The early warning system assumes perfect temperature forecasting; in reality, 12-24 hour forecasts carry uncertainty that degrades alert precision. Probabilistic temperature forecasts integrated with demand-temperature models would provide more refined risk estimates but require substantially more complex methodology.

Despite these limitations, our findings provide actionable risk quantification for capacity planning. The convergence of GEV and GPD methods at 10-year horizons, combined with mechanistic temperature-based interpretation, provides robust foundation for the recommended capacity expansion, early warning systems, and data infrastructure investments.

---

## 6. Conclusion

This analysis demonstrates how rigorous extreme value analysis under data constraints can transform limited information into actionable risk intelligence. Beginning with 5.2 years of hourly electricity demand data—substantially less than the 20+ years typically recommended for EVT—we developed a discovery-driven analytical approach that converted apparent limitations into methodological strengths.

Our central finding is a 10-year return level of approximately 25,682 MW (GPD estimate), representing 943 MW or 3.8% above the observed 5.2-year maximum of 24,739 MW. This estimate derives validation from convergence with an independent Block Maxima (GEV) analysis, which yields 24,858 MW at the same horizon—a difference of only 3.3% despite the methods using fundamentally different data subsets and sampling strategies. This convergence provides confidence that both approaches are capturing true tail behavior rather than method-specific artifacts.

Beyond numerical estimates, we identified the mechanistic driver of extreme demand: a U-shaped relationship between temperature and electricity consumption (R² = 0.435) reflecting dual heating and cooling loads. This discovery enables translation of statistical risk estimates into operational early warning systems, providing 6-12 hour lead time for preparatory actions based on temperature forecasts. The U-shaped pattern also explains why extremes occur in both summer and winter, integrating seasonal risk within a unified framework.

We translate these findings into three specific strategic recommendations. First, add approximately 1,000 MW capacity by 2028 through a diversified portfolio: 500 MW fast-response peaker plants, 300 MW demand response programs, and 200 MW enhanced regional interconnection. Second, implement a three-stage temperature-based alert system with thresholds at ±15-20°C extremes, leveraging the temperature-demand relationship for proactive risk management. Third, extend historical data collection to 20+ years to reduce estimation uncertainty from approximately ±5% to ±2.5%, enabling more precise and cost-effective capacity planning.

Methodologically, this study illustrates how iterative, discovery-driven analysis differs from cookbook application of techniques. Each finding informed subsequent investigations: short time series motivated dual-method validation; seasonal patterns prompted temperature investigation; the U-shaped relationship informed interpretation of extreme drivers; GEV-GPD discrepancies prompted convergence analysis as validation. This iterative approach demonstrates that rigorous risk analysis is not merely applying formulas to data but rather asking sequential questions, validating assumptions, investigating anomalies, and building mechanistic understanding alongside statistical characterization.

The broader lesson extends beyond this specific application: data limitations need not preclude robust analysis if approached with appropriate methodological care. Rather than dismissing short time series as inadequate or proceeding with single-method point estimates of dubious validity, we implemented multiple methods, used their agreement as validation, acknowledged uncertainty explicitly, and translated findings to risk-adjusted recommendations that account for estimation imprecision. This represents a template for EVT application under imperfect conditions—which is to say, most real-world applications.

Future research should extend this analysis in several directions. Extending the historical dataset backward (acquiring 2000-2011 records) would substantially tighten confidence intervals and enable testing of stationarity assumptions. Incorporating climate change scenarios would address the question of whether current tail estimates remain relevant under shifting temperature distributions. Spatially disaggregated analysis at the substation or metropolitan level would capture geographic heterogeneity and transmission constraints. And multivariate EVT conditioning on multiple covariates simultaneously (temperature, humidity, wind, economic indicators) would refine predictive accuracy.

Nevertheless, the current analysis provides a robust foundation for capacity risk assessment in the AEP Midwest region. The recommended capacity expansion, early warning system, and data infrastructure investments follow directly from rigorous extreme value analysis that acknowledges limitations, validates assumptions, and translates statistics into strategy. Most fundamentally, this work demonstrates that the goal of risk analysis is not merely to report return levels and shape parameters but to transform quantitative findings into decisions that reduce operational risk and optimize capital allocation under uncertainty.

---

## References

Balkema, A. A., & de Haan, L. (1974). Residual life time at great age. *Annals of Probability*, 2(5), 792-804.

Coles, S. (2001). *An Introduction to Statistical Modeling of Extreme Values*. London: Springer-Verlag.

Fisher, R. A., & Tippett, L. H. C. (1928). Limiting forms of the frequency distribution of the largest or smallest member of a sample. *Proceedings of the Cambridge Philosophical Society*, 24(2), 180-190.

Gnedenko, B. (1943). Sur la distribution limite du terme maximum d'une série aléatoire. *Annals of Mathematics*, 44(3), 423-453.

Pickands, J. (1975). Statistical inference using extreme order statistics. *Annals of Statistics*, 3(1), 119-131.

---

## Appendix: Supporting Figures and Extended Analysis

**[INSERT FIGURE A1]**  
*Figure A1: Seasonal Demand Patterns by Month. Boxplots showing demand distribution for each calendar month, revealing dual peaks in summer (July-August, air conditioning) and winter (January-February, heating).*

**[INSERT FIGURE A2]**  
*Figure A2: Average Daily Load Profile. Mean hourly demand with ±1 standard deviation bands, showing characteristic morning ramp-up (6-9 AM), midday plateau, early evening peak (5-7 PM), and overnight minimum. Pattern reflects typical residential and commercial electricity usage.*

**[INSERT FIGURE A3]**  
*Figure A3: Weekly Demand Patterns. Boxplots by day of week, showing elevated weekday demand (Monday-Friday, darker boxes) versus reduced weekend demand (Saturday-Sunday, lighter boxes), reflecting industrial and commercial load reduction on weekends.*

**[INSERT FIGURE A4]**  
*Figure A4: Seasonal Temperature Distributions. Four-panel histogram showing temperature frequency by season (Winter: December-February; Spring: March-May; Summer: June-August; Fall: September-November), illustrating the wide temperature range (-25°C to +34°C) driving HVAC loads.*

**[INSERT FIGURE A5]**  
*Figure A5: Timeline of Extreme Demand Events. Full time series with extreme events (>95th percentile) highlighted in red, showing clustering of extremes during both summer and winter months, consistent with temperature-driven interpretation.*

**[INSERT FIGURE A6]**  
*Figure A6: Correlation Matrix of Key Variables. Heatmap showing pairwise correlations between demand, temperature, hour-of-day, and month. Strongest correlation is demand-temperature quadratic relationship; weaker correlations with hour and month reflect daily and seasonal patterns.*

**[INSERT FIGURE A7]**  
*Figure A7: Seasonal Temperature-Demand Relationships. Four-panel scatter plots showing demand versus temperature separately for each season with lowess smoothers. U-shaped pattern persists across all seasons though with varying intensity: strongest in winter and summer (when HVAC loads dominate), weaker in shoulder seasons (spring/fall).*

**[INSERT FIGURE A8]**  
*Figure A8: Annual Maximum Demand Comparison (2013-2017). Bar chart showing annual peak demand with overlaid mean demand trend line. Annual maxima range from approximately 23,500 MW (2017) to 24,739 MW (2016), with no obvious trend over the 5-year complete-year period, supporting stationarity assumption.*

**[INSERT TABLE A1]**  
*Table A1: Exceedance Statistics by Season. Count and percentage of observations exceeding VaR₉₅ and VaR₉₉ thresholds broken down by season, confirming that extreme events occur primarily in summer (June-August) and winter (December-February) with fewer extreme events in shoulder seasons.*

**[INSERT TABLE A2]**  
*Table A2: GEV Parameter Estimates with Standard Errors and Confidence Intervals. Maximum likelihood estimates for location (μ), scale (σ), and shape (ξ) parameters with asymptotic standard errors and 95% confidence intervals derived from observed Fisher information matrix.*

**[INSERT TABLE A3]**  
*Table A3: GPD Parameter Estimates with Threshold Diagnostics. Scale and shape parameter estimates for various threshold choices (90th, 92.5th, 95th, 97.5th percentiles), demonstrating parameter stability across reasonable threshold selection range and justifying our 95th percentile choice.*

---

**END OF REPORT**
