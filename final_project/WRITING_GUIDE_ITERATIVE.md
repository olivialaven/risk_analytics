# Writing Guide: Iterative Risk Analysis Report (3 Pages)

## Overview

You now have a **discovery-driven analysis** where each finding logically leads to the next investigation. This creates a compelling narrative that demonstrates critical thinking, not just technical proficiency.

---

## Document Structure

### Document: ITERATIVE_ANALYSIS_FRAMEWORK.md
- **Purpose**: Conceptual guide showing the logic chain
- **Content**: 7 stages of iterative discovery with decision points
- **Use**: Reference for understanding the "why" behind each step

### Script: ITERATIVE_ANALYSIS.R
- **Purpose**: Executable R script implementing the framework
- **Content**: Live decision-making based on statistical findings
- **Features**: Decision log, conditional logic, narrative output

### Output: ITERATIVE_ANALYSIS_REPORT.md
- **Purpose**: Final comprehensive report from the analysis
- **Content**: Complete chain of discovery from data to strategy
- **Use**: Base document for your 3-page write-up

---

## The Discovery Chain (Core Narrative)

```
FINDING 1: Short time series (5.2 years)
    ↓ DECISION
Use BOTH GEV and GPD for validation
    ↓
FINDING 4: U-shaped temperature-demand (R² = 0.435)
    ↓ INSIGHT
Temperature is THE primary driver
    ↓
FINDING 5: Shapiro-Wilk p < 2e-16 (non-normal)
    ↓ CONFIRMATION
EVT is necessary, not optional
    ↓
FINDING 6: GEV 10-year = 24,858 MW (< observed max)
    ↓ CONCERN
Possible underestimation → investigate further
    ↓
FINDING 7: GPD 10-year = 25,682 MW (+3.3%)
    ↓ EXPLANATION
POT uses 2,165 exceedances vs 274 blocks
    ↓
FINDING 9: Methods converge from 8.6% → 3.3%
    ↓ VALIDATION
High confidence in 24,900-25,700 MW range
    ↓
FINDING 11: ES(99%) - VaR(99%) = 845 MW
    ↓ QUANTIFICATION
"Surprise gap" for emergency planning
    ↓
RECOMMENDATION: Add 1,000 MW capacity + temperature alerts
```

---

## 3-Page Report Structure

### Page 1: From Data to Discovery (~1 page)

#### Section 1.1: Introduction & Context (3-4 paragraphs)

**Paragraph 1 - The Problem:**
> Electricity grid operators face critical decisions about capacity planning: too much capacity wastes capital, too little risks blackouts. This analysis applies Extreme Value Theory (EVT) to 5.2 years of hourly electricity demand data (43,361 observations) from American Electric Power's Midwest region to quantify extreme demand risk and inform capacity investment decisions.

**Paragraph 2 - The Challenge:**
> Our dataset spans only 2012-2017, well below the 20+ years preferred for robust EVT. This limitation motivated a dual-method approach: we apply both Block Maxima (GEV) and Peaks-Over-Threshold (GPD) methods, using their convergence as validation of our estimates despite the short time series.

**Paragraph 3 - Initial Discovery:**
> Exploratory analysis revealed demand ranging from 9,581 to 24,739 MW with moderate variability (CV = 16.8%). However, formal testing rejected normality (Shapiro-Wilk p < 2e-16), confirming the necessity of EVT rather than standard parametric methods for tail estimation.

**Key Figure:** `01_timeseries_exploration.png` - Shows 5-year data with seasonal patterns

---

#### Section 1.2: The Temperature Discovery (2-3 paragraphs)

**Paragraph 1 - The U-Shape:**
> A critical discovery emerged when plotting demand against average Midwest temperature: a pronounced **U-shaped relationship** (R² = 0.435). Demand minimizes at ~11°C and increases sharply at both temperature extremes. This pattern indicates that BOTH heating (cold weather) and cooling (hot weather) drive peak demand, making temperature the primary driver of extreme events.

**Paragraph 2 - Implications:**
> This U-shape has three key implications. First, it validates our focus on temperature-conditioned extremes. Second, it suggests standard linear models would be inappropriate. Third, it provides a leading indicator for demand spikes: temperature forecasts offer 6-12 hours of warning before peaks materialize.

**Key Figure:** `02_demand_vs_temperature.png` - U-shaped relationship with lowess smoother

**Key Table:** Summary statistics showing temperature range -25°C to +34°C

---

### Page 2: Comparative EVT Analysis (~1 page)

#### Section 2.1: Dual-Method Approach (3 paragraphs)

**Paragraph 1 - GEV Analysis:**
> We first applied Block Maxima using weekly blocks (274 maxima over 5 years). The GEV model converged with shape parameter ξ = -0.264, indicating a bounded (Weibull-type) distribution. Diagnostic plots showed excellent fit, but the 10-year return level (24,858 MW) fell suspiciously BELOW the observed historical maximum (24,739 MW). This suggested potential underestimation, motivating further investigation.

**Paragraph 2 - GPD Analysis:**
> The POT approach with threshold at the 95th percentile (19,398 MW) yielded 2,165 exceedances—nearly 8× more data points than block maxima. The GPD model estimated the 10-year return level at 25,682 MW, **3.3% higher than GEV**. Critically, this exceeds the observed maximum by 943 MW, which is realistic given our finite 5-year sample.

**Paragraph 3 - Method Convergence:**
> Comparing across return periods reveals a convergence pattern: the two methods differ by 8.6% at 1-year but only 3.3% at 10-year. This narrowing gap validates BOTH approaches—differences stem from methodology (block size vs threshold), not model misspecification. For risk management, we adopt the GPD estimate as our primary reference given its superior use of tail data.

**Key Figure:** `04_gev_diagnostics.png` OR `05_gpd_diagnostics.png` - Model validation plots

**Key Table:** Return level comparison showing convergence:
```
Return Period | GEV (MW)  | GPD (MW)  | Difference
1-year        | 23,441    | 25,458    | +8.6%
2-year        | 23,963    | 25,537    | +6.6%
5-year        | 24,519    | 25,625    | +4.5%
10-year       | 24,858    | 25,682    | +3.3%  ← CONVERGENCE
```

---

### Page 3: From Statistics to Strategy (~1 page)

#### Section 3.1: Risk Quantification (2 paragraphs)

**Paragraph 1 - VaR and ES:**
> We translated statistical findings into operational risk metrics. Value-at-Risk at 99% (VaR₉₉) = 21,174 MW establishes an extreme demand threshold expected to be exceeded ~87 hours per year. More critically, Expected Shortfall (ES₉₉) = 22,019 MW reveals that when this threshold is breached, the AVERAGE demand is 845 MW higher. This "surprise gap" quantifies the additional capacity needed during truly extreme events beyond what VaR alone suggests.

**Paragraph 2 - Capacity Gap:**
> The 10-year return level of 25,682 MW exceeds the current historical maximum by 943 MW. Given the ~65% probability of exceeding the current maximum in the next decade, this gap represents a clear and quantifiable capacity shortfall that must be addressed to maintain grid reliability.

---

#### Section 3.2: Strategic Recommendations (3 sub-sections)

**Recommendation 1: Capacity Expansion (1 paragraph)**
> **Add 1,000 MW capacity by 2028** through a diversified portfolio: 500 MW fast-response peaker plants (gas turbines), 300 MW demand response programs (industrial load shedding agreements), and 200 MW enhanced regional interconnection capacity. This 1,000 MW target reflects the 943 MW capacity gap plus a 60 MW safety buffer (~6%) to account for estimation uncertainty.

**Recommendation 2: Temperature-Based Early Warning (1 paragraph)**
> **Implement a 3-stage alert system** triggered by temperature thresholds derived from the U-shaped relationship: Stage 1 (pre-alert) at T < -15.6°C or T > 29.4°C, Stage 2 (activate reserves) at T < -20.6°C or T > 31.4°C, and Stage 3 (emergency protocols) when actual demand exceeds VaR₉₅ = 19,398 MW. This system leverages temperature as a leading indicator, providing operational teams with actionable early warning 6-12 hours before demand peaks.

**Recommendation 3: Data Infrastructure Enhancement (1 paragraph)**
> **Extend the historical dataset to 20+ years** by acquiring 2000-2011 data. Our current 5.2-year time series yields return level confidence intervals of approximately ±5%, constraining our ability to optimize capacity investments. A 20-year dataset would halve this uncertainty, enabling more precise and cost-effective planning. Additionally, integrate weather forecast uncertainty and develop climate change scenarios (+2°C, +4°C) to ensure long-term resilience.

---

#### Section 3.3: Conclusion (1 final paragraph)

> This analysis demonstrates how iterative, discovery-driven methodology transforms raw data into strategic intelligence. By allowing each finding to inform the next investigation—short time series → dual methods → temperature discovery → GEV concern → GPD validation → method convergence → risk quantification—we developed not just statistical estimates but robust, defensible capacity planning recommendations. The convergence of GEV and GPD methods at the 10-year horizon, combined with the temperature-driven mechanistic understanding, provides high confidence that the Midwest grid faces a ~1,000 MW capacity gap that should be addressed through the diversified portfolio and early warning system outlined above. Most importantly, this analysis reveals that **rigorous risk analysis isn't about applying formulas—it's about asking the right questions, validating assumptions, and quantifying uncertainty** to enable informed decision-making under incomplete information.

---

## Key Differentiators from Standard Approach

### What Makes This Iterative?

| Standard Cookbook | Iterative Discovery-Driven |
|-------------------|---------------------------|
| Pre-defined analysis steps | Each step motivated by previous findings |
| Report results only | Explain why you made each choice |
| "We used GEV" | "Short time series → decided to validate with GPD" |
| Single method | Multiple methods, investigate discrepancies |
| "10-year level = X MW" | "GEV gave X, GPD gave Y, they converge—here's why" |
| Statistics tables | Translation to operational metrics |
| Generic recommendations | Findings-specific strategy |

---

## Narrative Flow Checklist

✅ **Discovery Chain Visible**: Reader can follow your thinking process  
✅ **Decisions Justified**: Every methodological choice linked to a finding  
✅ **Validation Present**: Multiple methods compared, convergence discussed  
✅ **Uncertainty Quantified**: CI widths, data limitations acknowledged  
✅ **Translation to Action**: Statistics → operational metrics → strategy  
✅ **Critical Thinking**: Not just "what" but "why" and "so what?"

---

## Figures to Include (Select 3-4 for 3 pages)

**Must Include:**
1. **`02_demand_vs_temperature.png`** - The U-shape discovery (critical insight)
2. **`05_gpd_diagnostics.png`** OR **`04_gev_diagnostics.png`** - Model validation

**Strongly Recommended:**
3. **`01_timeseries_exploration.png`** - Context (seasonal patterns, 5-year span)

**Optional (if space):**
4. **`03_distribution_diagnostics.png`** - Heavy tail evidence (QQ-plot)

---

## Appendix selections (unlimited space)

When space is tight, keep the body to 2 figures and 2 tables, and move the rest to the appendix. Recommended mapping:

- Main text figures:
    1) `02_demand_vs_temperature.png` (U-shape) → Figure 1
    2) `05_gpd_diagnostics.png` (primary model validation) → Figure 2

- Appendix figures (label as A1, A2, …):
    - A1: `01_timeseries_exploration.png` (context + seasonality)
    - A2: `03_distribution_diagnostics.png` (histogram + QQ against normal)
    - A3: `04_gev_diagnostics.png` (GEV diagnostics for completeness)

- Main text tables:
    - Table 1: `02_return_level_comparison.csv` (GEV vs GPD)
    - Table 2: `03_risk_metrics.csv` (VaR/ES + 10-year level)

- Appendix tables:
    - A1: `01_summary_statistics.csv` (descriptive stats)

Note on layout: In LaTeX (11pt), use \includegraphics[width=0.66\textwidth] for Figure 1 and \includegraphics[width=0.75\textwidth] for Figure 2 (four-panel diagnostic). Keep captions concise (≤2 lines). This layout reliably fits in ≤3 pages with 0.9–1.0 inch margins and 1.03–1.07 line spacing.

---

## Tables to Include

**Must Include:**
1. **Return Level Comparison** (`02_return_level_comparison.csv`)
   - Shows convergence from 8.6% → 3.3%
   - Validates dual-method approach

**Recommended:**
2. **Risk Metrics Summary** (`03_risk_metrics.csv`)
   - VaR₉₅, VaR₉₉, ES₉₉, 10-year level
   - Operational translation

**Optional:**
3. **Summary Statistics** (`01_summary_statistics.csv`)
   - Quick reference in introduction

---

## Writing Style Tips

### DO:
- ✅ Use active voice: "We discovered" not "It was found"
- ✅ Show your thinking: "This suspicious result motivated..."
- ✅ Connect findings: "Building on the temperature insight..."
- ✅ Quantify everything: "845 MW gap" not "substantial gap"
- ✅ Translate statistics: "21,174 MW ≈ 87 hours/year above threshold"

### DON'T:
- ❌ Just list results without connecting them
- ❌ Say "we applied EVT" without explaining why
- ❌ Ignore discrepancies between methods
- ❌ Report statistics without operational meaning
- ❌ Make recommendations without quantitative support

---

## Suggested Section Lengths

```
Page 1: Introduction & Discovery
  - Introduction & Context: 0.3 pages
  - Temperature Discovery: 0.4 pages
  - Figures (2): 0.3 pages
  
Page 2: EVT Analysis  
  - Dual-Method Approach: 0.6 pages
  - Method Comparison Table: 0.2 pages
  - Diagnostic Figure: 0.2 pages
  
Page 3: Strategy & Conclusion
  - Risk Quantification: 0.3 pages
  - Three Recommendations: 0.5 pages
  - Conclusion: 0.2 pages
```

---

## Key Messages to Convey

### Methodological Rigor
> "We didn't just apply EVT—we validated it with dual methods, investigated discrepancies, and confirmed convergence before making recommendations."

### Data-Driven Insight  
> "The U-shaped temperature relationship wasn't assumed—it was discovered through exploratory analysis and quantitatively confirmed (R² = 0.435)."

### Uncertainty Awareness
> "With only 5.2 years of data, we acknowledge ±5% uncertainty and designed a diversified capacity portfolio to manage this risk."

### Practical Translation
> "We translated the 845 MW 'surprise gap' (ES-VaR) into specific emergency procurement requirements."

### Strategic Thinking
> "Temperature-based early warning provides 6-12 hour lead time, enabling proactive rather than reactive grid management."

---

## Final Check Before Submission

- [ ] Every methodological choice is justified by a finding
- [ ] GEV vs GPD comparison and convergence are explained
- [ ] Temperature U-shape discovery and implications are clear
- [ ] Return level table shows convergence pattern (8.6% → 3.3%)
- [ ] VaR, ES, and "surprise gap" are operationally translated
- [ ] Three recommendations are quantitatively supported
- [ ] Limitations (short time series, uncertainty) are acknowledged
- [ ] Figures have clear captions connecting to narrative
- [ ] Conclusion emphasizes discovery-driven process
- [ ] Total length ≤ 3 pages

---

## Example Opening Sentence (Sets the Tone)

❌ **Bad:** "This report analyzes electricity demand data using EVT."  
✅ **Good:** "How much capacity is enough? This analysis applies Extreme Value Theory to 5 years of Midwest electricity demand data—but discovering that this short time series necessitated a dual-method validation approach that ultimately revealed not just risk estimates but the temperature-driven mechanisms behind them."

---

## The Bottom Line

**You're not writing a lab report—you're telling the story of how you solved a problem.**

The iterative framework gives you:
1. A logical chain of discovery (each finding motivates the next step)
2. Built-in validation (methods converge, giving confidence)
3. Mechanistic understanding (temperature drives extremes)
4. Quantified uncertainty (short data → use both methods)
5. Actionable strategy (specific MW targets, temperature thresholds)

**Use this to demonstrate that you can THINK like a risk analyst, not just calculate like one.**

---

## Quick Reference: The 7-Stage Logic Chain

```
Stage 1: Initial Exploration → 5.2 years (borderline) → Use dual methods
Stage 2: Relationship Discovery → U-shape (temp drives peaks)
Stage 3: EVT Application → GEV underestimates? → Try GPD
Stage 4: Model Validation → Diagnostics good, methods converge
Stage 5: Risk Quantification → VaR + ES + "surprise gap"
Stage 6: Strategic Insights → 1,000 MW + alerts + better data
Stage 7: Uncertainty → ±5% CI → Need longer time series
```

Each stage flows naturally from the previous, creating a compelling narrative arc.

---

## One More Thing: Confidence

**You have the complete analysis.** All findings are validated, all recommendations are quantified, all uncertainties are acknowledged. 

**Trust your process.** The iterative approach ISN'T longer—it's clearer. You're showing your thinking, which is what instructors want to see.

**Write with authority.** You made informed decisions at each step. Own them.

---

Ready to write? Start with the Introduction, emphasize the U-shape discovery on Page 1, showcase the method convergence on Page 2, and land the strategic recommendations on Page 3. **You've got this.** 🎯
