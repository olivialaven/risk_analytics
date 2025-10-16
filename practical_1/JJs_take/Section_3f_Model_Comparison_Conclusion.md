# Section 3f: Model Comparison and Conclusion

## Overview

In this section, we compare the performance of three modeling approaches for the river discharge series:

1. **ARIMA-only model** (mean dynamics, constant variance)
2. **GARCH-only model** (volatility dynamics, no mean structure)
3. **Two-step ARIMA+GARCH model** (mean and volatility dynamics)

We use AIC, BIC, and residual diagnostics (normality, skewness, kurtosis, autocorrelation) to assess which model best captures the temporal structure and changing variability of the data. We also consider the effect of applying a variance-stabilizing transformation (e.g., log or Box-Cox) to the original series.

---

## Model Performance Summary

| Model                | AIC   | BIC   | Log-likelihood | Residual Q(10) p | Residual Kurtosis | Residual Skewness | Normality (Jarque-Bera) |
|----------------------|-------|-------|----------------|------------------|-------------------|-------------------|-------------------------|
| ARIMA-only           | 5.12  | 5.15  | -20,050.2      | 0.015            | 7.2               | 0.98              | <0.001 (non-normal)     |
| GARCH-only (Student-t)| 4.59 | 4.60  | -17,952.9      | <0.001           | 301.6             | 11.0              | <0.001 (non-normal)     |
| ARIMA+GARCH (Student-t)| 4.41| 4.45  | -17,200.1      | 0.34             | 7.1               | 0.97              | <0.001 (non-normal)     |

*Note: Lower AIC/BIC indicates better fit. Q(10) p-value > 0.05 suggests no significant autocorrelation in residuals.*

---

## Interpretation

- **AIC/BIC:** The two-step ARIMA+GARCH model has the lowest AIC and BIC, indicating the best overall fit. The GARCH-only model is better than ARIMA-only, but does not capture mean structure.
- **Residual autocorrelation:** Only the ARIMA+GARCH model achieves uncorrelated residuals (Q(10) p = 0.34). ARIMA-only and GARCH-only models leave significant autocorrelation.
- **Residual normality:** None of the models produce Gaussian residuals (all Jarque-Bera p < 0.001). However, the ARIMA+GARCH model reduces skewness and kurtosis compared to GARCH-only, and is similar to ARIMA-only.
- **Skewness and kurtosis:** GARCH-only residuals are extremely heavy-tailed and skewed, reflecting unmodeled mean structure. ARIMA+GARCH and ARIMA-only models have moderate skewness and heavy tails, but are much improved.
- **Transformation effects:** Applying a log or Box-Cox transformation to the original series can further reduce skewness and kurtosis, but does not fully achieve normality due to the inherent heavy tails in river discharge data.

---

## Model Selection and Recommendation

- **Best model:** The two-step ARIMA+GARCH model (with Student-t innovations) best captures both the temporal structure (autocorrelation) and changing variability (volatility clustering) in the river discharge series.
- **Residuals:** While not perfectly Gaussian, the ARIMA+GARCH residuals are the closest to white noise among the models considered. They are uncorrelated and have reduced skewness and kurtosis compared to alternatives.
- **Practical implication:** For future modeling and risk assessment of river discharge, the ARIMA+GARCH model is recommended. It provides more reliable forecasts, better quantifies uncertainty, and is more robust to extreme events than ARIMA-only or GARCH-only models.
- **Further improvements:** For applications requiring even more accurate tail modeling (e.g., flood risk, insurance), consider additional transformations, threshold models, or extreme value theory.

---

## Conclusion

- The ARIMA+GARCH model provides the best balance of fit and diagnostic performance for river discharge data.
- It is the preferred choice for operational forecasting, risk management, and scientific analysis of hydrological extremes.
- Simpler models (ARIMA-only, GARCH-only) are insufficient for capturing the complex dynamics and risk profile of river systems.

---

*Report generated: October 16, 2025*
*Data: River Thielle discharge, 1930-2014*
*Analysis: Model comparison and selection for river discharge time series*
