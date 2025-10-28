# 02_energy_demand_analysis.R
# Full analysis workflow for energy demand and temperature risk analytics project
# Outputs summary and key results to markdown for report writing

library(data.table)
library(ggplot2)
library(lubridate)
library(forecast)
library(tseries)
library(fGarch)
library(extRemes)

setwd("./final_project")

# Output markdown file
md_out <- "energy_demand_analysis_output.md"
cat("# Energy Demand & Extreme Temperature Analysis\n\n", file=md_out)

# 1. Data Selection & Cleaning
cat("## 1. Data Selection & Cleaning\n", file=md_out, append=TRUE)
aep <- fread("AEP_hourly.csv")
temp <- fread("temperature.csv")
# Convert datetime columns
if("Datetime" %in% names(aep)) aep$Datetime <- ymd_hms(aep$Datetime)
if("datetime" %in% names(temp)) temp$datetime <- ymd_hms(temp$datetime)

# Select time window (e.g. 2016-2018)
start_date <- ymd_hms("2016-01-01 00:00:00")
end_date <- ymd_hms("2018-12-31 23:00:00")
aep_sub <- aep[Datetime >= start_date & Datetime <= end_date]
temp_sub <- temp[datetime >= start_date & datetime <= end_date]

# Use Chicago temperature for analysis
temp_sub$Temperature <- temp_sub$Chicago

cat("Selected time window: 2016-2018\n", file=md_out, append=TRUE)
cat("AEP rows:", nrow(aep_sub), " Temp rows:", nrow(temp_sub), "\n\n", file=md_out, append=TRUE)

# 2. Exploratory Data Analysis (EDA)
cat("## 2. Exploratory Data Analysis\n", file=md_out, append=TRUE)
cat("### Summary statistics\n", file=md_out, append=TRUE)
cat(paste(capture.output(summary(aep_sub$AEP_MW)), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)
cat(paste(capture.output(summary(temp_sub$Temperature)), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)

cat("### Extreme events\n", file=md_out, append=TRUE)
q_demand <- quantile(aep_sub$AEP_MW, 0.95, na.rm=TRUE)
q_temp <- quantile(temp_sub$Temperature, 0.95, na.rm=TRUE)
cat(paste("95th percentile demand:", round(q_demand,2)), file=md_out, append=TRUE)
cat(paste("95th percentile temperature:", round(q_temp,2)), file=md_out, append=TRUE)
cat("\n\n", file=md_out, append=TRUE)

# 3. Extreme Value Analysis
cat("## 3. Extreme Value Analysis\n", file=md_out, append=TRUE)
cat("### POT for demand\n", file=md_out, append=TRUE)
pot_fit <- fevd(aep_sub$AEP_MW, threshold=q_demand, type="GP")
cat(paste(capture.output(summary(pot_fit)), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)

# 4. Time Series Modeling
cat("## 4. Time Series Modeling\n", file=md_out, append=TRUE)
cat("### ARIMA model\n", file=md_out, append=TRUE)
fit_arima <- auto.arima(aep_sub$AEP_MW)
cat(paste(capture.output(summary(fit_arima)), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)

cat("### GARCH model (Student-t)\n", file=md_out, append=TRUE)
fit_garch <- garchFit(~garch(1,1), data=aep_sub$AEP_MW, cond.dist="std", trace=FALSE)
cat(paste(capture.output(summary(fit_garch)), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)

# 5. Causality & Dependence
cat("## 5. Causality & Dependence\n", file=md_out, append=TRUE)
cat("### Granger causality test\n", file=md_out, append=TRUE)
if(length(aep_sub$AEP_MW) == length(temp_sub$Temperature)) {
  gc_test <- grangertest(aep_sub$AEP_MW ~ temp_sub$Temperature, order=24)
  cat(paste(capture.output(gc_test), collapse="\n"), file=md_out, append=TRUE)
} else {
  cat("(Skipped: unequal lengths after filtering)\n", file=md_out, append=TRUE)
}
cat("\n", file=md_out, append=TRUE)

# 6. Model Diagnostics & Comparison
cat("## 6. Model Diagnostics & Comparison\n", file=md_out, append=TRUE)
cat("### ARIMA residuals\n", file=md_out, append=TRUE)
cat(paste(capture.output(Box.test(residuals(fit_arima), lag=24, type="Ljung-Box")), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)
cat("### GARCH residuals\n", file=md_out, append=TRUE)
cat(paste(capture.output(Box.test(residuals(fit_garch), lag=24, type="Ljung-Box")), collapse="\n"), file=md_out, append=TRUE)
cat("\n", file=md_out, append=TRUE)

# 7. Key Outputs & Takeaways
cat("## 7. Key Outputs & Takeaways\n", file=md_out, append=TRUE)
cat("- Time series plots (energy, temperature)\n- Extreme value analysis (return periods, thresholds)\n- Model comparison (AIC, residual diagnostics)\n- Causality results\n- Main findings and risk implications\n", file=md_out, append=TRUE)

cat("\n---\nAnalysis complete. Use this markdown output to guide your report writing.\n", file=md_out, append=TRUE)
