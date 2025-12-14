
# Risk Analytics - Practical 1

############### PACKAGES ###############
library(tidyverse)   
library(lubridate)   
library(moments)
library(fGarch)
library(forecast)
library(ggplot2)
library(DescTools)
library(fitdistrplus)  
library(nortest)       
library(MASS)          
library(cowplot)   
library(extremogram)
library(dplyr)
library(tidyr)
library(lmtest)
library(fpp2)
library(ggfortify)
library(tseries)
library(patchwork)

# PART 0 ----

############### 0 - (a) Upload data ###############

# Loading the data set
data <- read.csv("~/Desktop/MA3/Risk Analytics/Session 1/River_and_precip_Neuchatel.csv")

# Inspect data
head(data) # visualize the first rows
str(data) # check structure of dataset
glimpse(data) # Check column names and data types
summary(data) # summary table

# Converting Date column 
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
str(data) # checking that the conversion is successful


############### 0 - (b) Visual inspection ###############

# Discharge Plot:
plot(data$Date, data$RiverDischarge, type="l", 
     xlab="Date", ylab="Discharge (m3/s)",
     main="Daily River Discharge", col="#205cbc")

# Precipitation Plot:
plot(data$Date, data$Precipitation, type="l", 
     xlab="Date", ylab="Precipitation (mm)",
     main="Daily Precipitation", col="#ce2e24")

# Count missing values:
colSums(is.na(data)) # no missing values


# PART 1 ----

############### 1 - (a) Visual assessment of distribution ###############

# Histogram (with density) and QQ-plot (normal)
par(mfrow=c(1,2))

# Histogram and QQ-plot for River Discharge
hist(data$RiverDischarge,
     breaks = 40,
     col = "#E6E6E6",
     main = "Histogram of River Discharge",
     xlab = "Discharge (m³/s)",
     prob=TRUE)
lines(density(data$RiverDischarge), lwd=2)

qqnorm(data$RiverDischarge,
       main = "QQ-plot of River Discharge")
qqline(data$RiverDischarge, col = "#205cbc", lwd=2)

par(mfrow=c(1,1))

# Calculate skewness and kurtosis
skewness_val <- moments::skewness(data$RiverDischarge)
kurtosis_val <- moments::kurtosis(data$RiverDischarge)
cat("Skewness:", round(skewness_val, 1), "\n")
cat("Kurtosis:", round(kurtosis_val, 1), "\n")


############### 1 - (b) Formal assessment of distribution ###############

# Anderson-Darling test on raw discharge
ad.test(data$RiverDischarge)


############### 1 - (c) Fit a distribution ###############

# Fit distributions using fitdist
fit_norm <- fitdist(data$RiverDischarge, "norm")
fit_lnorm <- fitdist(data$RiverDischarge, "lnorm")   
fit_gamma <- fitdist(data$RiverDischarge, "gamma")   

# (Want to change colors) -> Save current palette
oldpal <- palette()

# Custom colors
palette(c("#3e90c4", "#205cbc", "#ce2e24"))

# Make the QQ comparison plot
qqcomp(
  list(fit_norm, fit_lnorm, fit_gamma),
  legendtext = c("Normal", "Lognormal", "Gamma"),
  main = "QQ comparison: Normal vs Lognormal vs Gamma"
)

# Restore original palette
palette(oldpal)

############### 1 - (d) Tail comparison and interpretation ###############

# Histogram with fitted density curves
hist(data$RiverDischarge, breaks=50, prob=TRUE,
     col = "#9A9A9A", border = "white",
     main="Histogram with fitted densities",
     xlab="Discharge (m3/s)", ylim=c(0, max(density(data$RiverDischarg)$y)*1.3))

xseq <- seq(min(data$RiverDischarge), max(data$RiverDischarge), length = 1000)

# Gamma density
dgamma_fit <- dgamma(xseq, shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"])
lines(xseq, dgamma_fit, lwd=2, col="#3e90c4")

# Normal density
dnorm_fit <- dnorm(xseq, mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
lines(xseq, dnorm_fit, lwd=2, col="#205cbc")

# Lognormal density 
dlnorm_fit <- dlnorm(xseq, meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])
lines(xseq, dlnorm_fit, lwd=2, col="#ce2e24")

# Add ledgend
legend("topright", legend=c("Normal fit", "Lognormal fit", "Gamma fit"),
       col=c("#205cbc","#ce2e24","#3e90c4"), lwd=2, bty="n")


### --- Looking at which distribution assigns more probability mass to extreme events: --- ###

# Define a high threshold
threshold_95 <- quantile(data$RiverDischarge, 0.95) # 95th percentile of the observed data
threshold_99 <- quantile(data$RiverDischarge, 0.99) # 99th percentile of the observed data

# Tail probabilities (P(X > 95th percentile)) under each fitted distribution
p_norm_tail_99 <- 1 - pnorm(threshold_99, mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
p_lnorm_tail_99 <- 1 - plnorm(threshold_99, meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])

# Tail probabilities (P(X > 99th percentile)) under each fitted distribution
p_norm_tail_95 <- 1 - pnorm(threshold_95, mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
p_lnorm_tail_95 <- 1 - plnorm(threshold_95, meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])

# Exploring ratios (to understand magnitude of difference between the distributions)
ratio_99 = p_lnorm_tail_99/p_norm_tail_99
ratio_95 = p_lnorm_tail_95/p_norm_tail_95

# Print results
cat("Tail probability (Normal) beyond the 95th threshold (", threshold_95, "):", p_norm_tail_95, "\n")
cat("Tail probability (Lognormal) beyond the 95th threshold (", threshold_95, "):", p_lnorm_tail_95, "\n")
cat("Tail probability (Normal) beyond the 99th threshold (", threshold_99, "):", p_norm_tail_99, "\n")
cat("Tail probability (Lognormal) beyond the 99th threshold (", threshold_99, "):", p_lnorm_tail_99, "\n")


# PART 2 ----

############### 2 - (a) Are river discharge and precipitation dependent? ###############

# Keep rows with both values present 
df <- data[order(data$Date), ]
df <- subset(df, !is.na(Precipitation) & !is.na(RiverDischarge))

# Pearson (linear)
cor.test(df$RiverDischarge, df$Precipitation, use = "complete.obs", method = "pearson")


############### 2 - (b) Lagged dependence: Cross-correlation function (CCF) ###############

ccf(df$Precipitation, df$RiverDischarge,
    lag.max = 30, na.action = na.omit,
    main = "CCF: Precipitation (leads) → River Discharge")


############### 2 - (c) Extremograms: Cross- and auto-dependence of extreme events ###############

maxlag <- 20

# Univariate: Precipitation extremogram
ext_precip <- extremogram1(
  data$Precipitation,
  quant = 0.95,
  maxlag = maxlag,
  type = 1,
  ploting = 0  # turn off individual plotting
)

# Univariate: Discharge extremogram
ext_dis <- extremogram1(
  data$RiverDischarge,
  quant = 0.95,
  maxlag = maxlag,
  type = 1,
  ploting = 0
)

# Cross-extremogram: precip -> discharge
A <- cbind(data$Precipitation, data$RiverDischarge)

ext_cross <- extremogram2(
  A,
  quant1 = 0.95,
  quant2 = 0.95,
  maxlag = maxlag,
  type = 1,
  ploting = 0
)

lags <- 0:(maxlag - 1)

ext_df <- data.frame(
  lag = rep(lags, 3),
  value = c(ext_precip, ext_dis, ext_cross),
  type = rep(c("Precipitation", "Discharge", "Precip → Discharge"), each = maxlag)
)

# Plot all on the same graph
ggplot(ext_df, aes(x = lag, y = value, color = type)) +
  geom_line(linewidth = 1.2) +            # FIXED
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Precipitation" = "#3e90c4",        
    "Discharge" = "#205cbc",            
    "Precip → Discharge" = "#ce2e24"    
  )) +
  labs(
    title = "Extremograms (95% Threshold)",
    x = "Lag (days)",
    y = "Extremogram Value",
    color = "Series"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.85, 0.85),     # <-- position inside plot
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA)
  )


############### 2 - (d) Predictive relationships ###############

# Try a few lag orders -> 1, 2, 3, 5, and 7
for(k in c(1,2,3,5,7)){
  cat("\n--- order =", k, " ---\n")
  print(grangertest(RiverDischarge ~ Precipitation, order = k, data = df))  # Precip ⇒ Discharge?
  print(grangertest(Precipitation ~ RiverDischarge, order = k, data = df))  # Discharge ⇒ Precip?
}

# Using JuroExtremes code
source("~/Desktop/MA3/Risk Analytics/Session 1/JuroExtremes.R")
Extreme_causality_test(df$Precipitation, df$RiverDischarge)
Extreme_causality_test(df$Precipitation, df$RiverDischarge, lag_future=2)
Extreme_causality_test(df$Precipitation, df$RiverDischarge, lag_future=3)
Extreme_causality_test(df$Precipitation, df$RiverDischarge, lag_future=5)
Extreme_causality_test(df$Precipitation, df$RiverDischarge, lag_future=7)

Extreme_causality_test(df$RiverDischarge,df$Precipitation)
Extreme_causality_test(df$RiverDischarge,df$Precipitation, lag_future = 2)
Extreme_causality_test(df$RiverDischarge,df$Precipitation, lag_future = 3)
Extreme_causality_test(df$RiverDischarge,df$Precipitation, lag_future = 5)
Extreme_causality_test(df$RiverDischarge,df$Precipitation, lag_future = 7)


# PART 3 ----

############### 3 - (a) Autocorrelation patterns ###############

# Turn discharge into a ts object 
discharge_ts <- ts(data$RiverDischarge)

# First difference (lag-1)
discharge_diff <- diff(discharge_ts)

# Plot
plot(discharge_ts)
plot(discharge_diff)

# Plot ACF & PACF
p1 <- ggAcf(discharge_ts, lag.max = 50) +
  ggtitle("ACF of raw river discharge") +
  theme(plot.title.position = "plot",
        plot.margin = margin(20, 10, 10, 10))

p2 <- ggPacf(discharge_ts, lag.max = 40) +
  ggtitle("PACF of raw river discharge") +
  theme(plot.title.position = "plot",
        plot.margin = margin(20, 10, 10, 10))

p3 <- ggAcf(discharge_diff, lag.max = 50) +
  ggtitle("ACF of differenced river discharge") +
  theme(plot.title.position = "plot",
        plot.margin = margin(20, 10, 10, 10))

p4 <- ggPacf(discharge_diff, lag.max = 50) +
  ggtitle("PACF of differenced river discharge") +
  theme(plot.title.position = "plot",
        plot.margin = margin(20, 10, 10, 10))

# ACF
(p1 | p3) 

# ACF & PACF
(p1 | p2) /
  (p3 | p4)


############### 3 - (b) Serial dependence testing ###############

# Ljung-Box test on raw discharge
Box.test(discharge_ts, lag = 1, type = "Ljung-Box")

# Ljung-Box test on differenced discharge
Box.test(discharge_diff, lag = 1, type = "Ljung-Box")


############### 3 - (c) ARIMA modeling ###############

arima_diagnostics <- function(model, title = "ARIMA Model Diagnostics", col = "#205cbc") {
  res <- residuals(model)
  
  # Set up a 2x2 grid for plots
  par(
    mfrow = c(2, 2),
    mar = c(4, 4, 2, 1),
    oma = c(0, 0, 3, 0) # space for an overall title
  )
  
  ## 1) Time plot of residuals
  plot(res, type = "l", main = "", ylab = "Residuals", xlab = "Time")
  mtext("Time Plot of Residuals", side = 3, line = 1, font = 2)
  abline(h = 0, col = "#ce2e24", lty = 2)
  
  ## 2) ACF of residuals
  acf(res, main = "")
  mtext("ACF of Residuals", side = 3, line = 1, font = 2)
  
  ## 3) Time plot of *squared* residuals (check for volatility/ARCH effects)
  acf(res^2, main = "")
  mtext("ACF of Squared Residuals", side = 3, line = 1, font = 2)
  
  ## 4) QQ-plot
  qqnorm(res, main = "")
  qqline(res, col = col)
  mtext("Normal Q–Q Plot", side = 3, line = 1, font = 2)
  
  ## Overall title
  mtext(title, outer = TRUE, cex = 1.2, font = 2)
}

arima_tests <- function(model, lags = 10) {
  
  library(DescTools)
  
  res <- as.numeric(residuals(model))
  res <- res[!is.na(res)]
  
  lb_res <- Box.test(res, lag = lags, type = "Ljung-Box")
  lb_sq  <- Box.test(res^2, lag = lags, type = "Ljung-Box")
  
  ad_test <- ad.test(residuals(model))
  
  cat("\n===== ARIMA Residual Diagnostics =====\n")
  
  cat("\nLjung–Box test (residuals):\n")
  print(lb_res)
  
  cat("\nLjung–Box test (squared residuals – ARCH check):\n")
  print(lb_sq)
  
  cat("\nAnderson–Darling test (normality):\n")
  print(ad_test)
  
  invisible(list(
    lb_residuals = lb_res,
    lb_squared   = lb_sq,
    ad_test      = ad_test
  ))
}


# Candidate ARIMA models
mods <- list(
  m1 = Arima(discharge_diff, order = c(0,0,2)),
  m2 = Arima(discharge_diff, order = c(2,0,2)),
  m3 = Arima(discharge_diff, order = c(2,0,3)),
  m4 = Arima(discharge_diff, order = c(3,0,2)),
  m5 = Arima(discharge_diff, order = c(3,0,3))
)

# Automatic selection
mod_auto <- auto.arima(discharge_diff)

# Comparison
sapply(c(mods, list(auto = mod_auto)), AIC)

# ACF/PACF: analyze
# 1) Plots
arima_diagnostics(mod_auto,
                  "Raw-differenced ARIMA",
                  col = "#205cbc")

# 2) Formal tests
arima_tests(mod_auto)
ad.test(residuals(mod_auto))
checkresiduals(mod_auto)
arch_test_result_raw <- ArchTest(residuals(mod_auto), lags = 10)
print(arch_test_result_raw)


# Test LOG-transformation
log_diff <- diff(log(discharge_ts))

ggAcf(log_diff, lag.max = 40) +
  ggtitle("ACF of differenced log discharge")

ggPacf(log_diff, lag.max = 40) +
  ggtitle("PACF of differenced log discharge")

mod_log_auto <- auto.arima(log_diff)

arima_diagnostics(mod_log_auto,
                  "Log-differenced ARIMA",
                  col = "#ce2e24")

arima_tests(mod_log_auto)
ad.test(residuals(mod_log_auto))
checkresiduals(mod_log_auto)
arch_test_result_log <- ArchTest(residuals(mod_log_auto), lags = 10)
print(arch_test_result_log)

# Compare residuals with Gaussian reference
par(mfrow = c(1, 2))

# Raw ARIMA residuals
hist(
  residuals(mod_auto),
  breaks = 40,
  probability = TRUE,
  main = "Raw ARIMA residuals",
  xlab = "Residuals"
)
curve(
  dnorm(x, mean(residuals(mod_auto)), sd(residuals(mod_auto))),
  add = TRUE,
  col = "#ce2e24",
  lwd = 2
)

# Log ARIMA residuals
hist(
  residuals(mod_log_auto),
  breaks = 40,
  probability = TRUE,
  main = "Log ARIMA residuals",
  xlab = "Residuals"
)
curve(
  dnorm(x, mean(residuals(mod_log_auto)), sd(residuals(mod_log_auto))),
  add = TRUE,
  col = "#ce2e24",
  lwd = 2
)

# Residuals vs fitted values for ARIMA models (variance diagnostics)
par(mfrow = c(1, 2))

# Raw ARIMA
plot(
  fitted(mod_auto), residuals(mod_auto),
  main = "Raw ARIMA: Residuals vs Fitted",
  xlab = "Fitted values",
  ylab = "Residuals",
  pch = 20, col = rgb(0, 0, 0, 0.5)
)
abline(h = 0, col = "#ce2e24", lwd = 2)

# Log ARIMA
plot(
  fitted(mod_log_auto), residuals(mod_log_auto),
  main = "Log ARIMA: Residuals vs Fitted",
  xlab = "Fitted values",
  ylab = "Residuals",
  pch = 20, col = rgb(0, 0, 0, 0.5)
)
abline(h = 0, col = "#ce2e24", lwd = 2)

# Skewness and excess kurtosis
arima_dist_stats <- data.frame(
  Model = c("ARIMA (Raw)", "ARIMA (Log)"),
  Skewness = c(
    skewness(residuals(mod_auto), na.rm = TRUE),
    skewness(residuals(mod_log_auto), na.rm = TRUE)
  ),
  Excess_Kurtosis = c(
    kurtosis(residuals(mod_auto), na.rm = TRUE) - 3,
    kurtosis(residuals(mod_log_auto), na.rm = TRUE) - 3
  ),
  stringsAsFactors = FALSE
)

# Round numeric columns ONLY
arima_dist_stats$Skewness <- round(arima_dist_stats$Skewness, 2)
arima_dist_stats$Excess_Kurtosis <- round(arima_dist_stats$Excess_Kurtosis, 2)

# Print output
print(arima_dist_stats)

############### 3 - (d) Modeling volatility: GARCH ###############

garch_diagnostics <- function(model, title = "GARCH Model Diagnostics", col = "#205cbc") {
  
  res <- residuals(model, standardize = TRUE)
  res <- as.numeric(res)
  res <- res[!is.na(res)]
  
  par(
    mfrow = c(2, 2),
    mar = c(4, 4, 2, 1),
    oma = c(0, 0, 3, 0)
  )
  
  ## 1) Time plot of standardized residuals
  plot(res, type = "l", main = "",
       ylab = "Standardized Residuals", xlab = "Time")
  abline(h = 0, col = "#ce2e24", lty = 2)
  mtext("Time Plot of Std. Residuals", side = 3, line = 1, font = 2)
  
  ## 2) ACF of standardized residuals
  acf(res, main = "")
  mtext("ACF of Std. Residuals", side = 3, line = 1, font = 2)
  
  ## 3) ACF of squared standardized residuals
  acf(res^2, main = "")
  mtext("ACF of Squared Std. Residuals", side = 3, line = 1, font = 2)
  
  ## 4) QQ-plot (distributional check only)
  qqnorm(res, main = "")
  qqline(res, col = col, lwd = 2)
  mtext("Q–Q Plot (Std. Residuals)", side = 3, line = 1, font = 2)
  
  ## Overall title
  mtext(title, outer = TRUE, cex = 1.2, font = 2)
}

garch_tests <- function(model, lags = 10) {
  
  res <- residuals(model, standardize = TRUE)
  res <- as.numeric(res)
  res <- res[!is.na(res)]
  
  lb_res <- Box.test(res, lag = lags, type = "Ljung-Box")
  lb_sq  <- Box.test(res^2, lag = lags, type = "Ljung-Box")
  
  cat("\n===== GARCH Residual Diagnostics =====\n")
  
  cat("\nLjung–Box test (standardized residuals):\n")
  print(lb_res)
  
  cat("\nLjung–Box test (squared standardized residuals – remaining ARCH):\n")
  print(lb_sq)
  
  invisible(list(
    lb_std_residuals = lb_res,
    lb_squared       = lb_sq
  ))
}

# Raw series
y <- ts(data$RiverDischarge)

# Log-transformed series
y_log <- log(y)

# Differenced (stationary) versions
dy <- diff(y)
dy_log <- diff(y_log)

### --- GARCH-only Models --- ###

# GARCH(1,1) on differenced RAW, Normal
garch_raw_norm <- garchFit(
  ~ garch(1,1),
  data = dy,
  cond.dist = "norm",
  trace = FALSE
)

garch_diagnostics(
  garch_raw_norm,
  title = "GARCH(1,1) (raw, Normal):"
)
garch_tests(garch_raw_norm)
summary(garch_raw_norm)

# GARCH(1,1) on differenced RAW, Student-t
garch_raw_t <- garchFit(
  ~ garch(1,1),
  data = dy,
  cond.dist = "std",
  trace = FALSE
)

garch_diagnostics(
  garch_raw_t,
  title = "GARCH(1,1) (raw, Student-t):"
)
garch_tests(garch_raw_t)
summary(garch_raw_t)

# GARCH(1,1) on differenced LOG, Normal
garch_log_norm <- garchFit(
  ~ garch(1,1),
  data = dy_log,
  cond.dist = "norm",
  trace = FALSE
)

garch_diagnostics(
  garch_log_norm,
  title = "GARCH(1,1) (log, Normal):"
)
garch_tests(garch_log_norm)
summary(garch_log_norm)

# GARCH(1,1) on differenced LOG, Student-t
garch_log_t <- garchFit(
  ~ garch(1,1),
  data = dy_log,
  cond.dist = "std",
  trace = FALSE
)

garch_diagnostics(
  garch_log_t,
  title = "GARCH(1,1) (log, Student-t):"
)
garch_tests(garch_log_t)
summary(garch_log_t)


############### 3 - (e) Two-step modeling approach ###############

# ARIMA on differenced RAW series
arima_raw <- auto.arima(dy)
#summary(arima_raw)
#checkresiduals(arima_raw)

# ARIMA on differenced LOG series
arima_log <- auto.arima(dy_log)
#summary(arima_log)
#checkresiduals(arima_log)

# Residuals from ARIMA models
res_arima_raw <- residuals(arima_raw)
res_arima_log <- residuals(arima_log)

# ARIMA (raw) + GARCH Normal
garch_arima_raw_norm <- garchFit(
  ~ garch(1,1),
  data = res_arima_raw,
  cond.dist = "norm",
  trace = FALSE
)
garch_diagnostics(
  garch_arima_raw_norm,
  title = "ARIMA+GARCH (raw, Normal):"
)
garch_tests(garch_arima_raw_norm)
summary(garch_arima_raw_norm)

# ARIMA (raw) + GARCH t-distribution
garch_arima_raw_t <- garchFit(
  ~ garch(1,1),
  data = res_arima_raw,
  cond.dist = "std",
  trace = FALSE
)
garch_diagnostics(
  garch_arima_raw_t,
  title = "ARIMA+GARCH (raw, Student-t):"
)
garch_tests(garch_arima_raw_t)
summary(garch_arima_raw_t)

# ARIMA (log) + GARCH Normal
garch_arima_log_norm <- garchFit(
  ~ garch(1,1),
  data = res_arima_log,
  cond.dist = "norm",
  trace = FALSE
)
garch_diagnostics(
  garch_arima_log_norm,
  title = "ARIMA+GARCH (log, Normal):"
)
garch_tests(garch_arima_log_norm)
summary(garch_arima_log_norm)

# ARIMA (log) + GARCH t-distribution
garch_arima_log_t <- garchFit(
  ~ garch(1,1),
  data = res_arima_log,
  cond.dist = "std",
  trace = FALSE
)
garch_diagnostics(
  garch_arima_log_t,
  title = "ARIMA+GARCH (log, Student-t):"
)
garch_tests(garch_arima_log_t)
summary(garch_arima_log_t)


############### 3 - (f) Model comparison and conclusion ###############

# AIC/BIC scores - original data
aic_norm  <- garch_raw_norm@fit$ics["AIC"]
bic_norm  <- garch_raw_norm@fit$ics["BIC"]

aic_t     <- garch_raw_t@fit$ics["AIC"]
bic_t     <- garch_raw_t@fit$ics["BIC"]

aic_combo_norm <- garch_arima_raw_norm@fit$ics["AIC"]
bic_combo_norm <- garch_arima_raw_norm@fit$ics["BIC"]

aic_combo_t <- garch_arima_raw_t@fit$ics["AIC"]
bic_combo_t <- garch_arima_raw_t@fit$ics["BIC"]

cbind(
  model = c("GARCH norm", "GARCH t", "ARIMA+GARCH Normal", "ARIMA+GARCH t"),
  AIC   = c(aic_norm, aic_t, aic_combo_norm, aic_combo_t),
  BIC   = c(bic_norm, bic_t, bic_combo_norm, bic_combo_t)
)

# Histograms of standardized residuals (raw data)
par(mfrow = c(2, 2))

plot_hist_with_norm <- function(z, main, breaks = 50) {
  hist(z,
       main = main,
       xlab = "Standardized residuals",
       prob = TRUE,
       col = "lightgray",
       border = "white",
       breaks = breaks)
  curve(
    dnorm(x, mean = mean(z, na.rm = TRUE), sd = sd(z, na.rm = TRUE)),
    add = TRUE, col = "#205cbc", lwd = 2
  )
}

# Helper to extract standardized residuals from fGarch objects
get_stdres <- function(fit) {
  residuals(fit, standardize = TRUE)
}

plot_hist_with_norm(get_stdres(garch_raw_norm),       "GARCH (Raw, Normal)")
plot_hist_with_norm(get_stdres(garch_raw_t),          "GARCH (Raw, Student-t)")
plot_hist_with_norm(get_stdres(garch_arima_raw_norm), "ARIMA+GARCH (Raw, Normal)")
plot_hist_with_norm(get_stdres(garch_arima_raw_t),    "ARIMA+GARCH (Raw, Student-t)")


# AIC/BIC scores - Log-transformed data
aic_log_norm  <- garch_log_norm@fit$ics["AIC"]
bic_log_norm  <- garch_log_norm@fit$ics["BIC"]

aic_log_t     <- garch_log_t@fit$ics["AIC"]
bic_log_t     <- garch_log_t@fit$ics["BIC"]

aic_log_combo_norm <- garch_arima_log_norm@fit$ics["AIC"]
bic_log_combo_norm <- garch_arima_log_norm@fit$ics["BIC"]

aic_log_combo_t <- garch_arima_log_t@fit$ics["AIC"]
bic_log_combo_t <- garch_arima_log_t@fit$ics["BIC"]

cbind(
  model = c("(Log) GARCH norm", "(Log) GARCH t", "(Log) ARIMA+GARCH Normal", "(Log) ARIMA+GARCH t"),
  AIC   = c(aic_log_norm, aic_log_t, aic_log_combo_norm, aic_log_combo_t),
  BIC   = c(bic_log_norm, bic_log_t, bic_log_combo_norm, bic_log_combo_t)
)

# Log-transformed data: Residuals (histograms)
plot_hist_with_norm <- function(z, main, breaks = 50) {
  hist(z,
       main = main,
       xlab = "Standardized residuals",
       prob = TRUE,
       col = "lightgray",
       border = "white",
       breaks = breaks)
  curve(dnorm(x, mean = mean(z, na.rm = TRUE), sd = sd(z, na.rm = TRUE)),
        add = TRUE, col = "#ce2e24", lwd = 2)
}

# Helper: standardized residuals for fGarch objects
get_stdres <- function(fit) {
  residuals(fit, standardize = TRUE)
}

par(mfrow = c(2, 2))

plot_hist_with_norm(get_stdres(garch_log_norm),       "GARCH (Log, Normal)")
plot_hist_with_norm(get_stdres(garch_log_t),          "GARCH (Log, Student-t)")
plot_hist_with_norm(get_stdres(garch_arima_log_norm), "ARIMA+GARCH (Log, Normal)")
plot_hist_with_norm(get_stdres(garch_arima_log_t),    "ARIMA+GARCH (Log, Student-t)")

# Skewness and excess curtosis
# Create a named list of models
models <- list(
  "GARCH (Raw, Normal)"        = garch_raw_norm,
  "GARCH (Raw, Student-t)"     = garch_raw_t,
  "ARIMA+GARCH (Raw, Normal)"  = garch_arima_raw_norm,
  "ARIMA+GARCH (Raw, Student-t)" = garch_arima_raw_t,
  "GARCH (Log, Normal)"        = garch_log_norm,
  "GARCH (Log, Student-t)"     = garch_log_t,
  "ARIMA+GARCH (Log, Normal)"  = garch_arima_log_norm,
  "ARIMA+GARCH (Log, Student-t)" = garch_arima_log_t
)

# Build data frame
garch_dist_stats <- data.frame(
  Model = names(models),
  Skewness = sapply(models, function(fit)
    skewness(get_stdres(fit), na.rm = TRUE)
  ),
  Excess_Kurtosis = sapply(models, function(fit)
    kurtosis(get_stdres(fit), na.rm = TRUE) - 3
  ),
  stringsAsFactors = FALSE
)

# Round numeric columns
garch_dist_stats$Skewness <- round(garch_dist_stats$Skewness, 2)
garch_dist_stats$Excess_Kurtosis <- round(garch_dist_stats$Excess_Kurtosis, 2)

# Print output
print(garch_dist_stats)


