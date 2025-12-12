
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

# Raw series ACF
autoplot(discharge_ts) +
  ggtitle("Raw River Discharge (Time Series)")
ggAcf(discharge_ts, lag.max = 50) +
  ggtitle("ACF of Raw River Discharge")

# First difference (lag-1)
discharge_diff <- diff(discharge_ts)

autoplot(discharge_diff) +
  ggtitle("Differenced River Discharge (lag-1)")
ggAcf(discharge_diff, lag.max = 50) +
  ggtitle("ACF of Differenced River Discharge")


############### 3 - (b) Serial dependence testing ###############

# Ljung-Box test on raw discharge
Box.test(discharge_ts, lag = 1, type = "Ljung-Box")

# Ljung-Box test on differenced discharge
Box.test(discharge_diff, lag = 1, type = "Ljung-Box")


############### 3 - (c) ARIMA modeling ###############

# Visual inspection-based candidate models
ggAcf(discharge_diff, lag.max = 40) +
  ggtitle("ACF of Differenced River Discharge")
ggPacf(discharge_diff, lag.max = 40) +
  ggtitle("PACF of Differenced River Discharge")

# Fit a candidate ARIMA model (on differenced discharge)
mod1 <- Arima(discharge_diff, order = c(1,0,1))
mod2 <- Arima(discharge_diff, order = c(2,0,1))
mod3 <- Arima(discharge_diff, order = c(1,0,2))
mod4 <- Arima(discharge_diff, order = c(2,0,2))

# Automatic model selection
mod_auto <- auto.arima(discharge_diff)

# Check model results
mod1
mod2
mod3
mod4
mod_auto

# Look at residuals (auto ARIMA on differenced discharge)
checkresiduals(mod_auto)

# Normality check
res_auto <- residuals(mod_auto)
qqnorm(res_auto, main = "Auto-ARIMA (on diff_dis): Q–Q Plot of Residuals")
qqline(res_auto, col="#205cbc")
ad.test(res_auto)

# Testing if Log-transformation is a better fit
log_discharge_ts <- log(discharge_ts)

# Then repeat differencing + ARIMA on log scale:
log_diff <- diff(log_discharge_ts)

ggAcf(log_diff) +
  ggtitle("ACF of Differenced Log River Discharge")
ggPacf(log_diff) +
  ggtitle("PACF of Differenced Log River Discharge")

mod_log_auto <- auto.arima(log_diff)

checkresiduals(mod_log_auto)

# Normality check -> Log-transformed
qqnorm(residuals(mod_log_auto), main = "Log-transformed auto-ARIMA: Q–Q Plot of Residuals")
qqline(residuals(mod_log_auto), col="#ce2e24")
ad.test(residuals(mod_log_auto))

# Histogram for residual distributions
hist(residuals(mod_auto), main="Residual Histogram", xlab="Residuals")
hist(residuals(mod_log_auto), main="Residual Histogram (log transform)", xlab="Residuals")


############### 3 - (d) Modeling volatility: GARCH ###############

# Raw series
y <- ts(data$RiverDischarge)

# Log-transformed series
y_log <- log(y)

# Differenced (stationary) versions
dy <- diff(y)
dy_log <- diff(y_log)

### --- GARCH-only Models --- ###

# GARCH(1,1) on differenced RAW, Normal
garch_raw_norm <- garchFit(~ garch(1,1), data = dy, cond.dist = "norm")
summary(garch_raw_norm)
res <- residuals(garch_raw_norm, standardize = TRUE)
ggAcf(res) + ggtitle("Residuals: GARCH(1,1) on diff Discharge, Normal")
ggAcf(res^2) + ggtitle("Residuals^2: GARCH(1,1) on diff Discharge, Normal")
qqnorm(res, main = "Q-Q: GARCH(1,1) on diff Discharge, Normal") 
qqline(res, col="#205cbc")

# GARCH(1,1) on differenced RAW, Student-t
garch_raw_t <- garchFit(~ garch(1,1), data = dy, cond.dist = "std", trace = FALSE)
summary(garch_raw_t)
res_t <- residuals(garch_raw_t, standardize = TRUE)
ggAcf(res_t) + ggtitle("Residuals: GARCH(1,1) on diff Discharge, Student-t")
ggAcf(res_t^2) + ggtitle("Residuals^2: GARCH(1,1) on diff Discharge, Student-t")
qqnorm(res_t, main = "Q-Q: GARCH(1,1) on diff Discharge, Student-t") 
qqline(res_t, col="#205cbc")

# GARCH(1,1) on differenced LOG, Normal
garch_log_norm <- garchFit(~ garch(1,1), data = dy_log, cond.dist = "norm", trace = FALSE)
summary(garch_log_norm)
res_log_norm <- residuals(garch_log_norm, standardize = TRUE)
ggAcf(res_log_norm) + ggtitle("Residuals: GARCH(1,1) on Log diff Discharge, Normal")
ggAcf(res_log_norm^2) + ggtitle("Residuals^2: GARCH(1,1) on Log diff Discharge, Normal")
qqnorm(res_log_norm, main = "Q-Q: GARCH(1,1) on Log diff Discharge, Normal") 
qqline(res_log_norm, col="#ce2e24")

# GARCH(1,1) on differenced LOG, Student-t
garch_log_t <- garchFit(~ garch(1,1), data = dy_log, cond.dist = "std", trace = FALSE)
summary(garch_log_t)
res_log_t <- residuals(garch_log_t, standardize = TRUE)
ggAcf(res_log_t) + ggtitle("Residuals: GARCH(1,1) on Log diff Discharge, Student-t")
ggAcf(res_log_t^2) + ggtitle("Residuals^2: GARCH(1,1) on Log diff Discharge, Student-t")
qqnorm(res_log_t, main = "Q-Q: GARCH(1,1) on Log diff Discharge, Student-t") 
qqline(res_log_t, col="#ce2e24")


############### 3 - (e) Two-step modeling approach ###############

# ARIMA on differenced RAW series
arima_raw <- auto.arima(dy)
summary(arima_raw)
checkresiduals(arima_raw)

# ARIMA on differenced LOG series
arima_log <- auto.arima(dy_log)
summary(arima_log)
checkresiduals(arima_log)

# Residuals from ARIMA models
res_arima_raw <- residuals(arima_raw)
res_arima_log <- residuals(arima_log)

# ARIMA (raw) + GARCH Normal
garch_arima_raw_norm <- garchFit(~ garch(1,1), data = res_arima_raw, cond.dist = "norm", trace = FALSE)
summary(garch_arima_raw_norm)
std_res <- residuals(garch_arima_raw_norm, standardize=TRUE)
ggAcf(std_res) + ggtitle("Residuals: ARIMA+GARCH on diff Discharge, Normal")
ggAcf(std_res^2) + ggtitle("Residuals^2: ARIMA+GARCH on diff Discharge, Normal")
qqnorm(std_res, main = "Q-Q: ARIMA+GARCH on diff Discharge, Normal") 
qqline(std_res, col="#205cbc")

# ARIMA (raw) + GARCH t-distribution
garch_arima_raw_t <- garchFit(~ garch(1,1), data = res_arima_raw, cond.dist = "std", trace = FALSE)
summary(garch_arima_raw_t)
std_res_t <- residuals(garch_arima_raw_t, standardize=TRUE)
ggAcf(std_res_t) + ggtitle("Residuals: ARIMA+GARCH on diff Discharge, t-distribution")
ggAcf(std_res_t^2) + ggtitle("Residuals^2: ARIMA+GARCH on diff Discharge, t-distribution")
qqnorm(std_res_t, main = "Q-Q: ARIMA+GARCH on diff Discharge, t-distribution") 
qqline(std_res_t, col="#205cbc")

# ARIMA (log) + GARCH Normal
garch_arima_log_norm <- garchFit(~ garch(1,1), data = res_arima_log, cond.dist = "norm", trace = FALSE)
summary(garch_arima_log_norm)
std_res_log_norm <- residuals(garch_arima_log_norm, standardize=TRUE)
ggAcf(std_res_log_norm) + ggtitle("Residuals: ARIMA+GARCH on LOG diff Discharge, Normal")
ggAcf(std_res_log_norm^2) + ggtitle("Residuals^2: ARIMA+GARCH on LOG diff Discharge, Normal")
qqnorm(std_res_log_norm, main = "Q-Q: ARIMA+GARCH on LOG diff Discharge, Normal") 
qqline(std_res_log_norm, col="#ce2e24")

# ARIMA (log) + GARCH t-distribution
garch_arima_log_t <- garchFit(~ garch(1,1), data = res_arima_log, cond.dist = "std", trace = FALSE)
summary(garch_arima_log_t)
std_res_log_t <- residuals(garch_arima_log_t, standardize=TRUE)
ggAcf(std_res_log_t) + ggtitle("Residuals: ARIMA+GARCH on LOG diff Discharge, t-distribution")
ggAcf(std_res_log_t^2) + ggtitle("Residuals^2: ARIMA+GARCH on LOG diff Discharge, t-distribution")
qqnorm(std_res_log_t, main = "Q-Q: ARIMA+GARCH on LOG diff Discharge, t-distribution") 
qqline(std_res_log_t, col="#ce2e24")


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

# Original data: Residuals (histograms)
par(mfrow = c(2, 2))

plot_hist_with_norm <- function(res, main, breaks = 50) {
  hist(res,
       main = main,
       xlab = "Residuals",
       prob = TRUE,
       col = "lightgray",
       border = "white",
       breaks = breaks)
  curve(dnorm(x, mean = mean(res), sd = sd(res)),
        add = TRUE, col = "#205cbc", lwd = 2)
}

plot_hist_with_norm(residuals(garch_raw_norm), "GARCH (Normal)")
plot_hist_with_norm(residuals(garch_raw_t), "GARCH (Student-t)")
plot_hist_with_norm(residuals(garch_arima_raw_norm), "ARIMA+GARCH (Normal)")
plot_hist_with_norm(residuals(garch_arima_raw_t), "ARIMA+GARCH (Student-t)")

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
par(mfrow = c(2, 2))

plot_hist_with_norm_log <- function(res, main, breaks = 50) {
  hist(res,
       main = main,
       xlab = "Residuals",
       prob = TRUE,
       col = "lightgray",
       border = "white",
       breaks = breaks)
  curve(dnorm(x, mean = mean(res), sd = sd(res)),
        add = TRUE, col = "#ce2e24", lwd = 2)
}

plot_hist_with_norm_log(residuals(garch_log_norm), "GARCH (Log, Normal)")
plot_hist_with_norm_log(residuals(garch_log_t), "GARCH (Log, Student-t)")
plot_hist_with_norm_log(residuals(garch_arima_log_norm), "ARIMA+GARCH (Log, Normal)")
plot_hist_with_norm_log(residuals(garch_arima_log_t), "ARIMA+GARCH (Log, Student-t)")





