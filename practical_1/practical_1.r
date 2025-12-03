
# Risk Analytics - Practical 1
#install.packages("tidyverse")
library(tidyverse)   # for data manipulation and plotting
library(lubridate)   # for date handling

#install.packages("moments")
library(moments)

# Part 0 ----

#(a) Upload data----

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

#(b) Visual inspection----

# Discharge Plot:
plot(data$Date, data$RiverDischarge, type="l", 
     xlab="Date", ylab="Discharge (m3/s)",
     main="Daily River Discharge", col="blue")

# Precipitation Plot:
plot(data$Date, data$Precipitation, type="l", 
     xlab="Date", ylab="Precipitation (mm)",
     main="Daily Precipitation", col="darkgreen")

# Count missing values:
colSums(is.na(data))

# Part 1 ----

#(a) Visual assessment of distribution----

# Downloading packages
#install.packages(c("fitdistrplus","nortest","MASS","ggplot2","cowplot"))  
library(fitdistrplus)  # for fitting distributions and diagnostic plots
library(nortest)       # for Anderson-Darling test (ad.test)
library(MASS)          # fitdistr (alternative)
library(ggplot2)       # nicer plotting
library(cowplot)       # combining ggplots (optional)

# Histogram (with density) and QQ-plot (normal)
par(mfrow=c(1,2))

# Histogram and QQ-plot for River Discharge
hist(data$RiverDischarge,
     breaks = 40,
     col = "skyblue",
     main = "Histogram of River Discharge",
     xlab = "Discharge (m³/s)",
     prob=TRUE)
lines(density(data$RiverDischarge), lwd=2)

qqnorm(data$RiverDischarge,
       main = "QQ-plot of River Discharge")
qqline(data$RiverDischarge, col = "red", lwd=2)

par(mfrow=c(1,1))

# Calculate skewness and kurtosis
skewness_val <- moments::skewness(data$RiverDischarge)
kurtosis_val <- moments::kurtosis(data$RiverDischarge)
cat("Skewness:", round(skewness_val, 3), "\n")
cat("Kurtosis:", round(kurtosis_val, 3), "\n")

# (b) Formal assessment of distribution----
# Install if needed
#install.packages("nortest")
library(nortest)
library(fitdistrplus)

# Anderson-Darling test on raw discharge
ad.test(data$RiverDischarge)

# AD test on log-transformed discharge (common hydrology practice)
#ad_log <- nortest::ad.test(log(df$RiverDischarge))
#ad_log

# (c) Fit a distribution----

# Fit distributions using fitdist (requires positive values for lnorm/gamma)
fit_norm <- fitdist(data$RiverDischarge, "norm")
fit_lnorm <- fitdist(data$RiverDischarge, "lnorm")   # lognormal
fit_gamma <- fitdist(data$RiverDischarge, "gamma")   # gamma

# QQ-plots to compare fits
qqcomp(list(fit_norm, fit_lnorm, fit_gamma), legendtext = c("Normal","Lognormal","Gamma"),
       main="QQ comparison: Normal vs Lognormal vs Gamma")

# (d) Tail comparison and interpretation----
# Histogram with fitted density curves
hist(data$RiverDischarge, breaks=50, prob=TRUE,
     col = "lightblue", border = "white",
     main="Histogram with fitted densities",
     xlab="Discharge (m3/s)", ylim=c(0, max(density(data$RiverDischarg)$y)*1.3))

xseq <- seq(min(data$RiverDischarge), max(data$RiverDischarge), length = 1000)

# Normal density
dnorm_fit <- dnorm(xseq, mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
lines(xseq, dnorm_fit, lwd=2, col="red")

# Lognormal density 
dlnorm_fit <- dlnorm(xseq, meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])
lines(xseq, dlnorm_fit, lwd=2, col="darkgreen")

# Gamma density
#dgamma_fit <- dgamma(xseq, shape=fit_gamma$estimate["shape"], rate=fit_gamma$estimate["rate"])
#lines(xseq, dgamma_fit, lwd=2, col="purple")

legend("topright", legend=c("Normal fit", "Lognormal fit"),
       col=c("red","darkgreen"), lwd=2, bty="n")

# Define a high threshold (e.g., 95th percentile of the observed data)
threshold_99 <- quantile(data$RiverDischarge, 0.99)

# Tail probabilities (P(X > threshold)) under each fitted distribution
p_norm_tail_99 <- 1 - pnorm(threshold_99, mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
p_lnorm_tail_99 <- 1 - plnorm(threshold_99, meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])

threshold_95 <- quantile(data$RiverDischarge, 0.95)

# Tail probabilities (P(X > threshold)) under each fitted distribution
p_norm_tail_95 <- 1 - pnorm(threshold_95, mean=fit_norm$estimate["mean"], sd=fit_norm$estimate["sd"])
p_lnorm_tail_95 <- 1 - plnorm(threshold_95, meanlog=fit_lnorm$estimate["meanlog"], sdlog=fit_lnorm$estimate["sdlog"])

ratio_99 = p_lnorm_tail_99/p_norm_tail_99
ratio_95 = p_lnorm_tail_95/p_norm_tail_95

# Print results
cat("Tail probability (Normal) beyond", threshold_95, ":", p_norm_tail_95, "\n")
cat("Tail probability (Lognormal) beyond", threshold_95, ":", p_lnorm_tail_95, "\n")

cat("Tail probability (Normal) beyond", threshold_99, ":", p_norm_tail_99, "\n")
cat("Tail probability (Lognormal) beyond", threshold_99, ":", p_lnorm_tail_99, "\n")

# Part 2 ----
# (a) Are river discharge and precipitation dependent?----

# keep rows with both values present
df <- data[order(data$Date), ]
df <- subset(df, !is.na(Precipitation) & !is.na(RiverDischarge))

# Pearson (linear)
cor.test(df$RiverDischarge, df$Precipitation, use = "complete.obs", method = "pearson")

# (b) Lagged dependence: Cross-correlation function (CCF)----
ccf(df$Precipitation, df$RiverDischarge,
    lag.max = 30, na.action = na.omit,
    main = "CCF: Precipitation (leads) → River Discharge")

#ccf(df$Precipitation, df$RiverDischarge, ylab = "cross-correlation")

# (c) Extremograms: Cross- and auto-dependence of extreme events----
# install.packages("extremogram")
library(extremogram)
library(dplyr)
library(tidyr)
library(ggplot2)

# Univariate extremograms
eg_precip <- extremogram1(df$Precipitation, maxlag = 20, quant = 0.95, type = 1)
eg_discharge <- extremogram1(df$RiverDischarge,  maxlag = 20, quant = 0.95, type = 1)

# Cross-extremogram: P_t extreme vs Q_{t+h} extreme
# Cross-extremogram: precip extremes leading discharge extremes
maxlag <- 20
q1 <- 0.95      # precip threshold (upper tail)
q2 <- 0.95      # discharge threshold (upper tail)
# build bivariate matrix
a <- cbind(df$Precipitation, df$RiverDischarge)

# cross-extremogram (upper tails)
eg_cross <- extremogram2(a,
             quant1 = 0.95,    # threshold for precipitation
             quant2 = 0.95,    # threshold for discharge
             maxlag = 20,
             type = 1,         # upper tails in both
             ploting = 1, cutoff = 1, start = 0)
# Make sure they have the same length (some implementations can return shorter vectors)
L <- min(length(eg_precip), length(eg_discharge), length(eg_cross))

eg_df <- tibble(
  lag = 0:(L - 1),
  `Precipitation (auto)` = eg_precip[1:L],
  `Discharge (auto)`     = eg_discharge[1:L],
  `Cross  P→Q`           = eg_cross[1:L]
) |>
  pivot_longer(-lag, names_to = "Series", values_to = "Extremogram")

# Independence reference line for upper-tail extremogram:
# under independence P(X_{t+h} > u | X_t > u) ≈ 1 - quantile = 0.05
ref_line <- 1 - q2

p_ext <- ggplot(eg_df, aes(lag, Extremogram, color = Series)) +
  geom_hline(yintercept = ref_line, linetype = "dashed", alpha = 0.6) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Precipitation (auto)" = "#E69F00",
    "Discharge (auto)"     = "#56B4E9",
    "Cross  P→Q"           = "#009E73"
  )) +
  labs(
    title = "Extremograms (upper tails, 95% quantile)",
    subtitle = "P(X_{t+h} > u | X_t > u); Cross is Precipitation_t → Discharge_{t+h}",
    x = "Lag (h)",
    y = "Extremogram"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_ext)

# (d) Predictive relationships----
# install.packages("lmtest")
library(lmtest)

# Try a few lag orders; pick the one with strongest (and sensible) evidence
for(k in c(1,2,3,5,7)){
  cat("\n--- order =", k, " ---\n")
  print(grangertest(RiverDischarge ~ Precipitation, order = k, data = df))  # Precip ⇒ Discharge?
  print(grangertest(Precipitation ~ RiverDischarge, order = k, data = df))  # Discharge ⇒ Precip?
}


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

# (e) Extreme events and predictive insight----

# (e-a) ----

# (e-b) ----


# Part 3 ----
# (a) Autocorrelation patterns----

#install.packages("fpp2")
#install.packages("ggfortify")
library(fpp2)

library(ggplot2)
library(ggfortify)

# Raw series
p1 <- autoplot(acf(df$RiverDischarge, plot = FALSE)) +
  ggtitle("ACF of River Discharge (raw)") +
  theme_minimal()

# Differenced series
p2 <- autoplot(acf(diff(df$RiverDischarge), plot = FALSE)) +
  ggtitle("ACF of Differenced River Discharge") +
  theme_minimal()

# Combine them (if you have cowplot installed)
library(cowplot)
plot_grid(p1, p2, ncol = 2)

# Raw discharge
ggAcf(df$RiverDischarge)

# Difference (lag-1)
ggAcf(diff(df$RiverDischarge))

#acf(df$RiverDischarge, main = "ACF of River Discharge")
#acf(diff(df$RiverDischarge), main = "ACF of Differenced River Discharge")

# (b) Serial dependence testing----
Box.test(df$RiverDischarge, lag = 1, type = "Ljung-Box")      # raw
Box.test(diff(df$RiverDischarge), lag = 1, type = "Ljung-Box")      # differenced

# (c) ARIMA modeling----
# install.packages("forecast")
library(forecast)
library(tseries)

# --- 1. Visual inspection-based candidate models ---

ggPacf(diff(df$RiverDischarge))
ggAcf(diff(df$RiverDischarge))

# Based on ACF/PACF, we might try ARMA(1,1), ARMA(2,1), ARMA(2,2) on differenced discharge
fit_arma11 <- Arima(diff(df$RiverDischarge), order = c(1,0,1))
fit_arma21 <- Arima(diff(df$RiverDischarge), order = c(2,0,1))
fit_arma22 <- Arima(diff(df$RiverDischarge), order = c(2,0,2))

summary(fit_arma11)
summary(fit_arma21)
summary(fit_arma22)

# --- 2. Automatic selection ---
model_arima <- auto.arima(diff(df$RiverDischarge), seasonal = FALSE)
summary(model_arima)

# --- 3. Check residuals (independence, variance, Gaussianity) ---
checkresiduals(model_arima)

# Histogram + QQ plot for residual distribution
hist(residuals(model_arima), main="Residual Histogram", xlab="Residuals")
qqnorm(residuals(model_arima)); qqline(residuals(model_arima), col="red")

# --- 4. Try log-transformation of the original series ---
model_arima_log <- auto.arima(log(df$RiverDischarge), seasonal = FALSE)
summary(model_arima_log)
checkresiduals(model_arima_log)

hist(residuals(model_arima_log), main="Residual Histogram (log transform)", xlab="Residuals")
qqnorm(residuals(model_arima_log)); qqline(residuals(model_arima_log), col="red")

# Automatic selection
#model_arima <- auto.arima(diff(df$RiverDischarge), seasonal = FALSE)
#summary(model_arima)

# Check residuals
#checkresiduals(model_arima)

#model_arima_log <- auto.arima(log(df$RiverDischarge), seasonal = FALSE)
#summary(model_arima_log)
#checkresiduals(model_arima_log)

# (d) Modeling volatility: GARCH----
#install.packages("fGarch")
library(fGarch)

# Fit GARCH(1,1) to differenced discharge
garch_norm <- garchFit(~ garch(1,1), data = diff(df$RiverDischarge), cond.dist = "norm")
garch_t    <- garchFit(~ garch(1,1), data = diff(df$RiverDischarge), cond.dist = "std")  # Student-t

summary(garch_norm)
summary(garch_t)

# Diagnostics
plot(garch_norm)
plot(garch_t)

# (e) Two-step modeling approach----
# Step 1: fit ARIMA to differenced series
model_arima <- auto.arima(diff(df$RiverDischarge), seasonal = FALSE)
resid_arima <- residuals(model_arima)
summary(model_arima)

# Step 2: fit GARCH(1,1) to ARIMA residuals
garch_combo_norm <- garchFit(~ garch(1,1), data = resid_arima, cond.dist = "norm")
summary(garch_combo_norm)
garch_combo_t <- garchFit(~ garch(1,1), data = resid_arima, cond.dist = "std")
summary(garch_combo_t)

# (f) Model comparison and conclusion----
# AIC/BIC from fGARCH objects
aic_norm  <- garch_norm@fit$ics["AIC"]
bic_norm  <- garch_norm@fit$ics["BIC"]

aic_t     <- garch_t@fit$ics["AIC"]
bic_t     <- garch_t@fit$ics["BIC"]

aic_combo_norm <- garch_combo_norm@fit$ics["AIC"]
bic_combo_norm <- garch_combo_norm@fit$ics["BIC"]

aic_combo_t <- garch_combo_t@fit$ics["AIC"]
bic_combo_t <- garch_combo_t@fit$ics["BIC"]

cbind(
  model = c("GARCH norm", "GARCH t", "ARIMA+GARCH Normal", "ARIMA+GARCH t"),
  AIC   = c(aic_norm, aic_t, aic_combo_norm, aic_combo_t),
  BIC   = c(bic_norm, bic_t, bic_combo_norm, bic_combo_t)
)


# Standardized residuals (divide by conditional SD)
res_norm  <- residuals(garch_norm,        standardize = TRUE)
res_t     <- residuals(garch_t,           standardize = TRUE)
res_combo_norm <- residuals(garch_combo_norm, standardize = TRUE)
res_combo_t    <- residuals(garch_combo_t,    standardize = TRUE)

# Function to quickly check residuals
check_res <- function(res, model_name) {
  par(mfrow = c(2,2))
  plot(res, type = "l", main = paste(model_name, "standardized residuals"), ylab = "resid")
  acf(res, main = paste(model_name, "ACF of residuals"))
  acf(res^2, main = paste(model_name, "ACF of squared residuals"))
  qqnorm(res, main = paste(model_name, "QQ-plot")); qqline(res, col = "red")
  par(mfrow = c(1,1))
  
  # Ljung-Box tests
  cat("\n", model_name, "\n")
  print(Box.test(res, lag = 10, type = "Ljung-Box"))   # check serial correlation
  print(Box.test(res^2, lag = 10, type = "Ljung-Box")) # check ARCH effects
}

# Run diagnostics for each model
check_res(res_norm,       "GARCH(1,1) Normal")
check_res(res_t,          "GARCH(1,1) Student-t")
check_res(res_combo_norm, "ARIMA + GARCH(1,1) Normal")
check_res(res_combo_t,    "ARIMA + GARCH(1,1) Student-t")
# --------------------------------------------------------------------

# a
q_ts <- ts(df$RiverDischarge)

# --------------------------------------------------------------------
# 3(a) Autocorrelation patterns (raw vs differenced)
# --------------------------------------------------------------------
autoplot(q_ts) + ggtitle("River Discharge (raw series)")

ggAcf(q_ts) + ggtitle("ACF: River Discharge (raw)")

q_ts_d1 <- diff(q_ts)  # lag-1 differencing (stabilises mean)
autoplot(q_ts_d1) + ggtitle("Differenced River Discharge (Δ)")

ggAcf(q_ts_d1) + ggtitle("ACF: Differenced River Discharge (Δ)")
ggPacf(q_ts_d1) + ggtitle("PACF: Differenced River Discharge (Δ)")


# Interpretation note:
# - Raw: slow ACF decay => non-stationary
# - Diff: quick ACF decay => closer to stationary => easier to model

# --------------------------------------------------------------------
# 3(b) Serial dependence (Ljung–Box on raw and differenced)
# --------------------------------------------------------------------
Box.test(as.numeric(q_ts),    lag = 1, type = "Ljung-Box")
Box.test(as.numeric(q_ts_d1), lag = 1, type = "Ljung-Box")

# Expect strong dependence in raw, weaker after differencing.

# --------------------------------------------------------------------
# 3(c) ARIMA modeling (differenced series)
# --------------------------------------------------------------------
# Visual cues from ACF/PACF above can suggest AR/MA orders.
# Let auto.arima select a good model (no seasonality).
fit_arima_d <- auto.arima(q_ts_d1, seasonal = FALSE)
fit_arima_d
summary(fit_arima_d)

# Residual diagnostics from forecast/fpp2
checkresiduals(fit_arima_d)  # shows ACF of residuals, Ljung-Box, and distribution

# Optional: try a log transform (if variance looks non-constant)
# (Guard against zeros before logging)
if (min(df$RiverDischarge, na.rm = TRUE) > 0) {
  q_ts_log <- ts(log(df$RiverDischarge))
  fit_arima_log <- auto.arima(q_ts_log, seasonal = FALSE)
  fit_arima_log
  checkresiduals(fit_arima_log)
}

# Comment in report:
# - Are residuals ~white noise?
# - QQ-plot/heavy tails? If heavy, Student-t in GARCH can help.

# --------------------------------------------------------------------
# 3(d) Volatility modeling: GARCH(1,1) on differenced series
# --------------------------------------------------------------------
# fGarch expects a numeric vector (no ts class needed)
x <- as.numeric(q_ts_d1)

garch_norm <- garchFit(~ garch(1,1), data = x, cond.dist = "norm")
garch_t    <- garchFit(~ garch(1,1), data = x, cond.dist = "std")  # Student-t innovations

summary(garch_norm)
summary(garch_t)

# Quick diagnostics (plots include residuals, ACF of squared residuals, etc.)
par(mfrow = c(2,2)); plot(garch_norm); par(mfrow = c(1,1))
par(mfrow = c(2,2)); plot(garch_t);    par(mfrow = c(1,1))

# Check for volatility clustering via squared residuals
res_norm <- residuals(garch_norm, standardize = TRUE)
res_t    <- residuals(garch_t,    standardize = TRUE)
ggAcf(res_norm^2) + ggtitle("ACF of squared standardized residuals (GARCH norm)")
ggAcf(res_t^2)    + ggtitle("ACF of squared standardized residuals (GARCH t)")

# --------------------------------------------------------------------
# 3(e) Two-step: ARIMA for mean + GARCH for volatility
# --------------------------------------------------------------------
# Step 1: ARIMA on differenced series
fit_arima_d <- auto.arima(q_ts_d1, seasonal = FALSE)
res_arima   <- as.numeric(residuals(fit_arima_d))

# Step 2: GARCH(1,1) on ARIMA residuals
garch_combo <- garchFit(~ garch(1,1), data = res_arima, cond.dist = "std")
summary(garch_combo)
par(mfrow = c(2,2)); plot(garch_combo); par(mfrow = c(1,1))

# Optional quick checks (white noise?)
checkresiduals(ts(residuals(garch_combo, standardize = TRUE)))

# --------------------------------------------------------------------
# 3(f) Model comparison (AIC/BIC) — ARIMA vs GARCH vs ARIMA+GARCH
# --------------------------------------------------------------------
# ARIMA AIC is standard:
aic_arima_d <- AIC(fit_arima_d)

# fGARCH objects store ICs in @fit$ics
aic_norm <- garch_norm@fit$ics["AIC"]; bic_norm <- garch_norm@fit$ics["BIC"]
aic_t    <- garch_t@fit$ics["AIC"];    bic_t    <- garch_t@fit$ics["BIC"]
aic_comb <- garch_combo@fit$ics["AIC"]; bic_comb <- garch_combo@fit$ics["BIC"]

comp_tbl <- data.frame(
  Model = c("ARIMA (Δ)", "GARCH(1,1) normal (Δ)", "GARCH(1,1) t (Δ)", "ARIMA(Δ)+GARCH t (resid)"),
  AIC   = c(aic_arima_d, aic_norm, aic_t, aic_comb),
  BIC   = c(NA,         bic_norm,  bic_t, bic_comb)  # ARIMA BIC available via BIC(fit_arima_d) if you want it
)

# If you’d like BIC for ARIMA too:
comp_tbl$BIC[1] <- BIC(fit_arima_d)

comp_tbl

summary(aic_comb)

# Report guidance:
# - Lower AIC/BIC => better.
# - Often ARIMA+GARCH(t) wins: ARIMA handles mean/autocorr; GARCH handles volatility clustering.
# - Check residuals are ~white noise and (ideally) close to Gaussian.




