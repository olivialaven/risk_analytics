############################################################
# Practical 3 – Risk Analytics
# Extreme Value Analysis of COMED Electricity Demand
#
# Structure:
#   0. Setup
#   1. Load & Clean Data
#   2. Aggregate to Daily Level
#   3. Exploratory Data Analysis (EDA)
#   4. Dependence / IID Checks
#   5. ARIMA residual approach (full year, robustness)
#   6. Block Maxima (GEV) – Summer only
#   7. Peaks-Over-Threshold (POT) – Summer only
#   8. Extra Risk Metrics (exceedance probabilities)
#   9. Export figures & tables
############################################################

###############################
# 0. Setup --------------------
###############################

pkgs <- c(
  "dplyr", "lubridate", "readr", "ggplot2",
  "extRemes", "POT", "scales",
  "moments", "nortest", "fitdistrplus",
  "tsibble", "forecast", "evd"
)

for (p in pkgs) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

theme_set(theme_minimal())

# Path to data (adapt as needed)
data_dir   <- "C:/Users/JJ/OneDrive - epfl.ch/Documents/SMT/RA/risk_analytics/final_project_v2"
comed_file <- file.path(data_dir, "COMED_hourly.csv")

###############################
# 1. Load & Clean Data --------
###############################

data <- read_csv(comed_file) %>%
  arrange(Datetime)

str(data)
head(data)

# Check duplicates (DST etc.)
dup_rows <- data %>%
  group_by(Datetime) %>%
  filter(n() > 1)

dup_rows

# Average duplicates loads due to differing values
data_clean <- data %>%
  group_by(Datetime) %>%
  summarise(COMED_MW = mean(COMED_MW), .groups = "drop")

# Datetime already POSIXct; keep a consistent name
comed <- data_clean %>%
  mutate(datetime = Datetime)

###############################
# 2a. Daily Aggregation --------
###############################

comed_daily_full <- comed %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(
    n_hours          = n(),
    daily_total_load = sum(COMED_MW, na.rm = TRUE),
    daily_max_load   = max(COMED_MW, na.rm = TRUE),
    .groups = "drop"
  )

table(comed_daily_full$n_hours)  # sanity check; mostly 24

# We keep all days (including a few with 23h/1h); note this in report.
data_daily <- comed_daily_full %>%
  arrange(date) %>%
  dplyr::select(date, daily_total_load, daily_max_load)

str(data_daily)
summary(data_daily)

###############################
# 2b. Restrict to summer months
###############################

# Full-year data_daily used for context & ARIMA;
# Summer subset used for EVT (block maxima & POT).

data_summer <- data_daily %>%
  dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::filter(month %in% c(6, 7, 8)) %>%  # June, July, August
  dplyr::select(-month) %>%
  dplyr::arrange(date)

str(data_summer)
summary(data_summer)

###############################
# 3. Exploratory Data Analysis
###############################

### 3.1 Full-year EDA -------------------------------------

# Time series of daily max load (full year)
ggplot(data_daily, aes(date, daily_max_load)) +
  geom_line() +
  labs(
    title = "Daily Maximum Electricity Demand (Full Year)",
    x = "Date", y = "Daily max load (MW)"
  )

# Histogram + density + QQ-plot (full-year daily maxima)
x_full <- data_daily$daily_max_load
x_full <- x_full[!is.na(x_full)]

par(mfrow = c(1, 2))
hist(x_full, breaks = 40, col = "skyblue", prob = TRUE,
     main = "Histogram of Daily Max Load (Full Year)",
     xlab = "Daily max load (MW)")
lines(density(x_full), lwd = 2)

qqnorm(x_full, main = "QQ-plot vs Normal – Full-Year Daily Max")
qqline(x_full, col = "#ce2e24", lwd = 2)
par(mfrow = c(1, 1))

# Skewness, kurtosis, normality (full-year)
skewness_full  <- moments::skewness(x_full)
kurtosis_full  <- moments::kurtosis(x_full)
cat("Skewness (full-year daily max):", skewness_full, "\n")
cat("Kurtosis (full-year daily max):", kurtosis_full, "\n")

nortest::ad.test(x_full)

# Normal vs lognormal fit (full-year, for intuition)
fit_norm_full  <- fitdistrplus::fitdist(x_full, "norm")
fit_lnorm_full <- fitdistrplus::fitdist(x_full, "lnorm")

fit_norm_full
fit_lnorm_full

fitdistrplus::qqcomp(
  list(fit_norm_full, fit_lnorm_full),
  legendtext = c("Normal", "Lognormal"),
  main = "QQ Compare – Full-Year Daily Max"
)

# Density overlay (full-year)
xseq_full <- seq(min(x_full), max(x_full), length = 1000)
hist(x_full, breaks = 50, prob = TRUE, col = "lightblue",
     main = "Full-Year Daily Max Load with Fitted Densities",
     xlab = "Daily max load (MW)")
lines(xseq_full, dnorm(xseq_full,
                       mean = fit_norm_full$estimate["mean"],
                       sd   = fit_norm_full$estimate["sd"]),
                       col = "#ce2e24")
lines(xseq_full, dlnorm(xseq_full,
                        meanlog = fit_lnorm_full$estimate["meanlog"],
                        sdlog   = fit_lnorm_full$estimate["sdlog"]),
                        col = "#205cbc")
legend("topright", legend = c("Normal", "Lognormal"),
       lwd = 2, lty = 1,
       col = c("#ce2e24", "#205cbc"))

### 3.2 Summer-only EDA -----------------------------------

# Time series of summer daily max load
ggplot(data_summer, aes(date, daily_max_load)) +
  geom_line() +
  labs(
    title = "Daily Maximum Electricity Demand (Summer Only)",
    x = "Date", y = "Daily max load (MW)"
  )

# Histogram + density + QQ-plot (summer daily maxima)
x_summer <- data_summer$daily_max_load
x_summer <- x_summer[!is.na(x_summer)]

par(mfrow = c(1, 2))
hist(x_summer, breaks = 40, col = "skyblue", prob = TRUE,
     main = "Histogram of Summer Daily Max Load",
     xlab = "Daily max load (MW)")
lines(density(x_summer), lwd = 2)

qqnorm(x_summer, main = "QQ-plot vs Normal – Summer Daily Max")
qqline(x_summer, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Skewness, kurtosis, normality (summer)
skewness_summer <- moments::skewness(x_summer)
kurtosis_summer <- moments::kurtosis(x_summer)
cat("Skewness (summer daily max):", skewness_summer, "\n")
cat("Kurtosis (summer daily max):", kurtosis_summer, "\n")

nortest::ad.test(x_summer)

# Normal vs lognormal fit (summer)
fit_norm_summer  <- fitdistrplus::fitdist(x_summer, "norm")
fit_lnorm_summer <- fitdistrplus::fitdist(x_summer, "lnorm")

fit_norm_summer
fit_lnorm_summer

fitdistrplus::qqcomp(
  list(fit_norm_summer, fit_lnorm_summer),
  legendtext = c("Normal", "Lognormal"),
  main = "QQ Compare – Summer Daily Max"
)

# Density overlay (summer)
xseq_summer <- seq(min(x_summer), max(x_summer), length = 1000)
hist(x_summer, breaks = 50, prob = TRUE, col = "lightblue",
     main = "Summer Daily Max Load with Fitted Densities",
     xlab = "Daily max load (MW)")
lines(xseq_summer, dnorm(xseq_summer,
                         mean = fit_norm_summer$estimate["mean"],
                         sd   = fit_norm_summer$estimate["sd"]))
lines(xseq_summer, dlnorm(xseq_summer,
                          meanlog = fit_lnorm_summer$estimate["meanlog"],
                          sdlog   = fit_lnorm_summer$estimate["sdlog"]))
legend("topright", legend = c("Normal", "Lognormal"),
       lwd = 2, lty = 1)

###############################
# 4. Dependence / IID Checks --
###############################

# 4.1 ACF of daily max load (full year)
acf(data_daily$daily_max_load,
    main = "ACF of Daily Max Load (Full Year)")

# Optional: ACF of differenced daily total load (full year)
data_daily$diff_load <- diff(c(NA, data_daily$daily_total_load))
acf(na.omit(data_daily$diff_load),
    main = "ACF of Differenced Daily Total Load (Full Year)")

# 4.2 ACF of summer daily max load (for appendix)
acf(data_summer$daily_max_load,
    main = "ACF of Summer Daily Max Load")

###############################
# 5. Check: ARIMA residual approach (full year)
###############################

# Robustness check: ARIMA model on full-year daily maxima,
# then EVT on weekly maxima of residuals.

run_arima <- TRUE  # set to FALSE to skip ARIMA robustness

if (run_arima) {
  
  # 5.1 Fit ARIMA to full-year daily maxima (weekly seasonality)
  ts_full <- ts(data_daily$daily_max_load, frequency = 7)
  
  fit_arima <- forecast::auto.arima(
    ts_full,
    seasonal = TRUE
  )
  
  print(fit_arima)
  summary(fit_arima)
  
  resid_arima <- residuals(fit_arima)
  
  # 5.2 Residual diagnostics
  par(mfrow = c(2, 2))
  plot(resid_arima,
       main = "ARIMA residuals (daily max, full year)",
       ylab = "Residuals",
       xlab = "Time")
  acf(resid_arima,
      main = "ACF of ARIMA residuals")
  qqnorm(resid_arima,
         main = "QQ-plot of ARIMA residuals")
  qqline(resid_arima, col = "#ce2e24")
  hist(resid_arima, breaks = 40, col = "lightblue",
       main = "Histogram of ARIMA residuals",
       xlab = "Residuals")
  par(mfrow = c(1, 1))
  
  print(Box.test(resid_arima, lag = 30, type = "Ljung-Box"))
  
  # 5.3 Weekly maxima of ARIMA residuals
  resid_df <- data_daily %>%
    mutate(
      resid = as.numeric(resid_arima),
      week  = floor_date(date, "week")
    ) %>%
    group_by(week) %>%
    summarise(
      weekly_max_resid = max(resid, na.rm = TRUE),
      .groups = "drop"
    )
  
  summary(resid_df$weekly_max_resid)
  acf(resid_df$weekly_max_resid,
      main = "ACF of Weekly Max ARIMA Residuals")
  
  # 5.4 GEV fit on weekly maxima of ARIMA residuals
  gev_resid <- extRemes::fevd(
    weekly_max_resid ~ 1,
    data = resid_df,
    type = "GEV"
  )
  
  summary(gev_resid)
  
  par(mfrow = c(2, 2))
  plot(gev_resid)
  par(mfrow = c(1, 1))
  
  # 5.5 Return levels on residual scale (diagnostic only)
  period_years_resid  <- c(1, 5, 10, 20)
  period_weeks_resid  <- period_years_resid * 52  # full-year weekly maxima
  
  gev_rl_resid <- return.level(gev_resid,
                               return.period = period_weeks_resid)
  print(gev_rl_resid)
  
} else {
  fit_arima   <- NULL
  resid_arima <- NULL
  resid_df    <- NULL
  gev_resid   <- NULL
}

###############################
# 6. Block Maxima (GEV) – Summer
###############################

# Weekly maxima using only summer days (more homogeneous blocks).

weekly_max <- data_summer %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    weekly_max_load = max(daily_max_load, na.rm = TRUE),
    .groups = "drop"
  )

summary(weekly_max$weekly_max_load)
nrow(weekly_max)  # number of summer weeks

# ACF of summer weekly maxima
acf(weekly_max$weekly_max_load,
    main = "ACF of Weekly Max Load (Summer Only)")

# GEV fit on summer weekly maxima
gev_weekly <- extRemes::fevd(
  weekly_max_load ~ 1,
  data = weekly_max,
  type = "GEV"
)

summary(gev_weekly)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(gev_weekly)
par(mfrow = c(1, 1))

# Return levels in "summers" (13 weeks per summer)
period_years <- c(1, 5, 10, 20)
period_weeks <- period_years * 13  # ~13 summer weeks per year

gev_rl <- return.level(gev_weekly,
                       return.period = period_weeks)
gev_rl

###############################
# 7. POT – Summer daily_max_load
###############################

# POT on summer daily maxima, with declustering.
# Here we (i) choose a main threshold + cluster size for the report,
# and (ii) run a robustness grid over thresholds and run lengths.

# 7.1 MRL plot + quantiles for threshold choice (summer only)
POT::mrlplot(data_summer$daily_max_load,
             main = "Mean Residual Life – Summer Daily Max Load")

quantile(data_summer$daily_max_load,
         probs = c(0.90, 0.95, 0.975),
         na.rm = TRUE)

########################################
# 7.2 Robustness grid over thresholds &
#     cluster run lengths (declustered)
########################################

# We explore several thresholds (quantiles of summer daily maxima)
# and several run lengths (tim.cond in days) for declustering.
# For each combination, we:
#  - decluster exceedances,
#  - fit a GPD to cluster excesses,
#  - compute number of clusters, cluster rate,
#  - estimate finite endpoint (if xi < 0),
#  - compute 5-, 10-, 20-summer return levels.

prob_grid      <- c(0.90, 0.92, 0.94, 0.95, 0.96, 0.975)
tim_cond_grid  <- c(1, 2, 3)  # 1–3 day run lengths for clustering
n_years_summer <- length(unique(lubridate::year(data_summer$date)))

run_pot_declust <- function(u, tim_cond, data_vec, n_years, T_vec = c(5, 10, 20)) {
  # If too few exceedances, skip
  if (sum(data_vec > u, na.rm = TRUE) < 5) return(NULL)
  
  declust_data <- data.frame(
    obs  = data_vec,
    time = seq_along(data_vec)
  )
  
  dcl <- POT::clust(
    declust_data,
    u        = as.numeric(u),
    tim.cond = tim_cond
  )
  
  cluster_max <- sapply(dcl, function(mat) max(mat["obs", ]))
  n_clusters  <- length(cluster_max)
  if (n_clusters < 5) return(NULL)
  
  excesses <- cluster_max - u
  df_excess <- data.frame(y = excesses)
  
  gpd_fit <- extRemes::fevd(
    y ~ 1,
    data      = df_excess,
    type      = "GP",
    threshold = 0
  )
  
  beta <- gpd_fit$results$par["scale"]
  xi   <- gpd_fit$results$par["shape"]
  
  endpoint <- if (xi < 0) as.numeric(u) - beta / xi else NA
  lambda   <- n_clusters / n_years
  
  gpd_RL <- function(T_years, beta, xi, lambda) {
    (beta / xi) * ((lambda * T_years)^xi - 1)
  }
  
  RL_excess <- sapply(T_vec, gpd_RL,
                      beta = beta, xi = xi, lambda = lambda)
  RL <- as.numeric(u) + RL_excess
  names(RL) <- paste0("RL_", T_vec, "y")
  
  out <- data.frame(
    prob_u     = NA_real_,  # will be filled outside
    u          = as.numeric(u),
    tim_cond   = tim_cond,
    n_clusters = n_clusters,
    lambda     = lambda,
    beta       = beta,
    xi         = xi,
    endpoint   = endpoint,
    t(RL)
  )
  out
}

pot_grid_list <- list()

for (p in prob_grid) {
  u_p <- as.numeric(quantile(data_summer$daily_max_load, p, na.rm = TRUE))
  for (tc in tim_cond_grid) {
    res <- try(
      run_pot_declust(u = u_p,
                      tim_cond = tc,
                      data_vec = data_summer$daily_max_load,
                      n_years  = n_years_summer),
      silent = TRUE
    )
    if (!inherits(res, "try-error") && !is.null(res)) {
      res$prob_u <- p
      pot_grid_list[[length(pot_grid_list) + 1]] <- res
    }
  }
}

pot_grid_results <- if (length(pot_grid_list) > 0) {
  do.call(rbind, pot_grid_list)
} else {
  NULL
}

# Inspect this in the console (and/or export) to see how sensitive
# cluster counts, endpoints, and return levels are to threshold & tim.cond.
print(pot_grid_results)

##################################################
# 7.3 Main modelling choice for the report:
#     u = 95% quantile, run length from extremal
#     index (≈ 2 days)
##################################################

# Choose threshold at 95% quantile (summer daily maxima)
u <- as.numeric(quantile(data_summer$daily_max_load, 0.95, na.rm = TRUE))
u

# Extremal index via evd::exi at this u
theta_hat <- evd::exi(data_summer$daily_max_load, u = u)
theta_hat

cluster_size_mean <- 1 / theta_hat
cluster_size_mean   # ~1.9 days

# Use run length ≈ mean cluster size (rounded)
tim_cond_main <- round(cluster_size_mean)

# 7.4 Declustering summer data at main choice (for report)
declust_data_main <- data.frame(
  obs  = data_summer$daily_max_load,
  time = 1:nrow(data_summer)
)

declust_main <- POT::clust(
  declust_data_main,
  u        = as.numeric(u),
  tim.cond = tim_cond_main
)

# Cluster maxima and excesses for main choice
cluster_max <- sapply(declust_main, function(mat) max(mat["obs", ]))
summary(cluster_max)
length(cluster_max)   # number of clusters at main choice

excesses <- cluster_max - as.numeric(u)
df_excess <- data.frame(y = excesses)

# 7.5 GPD fit to declustered excesses (main choice)
gpd_declust <- extRemes::fevd(
  y ~ 1,
  data      = df_excess,
  type      = "GP",
  threshold = 0
)
summary(gpd_declust)

beta_hat <- gpd_declust$results$par["scale"]
xi_hat   <- gpd_declust$results$par["shape"]

# Finite endpoint (if xi_hat < 0)
x_endpoint <- as.numeric(u) - beta_hat / xi_hat
x_endpoint

# 7.6 Exceedance rate (per summer) at main choice
n_clusters <- length(cluster_max)
n_years    <- length(unique(lubridate::year(data_summer$date)))
lambda_hat <- n_clusters / n_years
lambda_hat

# 7.7 GPD-based return levels ("per summer year") at main choice
gpd_RL <- function(T_years, beta, xi, lambda) {
  (beta / xi) * ((lambda * T_years)^xi - 1)
}

T_vec <- c(1, 5, 10, 20)
excess_RL <- sapply(
  T_vec, gpd_RL,
  beta = beta_hat, xi = xi_hat, lambda = lambda_hat
)

actual_RL <- as.numeric(u) + excess_RL
names(actual_RL) <- paste0(T_vec, "-year")
actual_RL

# 7.8 Summer series with threshold & cluster maxima (main choice)
cluster_indices <- sapply(declust_main, function(mat) max(mat["time", ]))
cluster_dates   <- data_summer$date[cluster_indices]

cluster_df <- data.frame(
  date = cluster_dates,
  daily_max_load = cluster_max
)

ggplot(data_summer, aes(date, daily_max_load)) +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = u, linetype = "dashed",
             color = "#205cbc") +
  geom_point(data = cluster_df,
             aes(x = date, y = daily_max_load),
             color = "#ce2e24", size = 2) +
  labs(
    title = "Summer Daily Max Demand – Threshold Exceedances & Cluster Maxima",
    x = "Date", y = "Daily max load (MW)"
  )

###########################################################
# 7.9 Threshold-sensitivity of xi (summer, no declustering)
#     (kept as a separate diagnostic: GPD fit to raw excesses)
###########################################################

u_grid <- quantile(data_summer$daily_max_load,
                   probs = c(0.90, 0.92, 0.94, 0.95, 0.96, 0.975),
                   na.rm = TRUE)
xi_grid <- numeric(length(u_grid))

for (i in seq_along(u_grid)) {
  exc_i <- data_summer$daily_max_load[data_summer$daily_max_load > u_grid[i]] -
    u_grid[i]
  df_i <- data.frame(y = exc_i)
  gpd_i <- extRemes::fevd(
    y ~ 1,
    data      = df_i,
    type      = "GP",
    threshold = 0
  )
  xi_grid[i] <- gpd_i$results$par["shape"]
}

xi_grid

###############################
# 8. Extra Risk Metrics -------
###############################

# Probability that summer daily max exceeds critical levels
# at least once in T summer seasons.

critical_levels <- c(22000, 23000, 23500)

gpd_tail_prob <- function(z, u, beta, xi) {
  x <- z - u
  if (xi == 0) {
    exp(-x / beta)
  } else {
    (1 + xi * x / beta)^(-1 / xi)
  }
}

results_list <- list()

for (z in critical_levels) {
  if (z <= u) next
  p_cond <- 1 - gpd_tail_prob(z, u, beta_hat, xi_hat)
  lambda_z <- lambda_hat * p_cond
  
  for (T_years in c(1, 5, 10)) {
    prob_at_least_one <- 1 - exp(-lambda_z * T_years)
    results_list[[length(results_list) + 1]] <- data.frame(
      Critical_Level_MW = z,
      Horizon_Years     = T_years,
      Lambda_z          = lambda_z,
      Prob_AtLeastOne   = prob_at_least_one
    )
  }
}

exceedance_probs <- do.call(rbind, results_list)
exceedance_probs

############################################################
# 9. Export figures & tables -------------------------------
############################################################

fig_dir <- "Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

## 9.1 Daily max time series (full year)
png(file.path(fig_dir, "P3_daily_max_demand.png"),
    width = 1600, height = 900, res = 150)
ggplot(data_daily, aes(date, daily_max_load)) +
  geom_line() +
  labs(
    title = "Daily Maximum Electricity Demand (Full Year)",
    x = "Date", y = "Daily max load (MW)"
  )
dev.off()

## 9.2 Daily max time series (summer only)
png(file.path(fig_dir, "P3_summer_daily_max_demand.png"),
    width = 1600, height = 900, res = 150)
ggplot(data_summer, aes(date, daily_max_load)) +
  geom_line() +
  labs(
    title = "Daily Maximum Electricity Demand (Summer Only)",
    x = "Date", y = "Daily max load (MW)"
  )
dev.off()

## 9.3 Histogram + QQ-plot (full year)
png(file.path(fig_dir, "P3_daily_max_load_AND_QQ_plot.png"),
    width = 1600, height = 900, res = 150)
par(mfrow = c(1, 2))
x <- x_full
hist(x, breaks = 40, col = "skyblue", prob = TRUE,
     main = "Histogram of Daily Max Load (Full Year)",
     xlab = "Daily max load (MW)")
lines(density(x), lwd = 2)
qqnorm(x, main = "QQ-plot vs Normal – Full-Year Daily Max")
qqline(x, col = "#ce2e24", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

## 9.4 Histogram + QQ-plot (summer only)
png(file.path(fig_dir, "P3_summer_daily_max_load_AND_QQ_plot.png"),
    width = 1600, height = 900, res = 150)
par(mfrow = c(1, 2))
x <- x_summer
hist(x, breaks = 40, col = "skyblue", prob = TRUE,
     main = "Histogram of Summer Daily Max Load",
     xlab = "Daily max load (MW)")
lines(density(x), lwd = 2)
qqnorm(x, main = "QQ-plot vs Normal – Summer Daily Max")
qqline(x, col = "#ce2e24", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

## 9.5 Full-year daily max with fitted densities
png(file.path(fig_dir, "P3_daily_load_w_densities.png"),
    width = 1600, height = 900, res = 150)
x <- x_full
xseq <- seq(min(x), max(x), length = 1000)
hist(x, breaks = 50, prob = TRUE, col = "lightblue",
     main = "Full-Year Daily Max Load with Fitted Densities",
     xlab = "Daily max load (MW)")
lines(xseq, dnorm(xseq,
                  mean = fit_norm_full$estimate["mean"],
                  sd   = fit_norm_full$estimate["sd"]))
lines(xseq, dlnorm(xseq,
                   meanlog = fit_lnorm_full$estimate["meanlog"],
                   sdlog   = fit_lnorm_full$estimate["sdlog"]))
legend("topright", legend = c("Normal", "Lognormal"),
       lwd = 2, lty = 1,
       col = c("#ce2e24", "#205cbc"))
dev.off()

## 9.6 Summer daily max with fitted densities
png(file.path(fig_dir, "P3_summer_daily_load_w_densities.png"),
    width = 1600, height = 900, res = 150)
x <- x_summer
xseq <- seq(min(x), max(x), length = 1000)
hist(x, breaks = 50, prob = TRUE, col = "lightblue",
     main = "Summer Daily Max Load with Fitted Densities",
     xlab = "Daily max load (MW)")
lines(xseq, dnorm(xseq,
                  mean = fit_norm_summer$estimate["mean"],
                  sd   = fit_norm_summer$estimate["sd"]))
lines(xseq, dlnorm(xseq,
                   meanlog = fit_lnorm_summer$estimate["meanlog"],
                   sdlog   = fit_lnorm_summer$estimate["sdlog"]))
legend("topright", legend = c("Normal", "Lognormal"),
       lwd = 2, lty = 1,
       col = c("#ce2e24", "#205cbc"))
dev.off()

## 9.7 ACF of daily max load (full year)
png(file.path(fig_dir, "P3_ACF_daily_max_load.png"),
    width = 1600, height = 900, res = 150)
acf(data_daily$daily_max_load,
    main = "ACF of Daily Max Load (Full Year)")
dev.off()

## 9.8 ACF of summer daily max load
png(file.path(fig_dir, "P3_ACF_daily_max_load_summer.png"),
    width = 1600, height = 900, res = 150)
acf(data_summer$daily_max_load,
    main = "ACF of Summer Daily Max Load")
dev.off()

## 9.9 ACF of weekly max load (summer-only)
png(file.path(fig_dir, "P3_ACF_weekly_max_load.png"),
    width = 1600, height = 900, res = 150)
acf(weekly_max$weekly_max_load,
    main = "ACF of Weekly Max Load (Summer Only)")
dev.off()

## 9.10 GEV diagnostic plots (summer weekly maxima)
png(file.path(fig_dir, "P3_diagnostic_plots_GEV_weekly_max_load.png"),
    width = 1600, height = 900, res = 150)
par(mfrow = c(2, 2))
plot(gev_weekly)
par(mfrow = c(1, 1))
dev.off()

## 9.11 MRL plot (summer-only, threshold choice)
png(file.path(fig_dir, "P3_mean_residual_daily_max_load_(threshold_decision).png"),
    width = 1600, height = 900, res = 150)
POT::mrlplot(data_summer$daily_max_load,
             main = "Mean Residual Life – Summer Daily Max Load")
dev.off()

## 9.12 POT time series with threshold & cluster maxima (summer-only)
png(file.path(fig_dir, "P3_daily_max_demand_POT_threshold_exceedance_cluster_maxima_13days.png"),
    width = 1600, height = 900, res = 150)
ggplot(data_summer, aes(date, daily_max_load)) +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = u, linetype = "dashed",
             color = "#205cbc") +
  geom_point(data = cluster_df,
             aes(x = date, y = daily_max_load),
             color = "#ce2e24", size = 2) +
  labs(
    title = "Summer Daily Max Demand – Threshold Exceedances & Cluster Maxima",
    x = "Date", y = "Daily max load (MW)"
  )
dev.off()

## 9.13 Threshold-sensitivity of xi (summer-only)
png(file.path(fig_dir, "P3_POT_shape_vs_threshold.png"),
    width = 1600, height = 900, res = 150)
plot(u_grid, xi_grid, type = "b",
     xlab = "Threshold u (MW)",
     ylab = "Estimated shape parameter xi",
     main = "GPD Shape vs Threshold (Summer Daily Max Load)")
abline(h = 0, lty = 2)
dev.off()

## 9.14 Parameter table (CSV) – summer-based GEV & GPD
gev_par  <- c(location = gev_weekly$results$par["location"],
              scale    = gev_weekly$results$par["scale"],
              shape    = gev_weekly$results$par["shape"])

gpd_par  <- c(scale = gpd_declust$results$par["scale"],
              shape = gpd_declust$results$par["shape"])

param_tab <- data.frame(
  Model     = c("GEV (summer weekly maxima)", "GPD (summer POT, declustered)"),
  Location  = c(unname(gev_par["location"]), NA),
  Scale     = c(unname(gev_par["scale"]),    unname(gpd_par["scale"])),
  Shape     = c(unname(gev_par["shape"]),    unname(gpd_par["shape"]))
)

write.csv(param_tab,
          file = file.path(fig_dir, "P3_parameter_estimates_GEV_GPD.csv"),
          row.names = FALSE)

## 9.15 Exceedance probabilities (CSV) – summer-based
write.csv(exceedance_probs,
          file = file.path(fig_dir, "P3_exceedance_probabilities.csv"),
          row.names = FALSE)

## 9.16 ARIMA residual diagnostics (optional, appendix)
if (run_arima && !is.null(resid_arima)) {
  png(file.path(fig_dir, "P3_ARIMA_residual_diagnostics.png"),
      width = 1600, height = 900, res = 150)
  par(mfrow = c(2, 2))
  plot(resid_arima,
       main = "ARIMA residuals (daily max, full year)",
       ylab = "Residuals",
       xlab = "Time")
  acf(resid_arima,
      main = "ACF of ARIMA residuals")
  qqnorm(resid_arima,
         main = "QQ-plot of ARIMA residuals")
  qqline(resid_arima, col = "#ce2e24")
  hist(resid_arima, breaks = 40, col = "lightblue",
       main = "Histogram of ARIMA residuals",
       xlab = "Residuals")
  par(mfrow = c(1, 1))
  dev.off()
}

if (run_arima && !is.null(gev_resid)) {
  png(file.path(fig_dir, "P3_ARIMA_GEV_weekly_max_resid.png"),
      width = 1600, height = 900, res = 150)
  par(mfrow = c(2, 2))
  plot(gev_resid)
  par(mfrow = c(1, 1))
  dev.off()
}

############################################################
# End of script
############################################################
