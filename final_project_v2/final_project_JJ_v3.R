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
#   5. Block Maxima (GEV) Analysis
#   6. Peaks-Over-Threshold (POT) Analysis
#   7. Extra Risk Metrics (exceedance probabilities)
#   8. Export figures & tables
############################################################

###############################
# 0. Setup --------------------
###############################

pkgs <- c(
  "dplyr", "lubridate", "readr", "ggplot2",
  "extRemes", "POT", "scales",
  "moments", "nortest", "fitdistrplus", "tsibble"
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

# Handle duplicates
dup_rows <- data %>%
  group_by(Datetime) %>%
  filter(n() > 1)

dup_rows

# Average duplicates loads due to differing values
data_clean <- data %>%
  group_by(Datetime) %>%
  summarise(COMED_MW = mean(COMED_MW), .groups = "drop")

# Datetime is already POSIXct from readr; just copy to a consistent name
comed <- data_clean %>%
  mutate(datetime = Datetime)

###############################
# 2. Daily Aggregation --------
###############################

comed_daily_full <- comed %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(
    n_hours         = n(),
    daily_total_load = sum(COMED_MW, na.rm = TRUE),
    daily_max_load   = max(COMED_MW, na.rm = TRUE),
    .groups = "drop"
  )

table(comed_daily_full$n_hours)  # sanity check; should mostly be 24

# We keep all days (no 23-hour filter); note this choice in the report.
data_daily <- comed_daily_full %>%
  arrange(date) %>%
  dplyr::select(date, daily_total_load, daily_max_load)

str(data_daily)
summary(data_daily)

###############################
# 3. Exploratory Data Analysis
###############################

# 3.1 Time series of daily max load
ggplot(data_daily, aes(date, daily_max_load)) +
  geom_line() +
  labs(
    title = "Daily Maximum Electricity Demand",
    x = "Date", y = "Daily max load (MW)"
  )

# 3.2 Histogram + density + QQ-plot (daily maxima)
x <- data_daily$daily_max_load
x <- x[!is.na(x)]

par(mfrow = c(1, 2))
hist(x, breaks = 40, col = "skyblue", prob = TRUE,
     main = "Histogram of Daily Max Load",
     xlab = "Daily max load (MW)")
lines(density(x), lwd = 2)

qqnorm(x, main = "QQ-plot vs Normal – Daily Max Load")
qqline(x, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# 3.3 Skewness, kurtosis, normality
skewness_val <- moments::skewness(x)
kurtosis_val <- moments::kurtosis(x)
cat("Skewness (daily max):", skewness_val, "\n")
cat("Kurtosis (daily max):", kurtosis_val, "\n")

nortest::ad.test(x)

# 3.4 Normal vs lognormal fit (just for intuition)
fit_norm  <- fitdistrplus::fitdist(x, "norm")
fit_lnorm <- fitdistrplus::fitdist(x, "lnorm")

fit_norm
fit_lnorm

fitdistrplus::qqcomp(
  list(fit_norm, fit_lnorm),
  legendtext = c("Normal", "Lognormal"),
  main = "QQ Compare – Normal vs Lognormal"
)

# Optional: density overlay
xseq <- seq(min(x), max(x), length = 1000)
hist(x, breaks = 50, prob = TRUE, col = "lightblue",
     main = "Daily Max Load with Fitted Densities",
     xlab = "Daily max load (MW)")
lines(xseq, dnorm(xseq,
                  mean = fit_norm$estimate["mean"],
                  sd   = fit_norm$estimate["sd"]))
lines(xseq, dlnorm(xseq,
                   meanlog = fit_lnorm$estimate["meanlog"],
                   sdlog   = fit_lnorm$estimate["sdlog"]))
legend("topright", legend = c("Normal", "Lognormal"),
       lwd = 2, lty = 1)

###############################
# 4. Dependence / IID Checks --
###############################

# 4.1 ACF of daily max load
acf(data_daily$daily_max_load,
    main = "ACF of Daily Max Load")

# Optional: ACF of differenced daily total load (not used later)
data_daily$diff_load <- diff(c(NA, data_daily$daily_total_load))
acf(na.omit(data_daily$diff_load),
    main = "ACF of Differenced Daily Total Load")

###############################
# 5. Block Maxima (GEV) -------
###############################

# 5.1 Weekly maxima of daily max load
weekly_max <- data_daily %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    weekly_max_load = max(daily_max_load, na.rm = TRUE),
    .groups = "drop"
  )

summary(weekly_max$weekly_max_load)
nrow(weekly_max)  # number of blocks (weeks)

# ACF of weekly maxima (to show reduced dependence)
acf(weekly_max$weekly_max_load,
    main = "ACF of Weekly Max Load")

# 5.2 GEV fit
gev_weekly <- extRemes::fevd(
  weekly_max_load ~ 1,
  data = weekly_max,
  type = "GEV"
)

summary(gev_weekly)

# 5.3 Diagnostic plots
par(mfrow = c(2, 2))
plot(gev_weekly)
par(mfrow = c(1, 1))

# 5.4 Return levels (in years; periods in weeks)
period_years <- c(1, 5, 10, 20)
period_weeks <- period_years * 52
gev_rl <- return.level(gev_weekly,
                       return.period = period_weeks)
gev_rl

# Optional: CIs for a couple of return levels (e.g. 10, 20 years)
# ci(gev_weekly, type = "return.level", return.period = 10 * 52)
# ci(gev_weekly, type = "return.level", return.period = 20 * 52)

###############################
# 6. POT – daily_max_load -----
###############################

# 6.1 Mean Residual Life (MRL) plot
POT::mrlplot(data_daily$daily_max_load,
             main = "Mean Residual Life – Daily Max Load")

quantile(data_daily$daily_max_load,
         probs = c(0.90, 0.95, 0.975),
         na.rm = TRUE)

# Choose threshold at 95% (u can be adjusted if needed)
u <- quantile(data_daily$daily_max_load, 0.95, na.rm = TRUE)
u

# 6.2 Extremal index (clustering)
library(evd)

theta_hat <- exi(data_daily$daily_max_load, u = u)
theta_hat

cluster_size_mean <- 1 / theta_hat
cluster_size_mean

# 6.3 Declustering with POT::clust()
declust_data <- data.frame(
  obs  = data_daily$daily_max_load,
  time = 1:nrow(data_daily)
)

tim_cond <- round(cluster_size_mean)    # suggested run length (≈ 2 days)

declust <- POT::clust(
  declust_data,
  u        = as.numeric(u),
  tim.cond = tim_cond
)

# 6.4 Cluster maxima and excesses
cluster_max <- sapply(declust, function(mat) max(mat["obs", ]))
summary(cluster_max)
length(cluster_max)   # number of clusters

excesses <- cluster_max - as.numeric(u)
df_excess <- data.frame(y = excesses)

# 6.5 GPD fit (declustered)
gpd_declust <- extRemes::fevd(
  y ~ 1,
  data      = df_excess,
  type      = "GP",
  threshold = 0
)
summary(gpd_declust)

beta_hat <- gpd_declust$results$par["scale"]
xi_hat   <- gpd_declust$results$par["shape"]

# Implied finite endpoint (if xi_hat < 0)
x_endpoint <- as.numeric(u) - beta_hat / xi_hat
x_endpoint

# 6.6 Exceedance rate (per year) for cluster maxima
n_clusters <- length(cluster_max)
n_years    <- nrow(data_daily) / 365
lambda_hat <- n_clusters / n_years
lambda_hat

# 6.7 GPD-based return levels (years)
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

# 6.8 Time series with threshold & cluster maxima
cluster_indices <- sapply(declust, function(mat) max(mat["time", ]))
cluster_dates   <- data_daily$date[cluster_indices]

cluster_df <- data.frame(
  date = cluster_dates,
  daily_max_load = cluster_max
)

ggplot(data_daily, aes(date, daily_max_load)) +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = u, linetype = "dashed",
             color = "darkgreen") +
  geom_point(data = cluster_df,
             aes(x = date, y = daily_max_load),
             color = "red", size = 2) +
  labs(
    title = "Daily Max Demand – Threshold Exceedances & Cluster Maxima",
    x = "Date", y = "Daily max load (MW)"
  )

# 6.9 (Optional) Threshold-sensitivity of xi (no declustering, purely diagnostic)
u_grid <- quantile(data_daily$daily_max_load,
                   probs = c(0.90, 0.95, 0.975),
                   na.rm = TRUE)
xi_grid <- numeric(length(u_grid))

for (i in seq_along(u_grid)) {
  exc_i <- data_daily$daily_max_load[data_daily$daily_max_load > u_grid[i]] -
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
# 7. Extra Risk Metrics -------
###############################

# Probability that daily max exceeds critical levels at least once in T years

critical_levels <- c(22000, 23000, 23500)

# Conditional exceedance probability P(X > z | X > u) under GPD
gpd_tail_prob <- function(z, u, beta, xi) {
  # z > u
  x <- z - u
  if (xi == 0) {
    exp(-x / beta)
  } else {
    (1 + xi * x / beta)^(-1 / xi)
  }
}

results_list <- list()

for (z in critical_levels) {
  if (z <= u) next  # skip if below threshold
  p_cond <- 1 - gpd_tail_prob(z, u, beta_hat, xi_hat)
  lambda_z <- lambda_hat * p_cond  # rate of exceedances above z per year
  
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
# 8. Export figures & tables -------------------------------
############################################################

fig_dir <- "Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

## 8.1 Daily max time series
png(file.path(fig_dir, "P3_daily_max_demand.png"),
    width = 1600, height = 900, res = 150)
ggplot(data_daily, aes(date, daily_max_load)) +
  geom_line() +
  labs(
    title = "Daily Maximum Electricity Demand",
    x = "Date", y = "Daily max load (MW)"
  )
dev.off()

## 8.2 Histogram + QQ-plot of daily max load
png(file.path(fig_dir, "P3_daily_max_load_AND_QQ_plot.png"),
    width = 1600, height = 900, res = 150)
par(mfrow = c(1, 2))
x <- data_daily$daily_max_load
x <- x[!is.na(x)]

hist(x, breaks = 40, col = "skyblue", prob = TRUE,
     main = "Histogram of Daily Max Load",
     xlab = "Daily max load (MW)")
lines(density(x), lwd = 2)

qqnorm(x, main = "QQ-plot vs Normal – Daily Max Load")
qqline(x, col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

## 8.3 Daily max load with fitted densities (optional)
png(file.path(fig_dir, "P3_daily_load_w_densities.png"),
    width = 1600, height = 900, res = 150)
xseq <- seq(min(x), max(x), length = 1000)
hist(x, breaks = 50, prob = TRUE, col = "lightblue",
     main = "Daily Max Load with Fitted Densities",
     xlab = "Daily max load (MW)")
lines(xseq, dnorm(xseq,
                  mean = fit_norm$estimate["mean"],
                  sd   = fit_norm$estimate["sd"]))
lines(xseq, dlnorm(xseq,
                   meanlog = fit_lnorm$estimate["meanlog"],
                   sdlog   = fit_lnorm$estimate["sdlog"]))
legend("topright", legend = c("Normal", "Lognormal"),
       lwd = 2, lty = 1)
dev.off()

## 8.4 ACF of daily max load
png(file.path(fig_dir, "P3_ACF_daily_max_load.png"),
    width = 1600, height = 900, res = 150)
acf(data_daily$daily_max_load,
    main = "ACF of Daily Max Load")
dev.off()

## 8.5 ACF of weekly max load (optional, dependence check)
png(file.path(fig_dir, "P3_ACF_weekly_max_load.png"),
    width = 1600, height = 900, res = 150)
acf(weekly_max$weekly_max_load,
    main = "ACF of Weekly Max Load")
dev.off()

## 8.6 GEV diagnostic plots
png(file.path(fig_dir, "P3_diagnostic_plots_GEV_weekly_max_load.png"),
    width = 1600, height = 900, res = 150)
par(mfrow = c(2, 2))
plot(gev_weekly)
par(mfrow = c(1, 1))
dev.off()

## 8.7 MRL plot
png(file.path(fig_dir, "P3_mean_residual_daily_max_load_(threshold_decision).png"),
    width = 1600, height = 900, res = 150)
POT::mrlplot(data_daily$daily_max_load,
             main = "Mean Residual Life – Daily Max Load")
dev.off()

## 8.8 POT time series with threshold & cluster maxima
png(file.path(fig_dir, "P3_daily_max_demand_POT_threshold_exceedance_cluster_maxima_13days.png"),
    width = 1600, height = 900, res = 150)
ggplot(data_daily, aes(date, daily_max_load)) +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = u, linetype = "dashed",
             color = "darkgreen") +
  geom_point(data = cluster_df,
             aes(x = date, y = daily_max_load),
             color = "red", size = 2) +
  labs(
    title = "Daily Max Demand – Threshold Exceedances & Cluster Maxima",
    x = "Date", y = "Daily max load (MW)"
  )
dev.off()

## 8.9 Threshold-sensitivity of xi (optional)
png(file.path(fig_dir, "P3_POT_shape_vs_threshold.png"),
    width = 1600, height = 900, res = 150)
plot(u_grid, xi_grid, type = "b",
     xlab = "Threshold u (MW)",
     ylab = "Estimated shape parameter xi",
     main = "GPD Shape vs Threshold (raw exceedances)")
abline(h = 0, lty = 2)
dev.off()

## 8.10 Parameter table (CSV)
gev_par  <- c(location = gev_weekly$results$par["location"],
              scale    = gev_weekly$results$par["scale"],
              shape    = gev_weekly$results$par["shape"])

gpd_par  <- c(scale = gpd_declust$results$par["scale"],
              shape = gpd_declust$results$par["shape"])

param_tab <- data.frame(
  Model     = c("GEV (weekly maxima)", "GPD (POT, declustered)"),
  Location  = c(unname(gev_weekly$results$par["location"]), NA),
  Scale     = c(unname(gev_weekly$results$par["scale"]), unname(gpd_declust$results$par["scale"])),
  Shape     = c(unname(gev_weekly$results$par["shape"]), unname(gpd_declust$results$par["shape"]))
)

write.csv(param_tab,
          file = file.path(fig_dir, "P3_parameter_estimates_GEV_GPD.csv"),
          row.names = FALSE)

# 8.11 Exceedance probabilities (CSV)
write.csv(exceedance_probs,
          file = file.path(fig_dir, "P3_exceedance_probabilities.csv"),
          row.names = FALSE)

############################################################
# End of script
############################################################
