# Copied Practical 1 — Full solutions (JJ's take)
# File: practical_1/JJs_take/Practical1_solutions.R
# Purpose: Copy of the fully worked solutions so teammates can work in a separate folder.
# Notes:
# - This script assumes the shared data file `practical_1/River_and_precip_Neuchatel.csv`
#   and helper `practical_1/JuroExtremes.R` are in the parent `practical_1/` folder.
# - Outputs (figures, RDS) are written to the parent `practical_1/` to keep a single canonical set of results.

# -----------------------------
# 0. Helpers and package management
# -----------------------------
required_pkgs <- c('readr','lubridate','ggplot2','dplyr','evd','ismev','forecast')
install_if_missing <- function(pkgs){
  to_install <- pkgs[!pkgs %in% installed.packages()[,1]]
  if(length(to_install)) install.packages(to_install, repos = 'https://cloud.r-project.org')
}
install_if_missing(required_pkgs)

library(readr); library(lubridate); library(ggplot2); library(dplyr)
library(evd); library(ismev)
library(forecast)

# Make figures directory in parent practical_1
fig_dir <- file.path('practical_1','figures')
if(!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# -----------------------------
# 1. Load data
# -----------------------------
data_path <- file.path('practical_1','River_and_precip_Neuchatel.csv')
stopifnot(file.exists(data_path))
raw <- read_csv(data_path, col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                             RiverDischarge = col_double(),
                                             Precipitation = col_double()))

# quick rename
df <- raw %>% rename(date = Date, discharge = RiverDischarge, precip = Precipitation)

# Create simple ts objects (daily frequency) and produce autoplot / ACF plots to match the time-series practice style
ts_start_year <- year(min(df$date, na.rm = TRUE))
ts_discharge <- ts(df$discharge, start = c(ts_start_year, 1), frequency = 365)
ts_precip <- ts(df$precip, start = c(ts_start_year, 1), frequency = 365)

p_ts_d <- autoplot(ts_discharge) + labs(y = 'River discharge', x = 'Time', title = 'River discharge (ts object)')
p_ts_p <- autoplot(ts_precip) + labs(y = 'Precipitation (mm)', x = 'Time', title = 'Precipitation (ts object)')
ggsave(file.path(fig_dir,'discharge_ts_tsobj.png'), p_ts_d, width = 10, height = 3)
ggsave(file.path(fig_dir,'precip_ts_tsobj.png'), p_ts_p, width = 10, height = 3)

# ACF plots similar to TimeSeries-practice
p_acf_d <- ggAcf(ts_discharge, lag.max = 50) + ggtitle('ACF - discharge (ts)')
p_acf_p <- ggAcf(ts_precip, lag.max = 50) + ggtitle('ACF - precip (ts)')
ggsave(file.path(fig_dir,'discharge_acf.png'), p_acf_d, width = 8, height = 4)
ggsave(file.path(fig_dir,'precip_acf.png'), p_acf_p, width = 8, height = 4)

# -----------------------------
# 2. Exploratory Data Analysis
# -----------------------------
# Time series quick plots
p1 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = discharge), color = 'blue') +
  labs(y = 'River discharge', x = 'Date', title = 'River discharge — time series')

p2 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = precip), color = 'darkgreen') +
  labs(y = 'Precipitation (mm)', x = 'Date', title = 'Precipitation — time series')

ggsave(file.path(fig_dir,'discharge_ts.png'), p1, width = 10, height = 3)
ggsave(file.path(fig_dir,'precip_ts.png'), p2, width = 10, height = 3)

# Monthly boxplots to reveal seasonality
df <- df %>% mutate(month = month(date, label = TRUE))
pm_d <- ggplot(df, aes(x=month, y=discharge)) + geom_boxplot() + labs(title='Monthly distribution of discharge')
pm_p <- ggplot(df, aes(x=month, y=precip)) + geom_boxplot() + labs(title='Monthly distribution of precipitation')

ggsave(file.path(fig_dir,'monthly_discharge_boxplot.png'), pm_d, width = 8, height = 4)
ggsave(file.path(fig_dir,'monthly_precip_boxplot.png'), pm_p, width = 8, height = 4)

# -----------------------------
# 3. Summary statistics and missing values
# -----------------------------
summary_stats <- df %>% summarize(
  n = n(),
  discharge_mean = mean(discharge, na.rm=TRUE),
  discharge_sd = sd(discharge, na.rm=TRUE),
  precip_mean = mean(precip, na.rm=TRUE),
  precip_sd = sd(precip, na.rm=TRUE),
  missing_discharge = sum(is.na(discharge)),
  missing_precip = sum(is.na(precip))
)
print(summary_stats)

# -----------------------------
# 4. Annual maxima (block maxima) for discharge -> GEV fit
# -----------------------------
# Create year column and compute annual maxima
df <- df %>% mutate(year = year(date))
annual_max <- df %>% group_by(year) %>% summarize(max_discharge = max(discharge, na.rm = TRUE)) %>% ungroup()

# Fit GEV to annual maxima using evd::fgev
gev_fit <- fgev(annual_max$max_discharge)
print(gev_fit)
# Extract parameters and compute 100-year return level with evd qgev
loc <- gev_fit$estimate['loc']; scale <- gev_fit$estimate['scale']; shape <- gev_fit$estimate['shape']
rl_100 <- qgev(1 - 1/100, loc = loc, scale = scale, shape = shape)
cat('Estimated 100-year return level (discharge, block maxima/Gev):', rl_100, '\n')

# Plot annual maxima and fitted GEV density (for visualization)
library(stats)
xv <- seq(min(annual_max$max_discharge)*0.9, max(annual_max$max_discharge)*1.1, length.out=200)
gev_dens <- dgev(xv, loc=loc, scale=scale, shape=shape)
dens_df <- data.frame(x = xv, y = gev_dens)
# Use after_stat(density) and provide the fitted density as a separate data.frame so aesthetics don't inherit
p_gev <- ggplot(annual_max, aes(x = max_discharge)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = 'grey80', color = 'black') +
  geom_line(data = dens_df, aes(x = x, y = y), color = 'red', linewidth = 1, inherit.aes = FALSE) +
  labs(title = 'Annual maxima and fitted GEV (discharge)')

ggsave(file.path(fig_dir,'gev_annual_max_discharge.png'), p_gev, width = 7, height = 4)

# -----------------------------
# 5. Peaks Over Threshold (POT) for precipitation -> GPD fit
# -----------------------------
# Simple exploratory threshold selection: mean + 1.5*sd and also 95% empirical quantile
threshold1 <- mean(df$precip, na.rm=TRUE) + 1.5*sd(df$precip, na.rm=TRUE)
threshold95 <- quantile(df$precip, 0.95, na.rm=TRUE)
cat('Candidate thresholds (precip):', threshold1, threshold95, '\n')

# Use the 95% quantile as a working threshold (common choice)
th <- as.numeric(threshold95)
exceedances <- df$precip[df$precip > th]
excesses <- exceedances - th
cat('Number of exceedances above threshold', th, ':', length(excesses), '\n')

# Fit GPD with ismev::gpd.fit (returns MLE for scale and shape)
gpd_fit <- gpd.fit(df$precip, threshold = th, show = FALSE)
print(gpd_fit)

# Return level for precipitation for return period T (in days) can be computed. For example 10-year exceedance
# if data daily and using POT, the T-year return level formula depends on rate of exceedances. App. example below:
nu_hat <- gpd_fit$mle[2]  # shape
beta_hat <- gpd_fit$mle[1] # scale (in ismev ordering)
# empirical exceedance probability
p_exceed <- mean(df$precip > th, na.rm=TRUE)
# For daily data, number of days per year ~ 365. Return level for T-year with prob p = 1/(T*365)
# compute T-year return level (approx) using the GPD tail extrapolation
return_level_pot <- function(T_years){
  p_annual <- 1/(T_YEARs*365)
  # required exceedance quantile of precipitation: quantile threshold + GPD quantile
  # Using formula z_p = th + (beta/xi) * (( (p_exceed / p_annual)^{xi} ) - 1)  for xi != 0
  if(abs(nu_hat) < 1e-6){
    z <- th + beta_hat * log(p_exceed / p_annual)
  } else {
    z <- th + (beta_hat/nu_hat) * ( (p_exceed / p_annual)^{nu_hat} - 1 )
  }
  return(z)
}
cat('Approx 10-year return level (precip):', return_level_pot(10), '\n')

# -----------------------------
# 6. Declustering (short-run declustering for precipitation extremes)
# -----------------------------
# Simple runs declustering: keep only the highest exceedance within a run of consecutive exceedances
decluster_runs <- function(dates, series, threshold, run_length_days = 3){
  idx_exceed <- which(series > threshold)
  if(length(idx_exceed)==0) return(integer(0))
  groups <- cumsum(c(1, diff(idx_exceed) > run_length_days))
  keep_idx <- tapply(idx_exceed, groups, function(idxs) idxs[which.max(series[idxs])])
  return(as.integer(unlist(keep_idx)))
}
kept <- decluster_runs(df$date, df$precip, th, run_length_days = 2)
cat('After declustering (run=2 days) kept exceedances:', length(kept), '\n')

# -----------------------------
# 7. Dependence between precip and discharge in the tail
# -----------------------------
# Empirical estimate of tail dependence (upper tail): P(discharge > q_d | precip > q_p)
q_p <- quantile(df$precip, 0.95, na.rm=TRUE)
q_d <- quantile(df$discharge, 0.95, na.rm=TRUE)
cond_prob <- mean(df$discharge > q_d & df$precip > q_p, na.rm=TRUE) / mean(df$precip > q_p, na.rm=TRUE)
cat('Empirical P(discharge>q_d | precip>q_p) at 95% quantiles:', cond_prob, '\n')

# Scatter plot of top quantiles
top_df <- df %>% filter(precip > q_p | discharge > q_d)
p_tail <- ggplot(top_df, aes(x=precip, y=discharge)) + geom_point(alpha=0.6) +
  geom_hline(yintercept = q_d, color='red', linetype='dashed') +
  geom_vline(xintercept = q_p, color='red', linetype='dashed') +
  labs(title='Scatter of precip vs discharge (top quantiles)')
ggsave(file.path(fig_dir,'tail_scatter_precip_discharge.png'), p_tail, width = 6, height = 5)

# -----------------------------
# 8. Extreme causality tests using provided functions
# -----------------------------
# Source the provided JuroExtremes.R (which defines Extreme_causality_test and the full graph estimator)
helpers_path <- file.path('practical_1','JuroExtremes.R')
if(file.exists(helpers_path)){
  source(helpers_path)
} else {
  stop('JuroExtremes.R not found in practical_1. Place the helper file there and re-run.')
}

# Prepare short vectors for the tests; align and remove NA
common_idx <- which(!is.na(df$discharge) & !is.na(df$precip))
xseries <- df$precip[common_idx]
yseries <- df$discharge[common_idx]

# Test: does precipitation cause discharge in extremes? (lag_future = 0..5)
res_list <- list()
for(lag_f in 0:3){
  res <- Extreme_causality_test(xseries, yseries, z = NULL, lag_future = lag_f, p_value_computation = FALSE, bootstrap_repetitions = 50)
  res_list[[as.character(lag_f)]] <- res
  cat('Lag', lag_f, ':', as.character(res$output), 'CTC=', res$CTC, 'baseline=', res$baseline, '\n')
}

# And reverse direction
res_rev <- list()
for(lag_f in 0:3){
  res2 <- Extreme_causality_test(yseries, xseries, z = NULL, lag_future = lag_f, p_value_computation = FALSE, bootstrap_repetitions = 50)
  res_rev[[as.character(lag_f)]] <- res2
  cat('Reverse Lag', lag_f, ':', as.character(res2$output), '\n')
}

# Save quick results to RDS for later inspection
saveRDS(list(gev_fit=gev_fit, gpd_fit=gpd_fit, causality_precip_to_discharge = res_list, causality_rev = res_rev), file = file.path('practical_1','practical1_results.rds'))

# -----------------------------
# 9. Final notes and reproducibility
# -----------------------------
cat('\nPractical1 full-solution script completed. Figures saved in', fig_dir, '\n')
cat('Results saved to practical_1/practical1_results.rds\n')

# Minimal quick-run verification block (only executed when called with RUN_PRACTICAL1_TEST=1)
if(identical(Sys.getenv('RUN_PRACTICAL1_TEST'), '1')){
  cat('\n[Quick verification] Data rows:', nrow(df), '\n')
  cat('First 3 rows:\n')
  print(head(df,3))
  cat('Annual maxima years:', paste(range(annual_max$year), collapse=' - '), '\n')
}

# End of script
