############################################################
# Risk Analytics — Practical 2
# River Discharge (Neuchâtel) — EVT: GEV, POT (GPD), Clustering
############################################################

###############################
# 0. Setup --------------------
###############################

# ---- Packages ----
pkgs <- c(
  "dplyr", "lubridate", "ggplot2",
  "extRemes", "ismev", "POT"
)

for (p in pkgs) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# ---- Paths ----
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
this_dir  <- if (!is.null(this_file)) dirname(this_file) else getwd()

csv_path <- file.path(this_dir, "River_and_precip_Neuchatel.csv")
fig_dir  <- file.path(this_dir, "Figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# ---- Palette (aligned with your Final Project style) ----
COL_BLUE   <- "#205cbc"
COL_RED    <- "#ce2e24"
COL_LTBLUE <- "lightblue"
COL_SKY    <- "skyblue"

# ---- ggplot theme ----
theme_set(theme_minimal(base_size = 13))

# ---- Helper functions ----

# Standard file naming with p2_ prefix
p2_fig <- function(name) file.path(fig_dir, paste0("p2_", name))

# Save ggplot with consistent dimensions/dpi
save_gg <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  ggsave(filename = filename, plot = plot, width = width, height = height, dpi = dpi)
}

# Save base plots robustly (supports par(), plot(), etc.)
save_base <- function(filename,
                      width_px = 1600,
                      height_px = 900,
                      res = 150,
                      expr) {
  
  # ---- 1) Draw to R console ----
  force(expr)
  
  # ---- 2) Draw again to file ----
  png(filename, width = width_px, height = height_px, res = res)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

###############################
# 1. Load & inspect data ------
###############################

df <- read.csv(csv_path)

head(df)
str(df)
summary(df)

df$Date <- as.Date(df$Date)
stopifnot(!any(is.na(df$Date)))

df <- df %>% mutate(year = lubridate::year(Date))

############################################################
# PART 1 ---- GEV (Block Maxima)
############################################################

###############################
# 1(a) Yearly maxima + plots ---
###############################

yearly_max <- df %>%
  group_by(year) %>%
  summarise(max_discharge = max(RiverDischarge, na.rm = TRUE), .groups = "drop")

# Yearly maxima line plot
p_yearly_max <- ggplot(yearly_max, aes(x = year, y = max_discharge)) +
  geom_line(color = COL_BLUE, linewidth = 0.8) +
  geom_point(color = COL_BLUE, size = 2) +
  labs(
    title = "Yearly Maximum River Discharge",
    x = "Year",
    y = "Maximum discharge (m³/s)"
  )

save_gg(p_yearly_max, p2_fig("yearly_max_discharge.png"), width = 8, height = 5)

# Histogram of yearly maxima
p_hist_max <- ggplot(yearly_max, aes(x = max_discharge)) +
  geom_histogram(bins = 20, fill = COL_SKY, color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Yearly Maximum River Discharge",
    x = "Yearly maximum discharge (m³/s)",
    y = "Frequency"
  )

save_gg(p_hist_max, p2_fig("hist_yearly_max_discharge.png"), width = 7, height = 5)

###############################
# 1(b) Linear model + forecast -
###############################

model_lm <- lm(max_discharge ~ year, data = yearly_max)
summary(model_lm)

future_years <- data.frame(year = seq(max(yearly_max$year) + 1, max(yearly_max$year) + 10))
pred <- predict(model_lm, newdata = future_years, interval = "prediction")

future_years$fit <- pred[, "fit"]
future_years$lwr <- pred[, "lwr"]
future_years$upr <- pred[, "upr"]

p_lm <- ggplot() +
  geom_point(data = yearly_max, aes(x = year, y = max_discharge), color = COL_BLUE) +
  geom_line(data = future_years, aes(x = year, y = fit), color = COL_RED, linewidth = 0.9) +
  geom_ribbon(data = future_years, aes(x = year, ymin = lwr, ymax = upr),
              fill = COL_RED, alpha = 0.18) +
  labs(
    title = "Linear Model Prediction of Yearly Maximum Discharge",
    x = "Year",
    y = "Max discharge (m³/s)"
  )

save_gg(p_lm, p2_fig("lm_yearly_max_forecast.png"), width = 8, height = 5)

###############################
# 1(c) GEV fits + AIC/BIC ------
###############################

gev_const <- extRemes::fevd(max_discharge ~ 1, data = yearly_max, type = "GEV")
gev_trend <- extRemes::fevd(max_discharge ~ year, data = yearly_max, type = "GEV")

summary(gev_const)
summary(gev_trend)

# Manual AIC/BIC (as in your original workflow)
logLik_const <- -gev_const$results$value
k_const      <- length(gev_const$results$par)
n_obs        <- nrow(yearly_max)

aic_const <- -2 * logLik_const + 2 * k_const
bic_const <- -2 * logLik_const + log(n_obs) * k_const

logLik_trend <- -gev_trend$results$value
k_trend      <- length(gev_trend$results$par)

aic_trend <- -2 * logLik_trend + 2 * k_trend
bic_trend <- -2 * logLik_trend + log(n_obs) * k_trend

cat("\nAIC (GEV const): ", aic_const, "\n")
cat("AIC (GEV trend): ", aic_trend, "\n")
cat("BIC (GEV const): ", bic_const, "\n")
cat("BIC (GEV trend): ", bic_trend, "\n")

###############################
# 1(d) GEV diagnostics (save) --
###############################

save_base(p2_fig("gev_const_diagnostics.png"), expr = {
  par(mfrow = c(2, 2))
  plot(gev_const)
  par(mfrow = c(1, 1))
})

save_base(p2_fig("gev_trend_diagnostics.png"), expr = {
  par(mfrow = c(2, 2))
  plot(gev_trend)
  par(mfrow = c(1, 1))
})

###############################
# 1(e) 10-year RL + CI + plot --
###############################

# Preferred model (explicit choice)
gev_pref <- gev_const

rl_10 <- extRemes::return.level(gev_pref, return.period = 10)
ci_10 <- extRemes::ci.fevd(
  gev_pref,
  type          = "return.level",
  return.period = 10,
  method        = "normal"
)


print(rl_10)
print(ci_10)

save_base(p2_fig("gev_return_level_plot.png"), expr = {
  plot(gev_pref, type = "rl")
  abline(v = 10, lty = 2)
  abline(h = ci_10[1], col = COL_RED, lwd = 2)       # point estimate
  abline(h = ci_10[2], col = COL_RED, lty = 3, lwd = 2)
  abline(h = ci_10[3], col = COL_RED, lty = 3, lwd = 2)
})

###############################
# 1(f) RL exceedance counts ----
###############################

periods <- c(10, 20, 50, 85)
rls <- extRemes::return.level(gev_pref, return.period = periods)

levels_vec <- as.numeric(rls)
names(levels_vec) <- periods

exceed_counts <- sapply(seq_along(levels_vec), function(i) {
  thr <- levels_vec[i]
  sum(yearly_max$max_discharge > thr)
})
names(exceed_counts) <- paste0(periods, "-year")

print(rls)
print(exceed_counts)

###############################
# 1(g) P(M > 100) next year ----
###############################

threshold <- 100

prob_exceed <- 1 - extRemes::pevd(
  threshold,
  loc   = gev_pref$results$par["location"],
  scale = gev_pref$results$par["scale"],
  shape = gev_pref$results$par["shape"],
  type  = "GEV"
)

cat("\nProbability annual maximum exceeds", threshold, "m³/s:", prob_exceed, "\n")

############################################################
# PART 2 ---- POT (GPD)
############################################################

###############################
# 2(a) Time series plot --------
###############################

p_ts <- ggplot(df, aes(x = Date, y = RiverDischarge)) +
  geom_line(color = COL_BLUE, linewidth = 0.6) +
  labs(
    title = "Daily River Discharge Over Time",
    x = "Date",
    y = "River discharge (m³/s)"
  )

save_gg(p_ts, p2_fig("discharge_timeseries.png"), width = 9, height = 4.5)

###############################
# 2(b) MRL plot + exceedances --
###############################

save_base(p2_fig("mrlplot.png"), expr = {
  POT::mrlplot(df$RiverDischarge, main = "Mean Residual Life Plot (River Discharge)")
})

u <- 40  # chosen threshold

p_thresh <- ggplot(df, aes(x = Date, y = RiverDischarge)) +
  geom_line(color = COL_BLUE, linewidth = 0.6) +
  geom_hline(yintercept = u, color = COL_RED, linetype = "dashed", linewidth = 0.7) +
  geom_point(
    data = subset(df, RiverDischarge > u),
    aes(x = Date, y = RiverDischarge),
    color = COL_RED,
    size = 1.2
  ) +
  labs(
    title = paste("Daily River Discharge with Threshold u =", u, "m³/s"),
    x = "Date",
    y = "Discharge (m³/s)"
  )

save_gg(p_thresh, p2_fig("threshold_exceedances.png"), width = 9, height = 4.5)

###############################
# 2(c) GPD fit (ismev) + diag --
###############################

gpd_mod <- ismev::gpd.fit(df$RiverDischarge, threshold = u)
print(gpd_mod)

save_base(p2_fig("gpd_diagnostics.png"), expr = {
  par(mfrow = c(2, 2))
  ismev::gpd.diag(gpd_mod)
  par(mfrow = c(1, 1))
})

###############################
# 2(d) Return levels (POT) -----
###############################

pars  <- gpd_mod$mle
sigma <- as.numeric(pars[1])
xi    <- as.numeric(pars[2])
u     <- gpd_mod$threshold

n_years <- length(unique(lubridate::year(df$Date)))
lambda  <- gpd_mod$nexc / n_years

T_vec <- c(10, 20, 50, 85)
z_T   <- u + (sigma / xi) * ((lambda * T_vec)^xi - 1)

return_levels <- data.frame(T_years = T_vec, ReturnLevel = z_T)
print(return_levels)

###############################
# 2(e) P(exceed 100) next year -
###############################

z <- 100
y <- z - u

p_cond <- (1 + xi * y / sigma)^(-1 / xi)
lambda_z <- lambda * p_cond
p_exceed_100 <- 1 - exp(-lambda_z)

cat("\nP(exceed 100 m³/s at least once next year; POT):", p_exceed_100, "\n")

############################################################
# PART 3 ---- Clustering / Decluster POT
############################################################

###############################
# 3(a) Extremal index ----------
###############################

u <- 40
theta_obj <- extRemes::extremalindex(df$RiverDischarge, threshold = u)
print(theta_obj)

###############################
# 3(b) Manual run declustering --
###############################

x <- df$RiverDischarge
exc_idx <- which(x > u)

r <- 4  # run length (days) used to separate clusters
diff_idx   <- diff(exc_idx)
cluster_id <- cumsum(c(1, diff_idx > r))

exc_vals <- x[exc_idx]
exc_df <- data.frame(discharge = exc_vals, cluster = cluster_id)

declust_vals <- tapply(exc_df$discharge, exc_df$cluster, max)
declust_vals <- as.numeric(declust_vals)

save_base(p2_fig("declustered_series.png"), width_px = 1600, height_px = 700, expr = {
  plot(
    declust_vals,
    type = "h",
    lwd  = 2,
    xlab = "Cluster index (extreme event number)",
    ylab = "Cluster maximum discharge (m³/s)",
    main = paste("Declustered extreme discharges (u =", u, ", r =", r, ")")
  )
  abline(h = u, lty = 2, col = COL_RED)
})

###############################
# 3(c) GP fits: raw vs declust -
###############################

raw_exceed     <- x[x > u]
declust_exceed <- declust_vals

raw_excess     <- raw_exceed - u
declust_excess <- declust_exceed - u

gpd_raw <- extRemes::fevd(raw_excess, type = "GP", threshold = 0)
gpd_dec <- extRemes::fevd(declust_excess, type = "GP", threshold = 0)

summary(gpd_raw)
summary(gpd_dec)

n_years <- length(unique(lubridate::year(df$Date)))
lambda_raw <- length(raw_exceed) / n_years
lambda_dec <- length(declust_exceed) / n_years

sigma_raw <- as.numeric(gpd_raw$results$par["scale"])
xi_raw    <- as.numeric(gpd_raw$results$par["shape"])
sigma_dec <- as.numeric(gpd_dec$results$par["scale"])
xi_dec    <- as.numeric(gpd_dec$results$par["shape"])

rl_pot <- function(u, sigma, xi, lambda, T) {
  if (abs(xi) < 1e-6) {
    u + sigma * log(lambda * T)
  } else {
    u + (sigma / xi) * ((lambda * T)^xi - 1)
  }
}

T <- 10
z10_raw <- rl_pot(u, sigma_raw, xi_raw, lambda_raw, T)
z10_dec <- rl_pot(u, sigma_dec, xi_dec, lambda_dec, T)

cat("\n10-year return level (raw POT):        ", z10_raw, "\n")
cat("10-year return level (declustered):    ", z10_dec, "\n")

############################################################
# End of script
############################################################
