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
    stop(paste("Package not installed:", p, "- please install it and re-run."))
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

# ---- ggplot theme (LaTeX-safe sizing) ----
theme_set(
  theme_minimal(base_size = 18) +
    theme(
      plot.title   = element_text(size = 20, face = "bold"),
      axis.title   = element_text(size = 18),
      axis.text    = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text  = element_text(size = 14)
    )
)

# ---- Standard figure dimensions for side-by-side plots ----
W_WIDE_PX  <- 1600
H_WIDE_PX  <- 900
W_WIDE_IN  <- 9
H_WIDE_IN  <- 9 * (H_WIDE_PX / W_WIDE_PX)  # keeps same aspect ratio

# ---- Helper functions ----

# Standard file naming with p2_ prefix
p2_fig <- function(name) file.path(fig_dir, paste0("p2_", name))

# Save ggplot with consistent dimensions/dpi AND show in console
save_gg <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  print(plot)  # ensures visibility in RStudio / interactive device
  ggsave(filename = filename, plot = plot, width = width, height = height, dpi = dpi)
}

# Save base plots correctly (evaluate plotting expression twice)
save_base <- function(filename,
                      width_px  = 1600,
                      height_px = 900,
                      res       = 150,
                      expr) {
  
  e <- substitute(expr)
  
  .set_par <- function() {
    par(cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.2, cex = 1.2)
    par(mfrow = c(1, 1))
  }
  
  # ---- console ----
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  .set_par()
  eval(e, envir = parent.frame())
  
  # ---- file ----
  png(filename, width = width_px, height = height_px, res = res)
  .set_par()
  eval(e, envir = parent.frame())
  dev.off()
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

# Create prediction grid that includes the historical years (for fitted line)
hist_years <- data.frame(year = yearly_max$year)
hist_years$fit <- predict(model_lm, newdata = hist_years)

# Future predictions (next 10 years) with prediction interval
future_years <- data.frame(
  year = seq(max(yearly_max$year) + 1, max(yearly_max$year) + 10)
)
pred_future <- predict(model_lm, newdata = future_years, interval = "prediction")

future_years$fit <- pred_future[, "fit"]
future_years$lwr <- pred_future[, "lwr"]
future_years$upr <- pred_future[, "upr"]

# Plot: points (historical maxima) + fitted line (historical) + future ribbon/line
p_lm <- ggplot() +
  geom_point(
    data = yearly_max,
    aes(x = year, y = max_discharge),
    color = COL_BLUE,
    size = 1.8
  ) +
  geom_line(
    data = hist_years,
    aes(x = year, y = fit),
    color = COL_RED,
    linewidth = 0.9
  ) +
  geom_ribbon(
    data = future_years,
    aes(x = year, ymin = lwr, ymax = upr),
    fill = COL_RED,
    alpha = 0.18
  ) +
  geom_line(
    data = future_years,
    aes(x = year, y = fit),
    color = COL_RED,
    linewidth = 0.9
  ) +
  labs(
    title = "LM: Annual Max Discharge (Fit + 10-Year Forecast)",
    x = "Year",
    y = "Max discharge (m³/s)"
  )

save_gg(p_lm, p2_fig("lm_yearly_max_forecast.png"), width = 8, height = 5)


###############################
# 1(c) GEV fits + AIC/BIC ------
###############################

gev_const <- extRemes::fevd(max_discharge ~ 1, data = yearly_max, type = "GEV")
gev_trend <- extRemes::fevd(max_discharge ~ year, data = yearly_max, type = "GEV")
gev_pref <- gev_const

###############################
# 1(e-prep) 10-year RL + CI ----
###############################

# 10-year return level (point estimate)
rl_10 <- extRemes::return.level(gev_pref, return.period = 10)

# 95% CI for the 10-year return level
ci_10 <- extRemes::ci.fevd(
  gev_pref,
  type          = "return.level",
  return.period = 10,
  method        = "normal"
)

print(rl_10)
print(ci_10)

# Robust extractor for the CI values (lower, upper)
get_ci_vals <- function(ci_obj) {
  # If already a numeric (atomic) vector
  if (is.atomic(ci_obj)) {
    v <- suppressWarnings(as.numeric(ci_obj))
    v <- v[is.finite(v)]
    return(v)
  }
  
  # If list-like with a CI element
  if (!is.null(ci_obj$CI)) {
    v <- suppressWarnings(as.numeric(ci_obj$CI))
    v <- v[is.finite(v)]
    return(v)
  }
  
  # If stored as an attribute
  v <- suppressWarnings(as.numeric(attr(ci_obj, "CI")))
  v <- v[is.finite(v)]
  return(v)
}



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
# 1(d.1) GEV QQ plot (manual, styled)
###############################

save_base(
  p2_fig("gev_qq_plot.png"),
  width_px = 1600, height_px = 1000, res = 150,
  expr = {
    
    y_emp <- sort(yearly_max$max_discharge)
    n     <- length(y_emp)
    p     <- (1:n) / (n + 1)
    
    pars  <- gev_pref$results$par
    mu    <- as.numeric(pars["location"])
    sig   <- as.numeric(pars["scale"])
    xi    <- as.numeric(pars["shape"])
    
    q_theo <- extRemes::qevd(p, loc = mu, scale = sig, shape = xi, type = "GEV")
    
    set.seed(123)
    M <- 500
    sim_mat <- replicate(
      M,
      sort(extRemes::revd(n, loc = mu, scale = sig, shape = xi, type = "GEV"))
    )
    lo <- apply(sim_mat, 1, quantile, probs = 0.025, names = FALSE)
    hi <- apply(sim_mat, 1, quantile, probs = 0.975, names = FALSE)
    
    # ensure polygon x is strictly ordered
    ord <- order(q_theo)
    qx  <- q_theo[ord]
    ye  <- y_emp[ord]
    lo2 <- lo[ord]
    hi2 <- hi[ord]
    
    lims <- range(c(ye, qx, lo2, hi2))
    
    plot(
      qx, ye,
      type = "n",
      xlim = lims, ylim = lims,
      xlab = "Model quantiles",
      ylab = "Empirical quantiles",
      main = "GEV QQ Plot (Annual Maxima)",
      cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.4
    )
    
    polygon(
      x = c(qx, rev(qx)),
      y = c(lo2, rev(hi2)),
      border = NA,
      col = adjustcolor(COL_BLUE, alpha.f = 0.18)
    )
    
    points(qx, ye, pch = 21, bg = COL_BLUE, col = "black", cex = 1.6)
    abline(0, 1, col = COL_RED, lwd = 2, lty = 2)
  }
)



###############################
# 1(e) 10-year RL + CI + plot --
###############################

save_base(
  p2_fig("gev_return_level_plot.png"),
  width_px = 1600, height_px = 1000, res = 150,
  expr = {
    
    fit <- gev_pref
    
    y <- sort(yearly_max$max_discharge)
    n <- length(y)
    
    pars <- fit$results$par
    mu   <- as.numeric(pars["location"])
    sig  <- as.numeric(pars["scale"])
    xi   <- as.numeric(pars["shape"])
    
    # Empirical plotting positions
    p_emp <- (1:n) / (n + 1)
    T_emp <- 1 / (1 - p_emp)
    T_emp[T_emp > 1e4] <- 1e4
    
    # Return-period grid (log-spaced)
    T_grid <- exp(seq(log(1.01), log(1000), length.out = 350))
    p_grid <- 1 - 1 / T_grid
    
    # Fitted return levels
    z_hat <- extRemes::qevd(p_grid, loc = mu, scale = sig, shape = xi, type = "GEV")
    
    # ---- Parametric bootstrap band (robust) ----
    set.seed(123)
    B <- 300
    z_boot <- matrix(NA_real_, nrow = length(T_grid), ncol = B)
    
    b <- 1
    attempts <- 0
    max_attempts <- B * 4
    
    while (b <= B && attempts < max_attempts) {
      attempts <- attempts + 1
      
      x_b <- extRemes::revd(n, loc = mu, scale = sig, shape = xi, type = "GEV")
      fit_b <- try(extRemes::fevd(x_b, type = "GEV"), silent = TRUE)
      if (inherits(fit_b, "try-error")) next
      
      pars_b <- fit_b$results$par
      z_boot[, b] <- extRemes::qevd(
        p_grid,
        loc   = as.numeric(pars_b["location"]),
        scale = as.numeric(pars_b["scale"]),
        shape = as.numeric(pars_b["shape"]),
        type  = "GEV"
      )
      b <- b + 1
    }
    
    if (b <= B) warning("Bootstrap completed with fewer successful refits than requested.")
    
    z_lo <- apply(z_boot, 1, quantile, probs = 0.025, na.rm = TRUE)
    z_hi <- apply(z_boot, 1, quantile, probs = 0.975, na.rm = TRUE)
    
    plot(
      T_grid, z_hat,
      type = "n",
      log  = "x",
      xlab = "Return period (years)",
      ylab = "Return level (m³/s)",
      main = "GEV Return Level Plot (Annual Maxima)",
      cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.4
    )
    
    polygon(
      x = c(T_grid, rev(T_grid)),
      y = c(z_lo,   rev(z_hi)),
      border = NA,
      col = adjustcolor(COL_BLUE, alpha.f = 0.18)
    )
    
    lines(T_grid, z_hat, col = COL_BLUE, lwd = 2.4)
    points(T_emp, y, pch = 21, bg = COL_BLUE, col = "black", cex = 1.3)
    
    abline(v = 10, lty = 2)
    
    rl_val  <- as.numeric(rl_10)
    ci_vals <- get_ci_vals(ci_10)
    
    abline(h = rl_val, col = COL_RED, lwd = 2)
    if (length(ci_vals) >= 2) {
      abline(h = ci_vals[1], col = COL_RED, lty = 3, lwd = 2)
      abline(h = ci_vals[2], col = COL_RED, lty = 3, lwd = 2)
    }
    
    legend(
      "topleft",
      legend = c("Fitted GEV RL", "95% bootstrap band", "Empirical annual maxima"),
      col    = c(COL_BLUE, adjustcolor(COL_BLUE, alpha.f = 0.30), "black"),
      lwd    = c(2.4, NA, NA),
      pch    = c(NA, 15, 21),
      pt.bg  = c(NA, adjustcolor(COL_BLUE, alpha.f = 0.30), COL_BLUE),
      bty    = "n"
    )
  }
)


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

save_base(p2_fig("mrlplot.png"), width_px = W_WIDE_PX, height_px = H_WIDE_PX, res = 150, expr = {
  
  x <- df$RiverDischarge
  thr_lines <- c(30, 40, 50)
  
  # Clean base-plot parameters to align with your EVT plot style
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(
    mar = c(4.5, 5.0, 3.5, 2.0),
    mgp = c(2.8, 0.9, 0),
    tcl = -0.3,
    las = 1
  )
  
  # Base MRL plot (POT draws the curve/points)
  POT::mrlplot(
    x,
    main = "Mean Residual Life Plot (River Discharge)",
    xlab = "Threshold u (m³/s)",
    ylab = "Mean excess above u (m³/s)"
  )
  
  # Emphasize the MRL curve visually (overlay a thicker line)
  # Recompute mean excess to overlay as a clean line
  thr_grid <- pretty(x, n = 25)
  thr_grid <- thr_grid[thr_grid < max(x, na.rm = TRUE)]
  
  mrl_mean_excess <- sapply(thr_grid, function(u) {
    exc <- x[x > u] - u
    if (length(exc) < 5) return(NA_real_)
    mean(exc, na.rm = TRUE)
  })
  
  lines(thr_grid, mrl_mean_excess, lwd = 2.2, col = COL_BLUE)
  
  # Candidate threshold lines
  abline(v = 30, col = COL_BLUE, lty = 2, lwd = 2)
  abline(v = 40, col = COL_RED,  lty = 2, lwd = 2.6)
  abline(v = 50, col = COL_BLUE, lty = 2, lwd = 2)
  
  legend(
    "topright",
    legend = c("MRL curve", "u = 30", "u = 40 (selected)", "u = 50"),
    col    = c(COL_BLUE, COL_BLUE, COL_RED, COL_BLUE),
    lty    = c(1, 2, 2, 2),
    lwd    = c(2.2, 2, 2.6, 2),
    bty    = "n"
  )
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

save_gg(p_thresh,
        p2_fig("threshold_exceedances.png"),
        width = W_WIDE_IN, height = H_WIDE_IN, dpi = 300)

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
# 2(c-alt) GPD QQ plot (single)
###############################

save_base(
  p2_fig("gpd_qq_plot.png"),
  width_px = 1600, height_px = 1000, res = 150,
  expr = {
    
    u <- gpd_mod$threshold
    y <- sort(df$RiverDischarge[df$RiverDischarge > u] - u)
    n <- length(y)
    p <- (1:n) / (n + 1)
    
    pars  <- gpd_mod$mle
    sigma <- as.numeric(pars[1])
    xi    <- as.numeric(pars[2])
    
    q_theo <- if (abs(xi) < 1e-6) {
      sigma * log(1 / (1 - p))
    } else {
      (sigma / xi) * ((1 - p)^(-xi) - 1)
    }
    
    set.seed(123)
    M <- 500
    sim_excess <- replicate(M, {
      U <- runif(n)
      if (abs(xi) < 1e-6) {
        sort(sigma * log(1 / (1 - U)))
      } else {
        sort((sigma / xi) * ((1 - U)^(-xi) - 1))
      }
    })
    
    lo <- apply(sim_excess, 1, quantile, probs = 0.025, names = FALSE)
    hi <- apply(sim_excess, 1, quantile, probs = 0.975, names = FALSE)
    
    ord <- order(q_theo)
    qx  <- q_theo[ord]
    ye  <- y[ord]
    lo2 <- lo[ord]
    hi2 <- hi[ord]
    
    lims <- range(c(ye, qx, lo2, hi2))
    
    plot(
      qx, ye,
      type = "n",
      xlim = lims, ylim = lims,
      xlab = "Model quantiles (excess above u)",
      ylab = "Empirical excesses (x − u)",
      main = "QQ Plot of the GPD Fit (Exceedances above 40 m³/s)",
      cex.main = 1.8, cex.lab = 1.6, cex.axis = 1.4
    )
    
    polygon(
      x = c(qx, rev(qx)),
      y = c(lo2, rev(hi2)),
      border = NA,
      col = adjustcolor(COL_BLUE, alpha.f = 0.18)
    )
    
    points(qx, ye, pch = 21, bg = COL_BLUE, col = "black", cex = 1.6)
    abline(0, 1, col = COL_RED, lwd = 2, lty = 2)
  }
)


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
