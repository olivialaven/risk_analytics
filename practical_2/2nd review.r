df <- read.csv("River_and_precip_Neuchatel.csv")
str(df)
head(df)

df$Date <- as.Date(df$Date)
str(df$Date)

# PART 1 OF THE ASSIGNMENT ----------------------------------------

# Extract the year from each date
install.packages("lubridate")
library(lubridate)
df$year <- year(df$Date)

# Computing Yearly maximum
library(dplyr)
yearly_max <- df %>%
  group_by(year) %>%
  summarise(max_discharge = max(RiverDischarge, na.rm = TRUE))

# Checking the results
head(yearly_max)

# Plotting the yearly maximum discharge
install.packages("ggplot2")
library(ggplot2)
ggplot(yearly_max, aes(x = year, y = max_discharge)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Maximum River Discharge",
       x = "Year",
       y = "Maximum Discharge (m³/s)") +
  theme_minimal()
ggsave("yearly_max_discharge.png")

# Plotting a histogram of the yearly maximum discharge
ggplot(yearly_max, aes(x = max_discharge)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Yearly Maximum River Discharge",
       x = "Maximum Discharge (m³/s)",
       y = "Frequency") +
  theme_minimal()
ggsave("histogram_yearly_max_discharge.png")

# Plotting a 2nd histogram with different code
ggplot(yearly_max, aes(x = max_discharge)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Yearly Maximum Discharge",
       x = "Yearly Maximum Discharge (m³/s)",
       y = "Frequency")
ggsave("histogram_yearly_max_discharge_v2.png")

# Question 1.b: Fitting a basic Linear Model
model_lm <- lm(max_discharge ~ year, data = yearly_max)
summary(model_lm)

future_years <- data.frame(
  year = seq(max(yearly_max$year) + 1, 
             max(yearly_max$year) + 10)
)

predictions <- predict(model_lm, newdata = future_years, 
                       interval = "prediction")

future_years$fit  <- predictions[,"fit"]
future_years$lwr  <- predictions[,"lwr"]
future_years$upr  <- predictions[,"upr"]

ggplot() +
  geom_point(data = yearly_max, 
             aes(x = year, y = max_discharge), color = "blue") +
  geom_line(data = future_years, 
            aes(x = year, y = fit), color = "red") +
  geom_ribbon(data = future_years,
              aes(x = year, ymin = lwr, ymax = upr),
              alpha = 0.2, fill = "red") +
  labs(title = "Linear Model Prediction of Yearly Maximum Discharge",
       x = "Year", 
       y = "Max Discharge (m³/s)")

# Question 1.c:
install.packages("extRemes")
library(extRemes)

# Fitting GEV model with constant parameters
gev_const <- fevd(max_discharge ~ 1,
                  data = yearly_max,
                  type = "GEV")
summary(gev_const)

# Fitting GEV model with time-varying location parameter
gev_trend <- fevd(max_discharge ~ year,
                  data = yearly_max,
                  type = "GEV")
summary(gev_trend)

# Comparing models using AIC and BIC
# Calculate AIC manually for the constant parameter model
logLik_const <- -gev_const$results$value
k_const <- length(gev_const$results$par)
aic_const <- -2 * logLik_const + 2 * k_const

# Calculate AIC manually for the time-varying parameter model
logLik_trend <- -gev_trend$results$value
k_trend <- length(gev_trend$results$par)
aic_trend <- -2 * logLik_trend + 2 * k_trend

# Print AIC values
print(paste("AIC for constant parameter model:", aic_const))
print(paste("AIC for time-varying parameter model:", aic_trend))

# Calculate BIC manually for the constant parameter model
n <- nrow(yearly_max)
bic_const <- -2 * logLik_const + log(n) * k_const
# Calculate BIC manually for the time-varying parameter model
bic_trend <- -2 * logLik_trend + log(n) * k_trend
# Print BIC values
print(paste("BIC for constant parameter model:", bic_const))
print(paste("BIC for time-varying parameter model:", bic_trend))
# Lower AIC and BIC values indicate a better model fit

# Question 1.d: Draw diagnostic plots of your GEV fits and save them
png("gev_const_diagnostics.png")
par(mfrow = c(2, 2))
plot(gev_const)
dev.off()
png("gev_trend_diagnostics.png")
par(mfrow = c(2, 2))
plot(gev_trend)
dev.off()

# Question 1.e: Estimate Return level
rl_10 <- return.level(gev_const, return.period = 10)
rl_10

ci_10 <- ci(gev_const,
            type          = "return.level",
            return.period = 10,
            method        = "normal")
ci_10

plot(gev_const, type = "rl")   # 'rl' = return level
abline(v = 10, lty = 2)                     # vertical line at 10 years
abline(h = ci_10[1], col = "red")           # point estimate
abline(h = ci_10[2], col = "red", lty = 3)  # lower CI
abline(h = ci_10[3], col = "red", lty = 3)  # upper CI

# Question 1.f: Rl
periods <- c(10, 20, 50, 85)
rls <- return.level(gev_const, return.period = periods)
rls

levels_vec <- as.numeric(rls)
names(levels_vec) <- periods

exceed_counts <- sapply(seq_along(levels_vec), function(i) {
  thr <- levels_vec[i]
  sum(yearly_max$max_discharge > thr)
})
names(exceed_counts) <- paste0(periods, "-year")
exceed_counts

# Question 1.g: Using the fitted model, compute the probability that the river discharge exceeds 100 m3/s on at least one day in the next year.
threshold <- 100
prob_exceed <- 1 - pevd(threshold,
                           loc = gev_const$results$par["location"],
                           scale = gev_const$results$par["scale"],
                           shape = gev_const$results$par["shape"],
                           type = "GEV")
prob_exceed
print(paste("Probability of exceeding", threshold, "m³/s in the next year:", prob_exceed))

# PART 2 OF THE ASSIGNMENT ----------------------------------------

# Question 2.a: Time series plot of the daily river discharge across the entire data range
ggplot(df, aes(x = Date, y = RiverDischarge)) +
  geom_line(color = "blue") +
  labs(title = "Daily River Discharge Over Time",
       x = "Date",
       y = "River Discharge (m³/s)") +
  theme_minimal()
ggsave("daily_river_discharge_timeseries.png")

# Question 2.b: 
library(POT)

mrlplot(df$RiverDischarge, main = "Mean Residual Life Plot")

# Choosing a threshold based on the MRL plot to highlight threshold exceedances in the time series plot
u <- 40  # chosen threshold

ggplot(df, aes(x = Date, y = RiverDischarge)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = u, color = "red", linetype = "dashed", linewidth = 0.7) +
  geom_point(
    data = subset(df, RiverDischarge > u),
    aes(x = Date, y = RiverDischarge),
    color = "red",
    size = 1.2
  ) +
  labs(
    title = paste("Daily River Discharge with Threshold u =", u, "m³/s"),
    x = "Date",
    y = "Discharge (m³/s)"
  ) +
  theme_minimal()
ggsave("river_discharge_threshold_exceedances_v2.png")

# Question 2.c: Fitting a GPD to the exceedances over the chosen threshold and drawing diagnostic plots
library(ismev)
u <- 40
gpd_mod <- gpd.fit(df$RiverDischarge, u)
gpd_mod

gpd.diag(gpd_mod)

# Saving diagnostic plots
png("gpd_diagnostics.png")
par(mfrow = c(2, 2))
gpd.diag(gpd_mod)
dev.off()

# Question 2.d: Using the fitted model, compute the 10-year, 20-year, 50-year, and 85-year return levels.
library(lubridate)

# 1. Extract GPD parameters (scale, shape) as pure numerics
pars  <- gpd_mod$mle           # <-- THIS is the right slot for ismev::gpd.fit
pars                           # just to see them if you're curious

sigma <- as.numeric(pars[1])   # scale
xi    <- as.numeric(pars[2])   # shape

# 2. Threshold (should be 40)
u <- gpd_mod$threshold

# 3. Exceedance rate per year λ
n_years <- length(unique(year(df$Date)))  # number of years
lambda  <- gpd_mod$nexc / n_years         # nexc = number of exceedances

lambda  # check roughly how many exceedances per year

# 4. Return periods we care about
T_vec <- c(10, 20, 50, 85)

# 5. Compute return levels z_T (general GPD formula, ξ ≠ 0)
z_T <- u + (sigma / xi) * ((lambda * T_vec)^xi - 1)

return_levels <- data.frame(
  T_years     = T_vec,
  ReturnLevel = z_T
)

return_levels

# Question 2.e: Using the fitted model, compute the probability that the river discharge exceeds 100 m3/s on at least one day in the next year.
# Threshold and parameters
u     <- gpd_mod$threshold
pars  <- gpd_mod$mle
sigma <- as.numeric(pars[1])
xi    <- as.numeric(pars[2])

# Exceedance rate above u per year
n_years <- length(unique(year(df$Date)))
lambda  <- gpd_mod$nexc / n_years

# Level of interest
z <- 100
y <- z - u  # excess above threshold

# Conditional tail probability P(X > z | X > u) = P(Y > y)
p_cond <- (1 + xi * y / sigma)^(-1/xi)

# Expected number of exceedances of z per year
lambda_100 <- lambda * p_cond

# Probability of at least one exceedance of z in a year
p_exceed_100 <- 1 - exp(-lambda_100)

lambda
lambda_100
p_exceed_100

# PART 3 OF THE ASSIGNMENT ----------------------------------

# Question 3(a): Compute the extremal index of the seasonal subset using an appropriately chosen threshold (e.g., with the extremalindex function in the extRemes package).
library(lubridate)
df$Month <- month(df$Date)
season_df <- subset(df, Month %in% 5:9)

library(extRemes)
theta_est <- extremalindex(season_df$RiverDischarge,
                           threshold = 40)
theta_est

# Do extreme values tend to cluster?
# A value of θ close to 1 suggests that extreme values are mostly isolated, while a value significantly less than 1 indicates clustering of extremes.
# What is the probability that if today’s discharge is extreme, tomorrow’s will be as well?
theta_hat <- as.numeric(theta_est["extremal.index"])
theta_hat
# 0.6016945

p_tomorrow_extreme <- 1 - theta_hat
p_tomorrow_extreme
# 0.3983055

# Question 3(b): Decluster the data using the chosen threshold (e.g., with the decluster function from extRemes). Plot the resulting declustered data.
u <- 40

declust <- decluster(season_df$RiverDischarge,
                     threshold = u,
                     r = 4,             # run length = 4 from theta_est
                     type = "run")

declust

declust_vals <- as.numeric(declust)
declust_vals

# Plotting the declustered data
declust_df <- data.frame(
  Event      = seq_along(declust_vals),
  ClusterMax = declust_vals
)

ggplot(declust_df, aes(Event, ClusterMax)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept = u, linetype = "dashed", color = "red") +
  labs(
    title = "Declustered Extreme Discharges (Cluster Maxima)",
    x = "Cluster Index",
    y = "Cluster Maximum (m³/s)"
  ) +
  theme_minimal()
ggsave("declustered_extreme_discharges.png")

# Question 3(c): Fit a Generalized Pareto Distribution (GPD) to both the raw and declustered data. Compare the two models and compute the 10-year return level. How does declustering affect the estimated tail behavior?
## ==== 3(c) – GPD on raw vs declustered + 10-year return level ====

# Load packages (install first if needed)
library(lubridate)
library(extRemes)
library(ismev)

# 1. Ensure Date is in Date format
df$Date <- as.Date(df$Date)

# 2. Seasonal subset: May–September (months 5–9)
df$Month    <- month(df$Date)
season_df   <- subset(df, Month %in% 5:9)

# 3. Set threshold
u <- 40

# 4. Fit GPD to RAW seasonal exceedances (all X > u in season)
gpd_raw <- gpd.fit(season_df$RiverDischarge, u)

# 5. Decluster seasonal data using run declustering with r = 4
declust_obj  <- decluster(season_df$RiverDischarge,
                          threshold = u,
                          r         = 4,
                          type      = "run")

declust_vals <- as.numeric(declust_obj)  # cluster maxima

# 6. Fit GPD to DECLUSTERED cluster maxima (same threshold u)
gpd_dec <- gpd.fit(declust_vals, u)

# 7. Number of seasonal years
n_years_season <- length(unique(year(season_df$Date)))

# 8. Exceedance rates per seasonal year
lambda_raw <- gpd_raw$nexc           / n_years_season      # all exceedances > u
lambda_dec <- length(declust_vals)   / n_years_season      # clusters per year

# 9. Extract GPD parameters (sigma, xi) for both models
pars_raw  <- gpd_raw$mle
sigma_raw <- as.numeric(pars_raw[1])
xi_raw    <- as.numeric(pars_raw[2])

pars_dec  <- gpd_dec$mle
sigma_dec <- as.numeric(pars_dec[1])
xi_dec    <- as.numeric(pars_dec[2])

# 10. 10-year return level (T = 10) for both models
T10 <- 10

z10_raw <- u + (sigma_raw / xi_raw) * ((lambda_raw * T10)^xi_raw - 1)
z10_dec <- u + (sigma_dec / xi_dec) * ((lambda_dec * T10)^xi_dec - 1)

# 11. Print everything we care about
cat("\n=== RAW seasonal exceedances ===\n")
cat("GPD parameters (sigma, xi):", sigma_raw, xi_raw, "\n")
cat("Exceedance rate lambda_raw (per seasonal year):", lambda_raw, "\n")
cat("10-year return level z10_raw:", z10_raw, "m^3/s\n")

cat("\n=== DECLUSTERED cluster maxima ===\n")
cat("GPD parameters (sigma, xi):", sigma_dec, xi_dec, "\n")
cat("Cluster rate lambda_dec (per seasonal year):", lambda_dec, "\n")
cat("10-year return level z10_dec:", z10_dec, "m^3/s\n")

cat("\nNumber of declustered events (clusters):", length(declust_vals), "\n")








