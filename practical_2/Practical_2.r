# Get absolute path of this script (works when run via source())
this_file <- normalizePath(sys.frame(1)$ofile)
this_dir  <- dirname(this_file)

# Build path to CSV assuming it is in the *same folder* as Practical_2.r
csv_path  <- file.path(this_dir, "River_and_precip_Neuchatel.csv")

df <- read.csv(csv_path)
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

# For dates
library(lubridate)

# For extreme value methods (Part 3)
library(extRemes)

df <- read.csv("River_and_precip_Neuchatel.csv")
str(df)

# Making sure dates are in the right format
df$Date <- as.Date(df$Date)

# Question 1: Computing the extremal index of the seasonal subset

library(extRemes)

u <- 40
theta_obj <- extremalindex(df$RiverDischarge, threshold = u)
theta_obj

# Question 2: Decluster the data using the chosen threshold (e.g., with the decluster function from extRemes). Plot the resulting declustered data.
u <- 40
x <- df$RiverDischarge

# Indices (positions) where discharge exceeds the threshold
exc_idx <- which(x > u)

length(exc_idx)     # how many exceedances (days above 40)?
head(exc_idx)       # first few indices

# Manually identifying clusters
# Run length from extremal index
r <- 4

# Differences between consecutive exceedance indices
diff_idx <- diff(exc_idx)

# Look at these differences
head(diff_idx)

# Create cluster labels
cluster_id <- cumsum(c(1, diff_idx > r))

# Check first few cluster labels
head(cluster_id)

# How many clusters do we have?
max(cluster_id)

# Discharge values at exceedance days
exc_vals <- x[exc_idx]

# Create a data frame to group by cluster
exc_df <- data.frame(
  discharge = exc_vals,
  cluster   = cluster_id
)

# Take the maximum discharge in each cluster
declust_vals <- tapply(exc_df$discharge,
                       exc_df$cluster,
                       max)

# Check result
length(declust_vals)
declust_vals

# Plotting the declustered data
u <- 40  # threshold (keep consistent)

plot(declust_vals,
     type = "h",
     lwd  = 2,
     xlab = "Cluster index (extreme event number)",
     ylab = "Cluster maximum discharge (m³/s)",
     main = "Declustered extreme discharges (u = 40, r = 4)")

abline(h = u, lty = 2)

# Save the plot
dev.copy(png, "declustered_discharge.png")
dev.off()

# Question 3: Fit a Generalized Pareto Distribution (GPD) to both the raw and declustered data.
# Preparing the 2 datasets
u <- 40

raw_exceed <- x[x > u]

length(raw_exceed)

declust_exceed <- declust_vals

length(declust_exceed)

# Fit GPD to raw exceedances
library(extRemes)

u <- 40

# Excesses over threshold
raw_excess <- raw_exceed - u

# Fit GPD to raw exceedances
gpd_raw <- fevd(raw_excess,
                type = "GP",
                threshold = 0)

summary(gpd_raw)

# Fit GPD to declustered exceedances
# Excesses over threshold (declustered)
declust_excess <- declust_exceed - u

# Fit GPD to declustered exceedances
gpd_declust <- fevd(declust_excess,
                    type = "GP",
                    threshold = 0)

summary(gpd_declust)

# Computing the return levels for both fits
# How many years are in the dataset?
n_years <- length(unique(format(df$Date, "%Y")))
n_years

lambda_raw     <- length(raw_exceed)     / n_years
lambda_declust <- length(declust_exceed) / n_years

lambda_raw
lambda_declust

u <- 40
T <- 10

# Extract parameters from the fitted models
sigma_raw <- as.numeric(gpd_raw$results$par["scale"])
xi_raw    <- as.numeric(gpd_raw$results$par["shape"])

sigma_dec <- as.numeric(gpd_declust$results$par["scale"])
xi_dec    <- as.numeric(gpd_declust$results$par["shape"])

# Helper function for return level
rl_pot <- function(u, sigma, xi, lambda, T) {
  if (abs(xi) < 1e-6) {
    return(u + sigma * log(lambda * T))
  } else {
    return(u + (sigma/xi) * ((lambda * T)^xi - 1))
  }
}

# 10-year return levels
z10_raw <- rl_pot(u, sigma_raw, xi_raw, lambda_raw, T)
z10_dec <- rl_pot(u, sigma_dec, xi_dec, lambda_declust, T)

z10_raw
z10_dec