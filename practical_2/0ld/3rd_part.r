# 3rd part of the second practical

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








