# Import libraries
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(extRemes)     
library(POT)          
library(scales)
library(moments)
library(nortest)
library(fitdistrplus)

# 1. Load + clean data ----

comed <- read_csv("~/Desktop/MA3/Risk Analytics/final_project/COMED_hourly.csv")
temp <- read_csv("~/Desktop/MA3/Risk Analytics/final_project/temperature.csv")

# Make sure timestamps are parsed correctly
comed$datetime <- ymd_hms(comed$Datetime)

bad_rows <- comed %>% filter(is.na(ymd_hms(Datetime)))
head(bad_rows$Datetime, 50)

comed_daily <- comed %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(
    n_hours = n(),
    daily_total_load = sum(COMED_MW),
    daily_max_load   = max(COMED_MW)
  ) %>%
  filter(n_hours == 23) %>%
  dplyr::select(date, daily_total_load, daily_max_load)


### Temperature: parse timestamps + convert to °C
temp <- temp %>%
  mutate(datetime = ymd_hms(datetime, quiet = TRUE)) %>%
  filter(!is.na(datetime)) %>%
  mutate(Chicago_C = Chicago - 273.15)

# 2. Aggregate to daily level ----

### COMED daily aggregation (only keep full 24-hour days)
comed_daily_check <- comed %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(
    n_hours = n(),
    daily_total_load = sum(COMED_MW, na.rm = TRUE),
    daily_max_load   = max(COMED_MW, na.rm = TRUE)
  )

table(comed_daily_check$n_hours)

### Chicago temperature -> daily average
temp_daily <- temp %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_mean_temp = mean(Chicago_C, na.rm = TRUE))

### Merge daily load + temperature
data_daily <- comed_daily %>%
  inner_join(temp_daily, by = "date") %>%
  arrange(date)

# 3. Exploratory Data Analysis----

### Histogram + QQ-Plot of Daily Total Load
x <- data_daily$daily_total_load
x <- x[!is.na(x)]

par(mfrow=c(1,2))
hist(x, breaks=40, col="skyblue", prob=TRUE,
     main="Histogram of Daily Total Load",
     xlab="Daily Total Load")
lines(density(x), lwd=2)
qqnorm(x); qqline(x, col="red", lwd=2)
par(mfrow=c(1,1))

### Skewness & Kurtosis
skewness_val  <- skewness(x)
kurtosis_val  <- kurtosis(x)
cat("Skewness:", skewness_val, "\n")
cat("Kurtosis:", kurtosis_val, "\n")

### Normality Test
ad.test(x)

### Fit Normal + Lognormal
fit_norm  <- fitdist(x, "norm")
fit_lnorm <- fitdist(x, "lnorm")

qqcomp(list(fit_norm, fit_lnorm),
       legendtext=c("Normal","Lognormal"))

### Density comparison
hist(x, breaks=50, prob=TRUE, col="lightblue",
     main="Histogram with Fitted Densities")
xseq <- seq(min(x), max(x), length=1000)

lines(xseq, dnorm(xseq,
                  mean=fit_norm$estimate["mean"],
                  sd=fit_norm$estimate["sd"]),
      col="red", lwd=2)

lines(xseq, dlnorm(xseq,
                   meanlog=fit_lnorm$estimate["meanlog"],
                   sdlog=fit_lnorm$estimate["sdlog"]),
      col="darkgreen", lwd=2)

legend("topright", legend=c("Normal","Lognormal"),
       col=c("red","darkgreen"), lwd=2)

### Tail probability comparison
threshold_95 <- quantile(x, 0.95)
threshold_99 <- quantile(x, 0.99)

p_norm_95  <- 1 - pnorm(threshold_95,  fit_norm$estimate["mean"],  fit_norm$estimate["sd"])
p_lnorm_95 <- 1 - plnorm(threshold_95, fit_lnorm$estimate["meanlog"], fit_lnorm$estimate["sdlog"])

p_norm_99  <- 1 - pnorm(threshold_99,  fit_norm$estimate["mean"],  fit_norm$estimate["sd"])
p_lnorm_99 <- 1 - plnorm(threshold_99, fit_lnorm$estimate["meanlog"], fit_lnorm$estimate["sdlog"])

cat("Normal tail P95:", p_norm_95, "\n")
cat("Lognorm tail P95:", p_lnorm_95, "\n")
cat("Ratio:", p_lnorm_95/p_norm_95, "\n\n")

cat("Normal tail P99:", p_norm_99, "\n")
cat("Lognorm tail P99:", p_lnorm_99, "\n")
cat("Ratio:", p_lnorm_99/p_norm_99, "\n")

### Time series plots
ggplot(data_daily, aes(date, daily_total_load)) +
  geom_line() +
  labs(title="Daily Total Electricity Demand", y="Total Load")

ggplot(data_daily, aes(date, daily_mean_temp)) +
  geom_line(color="firebrick") +
  labs(title="Daily Mean Temperature (Chicago)", y="°C")

ggplot(data_daily, aes(daily_mean_temp, daily_total_load)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="loess") +
  labs(title="Daily Total Load vs Temperature",
       x="Temperature (°C)", y="Daily Load")

# 4. Block Maxima (GEV)----

### Compute weekly maxima
weekly_max <- data_daily %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(weekly_max_total = max(daily_total_load))

### Fit stationary GEV
gev_tot <- fevd(weekly_max_total ~ 1,
                data=weekly_max, type="GEV")

summary(gev_tot)
plot(gev_tot)   # prob, qq, density, return-level

### Compute return levels
period_years <- c(1,5,10,20)
period_weeks <- period_years * 52

gev_rl <- return.level(gev_tot, return.period=period_weeks)
gev_rl

# 5. Peaks Over Threshold (POT)----

### Threshold selection (MRL plot)
mrlplot(data_daily$daily_total_load)
quantile(data_daily$daily_total_load, c(0.90,0.95,0.975))

# threshold for cluster detection
u <- quantile(data_daily$daily_total_load, 0.95)

# extremal index estimate
ei <- extremalindex(data_daily$daily_total_load, threshold = as.numeric(u))
ei

# decluster
declust_data <- data.frame(
  obs  = data_daily$daily_total_load,
  time = 1:nrow(data_daily)
)
str(declust_data)

declust <- clust(declust_data,
                 u = as.numeric(u),
                 tim.cond = 11)   # from extremal index

names(declust)

length(declust[[1]])
head(declust[[1]])

cluster_max <- sapply(declust, function(mat) max(mat["obs", ]))
cluster_max
is.numeric(cluster_max)
length(cluster_max)

excesses <- cluster_max - as.numeric(u)
summary(excesses)

df_excess <- data.frame(y = excesses)

gpd_declust <- fevd(y ~ 1,
                    data = df_excess,
                    type = "GP",
                    threshold = 0)
summary(gpd_declust)

### Diagnostic plots
n_clusters <- length(cluster_max)
n_years <- nrow(data_daily) / 365
lambda <- n_clusters / n_years

gpd_RL <- function(T, beta, xi, lambda){
  (beta/xi) * ((lambda*T)^xi - 1)
}

beta <- gpd_declust$results$par["scale"]
xi   <- gpd_declust$results$par["shape"]

declust_RL <- sapply(c(1,5,10,20),
                     gpd_RL,
                     beta=beta, xi=xi, lambda=lambda)

actual_RL <- as.numeric(u) + declust_RL
actual_RL

# extract time indices from clusters
cluster_indices <- sapply(declust, function(mat) max(mat["time", ]))
cluster_dates   <- data_daily$date[cluster_indices]
cluster_values  <- cluster_max

cluster_df <- data.frame(date = cluster_dates,
                         daily_total_load = cluster_values)

ggplot(data_daily, aes(date, daily_total_load)) +
  geom_segment(aes(x = date, xend = date,
                   y = 0, yend = daily_total_load),
               color = "blue", alpha = 0.4) +
  geom_point(data = cluster_df,
             aes(x = date, y = daily_total_load),
             color = "red", size = 2, inherit.aes = FALSE) +
  geom_hline(yintercept = u, linetype = "dashed",
             color = "darkgreen") +
  labs(title = "Daily Total Demand – Exceedances and Cluster Maxima",
       x = "Date", y = "Daily Total Load") +
  theme_minimal()


cluster_max <- sapply(declust, function(mat) max(mat["obs", ]))
excesses <- cluster_max - as.numeric(u)
df_excess <- data.frame(y = excesses)

gpd_declust <- fevd(y ~ 1, data=df_excess, type="GP", threshold=0)
summary(gpd_declust)

# 

u <- quantile(data_daily$daily_total_load, 0.95)

gpd_tot <- fevd(daily_total_load ~ 1,
                data = data_daily,
                type = "GP",
                threshold = u)

summary(gpd_tot)
plot(gpd_tot)             # multipanel

# declustered data plots
declust_df <- data_daily

declust_df$declust_load <- u   # fill every day with threshold
declust_df$declust_load[cluster_indices] <- cluster_max

cluster_indices <- sapply(declust, function(mat) max(mat["time", ]))
cluster_max     <- sapply(declust, function(mat) max(mat["obs", ]))

gpd_declust_fevd <- fevd(declust_load ~ 1,
                         data = declust_df,
                         type = "GP",
                         threshold = u)
plot(gpd_declust_fevd)

# 6. Non-stationary GPD (Temperature in Scale)----
gpd_temp <- fevd(daily_total_load ~ 1,
                 data=data_daily,
                 type="GP",
                 threshold=as.numeric(u),
                 scale = ~ daily_mean_temp,
                 time.units="days")

summary(gpd_temp)
summary(gpd_tot)$AIC
summary(gpd_temp)$AIC

# 7. Non-stationary GEV (Temperature in Location)----
weekly_max_temp <- data_daily %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(
    weekly_max_total = max(daily_total_load),
    weekly_mean_temp = mean(daily_mean_temp)
  )

gev_temp <- fevd(weekly_max_total ~ 1,
                 data=weekly_max_temp,
                 type="GEV",
                 location = ~ weekly_mean_temp)

summary(gev_temp)





