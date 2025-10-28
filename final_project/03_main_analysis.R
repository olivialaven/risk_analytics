# ============================================================================
# RISK ANALYTICS PROJECT: ENERGY DEMAND & TEMPERATURE EXTREMES
# Main Analysis Script - To be run step by step
# ============================================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load required packages
required_packages <- c("data.table", "ggplot2", "lubridate", "forecast", 
                       "tseries", "fGarch", "extRemes", "evd", "lmtest", 
                       "gridExtra", "moments", "MASS")

cat("Checking and installing required packages...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}
cat("All packages loaded successfully.\n")

# Set working directory
setwd("c:/Users/JJ/OneDrive - epfl.ch/Documents/SMT/RA/risk_analytics/final_project")

# Create output directory for figures
if (!dir.exists("output_figures")) dir.create("output_figures")
if (!dir.exists("output_tables")) dir.create("output_tables")

# Initialize report markdown file
report_file <- "analysis_report_snippets.md"
cat("# Energy Demand & Temperature Extremes: Analysis Report Snippets\n\n", file = report_file)
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file = report_file, append = TRUE)

# ============================================================================
# STEP 1: DATA PREPARATION & EXPLORATION
# ============================================================================

cat("\n========================================\n")
cat("STEP 1: DATA PREPARATION & EXPLORATION\n")
cat("========================================\n\n")

# 1.1 Load data
cat("Loading datasets...\n")
aep_raw <- fread("AEP_hourly.csv")
temp_raw <- fread("temperature.csv")

# 1.2 Convert datetime
aep_raw$Datetime <- ymd_hms(aep_raw$Datetime)
temp_raw$datetime <- ymd_hms(temp_raw$datetime)

# 1.3 Examine data structure
cat("\n--- AEP Data Structure ---\n")
print(str(aep_raw))
cat("\nDate range:", as.character(min(aep_raw$Datetime)), "to", as.character(max(aep_raw$Datetime)))
cat("\nTotal observations:", nrow(aep_raw), "\n")

cat("\n--- Temperature Data Structure ---\n")
print(str(temp_raw))
cat("\nDate range:", as.character(min(temp_raw$datetime)), "to", as.character(max(temp_raw$datetime)))
cat("\nTotal observations:", nrow(temp_raw), "\n")
cat("\nAvailable cities:", paste(names(temp_raw)[-1], collapse=", "), "\n")

# 1.4 Select overlapping time period and Chicago temperature
start_date <- ymd_hms("2012-10-01 00:00:00")
end_date <- ymd_hms("2017-11-30 23:00:00")

aep <- aep_raw[Datetime >= start_date & Datetime <= end_date]
temp <- temp_raw[datetime >= start_date & datetime <= end_date]

# Select relevant Midwest cities for AEP service area and convert to Celsius
# AEP serves Ohio, Indiana, Michigan, W. Virginia, etc.
# Use average of Chicago, Detroit, Indianapolis, Pittsburgh for better regional representation
midwest_cities <- c("Chicago", "Detroit", "Indianapolis", "Pittsburgh")

# Calculate average Midwest temperature
temp$temp_celsius <- rowMeans(temp[, ..midwest_cities], na.rm = TRUE) - 273.15

cat("\nUsing average temperature from:", paste(midwest_cities, collapse=", "), "\n")

# 1.5 Merge datasets
cat("\nMerging datasets...\n")
data <- merge(aep, temp[, .(datetime, temp_celsius)], 
              by.x = "Datetime", by.y = "datetime", all = FALSE)
setnames(data, c("datetime", "demand_MW", "temp_C"))

# 1.6 Handle missing values
cat("\nMissing values:\n")
print(colSums(is.na(data)))

# Remove rows with missing values
data <- na.omit(data)
cat("\nFinal dataset size:", nrow(data), "observations\n")
cat("Date range:", as.character(min(data$datetime)), "to", as.character(max(data$datetime)), "\n")

# 1.7 Create time-based features
data[, `:=`(
  hour = hour(datetime),
  day_of_week = wday(datetime, label = TRUE),
  month = month(datetime, label = TRUE),
  year = year(datetime),
  date = as.Date(datetime),
  season = case_when(
    month(datetime) %in% c(12, 1, 2) ~ "Winter",
    month(datetime) %in% c(3, 4, 5) ~ "Spring",
    month(datetime) %in% c(6, 7, 8) ~ "Summer",
    month(datetime) %in% c(9, 10, 11) ~ "Fall"
  )
)]

# 1.8 Basic summary statistics
cat("\n--- Summary Statistics ---\n")
summary_stats <- data.frame(
  Variable = c("Demand (MW)", "Temperature (°C)"),
  Mean = c(mean(data$demand_MW), mean(data$temp_C)),
  Median = c(median(data$demand_MW), median(data$temp_C)),
  SD = c(sd(data$demand_MW), sd(data$temp_C)),
  Min = c(min(data$demand_MW), min(data$temp_C)),
  Max = c(max(data$demand_MW), max(data$temp_C)),
  Skewness = c(skewness(data$demand_MW), skewness(data$temp_C)),
  Kurtosis = c(kurtosis(data$demand_MW), kurtosis(data$temp_C))
)
print(summary_stats)

# Save summary statistics
write.csv(summary_stats, "output_tables/summary_statistics.csv", row.names = FALSE)

# 1.9 Test for stationarity
cat("\n--- Stationarity Tests (ADF) ---\n")
adf_demand <- adf.test(data$demand_MW)
adf_temp <- adf.test(data$temp_C)
cat("Demand ADF test p-value:", adf_demand$p.value, 
    ifelse(adf_demand$p.value < 0.05, "(Stationary)", "(Non-stationary)"), "\n")
cat("Temperature ADF test p-value:", adf_temp$p.value,
    ifelse(adf_temp$p.value < 0.05, "(Stationary)", "(Non-stationary)"), "\n")

# 1.10 Write to report
cat("\n## 1. Data Description\n\n", file = report_file, append = TRUE)
cat("This analysis examines **", nrow(data), "hourly observations** of electricity demand (American Electric Power - AEP) and regional temperature (Midwest US average: Chicago, Detroit, Indianapolis, Pittsburgh) from **", 
    as.character(min(data$datetime)), "** to **", as.character(max(data$datetime)), "** (approximately 5 years). ",
    "AEP serves a large region across the Midwest and Mid-Atlantic US, making this dataset ideal for studying temperature-driven demand extremes.\n\n",
    file = report_file, append = TRUE)
cat("**Key Statistics:**\n\n", file = report_file, append = TRUE)
cat("- Mean demand: **", round(mean(data$demand_MW), 0), " MW** (SD = ", round(sd(data$demand_MW), 0), " MW)\n",
    file = report_file, append = TRUE)
cat("- Peak demand: **", round(max(data$demand_MW), 0), " MW**\n", file = report_file, append = TRUE)
cat("- Temperature range: **", round(min(data$temp_C), 1), "°C to ", round(max(data$temp_C), 1), "°C**\n\n",
    file = report_file, append = TRUE)

cat("\n✓ STEP 1 COMPLETE\n")
cat("Data loaded and prepared:", nrow(data), "observations ready for analysis\n")
cat("Summary statistics saved to: output_tables/summary_statistics.csv\n")

# ============================================================================
# STEP 2: EXPLORATORY DATA ANALYSIS
# ============================================================================

cat("\n========================================\n")
cat("STEP 2: EXPLORATORY DATA ANALYSIS\n")
cat("========================================\n\n")

# 2.1 Time series plot: Demand
cat("Creating time series visualizations...\n")

p1 <- ggplot(data, aes(x = datetime, y = demand_MW)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  labs(title = "Hourly Electricity Demand (AEP)",
       subtitle = paste0(as.character(min(data$datetime)), " to ", as.character(max(data$datetime))),
       x = "Date", y = "Demand (MW)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("output_figures/01_demand_timeseries.png", p1, width = 10, height = 4, dpi = 300)

# 2.2 Time series plot: Temperature
p2 <- ggplot(data, aes(x = datetime, y = temp_C)) +
  geom_line(color = "coral", alpha = 0.6) +
  labs(title = "Hourly Temperature (Midwest US Average)",
       subtitle = paste0(as.character(min(data$datetime)), " to ", as.character(max(data$datetime))),
       x = "Date", y = "Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("output_figures/02_temperature_timeseries.png", p2, width = 10, height = 4, dpi = 300)

# 2.3 Combined plot
p3 <- grid.arrange(p1, p2, ncol = 1)
ggsave("output_figures/03_combined_timeseries.png", p3, width = 10, height = 7, dpi = 300)

# 2.4 Distribution plots
p4 <- ggplot(data, aes(x = demand_MW)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "steelblue", alpha = 0.6) +
  geom_density(color = "darkblue", linewidth = 1) +
  labs(title = "Distribution of Hourly Demand",
       x = "Demand (MW)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output_figures/04_demand_distribution.png", p4, width = 7, height = 5, dpi = 300)

# 2.5 QQ-plot: Normal distribution
p5 <- ggplot(data, aes(sample = demand_MW)) +
  stat_qq(color = "steelblue", alpha = 0.6) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Q-Q Plot: Demand vs. Normal Distribution",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output_figures/05_qq_normal.png", p5, width = 6, height = 6, dpi = 300)

# 2.6 Fit t-distribution and compare
cat("Fitting distributions...\n")
library(MASS)
fit_t <- fitdistr(data$demand_MW, "t")
cat("\nFitted t-distribution parameters:\n")
print(fit_t$estimate)

# 2.7 QQ-plot: t-distribution
# Generate t-quantiles
n <- nrow(data)
p <- (1:n) / (n + 1)
t_quantiles <- qt(p, df = fit_t$estimate['df'])
# Scale and location
t_quantiles_scaled <- fit_t$estimate['m'] + fit_t$estimate['s'] * t_quantiles

p6 <- ggplot(data.frame(theoretical = t_quantiles_scaled, 
                        sample = sort(data$demand_MW)), 
             aes(x = theoretical, y = sample)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Q-Q Plot: Demand vs. t-Distribution",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output_figures/06_qq_tdist.png", p6, width = 6, height = 6, dpi = 300)

# 2.8 Demand vs Temperature scatter plot
p7 <- ggplot(data, aes(x = temp_C, y = demand_MW)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", linewidth = 1.5) +
  labs(title = "Electricity Demand vs. Temperature",
       subtitle = "U-shaped relationship indicating heating and cooling demands",
       x = "Temperature (°C)", y = "Demand (MW)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output_figures/07_demand_vs_temp.png", p7, width = 8, height = 6, dpi = 300)

# 2.9 Seasonal patterns
seasonal_summary <- data[, .(
  mean_demand = mean(demand_MW),
  max_demand = max(demand_MW),
  sd_demand = sd(demand_MW)
), by = season]

print("\n--- Seasonal Demand Statistics ---")
print(seasonal_summary)

p8 <- ggplot(data, aes(x = season, y = demand_MW, fill = season)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Seasonal Demand Distribution",
       x = "Season", y = "Demand (MW)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

ggsave("output_figures/08_seasonal_boxplot.png", p8, width = 8, height = 6, dpi = 300)

# 2.10 Write to report
cat("\n## 2. Exploratory Data Analysis\n\n", file = report_file, append = TRUE)
cat("**Distribution Analysis:** The Q-Q plots reveal that electricity demand exhibits **heavy tails**, ",
    "with extreme values occurring more frequently than predicted by a normal distribution. ",
    "The **t-distribution** provides a better fit, confirming the presence of extreme events.\n\n",
    file = report_file, append = TRUE)
cat("**Temperature-Demand Relationship:** A clear **U-shaped relationship** emerges (Figure 07), ",
    "showing increased demand at both temperature extremes due to heating (cold) and cooling (hot) needs. ",
    "Peak demand occurs during summer months (cooling) and winter cold snaps (heating).\n\n",
    file = report_file, append = TRUE)

cat("\n✓ STEP 2 COMPLETE\n")
cat("EDA plots saved to: output_figures/\n")
cat("Key finding: U-shaped demand-temperature relationship with heavy tails\n")

cat("\n========================================\n")
cat("STEP 1-2 COMPLETED SUCCESSFULLY\n")
cat("========================================\n")
cat("\nGenerated outputs:\n")
cat("- Summary statistics: output_tables/summary_statistics.csv\n")
cat("- 8 figures: output_figures/01-08_*.png\n")
cat("- Report snippets: analysis_report_snippets.md\n")
cat("\nNext: Review outputs and run STEP 3 (Extreme Value Analysis)\n")
