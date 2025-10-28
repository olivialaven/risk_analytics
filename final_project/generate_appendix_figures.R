# ============================================================================
# GENERATE APPENDIX FIGURES FOR ITERATIVE ANALYSIS
# Additional supporting visualizations for comprehensive reporting
# ============================================================================

setwd("c:/Users/JJ/OneDrive - epfl.ch/Documents/SMT/RA/risk_analytics/final_project")

# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

cat("Generating appendix figures...\n\n")

# Load the merged data (recreate from original sources)
aep <- fread("AEP_hourly.csv")
temp <- fread("temperature.csv")

aep$Datetime <- ymd_hms(aep$Datetime)
temp$datetime <- ymd_hms(temp$datetime)

start_date <- ymd_hms("2012-10-01 12:00:00")
end_date <- ymd_hms("2017-11-29 23:00:00")

aep_sub <- aep[Datetime >= start_date & Datetime <= end_date]
temp_sub <- temp[datetime >= start_date & datetime <= end_date]

temp_sub$temp_celsius <- rowMeans(temp_sub[, .(Chicago, Detroit, Indianapolis, Pittsburgh)], na.rm = TRUE) - 273.15

data <- merge(aep_sub, temp_sub[, .(datetime, temp_celsius)], 
              by.x = "Datetime", by.y = "datetime")
setnames(data, c("datetime", "demand_MW", "temp_C"))
data <- na.omit(data)

# Add temporal features
data$hour <- hour(data$datetime)
data$month <- month(data$datetime)
data$year <- year(data$datetime)
data$wday <- wday(data$datetime, label = TRUE)

# ============================================================================
# APPENDIX FIGURE A1: Seasonal Decomposition
# ============================================================================
cat("Creating Figure A1: Seasonal patterns by month...\n")

png("output_figures_iterative/A1_seasonal_boxplots.png", width = 1000, height = 600)
par(mar = c(5, 5, 3, 2))

monthly_data <- split(data$demand_MW, data$month)
boxplot(monthly_data, 
        names = month.abb,
        xlab = "Month", 
        ylab = "Demand (MW)",
        main = "Figure A1: Seasonal Demand Patterns",
        col = "lightblue",
        las = 1)
abline(h = mean(data$demand_MW), col = "red", lty = 2, lwd = 2)
legend("topright", legend = "Mean Demand", col = "red", lty = 2, lwd = 2)

dev.off()

# ============================================================================
# APPENDIX FIGURE A2: Hourly Patterns (Load Profile)
# ============================================================================
cat("Creating Figure A2: Daily load profile...\n")

png("output_figures_iterative/A2_hourly_load_profile.png", width = 1000, height = 600)
par(mar = c(5, 5, 3, 2))

hourly_stats <- data[, .(
  mean_demand = mean(demand_MW),
  sd_demand = sd(demand_MW),
  min_demand = min(demand_MW),
  max_demand = max(demand_MW)
), by = hour]

plot(hourly_stats$hour, hourly_stats$mean_demand, 
     type = "l", lwd = 3, col = "darkblue",
     xlab = "Hour of Day", 
     ylab = "Demand (MW)",
     main = "Figure A2: Average Daily Load Profile",
     ylim = range(c(hourly_stats$min_demand, hourly_stats$max_demand)),
     las = 1)

# Add confidence bands
polygon(c(hourly_stats$hour, rev(hourly_stats$hour)),
        c(hourly_stats$mean_demand + hourly_stats$sd_demand, 
          rev(hourly_stats$mean_demand - hourly_stats$sd_demand)),
        col = rgb(0, 0, 1, 0.2), border = NA)

lines(hourly_stats$hour, hourly_stats$mean_demand, lwd = 3, col = "darkblue")

legend("topleft", 
       legend = c("Mean Demand", "±1 SD"),
       col = c("darkblue", rgb(0, 0, 1, 0.2)),
       lty = c(1, NA),
       lwd = c(3, NA),
       pch = c(NA, 15),
       pt.cex = 2)

dev.off()

# ============================================================================
# APPENDIX FIGURE A3: Weekly Patterns
# ============================================================================
cat("Creating Figure A3: Day-of-week patterns...\n")

png("output_figures_iterative/A3_weekly_patterns.png", width = 1000, height = 600)
par(mar = c(5, 5, 3, 2))

weekly_data <- split(data$demand_MW, data$wday)
boxplot(weekly_data,
        xlab = "Day of Week",
        ylab = "Demand (MW)",
        main = "Figure A3: Weekly Demand Patterns",
        col = c(rep("lightcoral", 5), rep("lightgreen", 2)),  # Weekdays vs weekends
        las = 1)
abline(h = mean(data$demand_MW), col = "red", lty = 2, lwd = 2)
legend("topright", legend = "Mean Demand", col = "red", lty = 2, lwd = 2)

dev.off()

# ============================================================================
# APPENDIX FIGURE A4: Temperature Distribution by Season
# ============================================================================
cat("Creating Figure A4: Seasonal temperature distributions...\n")

png("output_figures_iterative/A4_seasonal_temperature.png", width = 1000, height = 600)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Define seasons
data$season <- ifelse(data$month %in% c(12, 1, 2), "Winter",
                      ifelse(data$month %in% c(3, 4, 5), "Spring",
                             ifelse(data$month %in% c(6, 7, 8), "Summer", "Fall")))

seasons <- c("Winter", "Spring", "Summer", "Fall")
for (s in seasons) {
  season_data <- data[data$season == s, ]
  hist(season_data$temp_C, 
       breaks = 30,
       col = "lightblue",
       xlab = "Temperature (°C)",
       ylab = "Frequency",
       main = paste(s, "Temperature Distribution"),
       las = 1)
  abline(v = mean(season_data$temp_C), col = "red", lwd = 2, lty = 2)
}

dev.off()

# ============================================================================
# APPENDIX FIGURE A5: Extreme Events Calendar
# ============================================================================
cat("Creating Figure A5: Extreme events timeline...\n")

png("output_figures_iterative/A5_extreme_events.png", width = 1200, height = 600)
par(mar = c(5, 5, 3, 2))

# Identify extreme events (> 95th percentile)
threshold <- quantile(data$demand_MW, 0.95)
extreme_events <- data[demand_MW > threshold]

plot(data$datetime, data$demand_MW, 
     type = "l", col = "gray70",
     xlab = "Date", ylab = "Demand (MW)",
     main = "Figure A5: Timeline of Extreme Demand Events (>95th Percentile)",
     las = 1)

points(extreme_events$datetime, extreme_events$demand_MW,
       col = "red", pch = 19, cex = 0.5)

abline(h = threshold, col = "blue", lty = 2, lwd = 2)
abline(h = max(data$demand_MW), col = "darkred", lty = 2, lwd = 2)

legend("topleft",
       legend = c("All Observations", "Extreme Events (>95%)", 
                  "95th Percentile", "Historical Maximum"),
       col = c("gray70", "red", "blue", "darkred"),
       lty = c(1, NA, 2, 2),
       pch = c(NA, 19, NA, NA),
       lwd = c(1, NA, 2, 2),
       cex = 0.8)

dev.off()

# ============================================================================
# APPENDIX FIGURE A6: Correlation Matrix
# ============================================================================
cat("Creating Figure A6: Variable correlations...\n")

png("output_figures_iterative/A6_correlation_matrix.png", width = 800, height = 800)
par(mar = c(2, 2, 3, 2))

# Create correlation matrix
cor_vars <- data[, .(demand_MW, temp_C, hour, month)]
cor_matrix <- cor(cor_vars, use = "complete.obs")

# Plot correlation matrix
library(graphics)
image(1:4, 1:4, cor_matrix,
      col = colorRampPalette(c("blue", "white", "red"))(20),
      xlab = "", ylab = "",
      main = "Figure A6: Correlation Matrix",
      axes = FALSE)

axis(1, at = 1:4, labels = c("Demand", "Temperature", "Hour", "Month"), las = 2)
axis(2, at = 1:4, labels = c("Demand", "Temperature", "Hour", "Month"), las = 1)

# Add correlation values
for (i in 1:4) {
  for (j in 1:4) {
    text(i, j, sprintf("%.2f", cor_matrix[i, j]), cex = 1.2)
  }
}

dev.off()

# ============================================================================
# APPENDIX FIGURE A7: Temperature vs Demand by Season
# ============================================================================
cat("Creating Figure A7: Seasonal temperature-demand relationships...\n")

png("output_figures_iterative/A7_seasonal_temp_demand.png", width = 1200, height = 1000)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (s in seasons) {
  season_data <- data[data$season == s, ]
  
  plot(season_data$temp_C, season_data$demand_MW,
       pch = 16, cex = 0.3, col = rgb(0, 0, 1, 0.3),
       xlab = "Temperature (°C)",
       ylab = "Demand (MW)",
       main = paste(s, "Demand-Temperature Relationship"),
       las = 1)
  
  # Add lowess smoother
  smooth_fit <- lowess(season_data$temp_C, season_data$demand_MW, f = 0.2)
  lines(smooth_fit, col = "red", lwd = 2)
  
  # Add mean lines
  abline(h = mean(season_data$demand_MW), col = "gray", lty = 2)
  abline(v = mean(season_data$temp_C), col = "gray", lty = 2)
}

dev.off()

# ============================================================================
# APPENDIX FIGURE A8: Year-over-Year Comparison
# ============================================================================
cat("Creating Figure A8: Annual comparisons...\n")

png("output_figures_iterative/A8_annual_comparison.png", width = 1200, height = 600)
par(mar = c(5, 5, 3, 2))

annual_stats <- data[, .(
  mean_demand = mean(demand_MW),
  max_demand = max(demand_MW),
  sd_demand = sd(demand_MW),
  n_extreme = sum(demand_MW > threshold)
), by = year]

# Exclude incomplete years
annual_stats <- annual_stats[year >= 2013 & year <= 2017]

barplot(annual_stats$max_demand,
        names.arg = annual_stats$year,
        col = "steelblue",
        xlab = "Year",
        ylab = "Maximum Demand (MW)",
        main = "Figure A8: Annual Maximum Demand (2013-2017)",
        ylim = c(0, max(annual_stats$max_demand) * 1.1),
        las = 1)

# Add mean demand line
points(1:nrow(annual_stats), annual_stats$mean_demand, 
       col = "red", pch = 19, cex = 1.5, type = "b", lwd = 2)

legend("topleft",
       legend = c("Annual Maximum", "Annual Mean"),
       col = c("steelblue", "red"),
       pch = c(15, 19),
       lwd = c(NA, 2),
       cex = 1)

dev.off()

cat("\n✓ All appendix figures generated successfully!\n")
cat("\nAppendix Figures Created:\n")
cat("  A1: Seasonal boxplots (monthly patterns)\n")
cat("  A2: Hourly load profile (daily cycle)\n")
cat("  A3: Weekly patterns (weekday vs weekend)\n")
cat("  A4: Seasonal temperature distributions\n")
cat("  A5: Extreme events timeline\n")
cat("  A6: Correlation matrix\n")
cat("  A7: Seasonal temperature-demand relationships\n")
cat("  A8: Annual maximum comparison\n")
