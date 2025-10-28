# 01_data_preview.R
# Initial data exploration for risk analytics project

# Load required libraries
library(data.table)
library(ggplot2)
library(lubridate)

# Set working directory to project folder
setwd("./final_project")

# Preview AEP hourly energy consumption data
cat("\n--- Preview: AEP_hourly.csv ---\n")
aep <- fread("AEP_hourly.csv")
print(head(aep, 10))
cat("\nColumns:", paste(names(aep), collapse=", "), "\n")
cat("Date range:", min(aep$Datetime), "to", max(aep$Datetime), "\n")

# Preview temperature data
cat("\n--- Preview: temperature.csv ---\n")
temp <- fread("temperature.csv")
print(head(temp, 10))
cat("\nColumns:", paste(names(temp), collapse=", "), "\n")

# Summarize available cities/locations in temperature data
if("City" %in% names(temp)) {
  cat("\nAvailable cities:", paste(unique(temp$City), collapse=", "), "\n")
} else if("station" %in% names(temp)) {
  cat("\nAvailable stations:", paste(unique(temp$station), collapse=", "), "\n")
} else {
  cat("\nNo city/station column found in temperature data.\n")
}

# Summarize time range in temperature data
if("Datetime" %in% names(temp)) {
  cat("Date range:", min(temp$Datetime), "to", max(temp$Datetime), "\n")
} else if("date" %in% names(temp)) {
  cat("Date range:", min(temp$date), "to", max(temp$date), "\n")
}

# Plot sample time series (first 1000 points)
cat("\n--- Sample Plots ---\n")
if("Datetime" %in% names(aep)) {
  ggplot(aep[1:1000], aes(x=Datetime, y=AEP_MW)) +
    geom_line() +
    ggtitle("AEP Hourly Demand (First 1000 points)")
}
if("Datetime" %in% names(temp) & ("City" %in% names(temp) | "station" %in% names(temp))) {
  city_col <- if("City" %in% names(temp)) "City" else "station"
  ggplot(temp[1:1000], aes_string(x="Datetime", y="Temperature", color=city_col)) +
    geom_line() +
    ggtitle("Temperature (First 1000 points)")
}

cat("\n--- Data preview complete. Ready for analysis structure planning. ---\n")
