# Test GARCH modeling section for Part 3(d)
library(fGarch)
library(readr)
library(dplyr)

# Load data
csv_path <- file.path('..', 'River_and_precip_Neuchatel.csv')
df <- read_csv(csv_path, col_types = cols(
  Date = col_date(format = "%Y-%m-%d"),
  RiverDischarge = col_double(),
  Precipitation = col_double()
)) %>%
  rename(date = Date, discharge = RiverDischarge, precip = Precipitation) %>%
  filter(!is.na(discharge))

discharge_diff <- diff(df$discharge)

cat("Part 3(d): GARCH modeling\n")
cat("="  %>% rep(60) %>% paste(collapse = ""), "\n\n")

tryCatch({
  # Fit GARCH(1,1) with Normal distribution
  cat("Fitting GARCH(1,1) with Normal distribution...\n")
  garch_normal <- garchFit(~ garch(1,1), data = discharge_diff, cond.dist = "norm", trace = FALSE)
  
  cat("\n--- GARCH(1,1) with Normal distribution ---\n")
  print(summary(garch_normal))
  
  # Extract residuals
  resid_normal <- residuals(garch_normal, standardize = TRUE)
  cat("\nStandardized residuals - Normal GARCH:\n")
  cat("Mean:", mean(resid_normal), "\n")
  cat("SD:", sd(resid_normal), "\n")
  cat("Skewness:", moments::skewness(resid_normal), "\n")
  cat("Kurtosis:", moments::kurtosis(resid_normal), "\n")
  
  # Fit GARCH(1,1) with Student-t distribution
  cat("\n\nFitting GARCH(1,1) with Student-t distribution...\n")
  garch_t <- garchFit(~ garch(1,1), data = discharge_diff, cond.dist = "std", trace = FALSE)
  
  cat("\n--- GARCH(1,1) with Student-t distribution ---\n")
  print(summary(garch_t))
  
  # Extract residuals
  resid_t <- residuals(garch_t, standardize = TRUE)
  cat("\nStandardized residuals - Student-t GARCH:\n")
  cat("Mean:", mean(resid_t), "\n")
  cat("SD:", sd(resid_t), "\n")
  cat("Skewness:", moments::skewness(resid_t), "\n")
  cat("Kurtosis:", moments::kurtosis(resid_t), "\n")
  
  # Compare AIC
  cat("\n\n=== MODEL COMPARISON ===\n")
  cat("AIC (Normal):   ", garch_normal@fit$ics[1], "\n")
  cat("AIC (Student-t):", garch_t@fit$ics[1], "\n")
  cat("Difference:     ", garch_normal@fit$ics[1] - garch_t@fit$ics[1], "\n")
  cat("\nBest model:", ifelse(garch_t@fit$ics[1] < garch_normal@fit$ics[1], "Student-t", "Normal"), "\n")
  
  # Degrees of freedom for Student-t
  if ("shape" %in% names(coef(garch_t))) {
    cat("\nStudent-t degrees of freedom:", round(coef(garch_t)["shape"], 2), "\n")
  }
  
}, error = function(e) {
  cat("Error in GARCH modeling:", e$message, "\n")
  print(traceback())
})

cat("\nScript completed.\n")
