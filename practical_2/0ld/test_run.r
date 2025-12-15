options(repos = c(CRAN = "https://cran.r-project.org"))

# Test POT installation
cat("Testing POT library...\n")
if (!require("POT", quietly = TRUE)) {
  cat("Installing POT...\n")
  install.packages("POT", dependencies = TRUE)
}
library(POT)
cat("POT loaded successfully\n")

# Test ismev installation
cat("Testing ismev library...\n")
if (!require("ismev", quietly = TRUE)) {
  cat("Installing ismev...\n")
  install.packages("ismev", dependencies = TRUE)
}
library(ismev)
cat("ismev loaded successfully\n")

# Test extRemes installation
cat("Testing extRemes library...\n")
if (!require("extRemes", quietly = TRUE)) {
  cat("Installing extRemes...\n")
  install.packages("extRemes", dependencies = TRUE)
}
library(extRemes)
cat("extRemes loaded successfully\n")

cat("All libraries loaded successfully!\n")
