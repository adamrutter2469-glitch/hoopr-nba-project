# ============================================================
# Script: scripts/00_install_packages.R
# Purpose: Install any packages the pipeline needs that aren't
#          already present, and print the versions in use.
#          Run once on a new machine, and again any time a
#          change adds a new dependency.
# ============================================================

required_packages <- c(
  "dplyr", "janitor", "hoopR", "purrr", "tidyr", "readr",
  "jsonlite", "slider", "tibble", "stringr", "arrow",
  "glmnet", "xgboost",  # models/train_rebounds_model.R
  "httr2"               # R/pull_bigballsdata_mapping.R
)

installed <- rownames(installed.packages())
missing   <- setdiff(required_packages, installed)

if (length(missing) == 0) {
  message("All required packages already installed.")
} else {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

message("\nPackage versions in use:")
for (p in required_packages) {
  message("  ", p, " ", as.character(utils::packageVersion(p)))
}
