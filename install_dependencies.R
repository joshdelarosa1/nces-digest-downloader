#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Script: install_dependencies.R
# -----------------------------------------------------------------------------
# Purpose: Install required R packages for downloader execution and supporting
#          development tasks.
# Notes: Intended for first-time environment setup.
# Usage: Rscript install_dependencies.R

cat("\n")
cat("┌─────────────────────────────────────────────────┐\n")
cat("│       NCES DIGEST DOWNLOADER - SETUP            │\n")
cat("└─────────────────────────────────────────────────┘\n\n")

#' Emit a formatted setup message for CLI output.
#'
#' @param text Message content.
#' @param type Message type (`info`, `success`, `warn`, `error`).
#' @return `NULL`.
msg <- function(text, type = "info") {
  prefix <- switch(type,
                  "info" = "ℹ️ ",
                  "success" = "✅ ",
                  "warn" = "⚠️ ",
                  "error" = "❌ ",
                  "")

  cat(paste0(prefix, text, "\n"))
}

required_packages <- c(
  # Core packages
  "dplyr", "purrr", "stringr", "tibble", "rvest", "xml2", "httr",
  "glue", "yaml", "digest",
  
  # File operations
  "fs", "readr",
  
  # Excel handling
  "readxl", "openxlsx",
  
  # Parallel processing
  "furrr", "future", "progressr",
  
  # New required dependency for session-based downloads
  "curl",
  
  # Optional but recommended
  "here", "knitr"
)

# Check which packages are missing
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) == 0) {
  msg("All required packages are already installed!", "success")
  cat("\nYou're all set! You can now run the NCES Digest Downloader.\n")
  cat("To start the download process, run: Rscript main.R\n\n")
  q(status = 0)
}

# Inform the user
msg(paste0("Installing ", length(missing_packages), " required packages"), "info")
cat("\nThe following packages will be installed:\n")
cat(paste0("  ", paste(missing_packages, collapse = ", "), "\n\n"))

# Ask for confirmation
confirm <- readline(prompt = "Would you like to continue? (y/n): ")

if (tolower(substring(confirm, 1, 1)) != "y") {
  msg("Installation canceled.", "info")
  q(status = 0)
}

# Try to install missing packages
install_results <- logical(length(missing_packages))
names(install_results) <- missing_packages

for (i in seq_along(missing_packages)) {
  pkg <- missing_packages[i]
  msg(paste0("Installing package ", i, "/", length(missing_packages), ": ", pkg), "info")

  tryCatch({
    utils::install.packages(pkg, quiet = TRUE)
    if (requireNamespace(pkg, quietly = TRUE)) {
      install_results[i] <- TRUE
      msg(paste0("Successfully installed ", pkg), "success")
    } else {
      install_results[i] <- FALSE
      msg(paste0("Failed to install ", pkg), "error")
    }
  }, error = function(e) {
    install_results[i] <- FALSE
    msg(paste0("Error installing ", pkg, ": ", e$message), "error")
  })
}

# Check installation results
successful_installs <- sum(install_results)
failed_installs <- length(missing_packages) - successful_installs

cat("\n")
msg(paste0("Installation complete: ", successful_installs, " packages installed successfully,",
          " ", failed_installs, " failed."), "info")

if (failed_installs > 0) {
  cat("\nThe following packages could not be installed:\n")
  cat(paste0("  ", paste(names(install_results)[!install_results], collapse = ", "), "\n\n"))

  msg("Please try installing these packages manually:", "warn")
  cat("  1. Open R or RStudio\n")
  cat("  2. Run this command for each package:\n")
  cat("     install.packages(\"package_name\")\n\n")
} else {
  msg("All packages were installed successfully!", "success")
  cat("\nYou're all set! You can now run the NCES Digest Downloader.\n")
  cat("To start the download process, run: Rscript main.R\n\n")
}
