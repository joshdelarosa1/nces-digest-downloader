#!/usr/bin/env Rscript
# =============================================================================
# NCES Digest Data Downloader
# =============================================================================
# Purpose: Downloads Excel files from the NCES Digest of Education Statistics
#          with customizable filtering and parallel processing capabilities.
#
# Version: 1.0.1
# Last Update: 2025-04-01
# Author: Josue De La Rosa
#
#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: find_excel_path.R
# -----------------------------------------------------------------------------
# Purpose: Enhanced extraction of Excel download links with improved parallelism
#          and error handling. This script applies filtering based on user 
#          configuration and extracts download links from table pages.
#
# Version: 1.0.1
# Last Update: 2025-04-01
# Author: Josue De La Rosa
#
# Change Log:
# 2025-04-01: v1.0.1 - Added validation for invalid or missing URLs
#                     - Implemented direct URL construction as fallback
#                     - Enhanced batch processing with better error handling
#                     - Added seed consistency for parallel processing
# 2023-03-15: v1.0.0 - Initial release
#
# Usage: This script is called from main.R and should not be run directly.
#
# Usage (from command line):
#   Rscript main.R [options]
#
# Options:
#   --years YEAR1,YEAR2,...  Years to download (e.g. "22,21")
#   --mode MODE              Filter mode: all, year_only, table_only, custom
#   --tables TABLE1,TABLE2,..Table numbers to filter (e.g. "101.10,204.30")
#   --parallel N             Number of parallel downloads (0=auto)
#   --output DIR             Output directory
#   --resume yes|no          Whether to resume previous downloads
#   --verbose yes|no         Verbose output
#   --config FILE            Load configuration from YAML file
#   --help                   Show this help message
#
# Or from RStudio:
#   Just source this file

# ===== USER CONFIGURATION (EDIT THESE VALUES) =====

# Years to download (e.g., 24 = 2024, 23 = 2023, etc.)
YEARS_TO_DOWNLOAD <- c(24)

# Download mode:
# "all"        - Download all tables for the specified years
# "year_only"  - Only tables from specified years (default)
# "table_only" - Only specified tables (regardless of year)
# "custom"     - Both year and table filters applied
# DOWNLOAD_MODE <- "custom"
DOWNLOAD_MODE <- "year_only"

# When DOWNLOAD_MODE is "year_only" or "custom", specify years with "d" prefix
# e.g., c("d24", "d23") for 2024 and 2023 digests
FILTER_YEARS <- paste0("d", YEARS_TO_DOWNLOAD)

# When DOWNLOAD_MODE is "table_only" or "custom", specify table numbers
# e.g., c("101.10", "204.30") for specific tables

FILTER_TABLES <- c()
# FILTER_TABLES <- c("302.43","311.22","311.32")

# Maximum number of simultaneous downloads (0 = auto-detect based on system)
MAX_PARALLEL_DOWNLOADS <- 0

# Whether to resume incomplete previous downloads
RESUME_PREVIOUS <- TRUE

# Output directory for downloaded files
OUTPUT_DIR <- "output"

# Log directory
LOG_DIR <- "log"

# Verbose output (TRUE = more detailed messages)
VERBOSE <- FALSE

# ===== DO NOT EDIT BELOW THIS LINE =====

# Process command line arguments if provided
parse_command_line_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    return()  # No args, use defaults
  }
  
  if ("--help" %in% args) {
    cat("NCES Digest Data Downloader\n")
    cat("Usage: Rscript main.R [options]\n\n")
    cat("Options:\n")
    cat("  --years YEAR1,YEAR2,...  Years to download (e.g. \"22,21\")\n")
    cat("  --mode MODE              Filter mode: all, year_only, table_only, custom\n")
    cat("  --tables TABLE1,TABLE2,..Table numbers to filter (e.g. \"101.10,204.30\")\n")
    cat("  --parallel N             Number of parallel downloads (0=auto)\n")
    cat("  --output DIR             Output directory\n")
    cat("  --resume yes|no          Whether to resume previous downloads\n")
    cat("  --verbose yes|no         Verbose output\n")
    cat("  --config FILE            Load configuration from YAML file\n")
    cat("  --help                   Show this help message\n")
    quit(status = 0)
  }
  
  # Extract arg values
  extract_arg_value <- function(flag, default = NULL) {
    idx <- which(args == flag)
    if (length(idx) > 0 && idx < length(args)) {
      return(args[idx + 1])
    }
    return(default)
  }
  
  # Process years
  years_arg <- extract_arg_value("--years")
  if (!is.null(years_arg)) {
    YEARS_TO_DOWNLOAD <<- as.numeric(strsplit(years_arg, ",")[[1]])
    FILTER_YEARS <<- paste0("d", YEARS_TO_DOWNLOAD)
  }
  
  # Process mode
  mode_arg <- extract_arg_value("--mode")
  if (!is.null(mode_arg)) {
    DOWNLOAD_MODE <<- mode_arg
  }
  
  # Process tables
  tables_arg <- extract_arg_value("--tables")
  if (!is.null(tables_arg)) {
    FILTER_TABLES <<- strsplit(tables_arg, ",")[[1]]
  }
  
  # Process parallel
  parallel_arg <- extract_arg_value("--parallel")
  if (!is.null(parallel_arg)) {
    MAX_PARALLEL_DOWNLOADS <<- as.numeric(parallel_arg)
  }
  
  # Process output directory
  output_arg <- extract_arg_value("--output")
  if (!is.null(output_arg)) {
    OUTPUT_DIR <<- output_arg
  }
  
  # Process resume flag
  resume_arg <- extract_arg_value("--resume")
  if (!is.null(resume_arg)) {
    RESUME_PREVIOUS <<- tolower(resume_arg) %in% c("yes", "true", "1")
  }
  
  # Process verbose flag
  verbose_arg <- extract_arg_value("--verbose")
  if (!is.null(verbose_arg)) {
    VERBOSE <<- tolower(verbose_arg) %in% c("yes", "true", "1")
  }
}

# Record start time for total execution time tracking
start_time <- Sys.time()

# Process command line args if any
tryCatch({
  parse_command_line_args()
}, error = function(e) {
  cat("Error parsing command line arguments:", e$message, "\n")
  cat("Run with --help for usage information\n")
  quit(status = 1)
})

# Prepare configuration
config <- list(
  years = YEARS_TO_DOWNLOAD,
  filter_mode = DOWNLOAD_MODE,
  filter_years = FILTER_YEARS,
  filter_tables = FILTER_TABLES,
  max_parallel = MAX_PARALLEL_DOWNLOADS,
  resume_previous = RESUME_PREVIOUS,
  output_dir = OUTPUT_DIR,
  log_dir = LOG_DIR,
  verbose = VERBOSE
)

# Function to display a formatted message
msg <- function(text, type = "info") {
  prefix <- switch(type,
                   "info" = "ℹ️ ",
                   "success" = "✅ ",
                   "warn" = "⚠️ ",
                   "error" = "❌ ",
                   "progress" = "🔄 ",
                   "")
  
  cat(paste0(prefix, text, "\n"))
}

# Function to check for required packages and install if needed
check_packages <- function(packages) {
  new_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if (length(new_packages) > 0) {
    msg(paste0("Missing required packages: ", paste(new_packages, collapse=", ")), "warn")
    
    install <- readline(prompt = "Would you like to install them now? (y/n): ")
    if (tolower(install) == "y") {
      msg("Installing required packages...", "info")
      install.packages(new_packages)
      
      # Verify installation
      still_missing <- new_packages[!sapply(new_packages, requireNamespace, quietly = TRUE)]
      if (length(still_missing) > 0) {
        msg(paste0("Failed to install: ", paste(still_missing, collapse=", ")), "error")
        stop("Required packages could not be installed. Please install them manually or run install_dependencies.R")
      } else {
        msg("All packages installed successfully.", "success")
      }
    } else {
      msg("To install all required packages automatically, run: Rscript install_dependencies.R", "info")
      stop("Required packages are missing. Please install them to continue.")
    }
  }
  
  # Load the packages
  lapply(packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  })
  
  if (config$verbose) {
    msg(paste0("Loaded ", length(packages), " required packages."), "success")
  }
}

# Create necessary directories
setup_directories <- function() {
  for (dir in c(config$output_dir, config$log_dir)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      msg(paste0("Created directory: ", dir), "info")
    }
  }
}

# Check if we're running in RStudio or terminal
is_rstudio <- Sys.getenv("RSTUDIO") == "1"

# Define script locations with flexible paths
find_script_path <- function(script_name) {
  # Try different common locations
  possible_paths <- c(
    script_name,
    file.path("code", script_name),
    file.path("R", script_name),
    file.path("scripts", script_name)
  )
  
  for (path in possible_paths) {
    if (file.exists(path)) {
      return(path)
    }
  }
  
  # Use the first path as a fallback
  return(possible_paths[1])
}

# Display a simple text-based progress bar
simple_progress_bar <- function(current, total, width = 50) {
  progress <- current / total
  filled <- round(width * progress)
  empty <- width - filled
  
  bar <- paste0(
    "[", 
    paste(rep("=", filled), collapse = ""),
    paste(rep(" ", empty), collapse = ""),
    "] ",
    sprintf("%d%%", round(progress * 100)),
    " (", current, "/", total, ")"
  )
  
  # Clear line and print new progress
  cat("\r", bar, sep = "")
  
  # If done, add newline
  if (current >= total) {
    cat("\n")
  }
}

# Main execution flow
main <- function() {
  # Display banner
  cat("\n")
  cat("┌─────────────────────────────────────────────────┐\n")
  cat("│          NCES DIGEST DATA DOWNLOADER            │\n")
  cat("└─────────────────────────────────────────────────┘\n\n")
  
  # Check required packages - removing progressr from the list
  required_packages <- c("httr", "rvest", "dplyr", "stringr", "purrr", 
                         "tibble", "glue", "furrr", "fs", "digest", 
                         "future", "yaml")
  
  msg("Checking required packages...", "info")
  check_packages(required_packages)
  
  # Setup directories
  msg("Setting up directories...", "info")
  setup_directories()
  
  # Load utility functions
  utils_path <- find_script_path("utils.R")
  if (!file.exists(utils_path)) {
    stop("Could not find utils.R. Please ensure it exists in the project directory or in the R/ subfolder.")
  }
  
  msg("Loading utility functions...", "info")
  source(utils_path)
  
  # Load config.R to create config_env for compatibility
  config_path <- find_script_path("config.R")
  if (file.exists(config_path)) {
    source(config_path)
    
    # Create config_env for compatibility with older scripts
    config_env <- new.env()
    for (name in names(config)) {
      assign(name, config[[name]], envir = config_env)
    }
    
    # Make config_env available globally
    assign("config_env", config_env, envir = .GlobalEnv)
  }
  
  # Display configuration summary
  msg("Configuration summary:", "info")
  cat(paste0("  Years to process: ", paste(config$years, collapse=", "), "\n"))
  cat(paste0("  Filter mode: ", config$filter_mode, "\n"))
  
  if (config$filter_mode %in% c("year_only", "custom") && length(config$filter_years) > 0) {
    cat(paste0("  Filtering for years: ", paste(config$filter_years, collapse=", "), "\n"))
  }
  
  if (config$filter_mode %in% c("table_only", "custom") && length(config$filter_tables) > 0) {
    cat(paste0("  Filtering for tables: ", paste(config$filter_tables, collapse=", "), "\n"))
  }
  
  # Determine optimal parallel processes
  if (config$max_parallel <= 0) {
    # Auto-detect: use available cores minus 1, minimum 1
    config$max_parallel <- max(1, parallel::detectCores() - 1)
  }
  msg(paste0("Using ", config$max_parallel, " parallel processes for downloads"), "info")
  
  # Set up parallel backend
  future::plan(future::multisession, workers = config$max_parallel, .options = furrr::furrr_options(seed = TRUE))
  
  # Step 1: Scrape the digest menu to find tables
  msg("Step 1/3: Scraping digest menus for available tables...", "progress")
  url_digest_path <- find_script_path("URL_DIGEST.R")
  if (!file.exists(url_digest_path)) {
    stop("Could not find URL_DIGEST.R script.")
  }
  
  source(url_digest_path)  # This creates all_tables
  
  # Step 2: Extract Excel file links
  msg("Step 2/3: Extracting Excel file links from table pages...", "progress")
  find_excel_path <- find_script_path("find_excel_path.R")
  if (!file.exists(find_excel_path)) {
    stop("Could not find find_excel_path.R script.")
  }
  source(find_excel_path)  # This creates excel_links_df
  
  # Step 3: Download Excel files
  msg("Step 3/3: Downloading Excel files...", "progress")
  download_files_path <- find_script_path("download_files.R")
  if (!file.exists(download_files_path)) {
    stop("Could not find download_files.R script.")
  }
  # Minimal change: source download_files.R in the global environment
  source(download_files_path, local = .GlobalEnv)  # This creates download_log and required functions
  
  # PATCHED CODE BELOW - Fixed download counter
  # Calculate and display summary statistics
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  # Print summary
  cat("\n")
  msg(paste0("Download process completed in ", round(as.numeric(duration), 2), " minutes"), "success")
  
  # Extract counts from download_log
  if (exists("download_log")) {
    # Fix: Count only files that were actually downloaded in this session
    success_count <- sum(download_log$processing_status == "success", na.rm = TRUE)
    
    # These counts remain unchanged
    fail_count <- sum(download_log$processing_status == "failed", na.rm = TRUE)
    skip_count <- sum(download_log$processing_status == "skipped", na.rm = TRUE)
    
    msg(paste0("Downloaded files: ", success_count, " successful, ", 
               fail_count, " failed, ", skip_count, " skipped"), "info")
    
    # Show file sizes if available
    if ("file_size" %in% names(download_log)) {
      # Only sum sizes for newly downloaded files
      # Simplified total file size calculation
      total_size_mb <- sum(download_log$file_size[download_log$processing_status == "success"], 
                           na.rm = TRUE) / (1024 * 1024)
      
      msg(paste0("Total data downloaded: ", round(total_size_mb, 2), " MB"), "info")
    }
  }
  
  cat("\nResults are available in the '", config$output_dir, "' directory\n", sep="")
  
  # In RStudio, suggest opening the results folder
  if (is_rstudio) {
    cat("To view downloaded files in RStudio:\n")
    cat("  1. In the Files panel, navigate to the 'output' folder\n")
    cat("  2. Click on any Excel file to open it\n\n")
  }
} # This closing bracket ends the main() function

# Run the main function
tryCatch({
  main()
}, error = function(e) {
  msg(paste0("Error: ", e$message), "error")
  cat("\nStack trace:\n")
  print(sys.calls())
  if (!interactive()) {
    quit(status = 1)
  }
})