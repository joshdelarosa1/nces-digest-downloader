# -----------------------------------------------------------------------------
# File: config.R
# Description: Simplified configuration system for the NCES Digest downloader.
#              Provides a clean interface for loading and validating user settings.
#
# Version: 1.0.0
# -----------------------------------------------------------------------------

#' Load Configuration
#'
#' Reads configuration either from environment variables, a YAML file,
#' or from the provided defaults.
#'
#' @param config_path Path to configuration YAML file (optional).
#' @param config_env Environment containing configuration variables (optional).
#'
#' @return A list with validated configuration settings.
load_config <- function(config_path = NULL, config_env = NULL) {
  # Start with default configuration
  config <- list(
    years = c(22),                    # Default to latest digest (2022)
    filter_mode = "year_only",        # Default filter mode
    filter_years = c("d22"),          # Default filter years
    filter_tables = character(0),     # No default table filters
    max_parallel = 2,                 # Conservative default parallelism
    output_dir = "output",            # Default output directory
    log_dir = "log",                  # Default log directory
    resume_previous = TRUE,           # Default to resuming
    verbose = FALSE                   # Default to minimal logging
  )

  # Check if config_env is provided and override defaults
  if (!is.null(config_env)) {
    # Transfer variables from config_env to config list
    if (exists("years", envir = config_env))
      config$years <- get("years", envir = config_env)

    if (exists("filter_mode", envir = config_env))
      config$filter_mode <- get("filter_mode", envir = config_env)

    if (exists("filter_years", envir = config_env))
      config$filter_years <- get("filter_years", envir = config_env)

    if (exists("filter_tables", envir = config_env))
      config$filter_tables <- get("filter_tables", envir = config_env)

    if (exists("max_parallel", envir = config_env))
      config$max_parallel <- get("max_parallel", envir = config_env)

    if (exists("output_dir", envir = config_env))
      config$output_dir <- get("output_dir", envir = config_env)

    if (exists("log_dir", envir = config_env))
      config$log_dir <- get("log_dir", envir = config_env)

    if (exists("resume_previous", envir = config_env))
      config$resume_previous <- get("resume_previous", envir = config_env)

    if (exists("verbose", envir = config_env))
      config$verbose <- get("verbose", envir = config_env)
  }

  # Check if config_path is provided and exists
  if (!is.null(config_path) && file.exists(config_path)) {
    tryCatch({
      # Load YAML configuration
      yaml_config <- yaml::read_yaml(config_path)

      # Override defaults with YAML values
      if (!is.null(yaml_config$years))
        config$years <- yaml_config$years

      if (!is.null(yaml_config$filter_mode))
        config$filter_mode <- yaml_config$filter_mode

      if (!is.null(yaml_config$filter_years))
        config$filter_years <- yaml_config$filter_years

      if (!is.null(yaml_config$filter_tables))
        config$filter_tables <- yaml_config$filter_tables

      if (!is.null(yaml_config$max_parallel))
        config$max_parallel <- yaml_config$max_parallel

      if (!is.null(yaml_config$output_dir))
        config$output_dir <- yaml_config$output_dir

      if (!is.null(yaml_config$log_dir))
        config$log_dir <- yaml_config$log_dir

      if (!is.null(yaml_config$resume_previous))
        config$resume_previous <- yaml_config$resume_previous

      if (!is.null(yaml_config$verbose))
        config$verbose <- yaml_config$verbose

      message("✅ Loaded configuration from: ", config_path)
    }, error = function(e) {
      warning("⚠️ Error loading config file: ", e$message,
              ". Using default/environment values.")
    })
  }

  # Validate configuration
  validate_config(config)

  return(config)
}

#' Validate Configuration
#'
#' Checks configuration values and ensures they are valid,
#' applying corrections if needed.
#'
#' @param config Configuration list to validate.
#'
#' @return Validated and corrected configuration list.
validate_config <- function(config) {
  # Validate years (must be numeric)
  if (!is.numeric(config$years)) {
    tryCatch({
      config$years <- as.numeric(config$years)
    }, error = function(e) {
      warning("⚠️ Invalid years in config. Using default (22).")
      config$years <- 22
    })
  }

  # Ensure filter_mode is valid
  valid_modes <- c("all", "year_only", "table_only", "custom")
  if (!config$filter_mode %in% valid_modes) {
    warning(paste0(
      "⚠️ Invalid filter_mode: '", config$filter_mode,
      "'. Using 'all' instead."
    ))
    config$filter_mode <- "all"
  }

  # Ensure filter_years has 'd' prefix
  if (length(config$filter_years) > 0) {
    config$filter_years <- sapply(config$filter_years, function(y) {
      if (!grepl("^d", y)) paste0("d", y) else y
    })
  } else if (config$filter_mode %in% c("year_only", "custom")) {
    # Auto-generate filter_years from years if needed
    config$filter_years <- paste0("d", config$years)
  }

  # Ensure max_parallel is a positive integer
  if (!is.numeric(config$max_parallel) || config$max_parallel < 0) {
    warning("⚠️ Invalid max_parallel value. Using auto-detection.")
    config$max_parallel <- 0  # 0 means auto-detect
  }

  # Ensure output_dir and log_dir are valid paths
  if (!is.character(config$output_dir) || !nzchar(config$output_dir)) {
    warning("⚠️ Invalid output_dir. Using 'output'.")
    config$output_dir <- "output"
  }

  if (!is.character(config$log_dir) || !nzchar(config$log_dir)) {
    warning("⚠️ Invalid log_dir. Using 'log'.")
    config$log_dir <- "log"
  }

  # Convert resume_previous and verbose to logical
  if (!is.logical(config$resume_previous)) {
    config$resume_previous <- as.logical(config$resume_previous)
  }

  if (!is.logical(config$verbose)) {
    config$verbose <- as.logical(config$verbose)
  }

  return(config)
}

#' Create Configuration Environment
#'
#' Creates an environment with configuration settings for use in scripts
#' that expect a config_env environment.
#'
#' @param config Configuration list.
#'
#' @return An environment containing configuration variables.
create_config_env <- function(config) {
  # Create a new environment
  env <- new.env()

  # Add configuration values to environment
  env$years <- config$years
  env$filter_mode <- config$filter_mode
  env$filter_years <- config$filter_years
  env$filter_tables <- config$filter_tables
  env$max_parallel <- config$max_parallel
  env$output_dir <- config$output_dir
  env$log_dir <- config$log_dir
  env$resume_previous <- config$resume_previous
  env$verbose <- config$verbose

  return(env)
}

# Export the config_env globally when this script is sourced
config_env <- new.env()
# Will be populated later by main.R
