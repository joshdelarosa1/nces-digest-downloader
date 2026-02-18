# -----------------------------------------------------------------------------
# Module: R/config.R
# Purpose: Central configuration defaults and validation for downloader runtime.
# Notes: This file defines canonical config keys shared by main orchestration
#        and downstream scripts.
# -----------------------------------------------------------------------------

#' Read environment variable with optional required/default behavior.
#'
#' @param name Environment variable key.
#' @param required Whether missing values should throw an error.
#' @param default Optional fallback value for empty/missing values.
#'
#' @return Character scalar value.
get_env <- function(name, required = TRUE, default = NULL) {
  env_value <- Sys.getenv(name, unset = "")
  if (nzchar(env_value)) {
    return(env_value)
  }

  if (!is.null(default)) {
    return(default)
  }

  if (isTRUE(required)) {
    stop("Missing required environment variable: ", name, call. = FALSE)
  }

  ""
}

# Shared constant: base URL for all NCES Digest endpoints.
# Defined here so every sourced script picks it up from one place.
NCES_BASE_URL <- get_env(
  name = "NCES_BASE_URL",
  required = FALSE,
  default = "https://nces.ed.gov/programs/digest/"
)

# Canonical list of user-facing config field names.
# Add a new field here and it automatically flows through load_config(),
# the YAML loader, validate_config(), and create_config_env().
CONFIG_FIELDS <- c(
  "years", "filter_mode", "filter_years", "filter_tables",
  "max_parallel", "output_dir", "log_dir", "resume_previous", "verbose",
  # Network tuning — override via YAML if you need to be more conservative
  "max_retries", "initial_delay", "max_delay", "min_dl_delay"
)

#' Load Downloader Configuration
#'
#' Reads configuration values from defaults, an optional compatibility
#' environment, and an optional YAML file.
#'
#' @param config_path Optional path to a YAML configuration file.
#' @param config_env Optional environment containing compatibility variables.
#'
#' @return A validated list of configuration settings.
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
    verbose = FALSE,                  # Default to minimal logging
    # Network tuning (override via YAML to be more/less aggressive)
    max_retries   = 5L,               # Retry attempts for HTTP requests
    initial_delay = 1,                # Initial backoff delay in seconds
    max_delay     = 30,               # Maximum backoff delay in seconds
    min_dl_delay  = 3                 # Minimum seconds between downloads
  )

  # Check if config_env is provided and override defaults
  if (!is.null(config_env)) {
    for (field in CONFIG_FIELDS) {
      if (exists(field, envir = config_env))
        config[[field]] <- get(field, envir = config_env)
    }
  }

  # Check if config_path is provided and exists
  if (!is.null(config_path) && file.exists(config_path)) {
    tryCatch({
      yaml_config <- yaml::read_yaml(config_path)
      for (field in CONFIG_FIELDS) {
        if (!is.null(yaml_config[[field]]))
          config[[field]] <- yaml_config[[field]]
      }
      message("Loaded configuration from: ", config_path)
    }, error = function(e) {
      warning("Error loading config file: ", e$message,
              ". Using default/environment values.")
    })
  }

  # Validate configuration
  validate_config(config)

  return(config)
}

#' Validate Downloader Configuration
#'
#' Ensures configuration values are usable and applies safe coercions when
#' possible.
#'
#' @param config Configuration list to validate.
#'
#' @return A validated and corrected configuration list.
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
    config$filter_years <- unname(vapply(
      config$filter_years,
      function(single_year) {
        if (!grepl("^d", single_year)) paste0("d", single_year) else single_year
      },
      FUN.VALUE = character(1)
    ))
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

#' Create Compatibility Configuration Environment
#'
#' Creates an environment with configuration values for scripts that expect
#' `config_env` instead of a list object.
#'
#' @param config Configuration list.
#'
#' @return An environment containing configuration variables.
create_config_env <- function(config) {
  env <- new.env()
  for (field in CONFIG_FIELDS) env[[field]] <- config[[field]]
  env
}

# Export the config_env globally when this script is sourced
config_env <- new.env()
# Will be populated later by main.R
