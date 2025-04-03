#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: utils.R
# -----------------------------------------------------------------------------
# Purpose: Enhanced utility functions for the NCES Digest download pipeline.
#          Provides robust methods for downloading, hashing, processing,
#          and error handling.
#
# Version: 1.1.0
# Last Update: 2025-04-01
# Author: Josh DeLaRosa
#
# Change Log:
# 2025-04-01: v1.0.1 - Enhanced safe_read_html with improved headers and encoding
#                       to properly handle older HTML formats (pre-2019 NCES documents)
#                     - Added validation to prevent errors with NA values in URLs
#                     - Implemented uncompressed content request headers for better
#                       compatibility with NCES servers
# 2023-03-15: v1.0.0 - Initial release
#
# Usage: This script is imported by main.R and provides utility functions
#        for the entire application.


#' Read HTML with Exponential Backoff Retries
#'
#' Downloads and parses HTML content from a URL with exponential backoff retry
#' mechanism for handling intermittent network issues. Includes improved headers
#' and encoding handling.
#'
#' @param url A character string containing the URL to fetch.
#' @param max_retries Integer specifying the maximum number of retry attempts (default: 5).
#' @param initial_delay Initial delay in seconds between retries (default: 1).
#' @param max_delay Maximum delay in seconds between retries (default: 30).
#' @param user_agent Custom user agent string for the request (default: "R-digest-downloader").
#'
#' @return An `xml_document` object if successful, or NULL if all attempts fail.
#'
#' @examples
#' # Basic usage
#' html <- safe_read_html("https://nces.ed.gov/programs/digest/d24/tables/dt24_101.10.asp")
#' 
#' # With custom parameters
#' html <- safe_read_html(
#'   "https://nces.ed.gov/programs/digest/d24/tables/dt24_101.10.asp",
#'   max_retries = 3,
#'   initial_delay = 2,
#'   user_agent = "NCES Data Collector"
#' )
#'
#' @export

safe_read_html <- function(url, max_retries = 5, initial_delay = 1, max_delay = 30,
                           user_agent = "R-digest-downloader") {
  # Validate URL to prevent errors with NA values
  if (is.na(url) || !is.character(url) || nchar(url) == 0) {
    message("Invalid URL provided: NA or empty string")
    return(NULL)
  }
  
  delay <- initial_delay
  
  # Updated headers: force uncompressed content and proper Accept header
  headers <- c(
    "User-Agent" = user_agent,
    "Accept" = "text/html,application/xhtml+xml,application/xml",
    "Accept-Encoding" = "identity"
  )
  
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      response <- httr::GET(
        url,
        httr::add_headers(.headers = headers),
        httr::timeout(30)
      )
      
      # Check if request was successful
      if (httr::status_code(response) == 200) {
        # Use ISO-8859-1 encoding per the page's meta tag
        html_content <- httr::content(response, as = "text", encoding = "ISO-8859-1")
        # Additional validation to prevent NA content causing errors
        if (is.na(html_content) || length(html_content) == 0 || nchar(html_content) == 0) {
          stop("Retrieved empty or NA content from server")
        }
        return(xml2::read_html(html_content))
      } else {
        stop(paste("HTTP error:", httr::status_code(response)))
      }
    }, error = function(e) {
      if (attempt == max_retries) {
        message(glue::glue("Failed to read HTML from {url} after {max_retries} attempts: {e$message}"))
        return(NULL)
      }
      
      # Calculate backoff delay with jitter to prevent thundering herd
      jitter <- runif(1, min = 0, max = 0.5 * delay)
      actual_delay <- min(delay + jitter, max_delay)
      
      message(glue::glue("Attempt {attempt}/{max_retries} for URL {url} failed: {e$message}"))
      message(glue::glue("Retrying in {round(actual_delay, 1)} seconds..."))
      
      Sys.sleep(actual_delay)
      delay <- min(delay * 2, max_delay)  # Exponential backoff
    })
  }
  
  return(NULL)  # If all attempts failed
}

#' Ensure Directory Exists
#'
#' Creates a directory including parent directories if it doesn't exist.
#' Returns the path invisibly for chaining operations.
#'
#' @param path A character string representing the directory path.
#' @param verbose Logical indicating whether to print messages (default: FALSE).
#'
#' @return The specified path, returned invisibly for chaining.
#'
#' @export
ensure_dir <- function(path, verbose = FALSE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (verbose) message(glue::glue("Created directory: {path}"))
  }
  invisible(path)
}

#' Calculate File Hash
#'
#' Calculates an MD5 hash of a file to detect changes between downloads.
#'
#' @param file_path Path to the file to hash.
#' @param algo Hash algorithm to use (default: "md5").
#'
#' @return A character string with the hash value or NA on failure.
#'
#' @export
calculate_file_hash <- function(file_path, algo = "md5") {
  if (!file.exists(file_path)) {
    warning(glue::glue("File not found: {file_path}"))
    return(NA_character_)
  }
  
  tryCatch({
    digest::digest(file = file_path, algo = algo)
  }, error = function(e) {
    warning(glue::glue("Failed to calculate hash for {file_path}: {e$message}"))
    return(NA_character_)
  })
}

#' Add Entry to Hash Registry
#'
#' Records file hash information in a cumulative CSV registry.
#'
#' @param file_path Path to the file being registered.
#' @param hash Hash value of the file.
#' @param download_date Date of download (defaults to current date).
#' @param registry_path Path to the registry CSV file.
#'
#' @return TRUE if successful, FALSE otherwise.
#'
#' @export
register_file_hash <- function(file_path, hash, download_date = Sys.Date(),
                               registry_path = file.path(config$log_dir, "hash_registry.csv")) {
  if (is.na(hash)) return(FALSE)
  
  # Create a new entry
  new_entry <- data.frame(
    file_path = file_path,
    hash = hash,
    download_date = as.character(download_date),
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # If registry exists, append; otherwise create new
  if (file.exists(registry_path)) {
    # Read existing registry
    registry <- tryCatch({
      utils::read.csv(registry_path, stringsAsFactors = FALSE)
    }, error = function(e) {
      # If reading fails, create a new registry
      new_entry
    })
    
    # Append new entry
    registry <- rbind(registry, new_entry)
  } else {
    registry <- new_entry
  }
  
  # Write back to file
  tryCatch({
    utils::write.csv(registry, registry_path, row.names = FALSE)
    return(TRUE)
  }, error = function(e) {
    warning(glue::glue("Failed to update hash registry: {e$message}"))
    return(FALSE)
  })
}

#' Check if File Exists in Hash Registry
#'
#' Checks if a file path exists in the hash registry and optionally compares hash.
#'
#' @param file_path Path to the file to check.
#' @param hash Optional hash value to compare against registered hash.
#' @param registry_path Path to the registry CSV file.
#'
#' @return A list with status and match information or FALSE if file not found.
#'
#' @export
check_hash_registry <- function(file_path, hash = NULL,
                                registry_path = file.path(config$log_dir, "hash_registry.csv")) {
  if (!file.exists(registry_path)) return(FALSE)
  
  # Read registry
  registry <- tryCatch({
    utils::read.csv(registry_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning(glue::glue("Failed to read hash registry: {e$message}"))
    return(FALSE)
  })
  
  # Find matching entries for this file path
  matches <- registry[registry$file_path == file_path, ]
  
  if (nrow(matches) == 0) {
    return(FALSE)
  }
  
  # Get most recent entry
  most_recent <- matches[which.max(as.POSIXct(matches$timestamp)), ]
  
  # If hash is provided, check for match
  if (!is.null(hash)) {
    hash_match <- most_recent$hash == hash
    return(list(
      exists = TRUE,
      hash_match = hash_match,
      last_download = most_recent$download_date,
      registered_hash = most_recent$hash
    ))
  } else {
    return(list(
      exists = TRUE,
      last_download = most_recent$download_date,
      registered_hash = most_recent$hash
    ))
  }
}
