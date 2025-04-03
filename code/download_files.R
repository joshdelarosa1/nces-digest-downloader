#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: download_files.R
# Description:
#   Downloads Excel files from NCES Digest URLs with optimized parallel processing
#   and comprehensive error handling. Manages both .xls and .xlsx formats with
#   integrity verification.
# -----------------------------------------------------------------------------

# ---- Setup ----
suppressPackageStartupMessages({
  library(httr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(furrr)
  library(digest)
  library(glue)
  library(tibble)
  library(fs)
})

if (!exists("excel_links_df")) {
  stop("The variable 'excel_links_df' does not exist. Run find_excel_path.R first.")
}

base_output_dir <- file.path(config$output_dir)
ensure_dir(base_output_dir)

base_log_dir <- file.path(config$log_dir)
ensure_dir(base_log_dir)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file_path <- file.path(base_log_dir, glue("download_log_{timestamp}.csv"))
hash_registry_path <- file.path(base_log_dir, "hash_registry.csv")

total_files <- nrow(excel_links_df)
success_count <- 0
fail_count <- 0
skip_count <- 0

throttle_config <- list(
  min_delay = 3,
  current_delay = 3,
  max_delay = 30,
  last_request_time = Sys.time() - as.difftime(100, units = "secs"),
  consecutive_failures = 0,
  failure_threshold = 3,
  backoff_factor = 1.5
)

apply_throttling <- function() {
  time_since_last <- as.numeric(difftime(Sys.time(), throttle_config$last_request_time, units = "secs"))
  if (time_since_last < throttle_config$current_delay) {
    Sys.sleep(throttle_config$current_delay - time_since_last)
  }
  throttle_config$last_request_time <- Sys.time()
}

adjust_throttling <- function(success) {
  if (success) {
    throttle_config$consecutive_failures <- 0
    throttle_config$current_delay <- max(throttle_config$min_delay, throttle_config$current_delay / sqrt(throttle_config$backoff_factor))
  } else {
    throttle_config$consecutive_failures <- throttle_config$consecutive_failures + 1
    if (throttle_config$consecutive_failures >= throttle_config$failure_threshold) {
      throttle_config$current_delay <- min(throttle_config$current_delay * throttle_config$backoff_factor, throttle_config$max_delay)
    }
  }
}

# Complete implementation of download_excel_file function
download_excel_file <- function(url, dest_path, min_size = 100, max_retries = 5, overwrite = FALSE, verify_extension = TRUE) {
  # Check if file already exists and handle according to overwrite parameter
  if (file.exists(dest_path) && !overwrite) {
    hash <- calculate_file_hash(dest_path)
    
    # Register the hash if not already in registry
    registry_result <- check_hash_registry(dest_path, hash)
    if (!is.list(registry_result) || !registry_result$exists) {
      register_file_hash(dest_path, hash)
    }
    
    return(list(
      success = TRUE,
      file_path = dest_path,
      hash = hash,
      status = "downloaded"
    ))
  }
  
  # Ensure destination directory exists
  ensure_dir(dirname(dest_path))
  
  # Verify URL has expected format for Excel file
  if (verify_extension && !grepl("\\.(xls|xlsx)($|\\?)", url, ignore.case = TRUE)) {
    warning(glue::glue("URL does not appear to be an Excel file: {url}"))
  }
  
  # Set up temporary file for download
  temp_file <- tempfile(fileext = ".tmp")
  on.exit(if (file.exists(temp_file)) file.remove(temp_file), add = TRUE)
  
  # Track the current retry delay for exponential backoff
  current_delay <- 1
  
  # Attempt download with retries and exponential backoff
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      if (attempt > 1) {
        message(glue::glue("Retry attempt {attempt}/{max_retries} for {basename(url)}"))
        # Add jitter to the delay to prevent thundering herd
        jitter <- runif(1, min = -0.1, max = 0.1) * current_delay
        actual_delay <- max(0.5, current_delay + jitter)
        message(glue::glue("Waiting {round(actual_delay, 1)} seconds before retry..."))
        Sys.sleep(actual_delay)
        # Increase delay for next attempt (exponential backoff)
        current_delay <- min(current_delay * 2, 30)  # Cap at 30 seconds
      }
      
      # Enhanced browser-like headers
      headers <- c(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        "Accept" = "application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*",
        "Accept-Language" = "en-US,en;q=0.5",
        "Connection" = "keep-alive"
      )
      
      # Use httr for more control over the download process
      response <- httr::GET(
        url,
        httr::write_disk(temp_file, overwrite = TRUE),
        httr::timeout(60),
        httr::add_headers(.headers = headers)
      )
      
      # Check if request was successful
      status_code <- httr::status_code(response)
      if (status_code != 200) {
        # Special handling for 404 Not Found - no point retrying these
        if (status_code == 404) {
          message(glue::glue("File not found (404): {basename(url)}"))
          return(list(
            success = FALSE,
            file_path = NA_character_,
            hash = NA_character_,
            error = "File not found (404 Not Found)",
            status = "failed"
          ))
        } else {
          stop(paste("HTTP error:", status_code))
        }
      }
      
      # Verify download
      if (!file.exists(temp_file)) {
        stop("Download failed: File not created")
      }
      
      # Check file size
      file_size <- file.info(temp_file)$size
      if (file_size < min_size) {
        stop(glue::glue("Downloaded file too small: {file_size} bytes"))
      }
      
      # Move file to destination
      file.copy(temp_file, dest_path, overwrite = TRUE)
      
      # Calculate hash
      hash <- calculate_file_hash(dest_path)
      
      # Register the hash
      register_file_hash(dest_path, hash)
      
      return(list(
        success = TRUE,
        file_path = dest_path,
        hash = hash,
        status = "downloaded"
      ))
    }, error = function(e) {
      if (attempt == max_retries) {
        return(list(
          success = FALSE,
          file_path = NA_character_,
          hash = NA_character_,
          error = e$message,
          status = "failed"
        ))
      }
      
      # Log error but continue to next retry
      message(glue::glue("Attempt {attempt}/{max_retries} failed: {e$message}"))
    })
  }
  
  # This should never be reached due to the return() in the error handler
  return(list(
    success = FALSE,
    file_path = NA_character_,
    hash = NA_character_,
    error = "Unexpected error in download_excel_file()",
    status = "failed"
  ))
}

extract_xml_properties <- function(file_path) {
  ext <- tolower(fs::path_ext(file_path))
  
  # Return NA for all fields if not xlsx
  if (ext != "xlsx") {
    return(list(
      creator = NA_character_,
      last_modified_by = NA_character_,
      created = NA_character_,
      modified = NA_character_,
      title = NA_character_
    ))
  }
  
  # Use a temporary directory for extracting XML
  temp_dir <- fs::file_temp()
  fs::dir_create(temp_dir)
  on.exit(fs::dir_delete(temp_dir), add = TRUE)
  
  # Try to unzip the core XML file containing metadata
  tryCatch({
    # Attempt to extract the core.xml file
    utils::unzip(file_path, files = "docProps/core.xml", exdir = temp_dir)
    
    # Load XML document
    core_xml_path <- file.path(temp_dir, "docProps", "core.xml")
    if (!fs::file_exists(core_xml_path)) {
      return(list(
        creator = NA_character_,
        last_modified_by = NA_character_,
        created = NA_character_,
        modified = NA_character_,
        title = NA_character_
      ))
    }
    
    # Read XML content
    doc <- xml2::read_xml(core_xml_path)
    
    # Define XML namespaces for proper extraction
    ns <- c(
      cp = "http://schemas.openxmlformats.org/package/2006/metadata/core-properties",
      dc = "http://purl.org/dc/elements/1.1/",
      dcterms = "http://purl.org/dc/terms/",
      dcmitype = "http://purl.org/dc/dcmitype/",
      xsi = "http://www.w3.org/2001/XMLSchema-instance"
    )
    
    # Extract metadata fields
    creator <- xml2::xml_text(xml2::xml_find_first(doc, ".//dc:creator", ns = ns))
    last_modified_by <- xml2::xml_text(xml2::xml_find_first(doc, ".//cp:lastModifiedBy", ns = ns))
    created <- xml2::xml_text(xml2::xml_find_first(doc, ".//dcterms:created", ns = ns))
    modified <- xml2::xml_text(xml2::xml_find_first(doc, ".//dcterms:modified", ns = ns))
    title <- xml2::xml_text(xml2::xml_find_first(doc, ".//dc:title", ns = ns))
    
    # Return list with extracted properties
    return(list(
      creator = if (length(creator) > 0 && nzchar(creator)) creator else NA_character_,
      last_modified_by = if (length(last_modified_by) > 0 && nzchar(last_modified_by)) 
        last_modified_by else NA_character_,
      created = if (length(created) > 0 && nzchar(created)) created else NA_character_,
      modified = if (length(modified) > 0 && nzchar(modified)) modified else NA_character_,
      title = if (length(title) > 0 && nzchar(title)) title else NA_character_
    ))
  }, error = function(e) {
    if (config$verbose) {
      message(glue::glue("Error extracting XML properties from {file_path}: {e$message}"))
    }
    return(list(
      creator = NA_character_,
      last_modified_by = NA_character_,
      created = NA_character_,
      modified = NA_character_,
      title = NA_character_
    ))
  })
}

# Simplified version of the function
download_excel_file_with_reporting <- function(row_index, row, overwrite = FALSE, min_file_size = 100) {
  # Extract basic information
  year <- row$year
  table_number <- row$table_number
  excel_url <- row$excel_url
  table_title <- row$table_title
  
  # Set up paths
  clean_table_number <- stringr::str_replace_all(table_number, "\\.", "_")
  subchapter <- stringr::str_extract(table_number, "^[0-9]{3}")
  chapter_folder <- substr(subchapter, 1, 1)
  ext <- tolower(fs::path_ext(excel_url))
  if (!(ext %in% c("xls", "xlsx"))) ext <- "xlsx"
  file_name <- glue::glue("{year}_tabn{clean_table_number}.{ext}")
  sub_dir <- file.path(base_output_dir, year, paste0("chapter_", chapter_folder), paste0("subchapter_", subchapter))
  file_path <- file.path(sub_dir, file_name)
  
  # Initialize log entry
  log_entry <- list(
    year = year,
    table_number = table_number,
    excel_url = excel_url,
    file_path = file_path,
    table_title = table_title,
    download_source_url = excel_url,
    download_timestamp = Sys.time(),
    processing_status = "pending",
    error_message = NA_character_,
    row_index = row_index,
    attempt_count = 1,
    download_duration = 0
  )
  
  # Handle missing URL
  if (is.na(excel_url) || excel_url == "") {
    log_entry$processing_status <- "failed"
    log_entry$error_message <- "Missing or empty URL"
    return(as_tibble(log_entry))
  }
  
  # Ensure directory exists and apply throttling
  ensure_dir(sub_dir)
  apply_throttling()
  
  # Perform download
  download_start <- Sys.time()
  result <- download_excel_file(excel_url, file_path, min_size = min_file_size, overwrite = overwrite)
  download_end <- Sys.time()
  download_duration <- as.numeric(difftime(download_end, download_start, units = "secs"))
  
  # Update log entry based on result
  if (result$success) {
    log_entry$checksum <- result$hash
    log_entry$download_duration <- download_duration
    
    if (result$status == "downloaded") {
      log_entry$processing_status <- "success"
      
      # Try to extract metadata
      tryCatch({
        # Basic file info
        file_info <- fs::file_info(file_path)
        log_entry$file_size <- file_info$size
        log_entry$creation_date <- if (!is.na(file_info$birth_time)) file_info$birth_time else file_info$modification_time
        log_entry$modification_date <- file_info$modification_time
        
        # Excel metadata
        xml_props <- extract_xml_properties(file_path)
        log_entry$doc_created <- xml_props$created
        log_entry$doc_modified <- xml_props$modified
        log_entry$author <- xml_props$creator
        log_entry$last_modified_by <- xml_props$last_modified_by
        
        # Try to get sheet count
        log_entry$number_of_sheets <- tryCatch({
          if (tolower(fs::path_ext(file_path)) == "xlsx") {
            length(readxl::excel_sheets(file_path))
          } else {
            NA_integer_
          }
        }, error = function(e) { NA_integer_ })
        
      }, error = function(e) {
        # If metadata extraction fails, still keep success status
        log_entry$error_message <- paste("Metadata extraction failed:", e$message)
      })
      
    } else {
      log_entry$processing_status <- "skipped"
      log_entry$error_message <- "File already exists"
    }
  } else {
    log_entry$processing_status <- "failed"
    log_entry$error_message <- result$error
    log_entry$download_duration <- download_duration
    adjust_throttling(FALSE)
  }
  
  # Print debugging info
  message(paste("Final status for", basename(file_path), ":", log_entry$processing_status))
  
  return(as_tibble(log_entry))
}

# --- Execution Loop ---
download_logs <- vector("list", total_files)
for (i in seq_len(total_files)) {
  row <- excel_links_df[i, ]
  result <- download_excel_file_with_reporting(i, row, overwrite = FALSE)
  download_logs[[i]] <- result
  
  # Show appropriate status in the message
  status <- result$processing_status
  message(glue("Downloaded {i}/{total_files}: {row$year} table {row$table_number} => {status}"))
}

log_df <- bind_rows(download_logs)

# Debug: Show all the processing statuses
message("Status counts in log:")
print(table(log_df$processing_status))

success_count <- sum(log_df$processing_status %in% c("success", "partial_success"), na.rm = TRUE)
fail_count <- sum(log_df$processing_status == "failed", na.rm = TRUE)
skip_count <- sum(log_df$processing_status == "skipped", na.rm = TRUE)

readr::write_csv(log_df, log_file_path)
message(glue("✅ Download log saved to: {log_file_path}"))
message(glue("Download Summary: {success_count} success, {fail_count} failed, {skip_count} skipped."))
download_log <- log_df
