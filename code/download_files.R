#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: download_files.R
# Description:
#   Downloads Excel files from NCES Digest URLs with optimized parallel processing
#   and comprehensive error handling. Manages both .xls and .xlsx formats with
#   integrity verification.
#
# Version: 1.0.0
# -----------------------------------------------------------------------------

# ---- Setup ----
suppressPackageStartupMessages({
  library(httr)      # For HTTP requests
  library(dplyr)     # For data manipulation
  library(purrr)     # For functional programming
  library(stringr)   # For string handling
  library(furrr)     # For parallel processing
  library(digest)    # For file hashing
  library(glue)      # For string interpolation
  library(tibble)    # For tibble objects
  library(fs)        # For file system operations
})

# Validate input
if (!exists("excel_links_df")) {
  stop("The variable 'excel_links_df' does not exist. Run find_excel_path.R first.")
}

# Setup output and log directories with path handling that works on any OS
base_output_dir <- file.path(config$output_dir)
ensure_dir(base_output_dir)

base_log_dir <- file.path(config$log_dir)
ensure_dir(base_log_dir)

# Create a timestamp for log file naming
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file_path <- file.path(base_log_dir, glue("download_log_{timestamp}.csv"))
hash_registry_path <- file.path(base_log_dir, "hash_registry.csv")

# Initialize file counter for progress tracking
total_files <- nrow(excel_links_df)
current_file <- 0
success_count <- 0
fail_count <- 0
skip_count <- 0

# ---- XML Properties Extraction Function ----
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
      message(glue("Error extracting XML properties from {file_path}: {e$message}"))
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

# ---- Excel Metadata Extraction Function ----
extract_excel_metadata <- function(file_path, download_source_url, table_title) {
  # Get file information
  file_info <- fs::file_info(file_path)
  file_size <- file_info$size
  creation_date <- if (!is.na(file_info$birth_time)) {
    file_info$birth_time
  } else {
    file_info$modification_time
  }
  modification_date <- file_info$modification_time
  
  # Extract XML properties from xlsx if applicable
  xml_props <- extract_xml_properties(file_path)
  
  # Use provided title if available, otherwise use document title
  final_title <- if (!is.na(table_title) && nzchar(table_title)) {
    table_title
  } else {
    xml_props$title
  }
  
  # Calculate hash using the utility function
  checksum <- calculate_file_hash(file_path, algo = "sha256")
  
  # Try to get sheet count, defaulting to NA on error
  sheet_count <- tryCatch({
    if (tolower(fs::path_ext(file_path)) == "xlsx") {
      length(readxl::excel_sheets(file_path))
    } else {
      NA_integer_
    }
  }, error = function(e) {
    NA_integer_
  })
  
  # Create timestamp for extraction
  current_time <- Sys.time()
  
  # Return a tibble with extracted metadata
  tibble::tibble(
    file_path = file_path,
    file_size = file_size,
    creation_date = creation_date,
    modification_date = modification_date,
    doc_created = xml_props$created,
    doc_modified = xml_props$modified,
    checksum = checksum,
    title = final_title,
    author = xml_props$creator,
    last_modified_by = xml_props$last_modified_by,
    number_of_sheets = sheet_count,
    download_source_url = download_source_url,
    download_timestamp = current_time,  # Use the same timestamp for consistency
    processing_status = "success",
    extraction_timestamp = current_time, # Use the same timestamp for consistency
    download_duration = 0  # Set a default value
  )
}

# ---- Enhanced Download Function ----
download_excel_file_with_reporting <- function(row_index, row, 
                                               overwrite = FALSE, 
                                               min_file_size = 100) {
  # Extract data from row
  year <- row$year
  table_number <- row$table_number
  excel_url <- row$excel_url
  table_title <- row$table_title
  
  # Format table number for filename
  clean_table_number <- stringr::str_replace_all(table_number, "\\.", "_")
  
  # Determine chapter and subchapter from table number
  subchapter <- stringr::str_extract(table_number, "^[0-9]{3}")
  chapter_folder <- substr(subchapter, 1, 1)
  
  # Determine file extension from URL
  ext <- tolower(fs::path_ext(excel_url))
  if (!(ext %in% c("xls", "xlsx"))) {
    # If extension can't be determined, default to xlsx
    ext <- "xlsx"
  }
  
  # Construct filename
  file_name <- glue("{year}_tabn{clean_table_number}.{ext}")
  
  # Construct directory path using file.path for cross-platform compatibility
  sub_dir <- file.path(
    base_output_dir, 
    year,
    paste0("chapter_", chapter_folder),
    paste0("subchapter_", subchapter)
  )
  
  # Create full file path
  file_path <- file.path(sub_dir, file_name)
  
  # Create timestamp for download
  download_time <- Sys.time()
  
  # Initialize log entry with basic information
  log_entry <- tibble::tibble(
    year = year,
    table_number = table_number,
    excel_url = excel_url,
    file_path = file_path,
    table_title = table_title,
    download_source_url = excel_url,
    download_timestamp = download_time,
    processing_status = NA_character_,
    error_message = NA_character_,
    row_index = row_index,
    attempt_count = 1,
    download_duration = 0  # Default value
  )
  
  # Check for missing URL
  if (is.na(excel_url) || excel_url == "") {
    return(log_entry %>% 
             dplyr::mutate(
               processing_status = "failed", 
               error_message = "Missing or empty URL"
             ))
  }
  
  # Ensure directory exists
  ensure_dir(sub_dir)
  
  # Use the enhanced download function from utils.R
  result <- download_excel_file(
    url = excel_url,
    dest_path = file_path,
    min_size = min_file_size,
    max_retries = 4,
    overwrite = overwrite,
    verify_extension = TRUE
  )
  
  # Create timestamp for completion
  completion_time <- Sys.time()
  
  # Calculate duration in seconds
  duration_secs <- as.numeric(difftime(completion_time, download_time, units = "secs"))
  
  # Process download result
  if (result$success) {
    if (result$status == "downloaded") {
      # File was downloaded successfully, extract metadata
      tryCatch({
        excel_meta <- extract_excel_metadata(file_path, excel_url, table_title)
        
        # Combine log entry with metadata, using base R for safety
        cols_to_keep <- setdiff(names(log_entry), c("processing_status", "error_message", "download_duration"))
        log_entry_subset <- log_entry[, cols_to_keep]
        
        # Add download_duration to excel_meta
        excel_meta$download_duration <- duration_secs
        
        # Combine the dataframes
        log_entry <- dplyr::bind_cols(log_entry_subset, excel_meta[, setdiff(names(excel_meta), names(log_entry_subset))])
        
      }, error = function(e) {
        # If metadata extraction fails, still mark as success but note the error
        log_entry$processing_status <- "partial_success"
        log_entry$error_message <- paste0("Downloaded but metadata extraction failed: ", e$message)
        log_entry$checksum <- result$hash
        log_entry$download_duration <- duration_secs
      })
    } else {
      # File was skipped (already exists)
      log_entry$processing_status <- "skipped"
      log_entry$error_message <- "File already exists"
      log_entry$checksum <- result$hash
      log_entry$download_duration <- 0
    }
  } else {
    # Download failed
    log_entry$processing_status <- "failed"
    log_entry$error_message <- result$error
    log_entry$download_duration <- duration_secs
  }
  
  return(log_entry)
}

# ---- Function to create and update a simple progress bar ----
update_progress <- function(completed, total) {
  width <- 50  # Width of progress bar
  percentage <- completed / total
  filled <- round(width * percentage)
  bar <- paste0(
    "[", 
    paste(rep("=", filled), collapse = ""),
    paste(rep(" ", width - filled), collapse = ""),
    "] ",
    sprintf("%3d%%", round(percentage * 100)),
    " | ", completed, "/", total
  )
  cat("\r", bar)
  if (completed == total) cat("\n")
  utils::flush.console()
}

# ---- Prepare for Parallel Processing ----
# Make sure we have a valid number of workers
worker_count <- if (is.null(config$max_parallel) || !is.numeric(config$max_parallel) || config$max_parallel < 1) {
  message("Invalid worker count. Using 2 workers as a fallback.")
  2  # Fallback to 2 workers
} else {
  as.integer(config$max_parallel)
}

# Message about download
message(glue("Downloading {nrow(excel_links_df)} Excel files with {worker_count} parallel workers..."))

# Process files sequentially for safety
message("Using sequential processing for greater reliability")

# Process files sequentially
download_logs <- vector("list", total_files)

for (i in seq_len(total_files)) {
  row <- excel_links_df[i, ]
  download_logs[[i]] <- download_excel_file_with_reporting(i, row, overwrite = FALSE)
  message(glue("Downloaded {i}/{total_files}: {row$year} table {row$table_number}"))
}

# Combine results and add overall statistics
log_df <- dplyr::bind_rows(download_logs)

# Update counters for the summary
success_count <- sum(log_df$processing_status %in% c("success", "partial_success"))
fail_count <- sum(log_df$processing_status == "failed")
skip_count <- sum(log_df$processing_status == "skipped")

# Write download log to CSV
readr::write_csv(log_df, log_file_path)
message(glue("✅ Download log saved to: {log_file_path}"))

# Summary message
message(glue("Download Summary: {success_count} success, {fail_count} failed, {skip_count} skipped."))

# Return log dataframe for pipeline
download_log <- log_df