#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Script: code/download_files.R
# Purpose: Download resolved Excel URLs, validate payloads, and persist
#          execution logs and hash history.
# Notes: This script is sourced by `main.R` after `excel_links_df` is prepared.
# -----------------------------------------------------------------------------

# ---- Setup ----
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

# ---------------------------------------------------------------------------
# In-memory hash registry: load once, append in-process, flush once at end.
# This avoids the O(n²) pattern where each file triggered a full CSV
# read + rewrite cycle.
# ---------------------------------------------------------------------------
.hash_reg <- new.env(parent = emptyenv())
.hash_reg$df <- if (file.exists(hash_registry_path)) {
  tryCatch(
    readr::read_csv(hash_registry_path, show_col_types = FALSE,
                    col_types = readr::cols(.default = "c")),
    error = function(e) {
      warning("Could not load hash registry; starting fresh: ", conditionMessage(e))
      data.frame(file_path = character(), hash = character(),
                 download_date = character(), timestamp = character(),
                 stringsAsFactors = FALSE)
    }
  )
} else {
  data.frame(file_path = character(), hash = character(),
             download_date = character(), timestamp = character(),
             stringsAsFactors = FALSE)
}

# In-memory variants shadow disk-based helpers for batch efficiency.
#'
#' Record a hash entry in the in-memory registry.
#'
#' @param file_path Destination file path.
#' @param hash Hash digest value for the file.
#' @param download_date Download date for the registry row.
#' @param registry_path Unused compatibility parameter.
#' @return Logical `TRUE` when entry is recorded, otherwise `FALSE`.
register_file_hash <- function(file_path, hash, download_date = Sys.Date(),
                                registry_path = NULL) {
  if (is.na(hash)) return(FALSE)
  .hash_reg$df <- rbind(.hash_reg$df, data.frame(
    file_path     = as.character(file_path),
    hash          = as.character(hash),
    download_date = as.character(download_date),
    timestamp     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  ))
  return(TRUE)
}

#'
#' Look up an existing hash registry entry for a file path.
#'
#' @param file_path Destination file path to search.
#' @param hash Optional hash for equality comparison.
#' @param registry_path Unused compatibility parameter.
#' @return Named list containing existence, hash match, and latest metadata.
check_hash_registry <- function(file_path, hash = NULL, registry_path = NULL) {
  empty <- list(exists = FALSE, hash_match = FALSE,
                last_download = NA_character_, registered_hash = NA_character_)
  df <- .hash_reg$df
  if (nrow(df) == 0) return(empty)
  matches <- df[df$file_path == file_path, ]
  if (nrow(matches) == 0) return(empty)
  most_recent <- matches[nrow(matches), ]
  list(exists = TRUE,
       hash_match = !is.null(hash) && isTRUE(most_recent$hash == hash),
       last_download = most_recent$download_date,
       registered_hash = most_recent$hash)
}

# Use an environment so mutations inside apply_throttling() / adjust_throttling()
# persist across calls. A plain list has value semantics in R, so assignments
# inside a function would only modify a local copy.
throttle_state <- new.env(parent = emptyenv())
throttle_state$min_delay         <- config$min_dl_delay %||% 3
throttle_state$current_delay     <- config$min_dl_delay %||% 3
throttle_state$max_delay         <- config$max_delay    %||% 30
throttle_state$last_request_time <- Sys.time() - as.difftime(100, units = "secs")
throttle_state$consecutive_fails <- 0L
throttle_state$failure_threshold <- 3L
throttle_state$backoff_factor    <- 1.5

#'
#' Enforce adaptive request delay between downloads.
#'
#' @return `NULL`; updates global throttle state by side effect.
apply_throttling <- function() {
  elapsed <- as.numeric(difftime(Sys.time(), throttle_state$last_request_time, units = "secs"))
  if (elapsed < throttle_state$current_delay) {
    Sys.sleep(throttle_state$current_delay - elapsed)
  }
  throttle_state$last_request_time <- Sys.time()
}

#'
#' Adjust adaptive throttling state based on prior download outcome.
#'
#' @param success Logical indicating success of the previous attempt.
#' @return `NULL`; updates global throttle state by side effect.
adjust_throttling <- function(success) {
  if (success) {
    throttle_state$consecutive_fails <- 0L
    throttle_state$current_delay <- max(
      throttle_state$min_delay,
      throttle_state$current_delay / sqrt(throttle_state$backoff_factor)
    )
  } else {
    throttle_state$consecutive_fails <- throttle_state$consecutive_fails + 1L
    if (throttle_state$consecutive_fails >= throttle_state$failure_threshold) {
      throttle_state$current_delay <- min(
        throttle_state$current_delay * throttle_state$backoff_factor,
        throttle_state$max_delay
      )
    }
  }
}

# Download one Excel artifact with retry, validation, and hash registration.
#'
#' Download one Excel file with retry, validation, and hash registration.
#'
#' @param url Source file URL.
#' @param dest_path Destination file path.
#' @param min_size Minimum byte size required for a valid payload.
#' @param max_retries Maximum download retry attempts.
#' @param overwrite Logical indicating whether to replace existing files.
#' @param verify_extension Logical indicating whether URL extension is validated.
#' @return List containing status, hash, destination path, and optional errors.
download_excel_file <- function(url, dest_path, min_size = 5000, max_retries = 5, overwrite = FALSE, verify_extension = TRUE) {
  # Check if file already exists and handle according to overwrite parameter
  if (file.exists(dest_path) && !overwrite) {
    hash <- calculate_file_hash(dest_path)
    
    # Register the hash if not already in registry
    registry_result <- check_hash_registry(dest_path, hash)
    if (!registry_result$exists) {
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
      
      # Check file size — guard against HTML error pages returned as 200 OK
      file_size <- file.info(temp_file)$size
      if (file_size < min_size) {
        stop(glue::glue("Downloaded file too small ({file_size} bytes < {min_size} byte minimum). Likely an error page, not a valid Excel file."))
      }

      # Magic-byte check: confirm the file is an Excel binary, not an HTML error page
      file_ext  <- tolower(tools::file_ext(dest_path))
      magic     <- readBin(temp_file, what = "raw", n = 4L)
      valid_magic <- switch(file_ext,
        xlsx = length(magic) >= 2L &&
                 magic[1L] == as.raw(0x50) && magic[2L] == as.raw(0x4B),
        xls  = length(magic) >= 4L &&
                 magic[1L] == as.raw(0xD0) && magic[2L] == as.raw(0xCF) &&
                 magic[3L] == as.raw(0x11) && magic[4L] == as.raw(0xE0),
        TRUE  # unknown extension: let it through
      )
      if (!valid_magic) {
        stop(glue::glue(
          "File failed magic-byte check for .{file_ext} format. ",
          "Likely an HTML error page masquerading as an Excel file."
        ))
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

# Extract document metadata from xlsx core XML properties when available.
#'
#' Extract metadata fields from an `.xlsx` file's core properties.
#'
#' @param file_path Local file path to an Excel artifact.
#' @return Named list of metadata fields with `NA` when unavailable.
extract_xml_properties <- function(file_path) {
  ext <- tolower(fs::path_ext(file_path))

  NA_XML_PROPS <- list(
    creator          = NA_character_,
    last_modified_by = NA_character_,
    created          = NA_character_,
    modified         = NA_character_,
    title            = NA_character_
  )

  # Return NA for all fields if not xlsx
  if (ext != "xlsx") {
    return(NA_XML_PROPS)
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
      return(NA_XML_PROPS)
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
    warning(glue::glue("Could not extract XML properties from {file_path}: {e$message}"))
    return(NA_XML_PROPS)
  })
}

# Wrap download execution and emit a normalized per-row log record.
#'
#' Download one table artifact and return a normalized log row.
#'
#' @param row_index Row index from `excel_links_df`.
#' @param row Single-row table metadata input.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' @param min_file_size Minimum accepted payload size in bytes.
#' @return Tibble with one row containing processing status and metadata.
download_excel_file_with_reporting <- function(row_index, row, overwrite = FALSE, min_file_size = 5000) {
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
progressr::with_progress({
  p <- progressr::progressor(steps = total_files)
  for (i in seq_len(total_files)) {
    row <- excel_links_df[i, ]
    result <- download_excel_file_with_reporting(i, row, overwrite = FALSE)
    download_logs[[i]] <- result
    status <- result$processing_status
    p(message = glue("{row$year} {row$table_number}: {status}"))
  }
})

log_df <- bind_rows(download_logs)

# Flush in-memory hash registry to disk once after all downloads complete.
tryCatch({
  readr::write_csv(.hash_reg$df, hash_registry_path)
}, error = function(e) {
  warning("Could not save hash registry to disk: ", conditionMessage(e))
})

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
