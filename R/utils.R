# -----------------------------------------------------------------------------
# File: utils.R
# Description: Enhanced utility functions for the NCES Digest download pipeline.
#              Provides robust methods for downloading, hashing, processing,
#              and error handling.
#
# Version: 1.0.0
# -----------------------------------------------------------------------------

#' Read HTML with Exponential Backoff Retries
#'
#' Downloads and parses HTML content from a URL with exponential backoff retry
#' mechanism for handling intermittent network issues.
#'
#' @param url A character string containing the URL to fetch.
#' @param max_retries Integer specifying the maximum number of retry attempts (default: 5).
#' @param initial_delay Initial delay in seconds between retries (default: 1).
#' @param max_delay Maximum delay in seconds between retries (default: 30).
#' @param user_agent Custom user agent string for the request (default: "R-digest-downloader").
#'
#' @return An `xml_document` object if successful, or NULL if all attempts fail.
#'
#' @export
safe_read_html <- function(url, max_retries = 5, initial_delay = 1, max_delay = 30,
                           user_agent = "R-digest-downloader") {
  delay <- initial_delay

  # Set up HTTP options
  headers <- c(
    "User-Agent" = user_agent,
    "Accept" = "text/html,application/xhtml+xml,application/xml"
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
        html_content <- httr::content(response, as = "text", encoding = "UTF-8")
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

#' Download Excel File with Verification
#'
#' Downloads an Excel file and verifies its integrity by file extension and size.
#'
#' @param url URL of the Excel file to download.
#' @param dest_path Destination path where the file should be saved.
#' @param min_size Minimum expected file size in bytes (default: 100).
#' @param max_retries Number of retry attempts (default: 4).
#' @param overwrite Whether to overwrite existing files (default: FALSE).
#' @param verify_extension Whether to verify file extension (default: TRUE).
#'
#' @return A list with download status information.
#'
#' @export
download_excel_file <- function(url, dest_path, min_size = 100, max_retries = 4,
                               overwrite = FALSE, verify_extension = TRUE) {
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
      status = "skipped_existing"
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

  # Attempt download with retries
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      # Use httr for more control over the download process
      response <- httr::GET(
        url,
        httr::write_disk(temp_file, overwrite = TRUE),
        httr::timeout(60),
        httr::add_headers(
          "User-Agent" = "R-digest-downloader",
          "Accept" = "application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      )

      # Check if request was successful
      if (httr::status_code(response) != 200) {
        stop(paste("HTTP error:", httr::status_code(response)))
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

      # Calculate backoff delay with jitter
      delay <- min(2^attempt, 30) + runif(1, 0, 2)
      message(glue::glue("Attempt {attempt}/{max_retries} failed: {e$message}"))
      message(glue::glue("Retrying in {round(delay, 1)} seconds..."))
      Sys.sleep(delay)
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

#' Extract Title from HTML Document
#'
#' Combines a title number and description from an HTML document
#' to produce a full table title. Returns NA if neither part is available.
#'
#' @param doc An xml_document returned by read_html().
#'
#' @return A character string with the combined title or NA if not found.
#'
#' @export
extract_title <- function(doc) {
  # Handle NULL input
  if (is.null(doc)) {
    return(NA_character_)
  }

  # Extract title parts using xpath
  title_num <- tryCatch({
    rvest::html_node(doc, xpath = "//table[@class='tableWidth']//td[@class='title' and @width]") %>%
      rvest::html_text(trim = TRUE)
  }, error = function(e) NA_character_)

  title_desc <- tryCatch({
    rvest::html_node(doc, xpath = "//table[@class='tableWidth']//td[@class='title' and not(@width)]") %>%
      rvest::html_text(trim = TRUE)
  }, error = function(e) NA_character_)

  # Combine title parts if available
  if (!is.na(title_num) && nzchar(title_num) &&
      !is.na(title_desc) && nzchar(title_desc)) {
    return(paste(title_num, title_desc))
  } else if (!is.na(title_desc) && nzchar(title_desc)) {
    return(title_desc)
  } else {
    return(NA_character_)
  }
}

#' Dynamic Throttling for Downloads
#'
#' Adaptively adjusts the number of parallel downloads based on success rate
#' and system performance.
#'
#' @param success_rate Recent success rate (0-1).
#' @param current_parallel Current number of parallel downloads.
#' @param min_parallel Minimum number of parallel downloads (default: 1).
#' @param max_parallel Maximum number of parallel downloads.
#'
#' @return Recommended number of parallel downloads.
#'
#' @export
adjust_parallelism <- function(success_rate, current_parallel,
                              min_parallel = 1, max_parallel = config$max_parallel) {
  # Don't adjust if we're at the minimum already
  if (current_parallel <= min_parallel) {
    return(min_parallel)
  }

  # If success rate is very high, we can try increasing parallelism
  if (success_rate > 0.95 && current_parallel < max_parallel) {
    return(min(current_parallel + 1, max_parallel))
  }

  # If success rate is poor, reduce parallelism
  if (success_rate < 0.7) {
    return(max(floor(current_parallel * 0.8), min_parallel))
  }

  # Otherwise, maintain current level
  return(current_parallel)
}

#' Process Links for Download
#'
#' Pre-processes links dataframe before download to ensure consistency.
#'
#' @param links_df A dataframe of links to process.
#'
#' @return Processed dataframe ready for download.
#'
#' @export
process_links <- function(links_df) {
  # Simple validation and cleaning
  links_df <- links_df %>%
    dplyr::mutate(
      # Ensure URLs have proper scheme
      excel_url = ifelse(
        !is.na(excel_url) & !grepl("^https?://", excel_url),
        paste0("https://", excel_url),
        excel_url
      ),
      # Clean up table titles
      table_title = stringr::str_squish(table_title)
    ) %>%
    # Remove rows with missing URLs
    dplyr::filter(!is.na(excel_url) & excel_url != "")

  return(links_df)
}
