#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: URL_DIGEST.R
# Description:
#   Enhanced web scraping of NCES Digest table metadata with improved error
#   handling and parallelism. This script identifies available tables for each
#   digest year and extracts key metadata.
#
# Version: 1.0.0
# -----------------------------------------------------------------------------

# ---- Load Libraries ----
suppressPackageStartupMessages({
  library(rvest)      # For web scraping
  library(dplyr)      # For data manipulation
  library(purrr)      # For functional programming
  library(furrr)      # For parallel processing
  library(stringr)    # For string manipulation
  library(glue)       # For string interpolation
  library(tibble)     # For tibble data frames
  library(httr)       # For HTTP requests
})

# Define the base URL template for NCES Digest menu pages
base_url <- "https://nces.ed.gov/programs/digest/20%02dmenu_tables.asp"

#' Extract Digest Table Metadata with Improved Error Handling
#'
#' Scrapes the NCES Digest menu page for a specified year, extracting
#' table metadata with comprehensive error handling and validation.
#'
#' @param year_short A numeric value representing the two-digit digest year.
#'
#' @return A tibble with table metadata or empty tibble on failure.
extract_digest_tables <- function(year_short) {
  # Format year with "d" prefix (e.g., "d22")
  digest_year <- sprintf("d%02d", year_short)
  
  # Construct menu URL
  menu_url <- sprintf(base_url, year_short)
  
  message(glue("Scraping menu for digest year 20{year_short}: {menu_url}"))
  
  # Try to read menu page with robust error handling
  menu_page <- tryCatch({
    # Use safe_read_html from utils.R with exponential backoff
    result <- safe_read_html(
      url = menu_url,
      max_retries = 5,
      initial_delay = 1,
      max_delay = 30
    )
    
    if (is.null(result)) {
      message(glue("⚠️ Failed to retrieve menu page for digest year 20{year_short}"))
      return(tibble::tibble())
    }
    
    result
  }, error = function(e) {
    message(glue("⚠️ Error scraping digest year 20{year_short}: {e$message}"))
    return(NULL)
  })
  
  # Return empty tibble if page retrieval failed
  if (is.null(menu_page)) {
    return(tibble::tibble())
  }
  
  # Extract table links with XPath
  nodes <- tryCatch({
    rvest::html_nodes(
      menu_page,
      xpath = sprintf(
        "//a[contains(@href, '%s/tables/dt%s_') and contains(@href, '.asp')]",
        digest_year, year_short
      )
    )
  }, error = function(e) {
    message(glue("⚠️ Error extracting table links for digest year 20{year_short}: {e$message}"))
    return(NULL)
  })
  
  # Return empty tibble if no nodes found
  if (is.null(nodes) || length(nodes) == 0) {
    message(glue("ℹ️ No table nodes found for digest year 20{year_short}"))
    return(tibble::tibble())
  }
  
  # Extract data from each link
  result_df <- purrr::map_dfr(nodes, function(a) {
    # Get href attribute
    href <- rvest::html_attr(a, "href")
    
    # Construct full URL
    table_url <- paste0("https://nces.ed.gov/programs/digest/", href)
    
    # Extract table numbers using regex patterns
    table_number <- stringr::str_extract(href, "\\d{3}\\.\\d{2}")
    
    # Extract chapter from href
    chapter <- stringr::str_extract(href, "(?<=dt\\d{2}_)(\\d{3})")
    
    # Get text from link as fallback title
    link_text <- rvest::html_text(a, trim = TRUE)
    
    # Create data row
    tibble::tibble(
      year = digest_year,
      chapter = chapter,
      table_number = table_number,
      table_title = link_text,  # We'll update this with page title later if available
      url = table_url
    )
  })
  
  # Report how many tables were found
  message(glue("ℹ️ Found {nrow(result_df)} tables for digest year 20{year_short}"))
  
  return(result_df)
}

#' Extract Page Titles in Parallel
#'
#' Enhances the table metadata by fetching actual page titles in parallel batches.
#'
#' @param tables_df Tibble with table metadata.
#' @param batch_size Number of pages to process in each batch.
#'
#' @return Enhanced tibble with page titles.
enhance_with_page_titles <- function(tables_df, batch_size = 10) {
  if (nrow(tables_df) == 0) {
    return(tables_df)
  }
  
  # Split data into batches for controlled parallelism
  batches <- split(
    tables_df,
    ceiling(seq_len(nrow(tables_df)) / batch_size)
  )
  
  # Process each batch
  message(glue("Processing {length(batches)} batches of table titles..."))
  
  # Counter for progress
  total_tables <- nrow(tables_df)
  processed <- 0
  
  enhanced_batches <- purrr::map_dfr(batches, function(batch) {
    # Process URLs in parallel within each batch
    batch_results <- furrr::future_map_dfr(
      split(batch, seq_len(nrow(batch))),
      function(row) {
        # Try to get the page title
        tryCatch({
          # Use the table URL to fetch the page
          page <- safe_read_html(row$url)
          
          if (!is.null(page)) {
            # Extract the page title
            page_title <- rvest::html_node(page, "title") %>%
              rvest::html_text(trim = TRUE)
            
            # Update the row with the page title
            row$table_title <- page_title
          }
          
          return(row)
        }, error = function(e) {
          # On error, keep the original row
          if (config$verbose) {
            message(glue("⚠️ Error fetching title for {row$url}: {e$message}"))
          }
          return(row)
        })
      }
    )
    
    # Update progress
    processed <<- processed + nrow(batch)
    message(glue("Processed {processed}/{total_tables} table titles"))
    
    return(batch_results)
  })
  
  return(enhanced_batches)
}

# -----------------------------------------------------------------------------
# Main Execution
# -----------------------------------------------------------------------------

message("Scraping digest tables...")

# Scrape tables for each year in parallel using furrr
all_tables_raw <- furrr::future_map_dfr(
  config$years,
  extract_digest_tables
)

# Enhance table data with proper page titles in controlled batches
message("Enhancing tables with page titles...")
all_tables <- enhance_with_page_titles(
  all_tables_raw,
  batch_size = min(10, max(1, config$max_parallel / 2))
)

# Message about results
message(glue("✅ Found {nrow(all_tables)} tables across {length(config$years)} digest years"))

# Assign to global environment for downstream scripts
assign("all_tables", all_tables, envir = .GlobalEnv)