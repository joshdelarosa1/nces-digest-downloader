#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: find_excel_path.R
# Description:
#   Enhanced extraction of Excel download links with improved parallelism
#   and error handling. This script applies filtering based on user 
#   configuration and extracts download links from table pages.
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
  library(tibble)     # For tibble data frames
  library(xml2)       # For XML processing
  library(glue)       # For string interpolation
})

#' Extract Excel Download Link and Full Title
#'
#' Enhanced function to extract Excel links and full titles from table pages
#' with comprehensive error handling and detailed logging.
#'
#' @param row Data for a single table.
#'
#' @return A tibble row with extracted data.
get_excel_link_from_page <- function(row) {
  year <- row$year
  chapter <- row$chapter
  table_number <- row$table_number
  page_url <- row$url
  
  # Default result with original data
  default_result <- tibble::tibble(
    year = year,
    chapter = chapter,
    table_number = table_number,
    table_title = row$table_title,
    page_url = page_url,
    excel_url = NA_character_,
    error = NA_character_
  )
  
  tryCatch({
    # Retrieve and parse the HTML content
    html <- safe_read_html(page_url)
    
    # If page couldn't be retrieved, return default with error
    if (is.null(html)) {
      return(default_result %>% 
               dplyr::mutate(error = "Failed to retrieve page"))
    }
    
    # Extract Excel links using XPath
    excel_links <- rvest::html_nodes(
      html, 
      xpath = "//a[contains(@href, '.xls') or contains(@href, '.xlsx')]"
    )
    
    # Extract href attributes from links
    excel_hrefs <- purrr::map_chr(excel_links, rvest::html_attr, "href")
    
    # If no Excel links found, return default with error
    if (length(excel_hrefs) == 0) {
      return(default_result %>% 
               dplyr::mutate(error = "No Excel links found on page"))
    }
    
    # Get the first Excel link
    link <- excel_hrefs[1]
    
    # Convert relative links to absolute URLs if needed
    if (!stringr::str_detect(link, "^https?://")) {
      link <- xml2::url_absolute(link, page_url)
    }
    
    # Extract title components using XPath
    title_parts <- tryCatch({
      # Title number is in a cell with width attribute
      title_num <- rvest::html_node(
        html, 
        xpath = "//table[@class='tableWidth']//td[@class='title' and @width]"
      ) %>%
        rvest::html_text(trim = TRUE)
      
      # Title description is in a cell without width attribute
      title_desc <- rvest::html_node(
        html, 
        xpath = "//table[@class='tableWidth']//td[@class='title' and not(@width)]"
      ) %>%
        rvest::html_text(trim = TRUE)
      
      # Combine title parts
      if (!is.na(title_num) && nzchar(title_num) &&
          !is.na(title_desc) && nzchar(title_desc)) {
        paste(title_num, title_desc)
      } else if (!is.na(title_desc) && nzchar(title_desc)) {
        title_desc
      } else {
        NA_character_
      }
    }, error = function(e) {
      # On error extracting title, return NA
      NA_character_
    })
    
    # Use the extracted title or fall back to the one from all_tables
    full_title <- if (!is.na(title_parts) && nzchar(title_parts)) {
      title_parts
    } else {
      row$table_title
    }
    
    # Return a tibble row with extracted data
    tibble::tibble(
      year = year,
      chapter = chapter,
      table_number = table_number,
      table_title = full_title,
      page_url = page_url,
      excel_url = link,
      error = NA_character_
    )
  }, error = function(e) {
    # On any error, return the default with error message
    return(default_result %>% 
             dplyr::mutate(error = glue("Error: {e$message}")))
  })
}

# -----------------------------------------------------------------------------
# Apply Filtering Based on Configuration
# -----------------------------------------------------------------------------

# Check if all_tables exists
if (!exists("all_tables")) {
  stop("The variable 'all_tables' does not exist. Run URL_DIGEST.R first.")
}

message("Filtering tables based on configuration...")

# Apply filtering based on configuration
filtered_tables <- all_tables

if (config$filter_mode == "year_only") {
  filtered_tables <- filtered_tables %>% 
    dplyr::filter(year %in% config$filter_years)
  message(glue("Applied year filter: {paste(config$filter_years, collapse=', ')}"))
  
} else if (config$filter_mode == "table_only") {
  filtered_tables <- filtered_tables %>% 
    dplyr::filter(table_number %in% config$filter_tables)
  message(glue("Applied table filter: {paste(config$filter_tables, collapse=', ')}"))
  
} else if (config$filter_mode == "custom") {
  filtered_tables <- filtered_tables %>% 
    dplyr::filter(
      (year %in% config$filter_years & table_number %in% config$filter_tables) |
        (table_number %in% config$filter_tables)
    )
  message("Applied custom filter combining year and table filters")
  
} else if (config$filter_mode == "all") {
  message("Using all available tables (no filtering)")
} else {
  stop(glue("Invalid filter_mode specified: '{config$filter_mode}'"))
}

message(glue("Filtered to {nrow(filtered_tables)} tables for processing"))

# -----------------------------------------------------------------------------
# Extract Excel Links
# -----------------------------------------------------------------------------
message("Extracting Excel links from table pages...")

# Simple counter for progress reporting
total_tables <- nrow(filtered_tables)
message(glue("Processing {total_tables} tables..."))

# Process in smaller batches to prevent overwhelming the server
batch_size <- min(10, max(1, config$max_parallel))
batches <- split(
  filtered_tables,
  ceiling(seq_len(nrow(filtered_tables)) / batch_size)
)

excel_links_list <- vector("list", length(batches))
processed_count <- 0

for (i in seq_along(batches)) {
  batch <- batches[[i]]
  
  message(glue("Processing batch {i}/{length(batches)} ({nrow(batch)} tables)"))
  
  # Process this batch in parallel
  batch_results <- furrr::future_map_dfr(
    split(batch, seq_len(nrow(batch))),
    get_excel_link_from_page
  )
  
  excel_links_list[[i]] <- batch_results
  
  # Update progress
  processed_count <- processed_count + nrow(batch)
  message(glue("Processed {processed_count}/{total_tables} tables"))
}

# Combine all batches
excel_links_df <- dplyr::bind_rows(excel_links_list)

# -----------------------------------------------------------------------------
# Summary and Clean-up
# -----------------------------------------------------------------------------

# Basic validation and cleaning
excel_links_df <- excel_links_df %>%
  # Add source of Excel URL
  dplyr::mutate(
    excel_url_source = ifelse(is.na(excel_url), NA_character_, "page"),
    # Clean up table titles (remove extra whitespace)
    table_title = stringr::str_squish(table_title)
  )

# Count results by status
total_tables <- nrow(excel_links_df)
with_excel <- sum(!is.na(excel_links_df$excel_url))
missing_excel <- total_tables - with_excel

# Display summary
message(glue("
  Excel Link Extraction Summary:
  - Total tables processed: {total_tables}
  - Tables with Excel links: {with_excel}
  - Tables missing Excel links: {missing_excel}
"))

# Assign to global environment for downstream scripts
assign("excel_links_df", excel_links_df, envir = .GlobalEnv)