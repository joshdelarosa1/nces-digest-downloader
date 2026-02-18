#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Script: code/find_excel_path.R
# -----------------------------------------------------------------------------
# Purpose: Apply configured filters to discovered tables and resolve each table
#          page to a downloadable Excel URL.
# Notes: This script is sourced by `main.R` after table discovery.


#' Extract Excel Link and Normalized Title
#'
#' Resolves the first available Excel link and preferred table title for a
#' single table row.
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
  
  # Check for invalid URL before proceeding
  if (is.na(page_url) || nchar(page_url) == 0) {
    return(default_result %>% 
             dplyr::mutate(error = "Invalid or missing page URL"))
  }
  
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
    
    # If no Excel links found, try direct URL construction as fallback
    if (length(excel_links) == 0) {
      # Extract table number from URL for direct construction
      url_table_num <- stringr::str_extract(page_url, "dt\\d+_(\\d+)\\.(\\d+)", group = 1)
      url_table_subnum <- stringr::str_extract(page_url, "dt\\d+_(\\d+)\\.(\\d+)", group = 2)
      
      if (!is.na(url_table_num) && !is.na(url_table_subnum)) {
        # Construct direct Excel URL — prefer .xlsx (NCES default since ~2018)
        # The download function retries on failure, so if .xlsx is wrong the
        # next attempt will surface the error clearly.
        constructed_url <- glue::glue(
          "{NCES_BASE_URL}{year}/tables/xls/tabn{url_table_num}{url_table_subnum}.xlsx"
        )
        
        message(glue::glue("No Excel link found, trying constructed URL: {constructed_url}"))
        
        # Return with constructed URL
        return(tibble::tibble(
          year = year,
          chapter = chapter,
          table_number = table_number,
          table_title = row$table_title,
          page_url = page_url,
          excel_url = constructed_url,
          error = NA_character_
        ))
      } else {
        # If we can't construct URL, return error
        return(default_result %>% 
                 dplyr::mutate(error = "No Excel links found on page and cannot construct direct URL"))
      }
    }
    
    # Extract href attributes from links
    excel_hrefs <- purrr::map_chr(excel_links, rvest::html_attr, "href")
    
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
             dplyr::mutate(error = glue::glue("Error: {e$message}")))
  })
}

#' Construct Direct Excel URL Without HTML Scraping
#'
#' Builds a direct Excel URL from table identifiers as a fallback when page
#' scraping does not expose a link.
#' 
#' @param row A row from the tables dataframe containing year and table_number
#' @return A string with the constructed Excel URL or NA if not possible
#' 
#' @examples
#' # Example row with year "d22" and table_number "101.20"
#' row <- list(year = "d22", table_number = "101.20")
#' url <- construct_direct_excel_url(row)
#' # Returns: "https://nces.ed.gov/programs/digest/d22/tables/xls/tabn10120.xls"

construct_direct_excel_url <- function(row) {
  # Extract components from row
  year <- row$year
  table_number <- row$table_number
  
  # Check for required values
  if (is.na(table_number) || !grepl("\\.", table_number)) {
    return(NA_character_)
  }
  
  # Split table number to extract main and sub parts
  parts <- strsplit(table_number, "\\.")[[1]]
  if (length(parts) != 2) {
    return(NA_character_)
  }
  
  main_num <- parts[1]
  sub_num <- parts[2]
  
  # Construct URL — prefer .xlsx (NCES default since ~2018).
  # Uses NCES_BASE_URL from config.R.
  url <- paste0(NCES_BASE_URL, year, "/tables/xls/tabn", main_num, sub_num, ".xlsx")

  return(url)
}

# -----------------------------------------------------------------------------
# Main Processing Section
# -----------------------------------------------------------------------------

# Check if all_tables exists
if (!exists("all_tables")) {
  stop("The variable 'all_tables' does not exist. Run URL_DIGEST.R first.")
}

message("Filtering tables based on configuration...")

# Apply filtering based on configuration
filtered_tables <- all_tables

if (config$filter_mode == "year_only") {
  # Print values for debugging
  if (config$verbose) {
    message("Filter years: ", paste(config$filter_years, collapse=", "))
    message("Table years: ", paste(unique(filtered_tables$year), collapse=", "))
  }
  
  # Make sure format is consistent before filtering
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
  
  # Create a temporary variable to ensure the current batch is available
  current_batch <- batch
  
  # Process this batch in parallel
  batch_results <- furrr::future_map_dfr(
    split(current_batch, seq_len(nrow(current_batch))),
    function(row) {
      result <- get_excel_link_from_page(row)
      # If no Excel URL was found, try direct URL construction
      if (is.na(result$excel_url)) {
        direct_url <- construct_direct_excel_url(row)
        if (!is.na(direct_url)) {
          result$excel_url <- direct_url
          result$excel_url_source <- "direct_construction"
          result$error <- NA_character_
          message(glue::glue("Using directly constructed URL for {row$table_number}: {direct_url}"))
        }
      }
      return(result)
    },
    .options = furrr::furrr_options(seed = TRUE, packages = c("rvest", "stringr", "xml2", "dplyr", "tibble", "glue"))
  )
  
  excel_links_list[[i]] <- batch_results
  
  # Update progress
  processed_count <- processed_count + nrow(batch)
  message(glue::glue("Processed {processed_count}/{total_tables} tables"))
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
