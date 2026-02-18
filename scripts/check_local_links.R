# CHANGELOG
# - 2026-02-17: Added offline local markdown link checker for docs CI.
# - 2026-02-17: Initial version.
# ============================================================================
# Script: scripts/check_local_links.R
# Purpose: Validate local markdown links and heading anchors without network
#   access for offline-compatible documentation checks.
# Inputs: Markdown files or directories passed as command-line arguments.
# Outputs: Exit code 0 on success; non-zero with printed failures.
# Assumptions:
#   - Relative markdown links should resolve from the source file location.
#   - Anchor slugs follow lowercase hyphenated heading text.
# How to run: Rscript scripts/check_local_links.R README.md docs
# Notes: External links (http/https/mailto/tel) are intentionally ignored.
# ============================================================================

# ---- Setup -----------------------------------------------------------------
# What this section does:
#   - Reads command-line targets and initializes error tracking.
# Outputs:
#   - `input_targets`, `markdown_files`, and `validation_errors` objects.
input_targets <- commandArgs(trailingOnly = TRUE)
if (length(input_targets) == 0) {
  input_targets <- c("README.md", "docs")
}

validation_errors <- character(0)

# ---- Helpers ----------------------------------------------------------------
# What this section does:
#   - Defines link extraction and anchor normalization helpers.
# Why it matters:
#   - Keeps link parsing and anchor checks deterministic.
extract_markdown_links <- function(markdown_line) {
  link_matches <- gregexpr("\\[[^]]+\\]\\(([^)]+)\\)", markdown_line, perl = TRUE)
  matched_links <- regmatches(markdown_line, link_matches)[[1]]

  if (length(matched_links) == 0 || identical(matched_links, "")) {
    return(character(0))
  }

  link_targets <- gsub("^\\[[^]]+\\]\\(([^)]+)\\)$", "\\1", matched_links, perl = TRUE)
  link_targets
}

normalize_anchor <- function(anchor_text) {
  normalized_anchor <- tolower(trimws(anchor_text))
  normalized_anchor <- gsub("`", "", normalized_anchor)
  normalized_anchor <- gsub("[^a-z0-9 -]", "", normalized_anchor)
  normalized_anchor <- gsub("[[:space:]]+", "-", normalized_anchor)
  normalized_anchor <- gsub("-+", "-", normalized_anchor)
  normalized_anchor <- gsub("^-|-$", "", normalized_anchor)
  normalized_anchor
}

collect_heading_anchors <- function(markdown_path) {
  markdown_lines <- readLines(markdown_path, warn = FALSE)
  heading_lines <- markdown_lines[grepl("^#{1,6}[[:space:]]+", markdown_lines)]
  heading_labels <- trimws(gsub("^#{1,6}[[:space:]]+", "", heading_lines))
  anchors <- normalize_anchor(heading_labels)
  anchors[nzchar(anchors)]
}

# ---- Load data ---------------------------------------------------------------
# What this section does:
#   - Expands file and directory arguments into markdown file paths.
# Outputs:
#   - `markdown_files` vector.
markdown_files <- character(0)
for (input_target in input_targets) {
  if (!file.exists(input_target)) {
    validation_errors <- c(validation_errors, paste0("Missing input target: ", input_target))
    next
  }

  if (dir.exists(input_target)) {
    directory_markdown_files <- list.files(
      input_target,
      pattern = "\\.md$",
      recursive = TRUE,
      full.names = TRUE
    )
    markdown_files <- c(markdown_files, directory_markdown_files)
  } else if (grepl("\\.md$", input_target)) {
    markdown_files <- c(markdown_files, input_target)
  }
}

markdown_files <- unique(markdown_files)

# ---- Validate assumptions ---------------------------------------------------
# What this section does:
#   - Ensures there are markdown files to validate.
if (length(markdown_files) == 0) {
  stop("No markdown files found for validation.")
}

# ---- Transform --------------------------------------------------------------
# What this section does:
#   - Validates local file targets and optional anchors for each markdown link.
# Outputs:
#   - Populated `validation_errors` when invalid links are found.
anchor_cache <- new.env(parent = emptyenv())

for (markdown_path in markdown_files) {
  markdown_lines <- readLines(markdown_path, warn = FALSE)

  for (line_index in seq_along(markdown_lines)) {
    line_links <- extract_markdown_links(markdown_lines[line_index])
    if (length(line_links) == 0) {
      next
    }

    for (link_target in line_links) {
      if (
        grepl("^(https?:|mailto:|tel:)", link_target) ||
          startsWith(link_target, "#")
      ) {
        if (startsWith(link_target, "#")) {
          anchor_name <- sub("^#", "", link_target)
          cache_key <- markdown_path
          if (!exists(cache_key, envir = anchor_cache, inherits = FALSE)) {
            assign(cache_key, collect_heading_anchors(markdown_path), envir = anchor_cache)
          }
          if (!(anchor_name %in% get(cache_key, envir = anchor_cache))) {
            validation_errors <- c(
              validation_errors,
              paste0(markdown_path, ":", line_index, " missing anchor #", anchor_name)
            )
          }
        }
        next
      }

      split_target <- strsplit(link_target, "#", fixed = TRUE)[[1]]
      linked_path <- split_target[1]
      linked_anchor <- if (length(split_target) > 1) split_target[2] else ""

      resolved_path <- normalizePath(
        file.path(dirname(markdown_path), linked_path),
        winslash = "/",
        mustWork = FALSE
      )

      if (!file.exists(resolved_path)) {
        validation_errors <- c(
          validation_errors,
          paste0(markdown_path, ":", line_index, " missing target ", link_target)
        )
        next
      }

      if (nzchar(linked_anchor) && grepl("\\.md$", resolved_path, ignore.case = TRUE)) {
        cache_key <- resolved_path
        if (!exists(cache_key, envir = anchor_cache, inherits = FALSE)) {
          assign(cache_key, collect_heading_anchors(resolved_path), envir = anchor_cache)
        }

        if (!(linked_anchor %in% get(cache_key, envir = anchor_cache))) {
          validation_errors <- c(
            validation_errors,
            paste0(markdown_path, ":", line_index, " missing anchor #", linked_anchor,
                   " in ", linked_path)
          )
        }
      }
    }
  }
}

# ---- Output -----------------------------------------------------------------
# What this section does:
#   - Prints validation results and exits with appropriate status code.
if (length(validation_errors) > 0) {
  message("Local link check failures:")
  for (validation_error in validation_errors) {
    message("- ", validation_error)
  }
  quit(status = 1)
}

message("Local markdown links validated successfully across ", length(markdown_files), " files.")
