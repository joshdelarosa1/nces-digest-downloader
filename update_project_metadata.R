#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# File: update_project_metadata.R
# -----------------------------------------------------------------------------
# Purpose: Automate the update of project metadata across various project artifacts.
#
# This script reads metadata from a centralized YAML file (e.g., project_config.yml)
# and updates the following:
#
#   1. DESCRIPTION file (if it exists) using the desc package.
#   2. R script headers for:
#        - main.R
#        - install_dependencies.R
#        - URL_DIGEST.R
#        - find_excel_path.R
#        - download_files.R
#        - utils.R
#   3. README.md to include the new version and release notes.
#   4. Optionally, creates an annotated Git tag for the new release (using usethis or system call).
#
# Usage: Rscript update_project_metadata.R
#
# -----------------------------------------------------------------------------

# Load required libraries
library(yaml)     # To read YAML configuration files
library(desc)     # To update the DESCRIPTION file
library(usethis)  # To create Git tags (optional)
library(stringr)  # For regex-based text replacement
library(readr)    # For reading and writing text files

# -----------------------------------------------------------------------------
# STEP 1: Read Project Metadata from YAML Configuration
# -----------------------------------------------------------------------------

# Define the path to the YAML configuration file (separate from config.R)
config_file <- "project_config.yml"

# Check if the configuration file exists
if (!file.exists(config_file)) {
  stop("Configuration file 'project_config.yml' not found.")
}

# Read the YAML file into a list
project_meta <- yaml::read_yaml(config_file)

# Extract metadata values (expected keys: version, release_date, changelog, author, project_name)
version      <- project_meta$version
release_date <- project_meta$release_date
changelog    <- project_meta$changelog
author       <- project_meta$author
project_name <- project_meta$project_name

# Optionally, check for a flag to create a Git tag (default FALSE if not provided)
create_git_tag <- ifelse(!is.null(project_meta$create_git_tag), project_meta$create_git_tag, FALSE)

message("Project metadata loaded:")
message("  Version: ", version)
message("  Release Date: ", release_date)
message("  Author: ", author)
message("  Project Name: ", project_name)

# -----------------------------------------------------------------------------
# STEP 2: Update DESCRIPTION File with the New Version
# -----------------------------------------------------------------------------

update_description <- function(new_version) {
  desc_file <- "DESCRIPTION"
  if (file.exists(desc_file)) {
    message("Updating DESCRIPTION file with new version...")
    d <- desc::desc(file = desc_file)
    d$set("Version", new_version)
    # You can update additional fields here if needed.
    d$write(file = desc_file)
    message("DESCRIPTION file updated.")
  } else {
    message("No DESCRIPTION file found. Skipping DESCRIPTION update.")
  }
}

update_description(version)

# -----------------------------------------------------------------------------
# STEP 3: Update R Script Headers with New Metadata
# -----------------------------------------------------------------------------

# List of R script files to update
# List of R script files to update with correct relative paths
r_files <- c("main.R", 
             "install_dependencies.R", 
             "code/URL_DIGEST.R", 
             "code/find_excel_path.R", 
             "code/download_files.R", 
             "R/utils.R")

# Function to update header lines in an R script file
update_script_header <- function(file_path, new_version, new_release_date, new_author) {
  if (!file.exists(file_path)) {
    message(paste("File not found:", file_path))
    return()
  }
  
  message(paste("Updating header in", file_path, "..."))
  
  # Read all lines from the file
  lines <- readLines(file_path, warn = FALSE)
  
  # Define regex patterns to detect header lines for version, release date, and author
  version_pattern <- "^#\\s*Version:"
  date_pattern    <- "^#\\s*Last Update:"
  author_pattern  <- "^#\\s*Author:"
  
  # Replace matching lines with new metadata values
  lines <- sapply(lines, function(line) {
    if (grepl(version_pattern, line)) {
      return(paste0("# Version: ", new_version))
    } else if (grepl(date_pattern, line)) {
      return(paste0("# Last Update: ", new_release_date))
    } else if (grepl(author_pattern, line)) {
      return(paste0("# Author: ", new_author))
    } else {
      return(line)
    }
  }, USE.NAMES = FALSE)
  
  # Write the updated lines back to the file
  writeLines(lines, file_path)
  message(paste("Header updated in", file_path))
}

# Loop over each R script file and update its header
for (file in r_files) {
  update_script_header(file, version, release_date, author)
}

# -----------------------------------------------------------------------------
# STEP 4: Update README.md with New Version and Changelog
# -----------------------------------------------------------------------------

update_readme <- function(new_version, new_release_date, new_changelog) {
  readme_file <- "README.md"
  if (!file.exists(readme_file)) {
    message("README.md not found. Skipping README update.")
    return()
  }
  
  message("Updating README.md with new project metadata...")
  
  # Read the current contents of README.md
  readme_lines <- readLines(readme_file, warn = FALSE)
  
  # Define markers for a metadata block in the README
  start_marker <- "<!-- Project Metadata Start -->"
  end_marker   <- "<!-- Project Metadata End -->"
  
  # Create a new metadata block with the updated information
  metadata_block <- c(
    start_marker,
    paste("**Version:**", new_version),
    paste("**Release Date:**", new_release_date),
    "",
    "**Changelog:**",
    new_changelog,
    end_marker
  )
  
  # Check if a metadata block already exists in README.md
  start_idx <- which(readme_lines == start_marker)
  end_idx   <- which(readme_lines == end_marker)
  
  if (length(start_idx) > 0 && length(end_idx) > 0 && start_idx < end_idx) {
    # Replace the existing metadata block
    readme_lines <- c(
      readme_lines[1:(start_idx - 1)],
      metadata_block,
      readme_lines[(end_idx + 1):length(readme_lines)]
    )
  } else {
    # If no metadata block exists, prepend the new block at the top
    readme_lines <- c(metadata_block, "", readme_lines)
  }
  
  # Write the updated content back to README.md
  writeLines(readme_lines, readme_file)
  message("README.md updated with new project metadata.")
}

update_readme(version, release_date, changelog)

# -----------------------------------------------------------------------------
# STEP 5: Optionally Create an Annotated Git Tag for the New Release
# -----------------------------------------------------------------------------

create_git_release_tag <- function(new_version, new_release_date, new_changelog) {
  # Check for a .git directory to confirm that Git is initialized
  if (!dir.exists(".git")) {
    message("No .git directory found. Skipping Git tag creation.")
    return()
  }
  
  tag_name <- paste0("v", new_version)
  tag_message <- paste0("Release ", new_version, " - ", new_release_date, "\n\nChangelog:\n", new_changelog)
  
  message(paste("Creating Git tag:", tag_name))
  
  # Attempt to create the tag using usethis
  tryCatch({
    usethis::use_git_tag(tag = tag_name, message = tag_message)
    message("Git tag created successfully.")
  }, error = function(e) {
    message("Error creating Git tag with usethis: ", e$message)
    message("Attempting to create Git tag using system command...")
    
    # Fallback: use a system call to git
    cmd <- sprintf('git tag -a %s -m "%s"', tag_name, tag_message)
    system(cmd)
    message("Git tag created using system command.")
  })
}

if (create_git_tag) {
  create_git_release_tag(version, release_date, changelog)
} else {
  message("Git tag creation is disabled in the configuration.")
}

# -----------------------------------------------------------------------------
# Completion Message
# -----------------------------------------------------------------------------

message("Project metadata update complete.")
