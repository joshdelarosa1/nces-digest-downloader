#!/usr/bin/env Rscript
# CHANGELOG
# - 2026-02-17: Added offline security scan for linting, package checks, and risky code patterns.
# - 2026-02-17: Initial version.
# ==========================================================================
# Script: scripts/scan_code.R
# Purpose: Run offline code-quality and security checks and write machine-readable reports.
# Inputs: Repository files under R/, code/, tests/, and project root.
# Outputs: reports/code_scan.txt, reports/code_scan.json, reports/r_cmd_check.txt.
# Assumptions:
#   - The script is run from the repository root.
#   - Network calls are not required for local scanning.
#   - Optional tools (`lintr`, `goodpractice`) may be unavailable.
# How to run: Rscript scripts/scan_code.R
# Notes: Exits non-zero when critical or high findings are detected.
# ==========================================================================

suppressPackageStartupMessages({
  if (requireNamespace("lintr", quietly = TRUE)) {
    library(lintr)
  }
})

# ---- Setup -----------------------------------------------------------------
# What this section does:
#   - Initializes report paths and helper containers for findings.
# Why it matters:
#   - Keeps scanner output deterministic and easy to consume in CI.
# Outputs:
#   - Report directory and in-memory findings list.
report_dir <- "reports"
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
text_report <- file.path(report_dir, "code_scan.txt")
json_report <- file.path(report_dir, "code_scan.json")
check_report <- file.path(report_dir, "r_cmd_check.txt")

severity_rank <- c(info = 0L, medium = 1L, high = 2L, critical = 3L)
findings <- data.frame(
  severity = character(),
  category = character(),
  file = character(),
  line = integer(),
  detail = character(),
  stringsAsFactors = FALSE
)

append_finding <- function(severity, category, file, line_number, detail) {
  findings <<- rbind(
    findings,
    data.frame(
      severity = severity,
      category = category,
      file = file,
      line = as.integer(line_number),
      detail = detail,
      stringsAsFactors = FALSE
    )
  )
}

# ---- Load data --------------------------------------------------------------
# What this section does:
#   - Enumerates candidate source files to scan.
# Outputs:
#   - Character vector of file paths.
source_files <- unique(c(
  list.files(".", pattern = "\\.R$", recursive = FALSE, full.names = TRUE),
  list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("code", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("tests", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files(".", pattern = "\\.(Rmd|qmd)$", recursive = TRUE, full.names = TRUE)
))
source_files <- source_files[file.exists(source_files)]

# ---- Validate assumptions ---------------------------------------------------
# What this section does:
#   - Fails fast if no candidate files are found.
# Why it matters:
#   - Prevents false "pass" results from empty scan targets.
if (length(source_files) == 0) {
  stop("No source files found for scanning.", call. = FALSE)
}

# ---- Transform --------------------------------------------------------------
# What this section does:
#   - Runs lintr, package checks, and risky-pattern matching.
# Why it matters:
#   - Identifies code quality and security issues before public release.
if (requireNamespace("lintr", quietly = TRUE)) {
  project_linters <- lintr::linters_with_defaults(
    assignment_linter = lintr::assignment_linter(),
    object_usage_linter = lintr::object_usage_linter(),
    undesirable_function_linter = lintr::undesirable_function_linter(
      c("system", "eval", "parse", "setwd", "shell", "download.file")
    )
  )
  for (source_path in source_files) {
    lint_results <- tryCatch(
      lintr::lint(source_path, linters = project_linters),
      error = function(e) e
    )
    if (inherits(lint_results, "error")) {
      append_finding("high", "lintr", source_path, 1L, conditionMessage(lint_results))
      next
    }
    if (length(lint_results) > 0) {
      for (lint_item in lint_results) {
        append_finding(
          "medium",
          "lintr",
          lint_item$filename,
          lint_item$line_number,
          lint_item$message
        )
      }
    }
  }
} else {
  append_finding("medium", "lintr", "<scanner>", 1L, "lintr not installed")
}

if (file.exists("DESCRIPTION") && file.exists("NAMESPACE")) {
  check_status <- system2(
    "R",
    c("CMD", "check", "--as-cran", "--no-manual", "."),
    stdout = check_report,
    stderr = check_report
  )
  check_lines <- readLines(check_report, warn = FALSE)
  warning_lines <- grep("(WARNING|ERROR)", check_lines, value = TRUE)
  if (check_status != 0 || length(warning_lines) > 0) {
    append_finding("high", "rcmdcheck", "DESCRIPTION", 1L, "R CMD check reported warnings/errors")
  }

  if (requireNamespace("goodpractice", quietly = TRUE)) {
    gp_status <- system2(
      "Rscript",
      c("-e", "goodpractice::gp()"),
      stdout = file.path(report_dir, "goodpractice.txt"),
      stderr = file.path(report_dir, "goodpractice.txt")
    )
    if (gp_status != 0) {
      append_finding("medium", "goodpractice", "DESCRIPTION", 1L, "goodpractice reported issues")
    }
  } else {
    append_finding("medium", "goodpractice", "DESCRIPTION", 1L, "goodpractice not installed")
  }
} else if (file.exists("DESCRIPTION")) {
  append_finding("medium", "rcmdcheck", "DESCRIPTION", 1L, "Skipped: NAMESPACE file not found")
}

pattern_spec <- list(
  list(name = "system_call", regex = "\\bsystem\\s*\\(", severity = "high"),
  list(name = "dynamic_eval", regex = "\\b(eval|parse)\\s*\\(", severity = "critical"),
  list(name = "setwd_call", regex = "\\bsetwd\\s*\\(", severity = "medium"),
  list(name = "download_file", regex = "download\\.file\\s*\\(", severity = "medium"),
  list(name = "tls_disabled", regex = "ssl_verifypeer\\s*=\\s*FALSE", severity = "critical"),
  list(name = "insecure_http", regex = "http://", severity = "high")
)

all_source_lines <- character()
for (source_path in source_files) {
  all_source_lines <- c(all_source_lines, readLines(source_path, warn = FALSE))
}

has_shiny <- file.exists("app.R") ||
  any(grepl("shiny::", all_source_lines, fixed = TRUE))
has_plumber <- any(grepl("plumber::", all_source_lines, fixed = TRUE)) ||
  any(grepl("plumber", basename(source_files), ignore.case = TRUE))

if (has_shiny || has_plumber) {
  pattern_spec <- c(
    pattern_spec,
    list(list(
      name = "runapp_public_http",
      regex = "runApp\\s*\\(.*host\\s*=\\s*[\"']0\\.0\\.0\\.0[\"'].*port\\s*=\\s*80",
      severity = "high"
    ))
  )
}

for (path in source_files) {
  file_lines <- readLines(path, warn = FALSE)
  for (line_index in seq_along(file_lines)) {
    current_line <- file_lines[[line_index]]
    for (rule_item in pattern_spec) {
      if (grepl(rule_item$regex, current_line, perl = TRUE)) {
        if (basename(path) == "scan_code.R" && grepl("list(name =", current_line, fixed = TRUE)) {
          next
        }
        if (rule_item$name == "insecure_http" &&
            grepl("openxmlformats|purl.org|w3.org", current_line)) {
          next
        }
        append_finding(rule_item$severity, rule_item$name, path, line_index, trimws(current_line))
      }
    }
  }
}

# ---- Output -----------------------------------------------------------------
# What this section does:
#   - Writes human-readable and JSON scan summaries.
# Outputs:
#   - `reports/code_scan.txt` and `reports/code_scan.json`.
severity_counts <- table(findings$severity)
critical_count <- if ("critical" %in% names(severity_counts)) severity_counts[["critical"]] else 0L
high_count <- if ("high" %in% names(severity_counts)) severity_counts[["high"]] else 0L

summary_lines <- c(
  sprintf("scan_time=%s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  sprintf("files_scanned=%d", length(source_files)),
  sprintf("findings_total=%d", nrow(findings)),
  sprintf("critical_count=%d", critical_count),
  sprintf("high_count=%d", high_count)
)
writeLines(c(summary_lines, "", capture.output(print(findings))), con = text_report)

if (requireNamespace("jsonlite", quietly = TRUE)) {
  summary_list <- list(
    scan_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    files_scanned = length(source_files),
    findings_total = nrow(findings),
    critical_count = as.integer(critical_count),
    high_count = as.integer(high_count)
  )
  jsonlite::write_json(
    list(summary = summary_list, findings = findings),
    path = json_report,
    auto_unbox = TRUE,
    pretty = TRUE
  )
} else {
  writeLines("{\"summary\":\"jsonlite not installed\"}", con = json_report)
}
message("Wrote code scan reports: ", text_report, ", ", json_report)

# ---- Sanity checks ----------------------------------------------------------
# What this section does:
#   - Exits non-zero on critical/high findings.
# Why it matters:
#   - Enforces fail-fast behavior in local and CI checks.
if (critical_count > 0 || high_count > 0) {
  quit(status = 1)
}
