# Load only what construct_direct_excel_url needs, in an isolated environment
# so that the guard-checks at the top of find_excel_path.R don't abort sourcing.
local({
  NCES_BASE_URL <- "https://nces.ed.gov/programs/digest/"
  source_lines <- readLines(here::here("code/find_excel_path.R"), warn = FALSE)
  main_section_line <- which(grepl("Main Processing Section", source_lines))[1]
  function_only_script <- withr::local_tempfile(fileext = ".R")
  writeLines(source_lines[seq_len(main_section_line - 2)], function_only_script)
  source(function_only_script, local = TRUE)

  # --------------------------------------------------------------------------- #
  # construct_direct_excel_url()                                                 #
  # --------------------------------------------------------------------------- #
  test_that("construct_direct_excel_url builds an .xlsx URL", {
    row <- list(year = "d24", table_number = "101.10")
    url <- construct_direct_excel_url(row)
    expect_match(url, "\\.xlsx$")
    expect_match(url, "nces\\.ed\\.gov")
    expect_match(url, "d24")
  })

  test_that("construct_direct_excel_url embeds the table number correctly", {
    row <- list(year = "d22", table_number = "204.30")
    url <- construct_direct_excel_url(row)
    expect_match(url, "tabn20430\\.xlsx$")
  })

  test_that("construct_direct_excel_url returns NA for table number without a dot", {
    row <- list(year = "d24", table_number = "101")
    expect_true(is.na(construct_direct_excel_url(row)))
  })

  test_that("construct_direct_excel_url returns NA for NA table number", {
    row <- list(year = "d24", table_number = NA_character_)
    expect_true(is.na(construct_direct_excel_url(row)))
  })

  test_that("construct_direct_excel_url returns NA when table number has more than one dot", {
    row <- list(year = "d24", table_number = "101.10.5")
    expect_true(is.na(construct_direct_excel_url(row)))
  })
})
