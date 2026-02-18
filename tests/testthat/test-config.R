source(here::here("R/utils.R"))   # provides %||%
source(here::here("R/config.R"))

# Helper: build a valid base config list so individual tests only override
# the field they care about.
base_cfg <- function(...) {
  cfg <- list(
    years          = 22,
    filter_mode    = "year_only",
    filter_years   = c("d22"),
    filter_tables  = character(0),
    max_parallel   = 2,
    output_dir     = "output",
    log_dir        = "log",
    resume_previous = TRUE,
    verbose        = FALSE,
    max_retries    = 5L,
    initial_delay  = 1,
    max_delay      = 30,
    min_dl_delay   = 3
  )
  modifyList(cfg, list(...))
}

# --------------------------------------------------------------------------- #
# get_env()                                                                    #
# --------------------------------------------------------------------------- #
test_that("get_env reads an existing environment variable", {
  withr::local_envvar(c(NCES_TEST_ENV = "set-value"))
  expect_equal(get_env("NCES_TEST_ENV"), "set-value")
})

test_that("get_env returns default for missing optional variable", {
  withr::local_envvar(c(NCES_TEST_ENV = NA))
  expect_equal(
    get_env("NCES_TEST_ENV", required = FALSE, default = "fallback"),
    "fallback"
  )
})

test_that("get_env errors for missing required variable", {
  withr::local_envvar(c(NCES_TEST_ENV = NA))
  expect_error(get_env("NCES_TEST_ENV"), "Missing required environment variable")
})

# --------------------------------------------------------------------------- #
# validate_config() — years field                                              #
# --------------------------------------------------------------------------- #
test_that("validate_config coerces character years to numeric", {
  result <- validate_config(base_cfg(years = "22"))
  expect_true(is.numeric(result$years))
  expect_equal(result$years, 22)
})

# --------------------------------------------------------------------------- #
# validate_config() — filter_mode field                                        #
# --------------------------------------------------------------------------- #
test_that("validate_config accepts all valid filter_mode values", {
  for (mode in c("all", "year_only", "table_only", "custom")) {
    result <- validate_config(base_cfg(filter_mode = mode))
    expect_equal(result$filter_mode, mode)
  }
})

test_that("validate_config resets unknown filter_mode to 'all' with a warning", {
  expect_warning(
    result <- validate_config(base_cfg(filter_mode = "bogus")),
    regexp = "Invalid filter_mode"
  )
  expect_equal(result$filter_mode, "all")
})

# --------------------------------------------------------------------------- #
# validate_config() — filter_years prefix                                      #
# --------------------------------------------------------------------------- #
test_that("validate_config adds 'd' prefix when missing", {
  result <- validate_config(base_cfg(filter_years = c("22", "23")))
  expect_equal(result$filter_years, c("d22", "d23"))
})

test_that("validate_config does not double-prefix existing 'd' prefix", {
  result <- validate_config(base_cfg(filter_years = c("d22")))
  expect_equal(result$filter_years, "d22")
})

# --------------------------------------------------------------------------- #
# validate_config() — max_parallel                                             #
# --------------------------------------------------------------------------- #
test_that("validate_config resets negative max_parallel to 0 with a warning", {
  expect_warning(
    result <- validate_config(base_cfg(max_parallel = -5)),
    regexp = "Invalid max_parallel"
  )
  expect_equal(result$max_parallel, 0)
})

# --------------------------------------------------------------------------- #
# validate_config() — logical coercion                                         #
# --------------------------------------------------------------------------- #
test_that("validate_config coerces character 'TRUE'/'FALSE' to logical", {
  result <- validate_config(base_cfg(resume_previous = "TRUE", verbose = "FALSE"))
  expect_true(is.logical(result$resume_previous))
  expect_true(is.logical(result$verbose))
  expect_true(result$resume_previous)
  expect_false(result$verbose)
})
