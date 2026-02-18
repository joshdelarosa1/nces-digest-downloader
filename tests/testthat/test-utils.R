source(here::here("R/utils.R"))

# --------------------------------------------------------------------------- #
# %||% null-coalescing operator                                                #
# --------------------------------------------------------------------------- #
test_that("%||% returns left-hand side when non-NULL", {
  expect_equal("a" %||% "b", "a")
  expect_equal(42L %||% 0L, 42L)
  expect_equal(FALSE %||% TRUE, FALSE)
})

test_that("%||% returns right-hand side when left is NULL", {
  expect_equal(NULL %||% "b", "b")
  expect_equal(NULL %||% 99, 99)
})

test_that("%||% treats NA as non-NULL (NA is a valid value)", {
  expect_true(is.na(NA %||% "fallback"))
})

# --------------------------------------------------------------------------- #
# ensure_dir()                                                                 #
# --------------------------------------------------------------------------- #
test_that("ensure_dir creates a non-existent nested directory", {
  tmp  <- withr::local_tempdir()
  path <- file.path(tmp, "a", "b", "c")
  expect_false(dir.exists(path))
  ensure_dir(path)
  expect_true(dir.exists(path))
})

test_that("ensure_dir is idempotent for existing directories", {
  tmp <- withr::local_tempdir()
  expect_no_error(ensure_dir(tmp))
  expect_true(dir.exists(tmp))
})

test_that("ensure_dir returns the path invisibly", {
  tmp  <- withr::local_tempdir()
  path <- file.path(tmp, "ret_test")
  ret  <- ensure_dir(path)
  expect_equal(ret, path)
})

# --------------------------------------------------------------------------- #
# calculate_file_hash()                                                        #
# --------------------------------------------------------------------------- #
test_that("calculate_file_hash returns a 32-char lowercase hex MD5 string", {
  tmp <- withr::local_tempfile()
  writeLines("hello world", tmp)
  h <- calculate_file_hash(tmp)
  expect_match(h, "^[0-9a-f]{32}$")
})

test_that("calculate_file_hash is deterministic for the same content", {
  tmp <- withr::local_tempfile()
  writeLines("consistent content", tmp)
  expect_equal(calculate_file_hash(tmp), calculate_file_hash(tmp))
})

test_that("calculate_file_hash differs for different file contents", {
  f1 <- withr::local_tempfile(); writeLines("aaa", f1)
  f2 <- withr::local_tempfile(); writeLines("bbb", f2)
  expect_false(calculate_file_hash(f1) == calculate_file_hash(f2))
})

test_that("calculate_file_hash returns NA and warns for missing file", {
  expect_warning(h <- calculate_file_hash("/no/such/path/file.txt"))
  expect_true(is.na(h))
})
