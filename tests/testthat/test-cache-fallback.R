# The cache falls back to the file backend when DBI (or a driver) is absent,
# instead of failing; DBI remains the opt-in SQL path.

test_that("a SQL cache request without DBI falls back to the file backend with a message", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  withr::local_envvar(MORIE_CACHE_BACKEND = "")
  testthat::local_mocked_bindings(.package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie", .morie_dbi_available = function() FALSE)
  tmp <- tempfile(fileext = ".db")
  expect_message(morie_cache_store(df, "fallback_tbl", db_path = tmp), "file backend")
  expect_false(file.exists(tmp))
  got <- suppressMessages(morie_cache_load("fallback_tbl"))
  expect_equal(nrow(got), 3L)
  expect_equal(got$a, 1:3)
})

test_that("with DBI available the SQL path is taken", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  df <- data.frame(a = c(2.5, 3.5))
  tmp <- tempfile(fileext = ".db")
  expect_silent(morie_cache_store(df, "sql_tbl", db_path = tmp))
  expect_true(file.exists(tmp))
  expect_equal(morie_cache_load("sql_tbl", db_path = tmp)$a, c(2.5, 3.5))
})
