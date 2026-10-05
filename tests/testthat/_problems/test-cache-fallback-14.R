# Extracted from test-cache-fallback.R:14

# test -------------------------------------------------------------------------
testthat::skip_on_covr()
df <- data.frame(a = 1:3, b = c("x", "y", "z"))
withr::local_envvar(MORIE_CACHE_BACKEND = "")
testthat::local_mocked_bindings(.package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie", .morie_dbi_available = function() FALSE)
tmp <- tempfile(fileext = ".db")
expect_message(morie_cache_store(df, "fallback_tbl", db_path = tmp), "file backend")
expect_false(file.exists(tmp))
got <- suppressMessages(morie_cache_load("fallback_tbl"))
expect_equal(nrow(got), 3L)
