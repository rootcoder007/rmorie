# SPDX-License-Identifier: AGPL-3.0-or-later
# Round-8 regressions: every expectation recomputed in the test body (base R, or the formula).

test_that("Kendall and Wilcoxon use base R's exact defaults", {
  set.seed(8)
  x <- rnorm(30)
  y <- rnorm(30)
  k <- morie_kendall_tau(x, y)
  expect_equal(k$p_value, stats::cor.test(x, y, method = "kendall")$p.value, tolerance = 1e-14)
  expect_equal(morie_kendall_tau(x, y, exact = FALSE)$p_value,
               stats::cor.test(x, y, method = "kendall", exact = FALSE)$p.value, tolerance = 1e-14)
  w <- morie_wilcoxon_signed_rank_test(x[1:20], y[1:20])
  expect_equal(w$p_value, stats::wilcox.test(x[1:20], y[1:20], paired = TRUE)$p.value, tolerance = 1e-14)
})

test_that("the IPW standard error is the HC3 sandwich of the weighted regression y ~ t", {
  set.seed(9)
  n <- 400
  x <- rnorm(n)
  t <- rbinom(n, 1, stats::plogis(0.5 * x))
  y <- 1 + 0.8 * t + x + rnorm(n)
  d <- data.frame(t = t, y = y, x = x)
  a <- morie_estimate_ate(d, "t", "y", "x")
  ps <- morie_estimate_propensity_scores(d, "t", "x")
  w <- t / ps + (1 - t) / (1 - ps)
  # the sandwich written out: OLS on sqrt(w)-scaled rows, HC3 meat
  X <- cbind(sqrt(w), sqrt(w) * t)
  bread <- solve(crossprod(X))
  e <- as.numeric(sqrt(w) * y - X %*% (bread %*% crossprod(X, sqrt(w) * y)))
  h <- rowSums((X %*% bread) * X)
  se <- sqrt((bread %*% crossprod(X * (e / (1 - h))) %*% bread)[2, 2])
  expect_equal(a$se, se, tolerance = 1e-12)
  expect_equal(a$ate, stats::weighted.mean(y[t == 1], w[t == 1]) - stats::weighted.mean(y[t == 0], w[t == 0]),
               tolerance = 1e-12)
})

test_that("DML folds come from the shared splitmix64 uniforms, one split for both nuisances", {
  f <- rmorie:::.morie_dml_folds(10, 3, 42)
  u <- rmoriebricklayer::core_uniforms(10, 42)
  expect_identical(f[order(u, method = "radix")], rep_len(1:3, 10))
  expect_identical(rmorie:::.morie_dml_folds(10, 3, 42), f)
})

test_that("an invalid Hawkes kernel/method pair is an error, as in Python", {
  df <- data.frame(OCC_DATE = as.character(as.Date("2024-01-01") + 0:149))
  expect_error(morie_tps_hawkes_advanced_fit(df, kernel = "exponential", baseline = "constant", method = "soe"),
               "completely monotone")
  expect_error(morie_tps_hawkes_advanced_fit(df, kernel = "gamma", baseline = "sinusoidal", method = "inar"),
               "stationary")
  expect_error(morie_tps_hawkes_advanced_fit(df, kernel = "gamma", method = "nope"), "method must be one of")
})

test_that("morie_hawkes_fit fits 50+ events with the bricklayer core, in this function's Lomax convention", {
  set.seed(10)
  ev <- sort(cumsum(stats::rexp(120, rate = 2)))
  fe <- morie_hawkes_fit(ev, kernel = "exponential")
  core <- rmoriebricklayer::core_hawkes_fit(ev, ev[length(ev)], "exponential")
  expect_identical(fe$backend, "rmoriebricklayer core")
  expect_equal(unname(fe$estimate), unname(core$theta), tolerance = 1e-12)
  expect_equal(fe$loglik, -core$nll, tolerance = 1e-12)
  fl <- morie_hawkes_fit(ev, kernel = "lomax")
  cl <- suppressWarnings(rmoriebricklayer::core_hawkes_fit(ev, ev[length(ev)], "lomax"))
  expect_equal(unname(fl$estimate[["alpha"]]), cl$theta[[3]] + 1, tolerance = 1e-12)
  if ("alpha" %in% cl$at_bound) expect_match(fl$note, "exponential")
})

test_that("CSV resources in Windows-1252 are read, and a bilingual CKAN package keeps both copies", {
  f <- tempfile(fileext = ".csv")
  writeBin(c(charToRaw("name,n\nO"), as.raw(0x92), charToRaw("Neil,3\n")), f)
  expect_identical(rmorie:::.morie_text_encoding(f), "windows-1252")
  d <- rmorie:::.morie_ckan_read_path(f, "csv")
  expect_identical(d$name, "O’Neil")
  g <- tempfile(fileext = ".csv")
  writeLines(enc2utf8("name,n\nO’Neil,3"), g, useBytes = TRUE)
  expect_identical(rmorie:::.morie_text_encoding(g), "UTF-8")
  res <- data.frame(name = c("Payments", "Payments", "Terms"), format = c("CSV", "CSV", "TXT"),
                    language = c("en", "fr", "en"), url = c("https://x/a.csv", "https://x/b.csv", "https://x/c.txt"),
                    id = c("a", "b", "c"), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(...) list(result = list(resources = res)),
    .morie_dataset_http_bytes = function(url, ...) charToRaw(paste0("v\n", basename(url), "\n")),
    .package = "rmorie"
  )
  p <- morie_datasets_ckan_package("https://example.org", "pkg")
  expect_identical(names(p), c("Payments (en)", "Payments (fr)"))
  expect_identical(p[["Payments (fr)"]]$v, "b.csv")
})

test_that("analyze tps runs on files and is an error without datasets", {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(OCC_DATE = "2024-01-01", MCI_CATEGORY = "Assault"), f, row.names = FALSE)
  capture.output(r <- cli_main("tps", "{}"))
  expect_identical(r$status, "error")
  seen <- NULL
  testthat::local_mocked_bindings(morie_tps_analyze_all = function(dfs, ...) {
    seen <<- dfs
    list(ok = TRUE)
  }, .package = "rmorie")
  capture.output(cli_main("tps", sprintf("{\"data\":\"%s\"}", f)))
  expect_identical(names(seen), tools::file_path_sans_ext(basename(f)))
  expect_identical(seen[[1]]$MCI_CATEGORY, "Assault")
})

test_that("a loader's cache is optional: an unwritable cache directory skips it with a message", {
  skip_on_os("windows")
  d <- tempfile("rocache")
  dir.create(d)
  Sys.chmod(d, "0555")
  on.exit(Sys.chmod(d, "0755"), add = TRUE)
  skip_if(file.access(d, 2L) == 0L, "running as root: directories are always writable")
  # the file backend: a morie.db left in tempdir() by another test would route this to SQL
  withr::local_envvar(MORIE_CACHE_DIR = d, MORIE_CACHE_BACKEND = "rds")
  expect_message(rmorie:::.morie_cache_store_soft(data.frame(a = 1), "t1"), "cache skipped for t1")
  expect_error(morie_cache_store(data.frame(a = 1), "t1"), "not writable")
})

test_that("workbook cells: merged labels carried down, Excel number noise cleaned", {
  df <- data.frame(J = c("Canada", NA, NA, "Ontario", NA), k = c("#", "1", "2", "#", "1"),
                   p = c("100", "51.959413779999998", "17.290320609999998", "100", "48.1"),
                   stringsAsFactors = FALSE)
  out <- rmorie:::.morie_xlsx_tidy_numbers(rmorie:::.morie_xlsx_fill_merged(df))
  expect_identical(out$J, c("Canada", "Canada", "Canada", "Ontario", "Ontario"))
  expect_identical(out$p, c(100, 51.95941378, 17.29032061, 100, 48.1))
  expect_identical(out$k, c("#", "1", "2", "#", "1"))
  f <- tempfile(fileext = ".csv")
  rmorie:::.morie_write_csv_minimal(data.frame(a = c("x,y", "q\"r", NA), b = c(1.5, NA, 51.95941378)), f)
  expect_identical(readLines(f), c("a,b", "\"x,y\",1.5", "\"q\"\"r\",", ",51.95941378"))
})

test_that("spwkth's integrated density is the exact omega integral, in milliseconds", {
  t0 <- proc.time()[["elapsed"]]
  r <- spwkth(function(h) exp(-2 * abs(h)), omega = c(0, 1, 3))
  expect_lt(proc.time()[["elapsed"]] - t0, 5)
  h <- seq(-200, 200, length.out = 40001)
  dh <- h[2] - h[1]
  wt <- rep(dh, length(h))
  wt[c(1, length(h))] <- dh / 2
  W <- 0.5 * pi / dh
  kern <- ifelse(h == 0, 2 * W, 2 * sin(W * h) / h)
  expect_equal(r$integrated_density, sum(wt * exp(-2 * abs(h)) * kern) / (2 * pi), tolerance = 1e-12)
})

test_that("download-bootstrap keeps a cached table instead of fetching it again", {
  fetched <- 0L
  testthat::local_mocked_bindings(
    morie_cache_load = function(table_name, ...) data.frame(w1 = 1:3),
    morie_fetch_ckan = function(...) {
      fetched <<- fetched + 1L
      data.frame(w1 = 1)
    },
    .package = "rmorie"
  )
  expect_message(n <- morie_download_bootstrap("ocs22bt"), "already cached")
  expect_identical(n, 1L)
  expect_identical(fetched, 0L)
})

test_that("a CKAN datastore that answers with an error status is reported as such, not as unreachable", {
  testthat::local_mocked_bindings(
    .morie_http_get_with_status = function(url, ...) list(status_code = 500L, body = ""),
    .morie_ckan_resource_file = function(...) data.frame(a = 1),
    .package = "rmorie"
  )
  expect_message(d <- morie_fetch_ckan("cpads", limit = 10, resource_id = "r1", db_path = tempfile(fileext = ".db")),
                 "answered HTTP 500")
  expect_identical(nrow(d), 1L)
})
