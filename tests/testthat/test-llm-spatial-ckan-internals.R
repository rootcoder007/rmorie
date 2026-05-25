# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2QQ: tests for remaining internal helpers across llm.R,
# spatial_voting.R, ingest_ckan.R, ingest_statcan.R, ipw.R,
# sprott_doob.R, doob_trends.R, mrm_arsau.R, mrm_diagnostics.R,
# fairness_temporal.R, fairness_cityprofile.R.

# ====================================================================== llm.R

test_that(".morie_llm_messages builds a system/user message pair", {
  out <- morie:::.morie_llm_messages("hello")
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_equal(out[[1]]$role, "system")
  expect_equal(out[[2]]$role, "user")
  expect_equal(out[[2]]$content, "hello")
})

test_that(".morie_llm_messages embeds context into the system prompt", {
  out <- morie:::.morie_llm_messages("q?",
                                       context = list(KEY = "VAL_XYZ"))
  expect_match(out[[1]]$content, "KEY: VAL_XYZ")
})

test_that(".morie_llm_messages accepts an explicit system_prompt", {
  out <- morie:::.morie_llm_messages("q?",
                                       system_prompt = "CUSTOM-SYS-MARKER")
  expect_equal(out[[1]]$content, "CUSTOM-SYS-MARKER")
})

test_that(".morie_llm_extract_text returns content from the first choice", {
  data <- list(choices = list(list(message = list(content = "hi"))))
  expect_equal(morie:::.morie_llm_extract_text(data), "hi")
})

test_that(".morie_llm_extract_text returns '' on empty choices", {
  expect_equal(morie:::.morie_llm_extract_text(list(choices = list())),
               "")
})

test_that(".morie_llm_extract_text returns '' when message is missing", {
  data <- list(choices = list(list()))
  expect_equal(morie:::.morie_llm_extract_text(data), "")
})

test_that(".morie_llm_local_fallback returns the canonical fallback text", {
  out <- morie:::.morie_llm_local_fallback("ignored prompt")
  expect_type(out, "character")
  expect_match(out, "local-only mode")
  expect_match(out, "morie")
})

test_that(".morie_llm_freeapi_model honours moriefam env override", {
  old <- Sys.getenv("moriefam", unset = NA)
  Sys.setenv(moriefam = "custom-model-xyz")
  on.exit({
    if (!is.na(old)) Sys.setenv(moriefam = old)
    else Sys.unsetenv("moriefam")
  }, add = TRUE)
  expect_equal(morie:::.morie_llm_freeapi_model(),
               "custom-model-xyz")
})

test_that(".morie_llm_freeapi_model falls back to DEFAULT_FREEAPI_MODEL", {
  old <- Sys.getenv("moriefam", unset = NA)
  Sys.unsetenv("moriefam")
  on.exit(if (!is.na(old)) Sys.setenv(moriefam = old), add = TRUE)
  out <- morie:::.morie_llm_freeapi_model()
  expect_true(is.character(out) && nzchar(out))
})

test_that(".morie_llm_messages_to_prompt assembles roles into one string", {
  msgs <- list(
    list(role = "system",    content = "SYS-X"),
    list(role = "user",      content = "U1"),
    list(role = "assistant", content = "A1"))
  out <- morie:::.morie_llm_messages_to_prompt(msgs)
  expect_type(out, "character")
  expect_match(out, "\\[System: SYS-X\\]")
  expect_match(out, "U1")
  expect_match(out, "Assistant: A1")
})

test_that(".morie_llm_strip_think removes <think>...</think> + trims", {
  out <- morie:::.morie_llm_strip_think(
    "  <think>chain-of-thought</think>final answer  ")
  expect_equal(out, "final answer")
})

test_that(".morie_llm_strip_think trims surrounding whitespace", {
  expect_equal(morie:::.morie_llm_strip_think("  final answer  "),
               "final answer")
})

test_that(".morie_llm_strip_think strips multi-line <think> block", {
  out <- morie:::.morie_llm_strip_think(
    "<think>line1\nline2</think>\n\nfinal")
  expect_equal(out, "final")
})

test_that(".morie_llm_strip_think no-op on plain text", {
  expect_equal(morie:::.morie_llm_strip_think("plain answer"),
               "plain answer")
})

# ============================================================ spatial_voting.R

test_that(".sv_as_matrix returns NULL on NULL input", {
  expect_null(morie:::.sv_as_matrix(NULL))
})

test_that(".sv_as_matrix coerces a data.frame to a double matrix", {
  out <- morie:::.sv_as_matrix(data.frame(a = 1:3, b = 4:6))
  expect_true(is.matrix(out))
  expect_equal(storage.mode(out), "double")
  expect_equal(dim(out), c(3L, 2L))
})

test_that(".sv_nanmean_col returns column-wise means ignoring NA", {
  M <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 2L)
  out <- morie:::.sv_nanmean_col(M)
  expect_length(out, 3L)
  expect_equal(out[1], 1)
  expect_equal(out[2], 3.5)
})

test_that(".sv_pairwise_dist returns a symmetric distance matrix", {
  set.seed(1L); X <- matrix(stats::rnorm(20), 10L, 2L)
  D <- morie:::.sv_pairwise_dist(X)
  expect_true(is.matrix(D))
  expect_equal(dim(D), c(10L, 10L))
  expect_equal(D, t(D), tolerance = 1e-10)
  expect_equal(unname(diag(D)), rep(0, 10L))
})

test_that(".sv_double_centering returns an n x n matrix", {
  D <- matrix(stats::runif(25), 5L, 5L)
  D <- (D + t(D)) / 2; diag(D) <- 0
  out <- morie:::.sv_double_centering(D)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(5L, 5L))
})

test_that(".sv_safe_pinv inverts an invertible matrix back to identity", {
  set.seed(2L)
  M <- diag(c(2, 3, 4))
  inv <- morie:::.sv_safe_pinv(M)
  expect_equal(M %*% inv, diag(3L), tolerance = 1e-8)
})

test_that(".sv_safe_pinv zero-eigenvalue component is suppressed", {
  # Rank-1 matrix: only one non-zero singular value; pinv should not blow up.
  v <- c(1, 2, 3)
  M <- v %*% t(v)
  out <- morie:::.sv_safe_pinv(M)
  expect_true(all(is.finite(out)))
})

test_that(".sv_isotonic_pava is monotone non-decreasing", {
  set.seed(3L)
  y <- c(3, 1, 4, 1, 5, 9, 2, 6)
  out <- morie:::.sv_isotonic_pava(y)
  expect_length(out, length(y))
  expect_true(all(diff(out) >= -1e-10))
})

test_that(".sv_isotonic_pava no-op on already-monotone input", {
  y <- c(1, 2, 3, 4)
  expect_equal(morie:::.sv_isotonic_pava(y), y, tolerance = 1e-10)
})

test_that(".sv_have_cpp returns logical", {
  expect_type(morie:::.sv_have_cpp(), "logical")
  expect_type(morie:::.sv_have_cpp("nonexistent_cpp_symbol_xyz"),
              "logical")
  expect_false(morie:::.sv_have_cpp("nonexistent_cpp_symbol_xyz"))
})

# ============================================================== ingest_ckan.R

test_that(".morie_ckan_sniff_format honours explicit as_format", {
  expect_equal(
    morie:::.morie_ckan_sniff_format("https://example/x.csv",
                                       as_format = "JSON"),
    "json")
})

test_that(".morie_ckan_sniff_format falls back to extension", {
  expect_equal(
    morie:::.morie_ckan_sniff_format("https://example/x.csv"),
    "csv")
  expect_equal(
    morie:::.morie_ckan_sniff_format("https://example/x.tsv?token=1"),
    "tsv")
})

test_that(".morie_ckan_sniff_format defaults to csv when no extension", {
  expect_equal(
    morie:::.morie_ckan_sniff_format("https://example/data"),
    "csv")
})

test_that(".morie_ckan_read_path reads a CSV resource into a data.frame", {
  tmp <- tempfile("ckan_csv_", fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")),
                   tmp, row.names = FALSE)
  out <- morie:::.morie_ckan_read_path(tmp, "csv")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
})

test_that(".morie_ckan_call dispatches with mocked HTTP backend", {
  # 3YY collapsed .morie_ckan_build_req + .morie_ckan_call into a
  # single .morie_ckan_call. Mock the HTTP layer it routes through
  # and verify the request shape (portal + action + params) lands
  # in the right place.
  testthat::local_mocked_bindings(
    .morie_dataset_http_text = function(url, query = NULL, ...) {
      # Return a JSON string the caller will fromJSON-parse.
      jsonlite::toJSON(list(success = TRUE,
                             result = list(captured_url = url)),
                        auto_unbox = TRUE)
    },
    .package = "morie"
  )
  out <- morie:::.morie_ckan_call("open.toronto.ca", "package_show",
                                    params = list(id = "demo"))
  expect_type(out, "list")
  expect_true(grepl("package_show", as.character(out)))
})

# ====================================================================== ipw.R

test_that(".weighted_prop computes weighted proportion", {
  expect_equal(morie:::.weighted_prop(c(1, 0, 1, 0), c(1, 1, 1, 1)),
               0.5)
  expect_equal(morie:::.weighted_prop(c(1, 0, 1, 0), c(3, 1, 1, 1)),
               4 / 6, tolerance = 1e-10)
})

test_that(".ess returns effective sample size", {
  # All-equal weights -> ESS = N.
  expect_equal(morie:::.ess(rep(1, 10L)), 10)
  # Concentrated weights -> ESS < N.
  out <- morie:::.ess(c(rep(0.01, 9L), 100))
  expect_true(out < 10 && out > 0)
})

# ============================================================== sprott_doob.R

test_that(".morie_siu_rich builds a morie_siu_result list", {
  out <- morie:::.morie_siu_rich(title = "SIU",
                                   summary_lines = list(N = 100))
  expect_type(out, "list")
  expect_s3_class(out, "morie_siu_result")
  expect_s3_class(out, "morie_rich_result")
})

# ============================================================== doob_trends.R

test_that(".doob_result builds a morie_result list", {
  out <- morie:::.doob_result(title = "Doob trends",
                                summary_lines = list(Years = "2018-2024"))
  expect_type(out, "list")
  expect_s3_class(out, "morie_result")
})

# ================================================================ mrm_arsau.R

test_that(".arsau_wrap aggregates sub-results into a wrapped list", {
  sub_results <- list(
    summary = list(warnings = character(0)),
    timing  = list(warnings = "minor cohort shrinkage"))
  out <- morie:::.arsau_wrap(
    title = "Wrap",
    call = "demo",
    sub_results = sub_results,
    data = data.frame(x = 1:3),
    sidecar = list(),
    year_or_range = "2024",
    kind = "main_records",
    language = "en",
    is_valid = FALSE)
  expect_type(out, "list")
})

# ========================================================== mrm_diagnostics.R

test_that(".morie_logistic_propensity returns ps in (0,1)", {
  set.seed(4L); n <- 100L
  D <- stats::rbinom(n, 1L, 0.5)
  X <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  e <- morie:::.morie_logistic_propensity(D, X)
  expect_length(e, n)
  expect_true(all(e > 0 & e < 1))
})

# ========================================================= fairness_temporal.R

test_that(".morie_fairness_mean_finite ignores Inf/NaN/NA", {
  expect_equal(morie:::.morie_fairness_mean_finite(c(1, 2, 3, NA,
                                                       Inf, NaN)),
               2)
  expect_true(is.na(morie:::.morie_fairness_mean_finite(
    c(NA, NaN, Inf))))
})

# ======================================================= fairness_cityprofile.R

test_that(".morie_fairness_init_registry seeds the 'generic' profile", {
  morie:::.morie_fairness_init_registry()
  reg <- get(".morie_fairness_registry", envir = asNamespace("morie"))
  expect_true(exists("generic", envir = reg, inherits = FALSE))
})

# ============================================================= ingest_statcan.R

test_that(".morie_statcan_csv_from_zip extracts a CSV from a zip", {
  tmp_csv <- tempfile("statcan_demo_", fileext = ".csv")
  utils::write.csv(data.frame(REF_DATE = 2020:2022, VALUE = c(1, 2, 3)),
                   tmp_csv, row.names = FALSE)
  on.exit(unlink(tmp_csv), add = TRUE)
  zip_path <- tempfile("statcan_demo_", fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)
  withr::local_dir(dirname(tmp_csv))
  utils::zip(zip_path, files = basename(tmp_csv))
  out <- morie:::.morie_statcan_csv_from_zip(zip_path)
  expect_s3_class(out, "data.frame")
  expect_true("REF_DATE" %in% names(out))
  expect_equal(nrow(out), 3L)
})

test_that(".morie_statcan_csv_from_zip errors on empty archive", {
  zip_path <- tempfile("statcan_empty_", fileext = ".zip")
  on.exit(unlink(zip_path), add = TRUE)
  # Build an empty zip via a no-files invocation -> archive with no CSVs.
  txt <- tempfile("notcsv_", fileext = ".txt")
  writeLines("hi", txt)
  on.exit(unlink(txt), add = TRUE)
  withr::local_dir(dirname(txt))
  utils::zip(zip_path, files = basename(txt))
  expect_error(morie:::.morie_statcan_csv_from_zip(zip_path),
               regexp = "No \\.csv")
})
