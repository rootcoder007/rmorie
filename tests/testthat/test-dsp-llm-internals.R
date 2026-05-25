# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2PP: tests for internal helpers across dsp_filters.R,
# dsp_spectral.R, dsp_detection.R, dsp_waveform.R, mrm_kulldorff.R,
# mrm_lisa.R, fairness_metrics.R, ingest_bigquery.R, and llm.R.

# ============================================================== dsp_filters.R

test_that(".morie_dsp_cpp_ok returns logical", {
  expect_type(morie:::.morie_dsp_cpp_ok("nonexistent_symbol_xyz"),
              "logical")
  expect_false(morie:::.morie_dsp_cpp_ok("nonexistent_symbol_xyz"))
})

test_that(".same_convolve returns length(x) output", {
  x <- 1:10
  k <- c(0.25, 0.5, 0.25)
  out <- morie:::.same_convolve(x, k)
  expect_length(out, length(x))
  expect_true(is.numeric(out))
})

test_that(".same_convolve with delta kernel reproduces input", {
  x <- as.numeric(1:11)
  k <- c(0, 1, 0)
  out <- morie:::.same_convolve(x, k)
  expect_equal(out, x, tolerance = 1e-10)
})

# ============================================================== dsp_spectral.R

test_that(".kaiser_window returns 1 for N = 1", {
  expect_equal(morie:::.kaiser_window(1L), 1)
})

test_that(".kaiser_window returns symmetric length-N window", {
  w <- morie:::.kaiser_window(11L, beta = 8)
  expect_length(w, 11L)
  expect_true(is.numeric(w))
  # Kaiser is symmetric about the centre.
  expect_equal(w, rev(w), tolerance = 1e-10)
  # Maximum is at the centre.
  expect_equal(which.max(w), 6L)
})

# ============================================================== dsp_detection.R

test_that(".unwrap_d removes a 2*pi step", {
  # Synthetic wrapped signal: linear ramp wrapped to (-pi, pi].
  raw <- c(3.0, -3.1, -2.9)
  out <- morie:::.unwrap_d(raw)
  expect_length(out, length(raw))
  # After unwrap the absolute jumps should not exceed pi.
  expect_true(max(abs(diff(out))) <= pi + 1e-8)
})

# ============================================================== dsp_waveform.R

test_that(".unwrap (waveform) removes a 2*pi step", {
  raw <- c(3.0, -3.1, -2.9)
  out <- morie:::.unwrap(raw)
  expect_length(out, length(raw))
  expect_true(max(abs(diff(out))) <= pi + 1e-8)
})

# ============================================================== mrm_kulldorff.R

test_that(".haversine_km_mat returns 0 for coincident points", {
  expect_equal(morie:::.haversine_km_mat(43.6532, -79.3832,
                                           43.6532, -79.3832),
               0, tolerance = 1e-8)
})

test_that(".haversine_km_mat gives ~504 km Toronto<->Montreal", {
  # YYZ ~ (43.6532, -79.3832); YUL ~ (45.5017, -73.5673).
  d <- morie:::.haversine_km_mat(43.6532, -79.3832,
                                   45.5017, -73.5673)
  expect_true(abs(d - 504) < 5)
})

test_that(".poisson_lrt returns 0 when n_obs <= n_exp", {
  expect_equal(morie:::.poisson_lrt(n_obs = 5, n_in = 100,
                                      n_exp = 5, n_tot = 1000), 0)
  expect_equal(morie:::.poisson_lrt(n_obs = 5, n_in = 100,
                                      n_exp = 10, n_tot = 1000), 0)
})

test_that(".poisson_lrt is positive on a true cluster", {
  out <- morie:::.poisson_lrt(n_obs = 30, n_in = 100,
                                n_exp = 10, n_tot = 1000)
  expect_true(is.numeric(out) && out > 0)
})

# ============================================================== mrm_lisa.R

test_that(".haversine_km_lisa returns 0 for coincident points", {
  expect_equal(morie:::.haversine_km_lisa(43.6, -79.3,
                                            43.6, -79.3),
               0, tolerance = 1e-8)
})

test_that(".knn_weights_lisa builds n x n row-normalised weight matrix", {
  set.seed(1L)
  lat <- 43.6 + stats::runif(10, 0, 0.1)
  lon <- -79.4 + stats::runif(10, 0, 0.1)
  W <- morie:::.knn_weights_lisa(lat, lon, k = 3L)
  expect_true(is.matrix(W))
  expect_equal(dim(W), c(10L, 10L))
  # Diagonal should be 0 (no self-edge).
  expect_equal(diag(W), rep(0, 10L))
  # Each row should sum to 1 (since k neighbours each get 1/k).
  expect_equal(rowSums(W), rep(1, 10L), tolerance = 1e-10)
})

# ============================================================ fairness_metrics.R

test_that(".morie_fairness_check_aligned passes on equal lengths", {
  expect_silent(morie:::.morie_fairness_check_aligned(
    list(name = "a", len = 5L),
    list(name = "b", len = 5L)))
})

test_that(".morie_fairness_check_aligned errors on length mismatch", {
  expect_error(morie:::.morie_fairness_check_aligned(
    list(name = "a", len = 5L),
    list(name = "b", len = 4L)),
    regexp = "length mismatch")
})

test_that(".morie_fairness_ordered_unique returns first-seen order", {
  expect_equal(morie:::.morie_fairness_ordered_unique(c("b", "a",
                                                          "b", "c")),
               c("b", "a", "c"))
})

test_that(".morie_fairness_favorable_rates computes per-group rates", {
  outcome <- c("hire", "hire", "no", "hire", "no", "no")
  group   <- c("A", "A", "A", "B", "B", "B")
  out <- morie:::.morie_fairness_favorable_rates(outcome, group, "hire")
  expect_type(out, "list")
  expect_named(out, c("A", "B"))
  expect_equal(out$A$rate, 2 / 3, tolerance = 1e-10)
  expect_equal(out$B$rate, 1 / 3, tolerance = 1e-10)
})

test_that(".morie_fairness_resolve_privileged picks highest-rate group", {
  rates <- list(
    A = list(g = "A", n = 3L, rate = 2 / 3),
    B = list(g = "B", n = 3L, rate = 1 / 3))
  w_env <- new.env()
  w_env$w <- character(0)
  out <- morie:::.morie_fairness_resolve_privileged(NULL, rates, w_env)
  # Returns a named scalar (name carried through from rates).
  expect_equal(unname(out), "A")
  # Should warn (via env$w) that privileged was inferred.
  expect_true(length(w_env$w) >= 1L)
})

test_that(".morie_fairness_resolve_privileged passes explicit privileged", {
  rates <- list(A = list(g = "A", n = 3L, rate = 0.6))
  w_env <- new.env(); w_env$w <- character(0)
  out <- morie:::.morie_fairness_resolve_privileged("A", rates, w_env)
  expect_equal(unname(out), "A")
})

test_that(".morie_fairness_resolve_privileged errors on unknown group", {
  rates <- list(A = list(g = "A", n = 3L, rate = 0.6))
  w_env <- new.env(); w_env$w <- character(0)
  expect_error(
    morie:::.morie_fairness_resolve_privileged("Z", rates, w_env),
    regexp = "not found")
})

test_that(".morie_fairness_rates_from_labels computes TPR + FPR", {
  y_true <- c(1, 1, 0, 0, 1, 0, 1, 0)
  y_pred <- c(1, 0, 0, 1, 1, 0, 1, 1)
  group  <- c("A", "A", "A", "A", "B", "B", "B", "B")
  out <- morie:::.morie_fairness_rates_from_labels(y_true, y_pred,
                                                     group, 1)
  expect_named(out, c("A", "B"))
  for (g in c("A", "B")) {
    expect_true(is.numeric(out[[g]]$tpr))
    expect_true(is.numeric(out[[g]]$fpr))
  }
})

test_that(".morie_fairness_gini_core returns 0 on uniform input", {
  expect_equal(morie:::.morie_fairness_gini_core(rep(5, 10L)), 0)
})

test_that(".morie_fairness_gini_core returns 0 for degenerate input", {
  expect_equal(morie:::.morie_fairness_gini_core(numeric(0)), 0)
  expect_equal(morie:::.morie_fairness_gini_core(c(5)), 0)
  expect_equal(morie:::.morie_fairness_gini_core(rep(0, 5L)), 0)
})

test_that(".morie_fairness_gini_core > 0 on skewed input", {
  out <- morie:::.morie_fairness_gini_core(c(rep(0, 99L), 1000))
  expect_true(out > 0.9)
})

test_that(".morie_fairness_result builds a result list", {
  out <- morie:::.morie_fairness_result(title = "Test",
                                          summary_lines = list(N = 10))
  expect_type(out, "list")
})

# ============================================================ ingest_bigquery.R

test_that(".morie_bq_quote_ident wraps a legal identifier in backticks", {
  expect_equal(morie:::.morie_bq_quote_ident("my_table"),
               "`my_table`")
  expect_equal(morie:::.morie_bq_quote_ident("ds-1_AbC"),
               "`ds-1_AbC`")
})

test_that(".morie_bq_quote_ident errors on illegal identifier", {
  expect_error(morie:::.morie_bq_quote_ident("bad name"),
               regexp = "Illegal BigQuery identifier")
  expect_error(morie:::.morie_bq_quote_ident(""),
               regexp = "Illegal BigQuery identifier")
})

test_that(".morie_bq_billing_project prefers explicit arg", {
  expect_equal(morie:::.morie_bq_billing_project("hadesllm"),
               "hadesllm")
})

test_that(".morie_bq_billing_project returns NULL on missing arg + env", {
  old <- Sys.getenv("GCP_PROJECT", unset = NA)
  Sys.unsetenv("GCP_PROJECT")
  on.exit(if (!is.na(old)) Sys.setenv(GCP_PROJECT = old), add = TRUE)
  expect_null(morie:::.morie_bq_billing_project(NULL))
})

test_that(".morie_bq_require errors when bigrquery is absent", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "bigrquery")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(morie:::.morie_bq_require(),
               regexp = "bigrquery")
})

# ====================================================================== llm.R

test_that(".morie_llm_env returns default for unset variable", {
  Sys.unsetenv("MORIE_TEST_LLM_UNSET")
  expect_equal(morie:::.morie_llm_env("MORIE_TEST_LLM_UNSET",
                                        default = "fallback"),
               "fallback")
})

test_that(".morie_llm_env reads a set variable + trims whitespace", {
  Sys.setenv(MORIE_TEST_LLM_SET = "  hello  ")
  on.exit(Sys.unsetenv("MORIE_TEST_LLM_SET"), add = TRUE)
  expect_equal(morie:::.morie_llm_env("MORIE_TEST_LLM_SET"), "hello")
})

test_that(".morie_llm_ollama_base strips trailing slashes", {
  old <- Sys.getenv("OLLAMA_BASE_URL", unset = NA)
  Sys.setenv(OLLAMA_BASE_URL = "http://localhost:11434//")
  on.exit({
    if (!is.na(old)) Sys.setenv(OLLAMA_BASE_URL = old)
    else Sys.unsetenv("OLLAMA_BASE_URL")
  }, add = TRUE)
  expect_equal(morie:::.morie_llm_ollama_base(),
               "http://localhost:11434")
})

test_that(".morie_llm_gemini_key returns NULL when env is unset", {
  old <- Sys.getenv("GEMINI_API_KEY", unset = NA)
  Sys.unsetenv("GEMINI_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(GEMINI_API_KEY = old), add = TRUE)
  expect_null(morie:::.morie_llm_gemini_key())
})

test_that(".morie_llm_openai_key returns NULL when env is unset", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = NA)
  Sys.unsetenv("OPENAI_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(OPENAI_API_KEY = old), add = TRUE)
  expect_null(morie:::.morie_llm_openai_key())
})

test_that(".morie_llm_api_base returns NULL when env is unset", {
  old <- Sys.getenv("LLM_API_BASE_URL", unset = NA)
  Sys.unsetenv("LLM_API_BASE_URL")
  on.exit(if (!is.na(old)) Sys.setenv(LLM_API_BASE_URL = old),
          add = TRUE)
  expect_null(morie:::.morie_llm_api_base())
})

test_that(".morie_llm_api_base strips trailing slashes when set", {
  old <- Sys.getenv("LLM_API_BASE_URL", unset = NA)
  Sys.setenv(LLM_API_BASE_URL = "https://api.example.com/v1//")
  on.exit({
    if (!is.na(old)) Sys.setenv(LLM_API_BASE_URL = old)
    else Sys.unsetenv("LLM_API_BASE_URL")
  }, add = TRUE)
  expect_equal(morie:::.morie_llm_api_base(),
               "https://api.example.com/v1")
})

test_that(".morie_llm_api_key returns NULL when env is unset", {
  old <- Sys.getenv("LLM_API_KEY", unset = NA)
  Sys.unsetenv("LLM_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(LLM_API_KEY = old), add = TRUE)
  expect_null(morie:::.morie_llm_api_key())
})

test_that(".morie_llm_gemini_model returns DEFAULT_GEMINI_MODEL when unset", {
  old <- Sys.getenv("GEMINI_MODEL", unset = NA)
  Sys.unsetenv("GEMINI_MODEL")
  on.exit(if (!is.na(old)) Sys.setenv(GEMINI_MODEL = old), add = TRUE)
  out <- morie:::.morie_llm_gemini_model()
  expect_true(is.character(out) && nzchar(out))
})

test_that(".morie_llm_system_prompt returns the MORIE system preamble", {
  out <- morie:::.morie_llm_system_prompt()
  expect_type(out, "character")
  expect_match(out, "MORIE")
})

test_that(".morie_llm_system_prompt embeds the context block", {
  out <- morie:::.morie_llm_system_prompt("EXTRA-CTX-MARKER-XYZ")
  expect_match(out, "EXTRA-CTX-MARKER-XYZ")
})
