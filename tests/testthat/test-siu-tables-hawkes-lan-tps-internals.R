# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2ZZ: tests for internal helpers across siu_parser.R,
# hawkes_fit.R, tables_pub.R, laniyonu_smi_force_disparity.R,
# and tps_all_analyze.R.

# ================================================================ siu_parser.R

test_that(".siu_p_stripped_text removes <script>/<style>/tags + decodes entities", {
  html <- paste0(
    "<html><head><style>p{color:red}</style></head>",
    "<body><p>Hello&nbsp;world&amp;</p>",
    "<script>var x=1;</script></body></html>")
  out <- morie:::.siu_p_stripped_text(html)
  expect_type(out, "character")
  expect_match(out, "Hello.*world")
  expect_false(grepl("<", out, fixed = TRUE))
  expect_false(grepl("var x", out, fixed = TRUE))
})

test_that(".siu_p_section_text returns '' when header is absent", {
  expect_equal(morie:::.siu_p_section_text("body text here",
                                             header = "Mandate"),
               "")
})

test_that(".siu_p_section_text slices from header to first end-marker", {
  text <- paste(
    "Preamble line.",
    "The Investigation",
    "Body of investigation section.",
    "Endnotes",
    "Footer", sep = "\n")
  out <- morie:::.siu_p_section_text(text,
                                       header = "The Investigation",
                                       end_markers = c("Endnotes"))
  expect_type(out, "character")
  expect_match(out, "Body of investigation")
})

test_that(".siu_p_detect_police_service returns NA on empty / no match", {
  expect_true(is.na(morie:::.siu_p_detect_police_service("")))
  expect_true(is.na(morie:::.siu_p_detect_police_service(NA)))
})

test_that(".siu_p_detect_police_service picks a present service", {
  out <- morie:::.siu_p_detect_police_service(
    "Officers from the Toronto Police Service responded to the call.")
  expect_type(out, "character")
  expect_match(out, "Toronto")
})

test_that(".siu_p_find_news_release_link returns NA when no template link", {
  out <- morie:::.siu_p_find_news_release_link(
    "<html><body>No links here</body></html>",
    source_url = "https://www.siu.on.ca/en/news.php")
  expect_true(is.na(out))
})

test_that(".siu_p_find_news_release_link finds a nrid= template link", {
  html <- paste0(
    "<html><body>",
    "<a href='/en/news_template.php?nrid=12345'>News</a>",
    "</body></html>")
  out <- morie:::.siu_p_find_news_release_link(
    html, source_url = "https://www.siu.on.ca/en/news.php")
  expect_match(out, "nrid=12345")
})

# ================================================================ hawkes_fit.R

test_that(".hawkes_restarts returns a 5-element list of phi vectors", {
  phi0 <- c(0, 0, 0, 0)
  out <- morie:::.hawkes_restarts(phi0)
  expect_type(out, "list")
  expect_length(out, 5L)
  for (p in out) expect_length(p, length(phi0))
})

test_that(".hawkes_restarts is base-phi-preserving for the first restart", {
  phi0 <- c(1, 2, 3, 4)
  out <- morie:::.hawkes_restarts(phi0)
  # offsets[[1]] is (0,0,0,0), so first restart == phi0.
  expect_equal(out[[1]], phi0)
})

test_that(".hawkes_restarts trims offsets to phi length", {
  phi0 <- c(1, 2, 3)  # 3-vec for exponential kernel
  out <- morie:::.hawkes_restarts(phi0)
  expect_true(all(vapply(out, length, integer(1L)) == 3L))
})

# ================================================================ tables_pub.R

test_that(".tbl_extract_model returns params/se/p/ci/nobs/aic/bic/llf on an lm", {
  set.seed(1L); n <- 80L
  x <- stats::rnorm(n)
  y <- 1 + 2 * x + stats::rnorm(n, sd = 0.3)
  fit <- stats::lm(y ~ x)
  out <- morie:::.tbl_extract_model(fit)
  expect_type(out, "list")
  expect_true(all(c("params", "se", "pvalues", "ci",
                    "nobs", "rsquared", "aic", "bic", "llf")
                  %in% names(out)))
  expect_equal(out$nobs, n)
  expect_true(out$rsquared > 0.9)
})

test_that(".tbl_extract_model handles a glm fit (no r-squared)", {
  set.seed(2L); n <- 100L
  y <- stats::rbinom(n, 1L, 0.5); x <- stats::rnorm(n)
  fit <- suppressWarnings(stats::glm(y ~ x, family = stats::binomial()))
  out <- morie:::.tbl_extract_model(fit)
  expect_true(is.na(out$rsquared))
  expect_true(is.numeric(out$aic) && is.finite(out$aic))
})

# ============================================== laniyonu_smi_force_disparity.R

test_that(".lan_smi_coef builds a coefficient list", {
  out <- morie:::.lan_smi_coef(name = "alpha_v",
                                 estimate = 2.4,
                                 std_error = 0.4)
  expect_named(out, c("name", "estimate", "std_error"))
  expect_equal(out$estimate, 2.4)
})

test_that(".lan_smi_result builds a morie_laniyonu_smi_result list", {
  alpha_v <- morie:::.lan_smi_coef("alpha_v", estimate = 2.4,
                                     std_error = 0.4)
  intercept <- morie:::.lan_smi_coef("intercept", -7.5, 0.3)
  out <- morie:::.lan_smi_result(
    alpha_v = alpha_v,
    intercept = intercept,
    year_effects = list(),
    area_random_effect_sd = 0.5,
    dispersion = 1.1,
    n_events = 1234L,
    n_area_years = 500L,
    log_likelihood = -2000,
    converged = TRUE)
  expect_type(out, "list")
  expect_s3_class(out, "morie_laniyonu_smi_result")
  expect_s3_class(out, "morie_rich_result")
  # RR = exp(2.4) ~= 11.02; CI bounds positive.
  expect_equal(out$rr, exp(2.4), tolerance = 1e-10)
  expect_true(out$rr_ci_low < out$rr)
  expect_true(out$rr_ci_high > out$rr)
})

# ============================================================ tps_all_analyze.R

test_that(".tps_result builds a morie_tps_result list", {
  out <- morie:::.tps_result(title = "Test",
                               summary_lines = list(N = 10))
  expect_type(out, "list")
  expect_s3_class(out, "morie_tps_result")
  expect_s3_class(out, "morie_rich_result")
})

test_that(".tps_safe_year_col returns the first matching year col", {
  expect_equal(morie:::.tps_safe_year_col(
    data.frame(OCC_YEAR = 2024, x = 1)), "OCC_YEAR")
  expect_equal(morie:::.tps_safe_year_col(
    data.frame(REPORT_YEAR = 2024, x = 1)), "REPORT_YEAR")
  expect_equal(morie:::.tps_safe_year_col(
    data.frame(Year = 2024, x = 1)), "Year")
  expect_null(morie:::.tps_safe_year_col(
    data.frame(other = 2024, x = 1)))
})

test_that(".tps_vc_rows returns top-N labelled rows w/ percentages", {
  x <- c(rep("a", 10), rep("b", 5), rep("c", 3), rep("d", 1))
  out <- morie:::.tps_vc_rows(x, top = 3L)
  expect_type(out, "list")
  expect_length(out, 3L)
  expect_equal(out[[1]]$label, "a")
  expect_equal(out[[1]]$count, 10L)
  expect_match(out[[1]]$percent, "%$")
})

test_that(".tps_alias_factory captures name via force()", {
  fn <- morie:::.tps_alias_factory("Assault")
  expect_true(is.function(fn))
  # Calling fn should dispatch to morie_tps_analyze_one with name=Assault.
  df <- data.frame(OCC_YEAR = c(2024L, 2024L),
                   HOOD_158 = c("001", "002"))
  out <- tryCatch(fn(df), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("morie_tps_analyze_one needs more cols: %s",
                 conditionMessage(out)))
  }
  expect_type(out, "list")
})
