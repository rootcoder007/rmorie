# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2SS: tests for internal helpers across tables_pub.R,
# data_access.R, crypto_keystore.R, datasets.R, and study_core.R.

# =============================================================== tables_pub.R

test_that(".tbl_fmt_num formats a numeric with default 2 digits", {
  expect_equal(morie:::.tbl_fmt_num(3.14159), "3.14")
})

test_that(".tbl_fmt_num returns '' on non-finite", {
  expect_equal(morie:::.tbl_fmt_num(NA_real_), "")
  expect_equal(morie:::.tbl_fmt_num(Inf), "")
})

test_that(".tbl_fmt_num apa style strips leading zero for |x| < 1", {
  expect_equal(morie:::.tbl_fmt_num(0.42, apa = TRUE), ".42")
  expect_equal(morie:::.tbl_fmt_num(-0.42, apa = TRUE), "-.42")
})

test_that(".tbl_fmt_pval formats a normal-range p-value", {
  expect_equal(morie:::.tbl_fmt_pval(0.045, digits = 3L), "0.045")
})

test_that(".tbl_fmt_pval renders tiny p as '<0.001'", {
  expect_equal(morie:::.tbl_fmt_pval(1e-9, digits = 3L), "<0.001")
})

test_that(".tbl_stars steps through ***/**/* thresholds", {
  expect_equal(morie:::.tbl_stars(0.0005), "***")
  expect_equal(morie:::.tbl_stars(0.005),  "**")
  expect_equal(morie:::.tbl_stars(0.04),   "*")
  expect_equal(morie:::.tbl_stars(0.5),    "")
  expect_equal(morie:::.tbl_stars(NA),     "")
})

test_that(".tbl_smd matches the canonical (m1-m2)/sqrt((s1^2+s2^2)/2)", {
  out <- morie:::.tbl_smd(m1 = 5, m2 = 3, sd1 = 1, sd2 = 1)
  expect_equal(out, 2 / sqrt(1), tolerance = 1e-10)
})

test_that(".tbl_smd returns 0 when pooled sd is ~0", {
  expect_equal(morie:::.tbl_smd(5, 3, 0, 0), 0)
})

test_that(".tbl_footnotes_new builds an empty notes registry", {
  reg <- morie:::.tbl_footnotes_new()
  expect_true(is.environment(reg))
  expect_length(reg$notes, 0L)
})

test_that(".tbl_footnotes_add appends a unique note", {
  reg <- morie:::.tbl_footnotes_new()
  morie:::.tbl_footnotes_add(reg, "note A")
  morie:::.tbl_footnotes_add(reg, "note B")
  morie:::.tbl_footnotes_add(reg, "note A")  # dup -> no-op
  expect_length(reg$notes, 2L)
})

test_that(".tbl_footnotes_render returns '' on empty registry", {
  reg <- morie:::.tbl_footnotes_new()
  expect_equal(morie:::.tbl_footnotes_render(reg), "")
})

test_that(".tbl_footnotes_render emits LaTeX / HTML / text variants", {
  reg <- morie:::.tbl_footnotes_new()
  morie:::.tbl_footnotes_add(reg, "see appendix")
  expect_match(morie:::.tbl_footnotes_render(reg, fmt = "latex"),
               "textsuperscript")
  expect_match(morie:::.tbl_footnotes_render(reg, fmt = "html"),
               "<sup>")
  expect_match(morie:::.tbl_footnotes_render(reg, fmt = "text"),
               "see appendix")
})

test_that(".tbl_to_format passes through on dataframe fmt", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  expect_identical(morie:::.tbl_to_format(df, "dataframe"), df)
})

test_that(".tbl_to_format renders csv with title + footnotes", {
  df <- data.frame(a = 1:2)
  out <- morie:::.tbl_to_format(df, "csv",
                                  title = "T", footnotes = "fn")
  expect_match(out, "# T")
  expect_match(out, "fn")
})

# =============================================================== data_access.R

test_that(".morie_url_with_params is a no-op on NULL/empty params", {
  expect_equal(
    morie:::.morie_url_with_params("https://example.com/x", NULL),
    "https://example.com/x")
  expect_equal(
    morie:::.morie_url_with_params("https://example.com/x", list()),
    "https://example.com/x")
})

test_that(".morie_url_with_params appends query string", {
  out <- morie:::.morie_url_with_params(
    "https://example.com/api",
    list(year = 2024, kind = "data"))
  expect_match(out, "\\?year=2024")
  expect_match(out, "kind=data")
})

test_that(".morie_url_with_params uses '&' when url already has '?'", {
  out <- morie:::.morie_url_with_params(
    "https://example.com/api?token=x",
    list(year = 2024))
  expect_match(out, "&year=2024")
})

test_that(".morie_ckan_portal maps known slugs", {
  out <- morie:::.morie_ckan_portal("open.canada.ca")
  expect_match(out, "^https://")
})

test_that(".morie_ckan_portal passes a full URL through (trailing-slash strip)", {
  expect_equal(
    morie:::.morie_ckan_portal("https://example.com/ckan///"),
    "https://example.com/ckan")
})

test_that(".morie_ckan_portal errors on unknown slug", {
  expect_error(morie:::.morie_ckan_portal("nonexistent.portal"),
               regexp = "Unknown CKAN portal")
})

test_that(".morie_detect_format falls back to URL extension on no-network", {
  out <- morie:::.morie_detect_format("https://example.invalid/x.csv")
  expect_equal(out, "csv")
})

test_that(".morie_detect_format extension fallback covers known types", {
  for (ext_fmt in list(c("data.json", "json"), c("d.tsv", "tsv"),
                       c("d.xlsx", "xlsx"), c("d.xml", "xml"),
                       c("d.zip", "zip"), c("d.html", "html"))) {
    out <- morie:::.morie_detect_format(
      paste0("https://example.invalid/", ext_fmt[1]))
    expect_equal(out, ext_fmt[2])
  }
})

test_that(".nz returns the first non-empty/non-NA string", {
  expect_equal(morie:::.nz(NULL, NA, "", "found", "next"), "found")
  expect_equal(morie:::.nz(NULL, NA, ""), "")
})

# =========================================================== crypto_keystore.R

test_that(".morie_resolve_path expands ~ + normalises", {
  out <- morie:::.morie_resolve_path("~/morie_test_path")
  expect_type(out, "character")
  expect_false(grepl("^~", out))
})

test_that(".morie_hex_to_raw round-trips with .morie_raw_to_hex", {
  rv <- as.raw(c(0x00, 0x01, 0xab, 0xff))
  h <- morie:::.morie_raw_to_hex(rv)
  expect_equal(h, "0001abff")
  expect_equal(morie:::.morie_hex_to_raw(h), rv)
})

test_that(".morie_hex_to_raw returns empty raw on empty string", {
  expect_equal(morie:::.morie_hex_to_raw(""), raw(0))
})

test_that(".morie_hex_to_raw errors on odd-length hex", {
  expect_error(morie:::.morie_hex_to_raw("abc"),
               regexp = "odd length")
})

test_that(".morie_hex_to_raw errors on non-character / multi-element input", {
  expect_error(morie:::.morie_hex_to_raw(c("aa", "bb")),
               regexp = "single hex string")
})

test_that(".morie_raw_to_hex errors on non-raw input", {
  expect_error(morie:::.morie_raw_to_hex(c(1, 2, 3)),
               regexp = "expected raw vector")
})

test_that(".morie_keystore_require errors if sodium is missing", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("sodium")
  if (requireNamespace("sodium", quietly = TRUE) &&
      requireNamespace("jsonlite", quietly = TRUE)) {
    expect_silent(morie:::.morie_keystore_require())
  } else {
    expect_error(morie:::.morie_keystore_require())
  }
})

# =================================================================== datasets.R

test_that(".morie_dataset_pkg_csv returns NA for unknown name", {
  expect_true(is.na(morie:::.morie_dataset_pkg_csv("__nonexistent__")))
})

test_that(".morie_dataset_year_where returns '1=1' on NULL", {
  expect_equal(morie:::.morie_dataset_year_where(NULL), "1=1")
})

test_that(".morie_dataset_year_where formats numeric year", {
  expect_equal(morie:::.morie_dataset_year_where(2024),
               "OCC_YEAR = 2024")
})

test_that(".morie_dataset_records_to_df passes data.frame through", {
  df <- data.frame(a = 1:3)
  expect_identical(morie:::.morie_dataset_records_to_df(df), df)
})

test_that(".morie_dataset_records_to_df returns 0-row frame on empty input", {
  out <- morie:::.morie_dataset_records_to_df(list())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that(".morie_dataset_records_to_df rbinds a list-of-records", {
  out <- morie:::.morie_dataset_records_to_df(list(
    list(case_number = "C-001", arrest = "Y"),
    list(case_number = "C-002", arrest = "N")))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

# =================================================================== study_core.R

test_that(".na_omit_cols drops rows with NA in the chosen cols", {
  df <- data.frame(a = c(1, NA, 3), b = c("x", "y", "z"))
  out <- morie:::.na_omit_cols(df, c("a"))
  expect_equal(nrow(out), 2L)
})

test_that(".safe_divide returns NA on zero or NA denom", {
  expect_true(is.na(morie:::.safe_divide(1, 0)))
  expect_true(is.na(morie:::.safe_divide(1, NA)))
  expect_equal(morie:::.safe_divide(4, 2), 2)
})

test_that(".wald_ci returns symmetric 1.96-SE bounds", {
  expect_equal(morie:::.wald_ci(0, 1), c(-1.96, 1.96))
  expect_equal(morie:::.wald_ci(5, 0), c(5, 5))
})

test_that(".binary_ci returns p, se, ci on a small sample", {
  out <- morie:::.binary_ci(successes = 30, n = 100)
  expect_equal(out$p, 0.3)
  expect_true(out$se > 0)
  expect_length(out$ci, 2L)
  expect_true(out$ci[1] >= 0 && out$ci[2] <= 1)
})

test_that(".weighted_binary_estimate handles ordinary all-1 weights", {
  out <- morie:::.weighted_binary_estimate(
    x = c(1, 0, 1, 0, 1),
    w = c(1, 1, 1, 1, 1))
  expect_equal(out$p, 3 / 5)
  expect_equal(out$n, 5L)
  expect_true(is.numeric(out$n_eff))
})

test_that(".weighted_binary_estimate ignores NA in either x or w", {
  out <- morie:::.weighted_binary_estimate(
    x = c(1, NA, 1, 0, NA),
    w = c(1, 1, NA, 1, 1))
  expect_equal(out$n, 2L)
})

test_that(".weighted_binary_estimate degenerate on all-NA", {
  out <- morie:::.weighted_binary_estimate(
    x = c(NA, NA), w = c(NA, NA))
  expect_true(is.na(out$p))
  expect_equal(out$n, 0L)
})

test_that(".clip_exp clamps |x| to 700 before exp() to avoid overflow", {
  expect_equal(morie:::.clip_exp(0), 1)
  expect_true(is.finite(morie:::.clip_exp(1e6)))
  expect_equal(morie:::.clip_exp(1e6), exp(700), tolerance = 1e-10)
})

test_that(".safe_confint returns 2-column matrix on a fitted glm", {
  set.seed(1L)
  y <- stats::rbinom(80L, 1L, 0.5)
  x <- stats::rnorm(80L)
  fit <- suppressWarnings(stats::glm(y ~ x, family = stats::binomial()))
  out <- morie:::.safe_confint(fit)
  expect_true(is.matrix(out))
  expect_equal(ncol(out), 2L)
})
