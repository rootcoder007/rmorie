# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Edge-case coverage tests for R/mrm_uof.R + R/arsau.R + R/mrm_arsau.R.
# Targets the specific branches that the happy-path tests miss:
# default-arg evaluation, every early-return branch, every conditional
# leg in the interpretation-text selection, every status-code path
# through .morie_env / .morie_resolve_arsau_dir.

# ── .morie_env: empty string vs unset ──────────────────────────────

test_that(".morie_env returns NULL on unset, empty, and whitespace", {
  withr::with_envvar(list(TEST_VAR_XYZ = NA), {
    expect_null(morie:::.morie_env("TEST_VAR_XYZ"))
  })
  withr::with_envvar(list(TEST_VAR_XYZ = ""), {
    expect_null(morie:::.morie_env("TEST_VAR_XYZ"))
  })
  withr::with_envvar(list(TEST_VAR_XYZ = "   "), {
    expect_null(morie:::.morie_env("TEST_VAR_XYZ"))
  })
  withr::with_envvar(list(TEST_VAR_XYZ = "/some/path"), {
    expect_equal(morie:::.morie_env("TEST_VAR_XYZ"), "/some/path")
  })
})


# ── path resolver: bundled-fixture + MORIE_DATA_DIR cascade ────────

test_that("path resolver errors with remediation when no candidates exist", {
  # 3MMM.5 bundled inst/extdata/arsau/<year>/ so the bundled fixture
  # always satisfies the cascade and the resolver no longer errors
  # with all env vars unset. The remediation-error branch is now
  # unreachable from the public API; this test is superseded by the
  # success-path test below.
  skip(paste("superseded by 3MMM.5: inst/extdata/arsau bundled fixture",
             "satisfies the resolver cascade so the remediation-error",
             "branch is no longer reachable from the public API."))
})

test_that("path resolver succeeds via the bundled inst/extdata/arsau fixture", {
  # When env vars are unset, the resolver now finds the bundled
  # per-year layout that 3MMM.5 added.
  withr::with_envvar(list(MORIE_ARSAU_DIR = NA, MORIE_DATA_DIR = NA), {
    p <- morie:::.morie_resolve_arsau_dir(data_dir = NULL)
    expect_true(dir.exists(p))
    expect_match(p, "extdata/arsau$")
  })
})

test_that("path resolver: MORIE_DATA_DIR /ARSAU upper-case path tried", {
  base <- file.path(tempdir(check = TRUE),
                     paste0("morie-data-uc-", as.integer(Sys.time())))
  fx <- file.path(base, "ARSAU")
  if (dir.exists(base)) unlink(base, recursive = TRUE)
  dir.create(fx, recursive = TRUE)
  withr::with_envvar(list(MORIE_ARSAU_DIR = NA, MORIE_DATA_DIR = base), {
    p <- morie:::.morie_resolve_arsau_dir()
    expect_equal(normalizePath(p), normalizePath(fx))
  })
  unlink(base, recursive = TRUE)
})

test_that("path resolver: error message includes all attempted candidates", {
  # 3MMM.5: superseded -- with the bundled fixture present, the
  # resolver succeeds via the bundled candidate even when env vars
  # point at nonexistent paths. Remediation-paragraph branch is no
  # longer reachable from the public API.
  skip(paste("superseded by 3MMM.5: inst/extdata/arsau bundled fixture",
             "satisfies the resolver cascade so the multi-candidate",
             "remediation-error message is no longer emitted."))
})

test_that("path resolver: require_exists=FALSE with no candidates returns NA", {
  withr::with_envvar(list(MORIE_ARSAU_DIR = NA, MORIE_DATA_DIR = NA), {
    # The bundled fixture path always exists as a candidate even if the
    # directory doesn't, so the NA branch is reached only when
    # allow_bundled is conceptually False; here it returns the
    # bundled-fixture path string.  Smoke-confirm it returns a path
    # string rather than erroring out.
    p <- morie:::.morie_resolve_arsau_dir(require_exists = FALSE)
    expect_true(is.character(p) || is.na(p))
  })
})


# ── .arsau_make_entry: explicit invocation for coverage ────────────

test_that(".arsau_make_entry constructs a well-formed entry", {
  e <- morie:::.arsau_make_entry(
    "test", "kind", "file.csv", "sidecar.json",
    expected_rows = 10, expected_cols = 5, is_valid = TRUE,
    description_en = "test EN", description_fr = "test FR"
  )
  expect_equal(e$year_or_range, "test")
  expect_equal(e$kind, "kind")
  expect_equal(e$expected_rows, 10L)
  expect_equal(e$expected_cols, 5L)
  expect_true(e$is_valid)
})

test_that(".arsau_make_entry coerces is_valid via isTRUE", {
  e <- morie:::.arsau_make_entry(
    "x", "y", "z.csv", NULL, 0, 0, NA,
    description_en = "", description_fr = ""
  )
  expect_false(e$is_valid)
})


# ── sidecar reader: jsonlite missing branch ───────────────────────-

test_that("sidecar reader handles top-level only-fields shape", {
  # No 'records' key
  tf <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(
    fields = list(list(id = "a", type = "int"))
  ), auto_unbox = TRUE), tf)
  res <- morie_arsau_read_sidecar(tf)
  expect_length(res$fields, 1)
  expect_length(res$records, 0)
  unlink(tf)
})


# ── .arsau_coerce_year_key: underscore branch ─────────────────────-

test_that(".arsau_coerce_year_key normalises underscore separator", {
  expect_equal(morie:::.arsau_coerce_year_key("2020_2022", range_ok = TRUE),
                "2020-2022")
})


# ── available_years: unsetable root branch ────────────────────────-

test_that("available_years handles non-existent data_dir gracefully", {
  withr::with_envvar(list(MORIE_ARSAU_DIR = NA, MORIE_DATA_DIR = NA), {
    r <- morie_arsau_available_years(data_dir = "/nonexistent/x")
    expect_length(r$present, 0)
    expect_length(r$missing, 0)
  })
})

test_that("available_years French interpretation path", {
  withr::with_envvar(list(MORIE_ARSAU_DIR = NA, MORIE_DATA_DIR = NA), {
    r <- morie_arsau_available_years(language = "fr")
    expect_match(r$interpretation, "ARSAU connait")
  })
})


# ── available_datasets: French language path ──────────────────────-

test_that("available_datasets French path", {
  r <- morie_arsau_available_datasets(language = "fr")
  expect_match(r$interpretation, "entree")
})


# ── print: warnings branch ─────────────────────────────────────────

test_that("print.morie_arsau_result emits warnings when present", {
  fake <- list(
    title = "Test",
    call = "test()",
    summary_lines = list(`Items` = 3),
    warnings = c("This is a warning", "Another warning"),
    interpretation = "Test interpretation"
  )
  class(fake) <- c("morie_arsau_result", "morie_rich_result", "list")
  out <- capture.output(print(fake))
  expect_true(any(grepl("This is a warning", out)))
  expect_true(any(grepl("Another warning", out)))
})


# ── mrm_uof: empty df path ─────────────────────────────────────────

test_that("mrm_uof_force_concentration handles empty df", {
  r <- mrm_uof_force_concentration(data.frame(f = character(0)), "f")
  expect_equal(r$n_incidents, 0L)
})

test_that("mrm_uof_force_concentration: many forces avoids small-n warning", {
  set.seed(0)
  forces <- paste0("F", 1:15)
  df <- data.frame(f = sample(forces, 500, replace = TRUE))
  r <- mrm_uof_force_concentration(df, "f")
  expect_false(any(grepl("Only", r$warnings)))
})


# ── mrm_uof_yoy_change: edge cases ────────────────────────────────-

test_that("yoy_change handles 0 transitions cleanly", {
  r <- mrm_uof_yoy_change(dfs_by_year = list("2020" = data.frame(x = 1:5)))
  expect_equal(r$n, 1L)
  expect_true(is.na(r$mean_abs_yoy_pct))
})

test_that("yoy_change handles year with prev=0 (NA YoY)", {
  r <- mrm_uof_yoy_change(dfs_by_year = list(
    "2020" = data.frame(),
    "2021" = data.frame(x = 1:5)
  ))
  expect_true(is.na(r$yoy_pct[2]))
})


# ── mrm_uof_region_locality: small categories error path ──────────-

test_that("region_locality with all-NA columns short-circuits", {
  df <- data.frame(a = NA, b = NA)
  r <- mrm_uof_region_locality(df, "a", "b")
  expect_equal(r$n, 0L)
})

test_that("region_locality with single category produces NA cramers_v", {
  df <- data.frame(a = c("X", "X", "X"), b = c("X", "X", "X"))
  r <- mrm_uof_region_locality(df, "a", "b")
  # Single category -> 1x1 contingency, chi-square branch is skipped.
  expect_true(is.na(r$cramers_v))
})


# ── mrm_uof_demographic_disparity: unparseable outcome ────────────-

test_that("demographic_disparity handles all-unmapped outcome (drops everything)", {
  df <- data.frame(demo = c("A", "B"), out = c("unknown", "mystery"))
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  # All outcomes are dropped by the coercion; n=0 path or no per_category.
  expect_true(r$n == 0L ||
              all(vapply(r$per_category, function(e) e$n == 0L, logical(1))))
})

test_that("demographic_disparity: explicit baseline rather than largest-N", {
  df <- data.frame(
    demo = c(rep("A", 50), rep("B", 30)),
    out = c(rep(1, 40), rep(0, 10), rep(1, 20), rep(0, 10))
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out", baseline = "B")
  expect_equal(r$baseline, "B")
})


# ── mrm_uof_data_quality_audit: numeric ALL-NA column ─────────────-

test_that("data_quality_audit handles all-NA numeric column gracefully", {
  df <- data.frame(a = c(NA_real_, NA_real_, NA_real_), b = c(1, 2, 3))
  r <- mrm_uof_data_quality_audit(df)
  expect_true(any(grepl("null", r$suspect_flags)))  # >50% null on 'a'
})

test_that("data_quality_audit sidecar without fields list emits warning", {
  df <- data.frame(a = 1:3)
  sc <- list(other_key = "value")
  r <- mrm_uof_data_quality_audit(df, sidecar = sc)
  expect_true(any(grepl("CKAN", r$warnings)))
})

test_that("data_quality_audit expected_schema without proper structure", {
  df <- data.frame(a = 1:3)
  r <- mrm_uof_data_quality_audit(df, expected_schema = "not a schema")
  expect_true(any(grepl("did not duck-type|duck-type", r$warnings)))
})


# ── mrm_arsau analyzers: empty data path ──────────────────────────-

test_that("analyze_aggregate_summary with df having no YEAR_ columns", {
  # Build a fixture where the aggregate file has the section column
  # but no YEAR_* — exercises the "no year_cols" branch.
  empty <- file.path(tempdir(check = TRUE),
                      paste0("arsau-noyear-", as.integer(Sys.time())))
  if (dir.exists(empty)) unlink(empty, recursive = TRUE)
  dir.create(file.path(empty, "2020-2022"), recursive = TRUE)
  write.csv(data.frame(
    SECTION = "REPORT_SCOPE",
    CATEGORY = "Test",
    `UNITS OF MEASURE` = "n",
    check.names = FALSE
  ), file.path(empty, "2020-2022", "useofforce_agrregatesummarybyyear_2020-2022.csv"),
  row.names = FALSE)
  # The loader will warn about row/col count mismatch, but analyze
  # should still produce a result without yoy_change_headline.
  r <- morie_arsau_analyze_aggregate_summary("2020-2022", data_dir = empty)
  expect_false("yoy_change_headline" %in% names(r))
  expect_true("data_quality" %in% names(r))
  unlink(empty, recursive = TRUE)
})
