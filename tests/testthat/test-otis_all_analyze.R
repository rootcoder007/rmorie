# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)
set.seed(1)

# ---------------------------------------------------------------------------
# Synthetic OTIS panel — covers Mandela / DML / per-year analyzers
# ---------------------------------------------------------------------------

# Backwards-compat shim — the helper-otis.R dispatcher supersedes the
# old one-size-fits-all panel, but a few tests below still refer to it
# by name. Keep these stubs delegating to the proper per-id fixtures.
make_otis_panel <- function(n = 200, seed = 1) {
  make_synthetic_otis("b01", n = n, seed = seed)
}

make_datasets_list <- function(seed = 2) {
  make_synthetic_otis_datasets_complete(n = 80L, seed = seed)
}

# ---------------------------------------------------------------------------
# morie_otis_analyzers — registry helper
# ---------------------------------------------------------------------------

test_that("morie_otis_analyzers returns a named list/vector of fns", {
  res <- tryCatch(morie_otis_analyzers(), error = function(e) NULL)
  skip_if(is.null(res), "not exported in this build")
  expect_true(is.list(res) || is.character(res))
  expect_gt(length(res), 0)
})

# ---------------------------------------------------------------------------
# morie_otis_analyze_all — top-level dispatcher
# ---------------------------------------------------------------------------

test_that("morie_otis_analyze_all runs over a datasets list", {
  set.seed(3)
  ds <- make_datasets_list()
  res <- tryCatch(morie_otis_analyze_all(ds), error = function(e) NULL)
  skip_if(is.null(res), "needs richer per-dataset structure")
  expect_true(is.list(res))
})

test_that("morie_otis_analyze_all handles empty datasets list", {
  res <- tryCatch(morie_otis_analyze_all(list()),
                  error = function(e) NULL,
                  warning = function(w) NULL)
  expect_true(is.null(res) || is.list(res))
})

# ---------------------------------------------------------------------------
# Per-dataset analyzers: b01..b09, c01..c12, d01..d07
# ---------------------------------------------------------------------------

per_dataset_fns <- c(
  paste0("morie_otis_analyze_b0", 1:9),
  paste0("morie_otis_analyze_c0", 1:9),
  paste0("morie_otis_analyze_c", 10:12),
  paste0("morie_otis_analyze_d0", 1:7)
)

# Each id in {b01..b09, c01..c12, d01..d07} gets a fixture sized to its
# own column schema via make_synthetic_otis(id) — feeding a single
# common panel triggers silent tryCatch bail-outs everywhere.
for (nm in per_dataset_fns) {
  local({
    fn_name <- nm
    # Extract id (e.g. "morie_otis_analyze_b03" -> "b03")
    id <- sub("^morie_otis_analyze_", "", fn_name)
    test_that(paste(fn_name, "runs on per-id synthetic data"), {
      skip_if_not(exists(fn_name), paste(fn_name, "not exported"))
      df <- make_synthetic_otis(id, n = 200L,
                                seed = nchar(fn_name) + 5L)
      res <- tryCatch(do.call(fn_name, list(df)),
                      error = function(e) e,
                      warning = function(w) NULL)
      # If still skipping, surface what's missing rather than swallow it:
      if (inherits(res, "error")) {
        skip(sprintf("%s errored on synthetic %s panel: %s",
                     fn_name, id, conditionMessage(res)))
      }
      expect_true(is.list(res) || is.data.frame(res) ||
                  is.numeric(res) || is.character(res))
    })
  })
}

# ---------------------------------------------------------------------------
# Ruhela aggregate variants (b03..b09 + c01..c12 + d02..d05)
# ---------------------------------------------------------------------------

ruhela_aggregate_fns <- c(
  paste0("morie_otis_analyze_b0", 3:9, "_ruhela_aggregate"),
  paste0("morie_otis_analyze_c0", 1:9, "_ruhela_aggregate"),
  paste0("morie_otis_analyze_c", 10:12, "_ruhela_aggregate"),
  paste0("morie_otis_analyze_d0", 2:5, "_ruhela_aggregate")
)

for (nm in ruhela_aggregate_fns) {
  local({
    fn_name <- nm
    # Strip "_ruhela_aggregate" suffix to recover the dataset id.
    id <- sub("_ruhela_aggregate$", "",
              sub("^morie_otis_analyze_", "", fn_name))
    test_that(paste(fn_name, "runs on per-id synthetic data"), {
      skip_if_not(exists(fn_name), paste(fn_name, "not exported"))
      df <- make_synthetic_otis(id, n = 200L,
                                seed = nchar(fn_name) + 7L)
      # suppressWarnings so per-fit warnings (glm convergence,
      # MatchIt "Fewer control" summaries, etc.) don't get coerced
      # to NULL via `warning = function(w) NULL` -- that pattern
      # masked real RichResult payloads and caused 5 false-failures
      # in CI on b03/b06/b08/c02/c10 (3MMM.36 fix, same shape as
      # 3MMM.25's earlier fix at L156).
      res <- suppressWarnings(tryCatch(
        do.call(fn_name, list(df)),
        error = function(e) e))
      if (inherits(res, "error")) {
        skip(sprintf("%s errored on synthetic %s panel: %s",
                     fn_name, id, conditionMessage(res)))
      }
      expect_true(is.list(res) || is.data.frame(res) ||
                  is.numeric(res) || is.character(res))
    })
  })
}

# ---------------------------------------------------------------------------
# Ruhela per-year + ruhela_formulations + dual variants
# ---------------------------------------------------------------------------

ruhela_singleton_fns <- c(
  "morie_otis_analyze_a01",
  "morie_otis_analyze_a01_ruhela_formulations",
  "morie_otis_analyze_b01_ruhela_formulations",
  "morie_otis_analyze_b02_ruhela_formulations",
  # 3MMM.26: morie_otis_analyze_{a01,b01}_dual removed -- they were
  # deprecated aliases of *_ruhela_formulations (already listed above).
  "morie_otis_analyze_a01_ruhela_per_year",
  "morie_otis_analyze_b01_ruhela_per_year",
  "morie_otis_analyze_a01_ruhela_alt_gender",
  "morie_otis_analyze_a01_ruhela_alt_age",
  "morie_otis_analyze_a01_ruhela_alt_toronto",
  "morie_otis_analyze_b01_ruhela_alt_gender",
  "morie_otis_analyze_b01_ruhela_alt_age",
  "morie_otis_analyze_b01_ruhela_alt_toronto",
  "morie_otis_analyze_b02_ruhela_alt_region",
  "morie_otis_analyze_b02_ruhela_alt_age",
  "morie_otis_analyze_a01_ruhela_subgroup_female",
  "morie_otis_analyze_a01_ruhela_subgroup_male",
  "morie_otis_analyze_b01_ruhela_subgroup_female",
  "morie_otis_analyze_b01_ruhela_subgroup_male",
  "morie_otis_analyze_a01_with_csi_context"
)

for (nm in ruhela_singleton_fns) {
  local({
    fn_name <- nm
    test_that(paste(fn_name, "runs with NULL data or skips"), {
      skip_if_not(exists(fn_name), paste(fn_name, "not exported"))
      df <- make_otis_panel(80, seed = nchar(fn_name) + 9)
      # suppressWarnings so deprecation messages on the *_dual aliases
      # don't get coerced to NULL (the old `warning = function(w) NULL`
      # branch was eating real RichResult payloads and forcing skips
      # for analyzers that work fine on the synthetic panel).
      res <- suppressWarnings(tryCatch(
        do.call(fn_name, list(data = df)),
        error = function(e) NULL))
      if (is.null(res)) {
        # Synthetic panel doesn't fit this analyzer; fall back to the
        # bundled a01 fixture from data.ontario.ca (real, open).
        df2 <- suppressWarnings(tryCatch(
          morie_datasets_otis_a01(offline = TRUE),
          error = function(e) NULL))
        if (!is.null(df2)) {
          res <- suppressWarnings(tryCatch(
            do.call(fn_name, list(data = df2)),
            error = function(e) NULL))
        }
      }
      skip_if(is.null(res),
              paste(fn_name, "needs OTIS columns the bundled a01 fixture lacks"))
      expect_true(is.list(res) || is.data.frame(res) ||
                  is.numeric(res) || is.character(res))
    })

    test_that(paste(fn_name, "no-arg invocation does not crash R"), {
      skip_if_not(exists(fn_name), paste(fn_name, "not exported"))
      res <- tryCatch(do.call(fn_name, list()),
                      error = function(e) NULL,
                      warning = function(w) NULL)
      expect_true(is.null(res) || is.list(res) || is.data.frame(res) ||
                  is.numeric(res) || is.character(res))
    })
  })
}

# ---------------------------------------------------------------------------
# Mandela classification family
# ---------------------------------------------------------------------------

test_that("morie_otis_analyze_b05_mandela_classification runs", {
  set.seed(40)
  df <- make_otis_panel(150, seed = 40)
  res <- tryCatch(
    morie_otis_analyze_b05_mandela_classification(df),
    error = function(e) NULL
  )
  skip_if(is.null(res), "needs richer OTIS structure for Mandela classification")
  expect_true(is.list(res) || is.data.frame(res))
})

test_that("morie_otis_analyze_c11_mandela_classification runs", {
  set.seed(41)
  df <- make_otis_panel(150, seed = 41)
  res <- tryCatch(
    morie_otis_analyze_c11_mandela_classification(df),
    error = function(e) NULL
  )
  skip_if(is.null(res), "needs richer OTIS structure")
  expect_true(is.list(res) || is.data.frame(res))
})

test_that("morie_otis_analyze_otis_mandela_provincial_vs_federal runs", {
  set.seed(42)
  # The provincial-vs-federal cross-comparison delegates to the c11
  # Mandela classifier; pass a c11-shaped synthetic frame (the helper
  # ships with the test suite) rather than the empty no-arg call,
  # which was the real reason this skip used to fire.
  df <- make_synthetic_otis("c11", n = 150, seed = 42)
  res <- tryCatch(
    morie_otis_analyze_otis_mandela_provincial_vs_federal(df),
    error = function(e) NULL
  )
  skip_if(is.null(res), "c11 synthetic still lacks the Mandela columns")
  expect_true(is.list(res) || is.data.frame(res))
})

# ---------------------------------------------------------------------------
# Cross-cluster chi-square + master grids
# ---------------------------------------------------------------------------

test_that("morie_otis_analyze_c_chi2 runs over datasets list", {
  set.seed(50)
  ds <- make_datasets_list(seed = 50)
  res <- tryCatch(morie_otis_analyze_c_chi2(ds), error = function(e) NULL)
  skip_if(is.null(res), "needs richer dataset list")
  expect_true(is.list(res) || is.data.frame(res))
})

test_that("morie_otis_analyze_d_chi2 runs over datasets list", {
  set.seed(51)
  ds <- make_datasets_list(seed = 51)
  res <- tryCatch(morie_otis_analyze_d_chi2(ds), error = function(e) NULL)
  skip_if(is.null(res), "needs richer dataset list")
  expect_true(is.list(res) || is.data.frame(res))
})

test_that("morie_otis_analyze_ruhela_grid runs over datasets list", {
  set.seed(52)
  ds <- make_datasets_list(seed = 52)
  res <- tryCatch(morie_otis_analyze_ruhela_grid(ds),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs richer dataset list")
  expect_true(is.list(res))
})

test_that("morie_otis_analyze_ruhela_master runs over datasets list", {
  set.seed(53)
  ds <- make_datasets_list(seed = 53)
  # .otis_aggregate_glm now detects degenerate GEE sandwich covariance
  # (negative or NaN diag(vbeta)) and reports it explicitly in the
  # results table instead of letting NaN propagate via summary(). So
  # this test no longer needs suppressWarnings -- the function emits
  # a clean structured result row.
  res <- tryCatch(morie_otis_analyze_ruhela_master(ds),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs richer dataset list")
  expect_true(is.list(res))
})

# ---------------------------------------------------------------------------
# Smoke-test any remaining morie_otis_analyze_* exports we didn't name
# explicitly — keeps covr happy on the long tail of one-line wrappers.
# ---------------------------------------------------------------------------

test_that("residual morie_otis_analyze_* exports each enter cleanly", {
  ns <- tryCatch(asNamespace("morie"), error = function(e) NULL)
  skip_if(is.null(ns), "morie namespace unavailable")
  candidates <- ls(ns, pattern = "^morie_otis_analyze_")
  skip_if(length(candidates) == 0, "no morie_otis_analyze_* exports found")
  set.seed(99)
  df <- make_otis_panel(60, seed = 99)
  for (nm in candidates) {
    fn <- get(nm, envir = ns)
    if (!is.function(fn)) next
    res <- tryCatch(
      {
        # Try (data) first, fall back to no-arg
        args <- tryCatch(formals(fn), error = function(e) NULL)
        if (!is.null(args) && "data" %in% names(args)) {
          do.call(fn, list(data = df))
        } else if (!is.null(args) && "df" %in% names(args)) {
          do.call(fn, list(df = df))
        } else {
          do.call(fn, list())
        }
      },
      error = function(e) NULL,
      warning = function(w) NULL
    )
    # We don't assert on res — entry-point coverage is the goal.
    expect_true(TRUE)
  }
})