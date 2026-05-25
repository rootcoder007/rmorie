# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Hit every per-loader / per-analyzer year-mismatch error branch
# explicitly, plus the remaining sidecar / describe / mrm_uof edge
# branches that the main test files left uncovered.

# ── Loader year-mismatch branches (one per loader) ─────────────────

test_that("load_main_records errors for 2020-2022 (kind not published)", {
  expect_error(morie_arsau_load_main_records("2020-2022"),
                "main_records not published")
})

test_that("load_individual_records errors for 2020-2022", {
  expect_error(morie_arsau_load_individual_records("2020-2022"),
                "individual_records not published")
})

test_that("load_probe_cycle_records errors for 2020-2022", {
  expect_error(morie_arsau_load_probe_cycle_records("2020-2022"),
                "probe_cycle_records not published")
})

test_that("load_weapon_records errors for 2020-2022", {
  expect_error(morie_arsau_load_weapon_records("2020-2022"),
                "weapon_records not published")
})

test_that("load_aggregate_summary errors for 2023 (only published for 2020-2022)", {
  expect_error(morie_arsau_load_aggregate_summary("2023"),
                "aggregate_summary not published")
})

test_that("load_detailed_dataset errors for 2024", {
  expect_error(morie_arsau_load_detailed_dataset("2024"),
                "detailed_dataset not published")
})


# ── Discovery branch fixers ────────────────────────────────────────

test_that("arsau_describe errors on unknown (kind, year) combination", {
  expect_error(
    morie_arsau_describe("aggregate_summary", "2024"),
    "has no .* entry for"
  )
})

test_that("arsau_available_datasets year-filter returns the right count for 2023", {
  r <- morie_arsau_available_datasets(year = "2023")
  expect_equal(r$n, 4L)  # main + individual + probe_cycle + weapon
})

test_that("arsau_available_datasets year-filter for 2020-2022 returns 2 entries", {
  r <- morie_arsau_available_datasets(year = "2020-2022")
  expect_equal(r$n, 2L)  # aggregate_summary + detailed_dataset only
})


# ── morie_arsau_read_sidecar requireNamespace branch (best-effort) ─

test_that("read_sidecar with corrupt JSON raises a parse error", {
  tf <- tempfile(fileext = ".json")
  writeLines("{ not a valid json", tf)
  expect_error(morie_arsau_read_sidecar(tf))
  unlink(tf)
})


# ── mrm_uof_force_concentration: early-return branch coverage ──────

test_that("force_concentration with zero rows + count_col present", {
  df <- data.frame(f = character(0), cnt = integer(0))
  r <- mrm_uof_force_concentration(df, "f", count_col = "cnt")
  expect_equal(r$n_incidents, 0L)
})

test_that("force_concentration with NA values in count_col (sum_min_count)", {
  df <- data.frame(f = c("A", "B", "B"), cnt = c(NA_real_, 5, 3))
  r <- mrm_uof_force_concentration(df, "f", count_col = "cnt")
  expect_true(r$n_incidents > 0)
})


# ── mrm_uof_weapon_diversity: alternate degenerate paths ───────────

test_that("weapon_diversity with one-row df is degenerate", {
  df <- data.frame(w = "A", f = "X")
  r <- mrm_uof_weapon_diversity(df, "w", "f")
  expect_equal(r$n, 1L)
  expect_match(r$warnings[1], "degenerate")
})


# ── mrm_uof_yoy_change: ruptures fallback path explicit ────────────

test_that("yoy_change with exact 3 years exercises change-point detect", {
  dfs <- list(
    "2020" = data.frame(x = 1:100),
    "2021" = data.frame(x = 1:50),    # big drop
    "2022" = data.frame(x = 1:120)    # big jump
  )
  r <- mrm_uof_yoy_change(dfs_by_year = dfs)
  expect_true(!is.na(r$change_point_year))
})


# ── mrm_uof_demographic_disparity: bootstrap with sub_cat empty ────

test_that("demographic_disparity baseline-only data (single category)", {
  df <- data.frame(
    demo = rep("A", 50),
    out = sample(c(0, 1), 50, replace = TRUE)
  )
  r <- mrm_uof_demographic_disparity(df, "demo", "out")
  # Only 1 category — RR table will have 1 row (baseline).
  expect_equal(length(r$per_category), 1L)
})


# ── mrm_uof_data_quality_audit: empty dataframe ────────────────────

test_that("data_quality_audit on empty df handles gracefully", {
  df <- data.frame(a = numeric(0), b = character(0))
  r <- mrm_uof_data_quality_audit(df)
  expect_equal(r$n_rows, 0L)
})


# ── analyze_aggregate_summary uncovered branches ───────────────────

test_that("analyze_aggregate_summary with mask not matching gracefully falls through", {
  empty <- file.path(tempdir(check = TRUE),
                      paste0("arsau-nomatch-", as.integer(Sys.time())))
  if (dir.exists(empty)) unlink(empty, recursive = TRUE)
  dir.create(file.path(empty, "2020-2022"), recursive = TRUE)
  write.csv(data.frame(
    SECTION = c("UNUSED", "UNUSED"),   # no REPORT_SCOPE rows
    CATEGORY = c("A", "B"),
    `UNITS OF MEASURE` = c("n", "n"),
    YEAR_2020 = c(10, 20),
    YEAR_2021 = c(15, 25),
    YEAR_2022 = c(12, 22),
    check.names = FALSE
  ), file.path(empty, "2020-2022", "useofforce_agrregatesummarybyyear_2020-2022.csv"),
  row.names = FALSE)
  r <- morie_arsau_analyze_aggregate_summary("2020-2022", data_dir = empty)
  # No REPORT_SCOPE row -> which(mask)[1] is NA -> yoy_change_headline
  # is intentionally skipped.  Only data_quality is produced.
  expect_false("yoy_change_headline" %in% names(r))
  expect_true("data_quality" %in% names(r))
  unlink(empty, recursive = TRUE)
})
