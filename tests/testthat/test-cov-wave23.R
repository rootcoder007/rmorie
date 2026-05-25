# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 23 -- two surgical targets:
#   * R/frns_metrics.R   -- the fairness-metric branches covr left bare:
#     empty-input guard, single-element Gini, all-NA worst-abs, the
#     inferred-privileged warning path, zero-base disparate impact,
#     adverse-impact / violation interpretations.
#   * R/study_reporting.R -- the three internal module runners
#     (.run_power_design_module_extended, .run_tables_module_internal,
#     .run_final_report_module_internal).

# ---- frns_metrics.R : internal helpers ------------------------------------

test_that(".frns helpers cover empty / degenerate inputs", {
  expect_error(
    morie:::.frns_check_aligned(list("a", integer(0)), list("b", integer(0))),
    "empty"
  )
  expect_equal(morie:::.frns_gini(5), 0) # single element -> 0
  expect_equal(morie:::.frns_gini(numeric(0)), 0) # empty -> 0
  expect_true(is.na(morie:::.frns_worst_abs(c(NA_real_, NaN, Inf, -Inf))))
})

# ---- morie_fairness_disparate_impact --------------------------------------------

test_that("morie_fairness_disparate_impact: zero-base, adverse, inferred priv", {
  # privileged group has a zero favourable-outcome rate -> undefined ratios
  z <- morie_fairness_disparate_impact(c(0, 0, 0, 1, 1, 0),
    c("A", "A", "A", "B", "B", "B"),
    privileged = "A"
  )
  expect_true(any(grepl("zero favourable", z$warnings)))
  expect_true(is.na(z$value))
  expect_match(z$interpretation, "could not be computed")

  # clear adverse impact: B rate 0.25 vs A rate 1.0
  a <- morie_fairness_disparate_impact(c(1, 1, 1, 1, 1, 0, 0, 0),
    c(rep("A", 4), rep("B", 4)),
    privileged = "A"
  )
  expect_true(a$adverse_impact)
  expect_match(a$interpretation, "Adverse impact detected")

  # privileged = NULL -> the inferred-reference warning path
  inf <- morie_fairness_disparate_impact(
    c(1, 1, 0, 0, 1, 0),
    c("A", "A", "A", "B", "B", "B")
  )
  expect_true(any(grepl("inferred", inf$warnings)))
})

# ---- morie_fairness_demographic_parity ------------------------------------------

test_that("morie_fairness_demographic_parity: <2 groups, inferred priv, gap interp", {
  expect_error(
    morie_fairness_demographic_parity(c(1, 0), c("A", "A")),
    "at least two groups"
  )
  res <- morie_fairness_demographic_parity(
    c(1, 1, 1, 1, 0, 0, 0, 0),
    c(rep("A", 4), rep("B", 4))
  )
  expect_true(any(grepl("inferred", res$warnings)))
  expect_match(res$interpretation, "differ materially")
  near <- morie_fairness_demographic_parity(c(1, 1, 1, 0, 1, 1, 0, 1),
    c(rep("A", 4), rep("B", 4)),
    privileged = "A"
  )
  expect_match(near$interpretation, "close to parity")
})

# ---- morie_fairness_equalized_odds / average_odds_difference --------------------

test_that("morie_fairness_equalized_odds: undefined-rate warning + violation", {
  expect_error(
    morie_fairness_equalized_odds(c(1, 0), c(1, 0), c("A", "A")),
    "at least two groups"
  )
  # group B has no positive ground-truth cases -> NA TPR -> warning
  eo <- morie_fairness_equalized_odds(
    y_true = c(1, 1, 0, 0, 0, 0, 0, 0),
    y_pred = c(1, 0, 1, 0, 1, 1, 0, 1),
    group = c(rep("A", 4), rep("B", 4)),
    privileged = "A"
  )
  expect_true(any(grepl("undefined", eo$warnings)))
  # a large, well-defined gap -> the violation interpretation
  viol <- morie_fairness_equalized_odds(
    y_true = c(1, 1, 0, 0, 1, 1, 0, 0),
    y_pred = c(1, 1, 0, 0, 0, 0, 1, 1),
    group = c(rep("A", 4), rep("B", 4)),
    privileged = "A"
  )
  expect_true(viol$violation)
  expect_match(viol$interpretation, "differ substantially")
})

test_that("morie_fairness_average_odds_difference runs with inferred privileged", {
  aod <- morie_fairness_average_odds_difference(
    y_true = c(1, 1, 0, 0, 1, 1, 0, 0),
    y_pred = c(1, 1, 0, 0, 0, 0, 1, 1),
    group  = c(rep("A", 4), rep("B", 4))
  )
  expect_true(is.numeric(aod$value))
  expect_match(aod$interpretation, "average odds difference")
})

# ---- morie_fairness_bias_amplification ------------------------------------------

test_that("morie_fairness_bias_amplification: amplified and quiet regimes", {
  amp <- morie_fairness_bias_amplification(c(1, 1, 1, 1, 0, 0, 0, 0),
    c(rep("A", 4), rep("B", 4)),
    privileged = "A"
  )
  expect_match(amp$interpretation, "directional disparity")
  quiet <- morie_fairness_bias_amplification(
    c(1, 1, 1, 0, 1, 1, 0, 1),
    c(rep("A", 4), rep("B", 4))
  )
  expect_match(quiet$interpretation, "little amplification")
})

# ---- study_reporting.R : internal module runners --------------------------

test_that(".run_power_design_module_extended runs on canonical CPADS data", {
  out <- morie:::.run_power_design_module_extended(make_canonical_cpads())
  expect_true(is.list(out))
})

test_that(".run_tables_module_internal runs with and without an output dir", {
  expect_true(is.list(morie:::.run_tables_module_internal(
    make_canonical_cpads()
  )))
  od <- tempfile("morie-tables-")
  dir.create(od, recursive = TRUE)
  expect_true(is.list(morie:::.run_tables_module_internal(
    make_canonical_cpads(),
    output_dir = od
  )))
})

test_that(".run_final_report_module_internal builds the coverage report", {
  # no output_dir -> a fresh tempdir is created
  r1 <- morie:::.run_final_report_module_internal(make_canonical_cpads())
  expect_true(is.list(r1))
  expect_true("ebac_final_output_coverage" %in% names(r1))

  # output_dir with a readable csv and an unreadable "csv" (a directory)
  od <- tempfile("morie-final-")
  dir.create(od, recursive = TRUE)
  utils::write.csv(data.frame(a = 1:3, b = 4:6),
    file.path(od, "binomial_summaries.csv"),
    row.names = FALSE
  )
  dir.create(file.path(od, "broken.csv")) # read.csv on a dir -> NULL -> next
  r2 <- morie:::.run_final_report_module_internal(make_canonical_cpads(),
    output_dir = od
  )
  expect_true(nrow(r2$ebac_final_output_shapes) >= 1L)
})
