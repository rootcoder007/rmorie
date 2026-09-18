# SPDX-License-Identifier: AGPL-3.0-or-later
# otis-analysis + mapq-psychometrics pipeline module runners.

test_that(".otis_b01_canonical maps the b01 headers to the analyzer schema", {
  df <- rmorie:::.otis_b01_canonical(morie_sample("otis_b01"))
  expect_true(all(c(
    "end_fiscal_year", "unique_individual_id", "gender",
    "region_at_time_of_placement", "region_most_recent_placement",
    "age_category", "mental_health_alert", "suicide_risk_alert",
    "suicide_watch_alert", "number_consecutive_days_segregation"
  ) %in% names(df)))
})

test_that("otis-analysis runner emits the four declared output frames", {
  out <- rmorie:::.run_otis_analysis_module_internal()
  expect_named(out, c(
    "otis_descriptives", "otis_alert_combos",
    "otis_dml_results", "otis_trends"
  ))
  for (nm in names(out)) expect_s3_class(out[[nm]], "data.frame")

  desc <- out$otis_descriptives
  expect_true("n_individuals_unique" %in% desc$metric)
  expect_true(all(is.finite(desc$value)))
  expect_gt(desc$value[desc$metric == "n_records"], 0)

  combos <- out$otis_alert_combos
  expect_named(combos, c("ac", "n_persons"))
  expect_true(all(combos$n_persons > 0))

  dml <- out$otis_dml_results
  expect_equal(dml$estimand, c("ATE", "ATT"))
  expect_true(all(is.finite(dml$estimate)))
  expect_true(all(dml$se > 0))
  expect_true(all(dml$p_value >= 0 & dml$p_value <= 1))

  trends <- out$otis_trends
  expect_true(all(c("year", "region", "n_individuals", "n_placements") %in% names(trends)))
  expect_gt(nrow(trends), 0)
})

test_that("MAPQ synthetic panel is deterministic and structurally valid", {
  p1 <- rmorie:::.morie_mapq_synth_panel()
  p2 <- rmorie:::.morie_mapq_synth_panel()
  expect_identical(p1, p2)
  items <- unlist(rmorie:::.mapq_subscales(), use.names = FALSE)
  expect_length(items, 20L)
  expect_true(all(items %in% names(p1)))
  expect_true(all(as.matrix(p1[, items]) %in% 1:5))
  expect_true(all(p1$gender_male %in% 0:1))
})

test_that("mapq-psychometrics runner emits the three declared output frames", {
  out <- rmorie:::.run_mapq_psychometrics_module_internal()
  expect_named(out, c(
    "mapq_reliability", "mapq_factor_loadings", "mapq_dml_results"
  ))

  rel <- out$mapq_reliability
  expect_equal(rel$scale, c("EE", "EA", "UA", "ER", "Total"))
  # The planted factor structure is strong; subscale alphas must be high.
  expect_true(all(rel$alpha_raw[rel$scale != "Total"] > 0.7))
  expect_true(all(rel$omega_total >= 0 & rel$omega_total <= 1))
  expect_true(all(rel$splithalf_sb <= 1))

  loads <- out$mapq_factor_loadings
  expect_equal(nrow(loads), 20L)
  expect_equal(loads$assigned_subscale, rep(c("EE", "EA", "UA", "ER"), each = 5L))
  # Each item loads dominantly (> 0.4) on some factor.
  lmat <- abs(as.matrix(loads[, c("factor1", "factor2", "factor3", "factor4")]))
  expect_true(all(apply(lmat, 1, max) > 0.4))

  dml <- out$mapq_dml_results
  expect_equal(dml$estimand, c("ATE", "ATT"))
  # The generator plants a +1.5 gender effect on KS; DML must recover it.
  ate <- dml$estimate[dml$estimand == "ATE"]
  expect_gt(ate, 0.8)
  expect_lt(ate, 2.2)
  expect_lt(dml$p_value[dml$estimand == "ATE"], 0.05)
})

test_that("morie_run_morie_module dispatches the two new modules", {
  mods <- morie_list_morie_modules()
  expect_true(all(c("otis-analysis", "mapq-psychometrics") %in% mods$name))
})
