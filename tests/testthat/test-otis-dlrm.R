# SPDX-License-Identifier: AGPL-3.0-or-later
# The OTIS DLRM estimators and Ruhela-formulation analyses; every expected
# value is recomputed in the test body.

.dlrm_df <- function(n = 400L, seed = 3L) {
  set.seed(seed)
  x <- rnorm(n)
  g <- sample(c("a", "b", "c"), n, TRUE)
  d <- rbinom(n, 1, plogis(0.5 * x))
  data.frame(d = d, y = 1 + 0.7 * d + x + (g == "b") + rnorm(n), x = x, g = g,
             yr = rep(2023:2024, length.out = n), stringsAsFactors = FALSE)
}

test_that("g-computation of a linear outcome model is its treatment coefficient", {
  df <- .dlrm_df()
  g <- morie_otis_gcomputation(df, "d", "y", c("x", "g"), n_bootstrap = 40L)
  expect_equal(g$ate, unname(coef(lm(y ~ d + x + g, df))["d"]), tolerance = 1e-10)
  # bootstrap SE: the same resamples, refitted by lm
  set.seed(123)
  b <- vapply(1:40, function(i) {
    s <- df[sample.int(nrow(df), nrow(df), replace = TRUE), ]
    unname(coef(lm(y ~ d + x + g, s))["d"])
  }, numeric(1))
  expect_equal(g$ate_se, sd(b), tolerance = 1e-10)
  expect_identical(g$estimator, "g-computation")
})

test_that("the ATC is IRM-DML's control-arm score on the same five folds", {
  df <- .dlrm_df()
  a <- morie_otis_atc(df, "d", "y", c("x", "g"))
  f <- morie_otis_irm_dml(df, "d", "y", c("x", "g"), n_folds = 5L)
  expect_equal(c(a$ate, a$ate_se), c(f$atc, f$atc_se), tolerance = 1e-14)
  expect_identical(a$n, nrow(df))
})

test_that("balance reports pooled-SD SMDs raw, IPW-weighted and matched", {
  df <- .dlrm_df()
  b <- morie_otis_balance(df, "d", c("x", "g"))
  expect_identical(b$covariate, c("x", "gb", "gc"))
  xt <- df$x[df$d == 1]
  xc <- df$x[df$d == 0]
  expect_equal(b$smd_raw[1], (mean(xt) - mean(xc)) / sqrt((var(xt) + var(xc)) / 2),
               tolerance = 1e-12)
  X <- model.matrix(~ x + g, df)
  e <- pmin(pmax(plogis(X %*% rmorie:::.otis_logit_fit(X, df$d)), 0.02), 0.98)
  w <- ifelse(df$d == 1, 1 / e, 1 / (1 - e))
  m1 <- weighted.mean(df$x[df$d == 1], w[df$d == 1])
  m0 <- weighted.mean(df$x[df$d == 0], w[df$d == 0])
  v1 <- sum(w[df$d == 1] * (df$x[df$d == 1] - m1)^2) / sum(w[df$d == 1])
  v0 <- sum(w[df$d == 0] * (df$x[df$d == 0] - m0)^2) / sum(w[df$d == 0])
  expect_equal(b$smd_ipw[1], (m1 - m0) / sqrt((v1 + v0) / 2), tolerance = 1e-10)
  expect_lt(abs(b$smd_ipw[1]), abs(b$smd_raw[1]))  # x drives treatment; weighting removes most of it
})

test_that("per-year IRM-DML is the IRM-DML of each year's rows", {
  df <- .dlrm_df()
  py <- morie_otis_per_year_irm_dml(df, "d", "y", "x", year_col = "yr")
  expect_identical(names(py), c("2023", "2024"))
  expect_equal(py[["2024"]]$ate, morie_otis_irm_dml(df[df$yr == 2024, ], "d", "y", "x")$ate,
               tolerance = 1e-14)
  fb <- morie_otis_per_year_irm_dml(df, "d", "y", "x", year_col = "yr", full_battery = TRUE)
  expect_equal(fb[["2023"]]$ipw$ate, morie_otis_ipw_ate(df[df$yr == 2023, ], "d", "y", "x")$ate,
               tolerance = 1e-14)
  expect_equal(fb[["2023"]]$match_first$ate,
               morie_otis_irm_dml(df[df$yr == 2023, ], "d", "y", "x", match_first = TRUE)$ate,
               tolerance = 1e-14)
})

test_that("a01 Ruhela formulations run all ten estimators on the canonical pair", {
  a01 <- morie_synth_otis("a01", n = 600L, seed = 4L)
  r <- morie_otis_analyze_a01_ruhela_formulations(a01)
  expect_s3_class(r, "morie_otis_analysis_result")
  expect_identical(morie_otis_analyze_a01_dlrm, morie_otis_analyze_a01_ruhela_formulations)
  rows <- r$tables[[1L]]$rows
  expect_length(rows, 12L)
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(a01)
  ipw <- morie_otis_ipw_ate(pair$data, pair$T, pair$Y, pair$covariates)
  expect_identical(rows[[1L]][3L], sprintf("%+.4f", ipw$ate))
  irm <- morie_otis_irm_dml(pair$data, pair$T, pair$Y, pair$covariates,
                            cluster_cols = "EndFiscalYear")
  expect_identical(rows[[6L]][3L], sprintf("%+.4f", irm$ate))
  expect_identical(r$summary_lines$n, nrow(pair$data))
  # four SEs, one point estimate; Naive arm present
  expect_length(r$tables, 3L)
  expect_true(all(vapply(r$tables[[2L]]$rows, `[`, "", 2L) == sprintf("%+.4f", irm$ate)))
})

test_that("b02 formulations treat on Female and adjust for region, age and year", {
  b02 <- morie_synth_otis("b02", n = 600L, seed = 5L)
  r <- morie_otis_analyze_b02_ruhela_formulations(b02)
  expect_identical(r$payload$formulation$treatment, "T_female")
  w <- b02
  w$T_female <- as.integer(tolower(w$Gender) == "female")
  ipw <- morie_otis_ipw_ate(w, "T_female", "TotalAggregatedDays_Segregation",
                            c("Region_MostRecentPlacement", "Age_Category", "EndFiscalYear"))
  expect_equal(r$payload$ensemble[[1L]]$estimate, ipw$ate, tolerance = 1e-14)
  expect_length(r$tables, 2L)  # no Naive arm; UniqueIndividual_ID not in the arm -> n/a row
  expect_identical(r$tables[[2L]]$rows[[3L]][3L], "n/a (col missing)")
  expect_error(morie_otis_analyze_b02_ruhela_alt_age(data.frame(Gender = "Female")),
               "missing column")
})

test_that("alt-T and subgroup formulations build their treatment from the cell frame", {
  a01 <- morie_synth_otis("a01", n = 600L, seed = 6L)
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(a01)
  r <- morie_otis_analyze_a01_ruhela_alt_toronto(a01)
  expect_identical(r$payload$formulation$treatment, "T_toronto")
  expect_identical(r$summary_lines$`Treated prevalence`,
                   sprintf("%.1f%%", 100 * mean(tolower(pair$data$regA) == "toronto")))
  m <- morie_otis_analyze_a01_ruhela_subgroup_male(a01)
  expect_identical(m$summary_lines$n, sum(tolower(pair$data$Gender) != "female"))
  expect_false("Gender" %in% m$payload$formulation$covariates)
  tiny <- morie_otis_analyze_b01_ruhela_subgroup_female(morie_synth_otis("b01", n = 40L, seed = 1L))
  expect_match(tiny$warnings, "female cells; too few")
})

test_that("the per-year driver tabulates twelve rows per fiscal year", {
  df <- .dlrm_df(n = 600L)
  r <- morie_otis_analyze_ruhela_per_year(df, ds_id = "t", treatment = "d", outcome = "y",
                                          covariates = "x", year_col = "yr", cluster_col = NULL)
  expect_length(r$tables[[1L]]$rows, 24L)
  expect_identical(r$summary_lines$`Years analysed`, 2L)
  fb <- r$payload$by_year
  expect_identical(r$tables[[1L]]$rows[[1L]][3L], sprintf("%+.4f", fb[["2023"]]$ipw$ate))
})

test_that("the CSI context is the StatsCan index of the supplied TPS counts", {
  a01 <- morie_synth_otis("a01", n = 400L, seed = 7L)
  cats <- MORIE_TPS_CSI_CATEGORIES()
  tps <- stats::setNames(lapply(seq_along(cats), function(i)
    data.frame(OCC_YEAR = rep(2022:2025, times = 5 * i))), cats)
  r <- morie_otis_analyze_a01_with_csi_context(a01, tps_data = tps)
  counts <- lapply(stats::setNames(nm = as.character(2022:2025)), function(y)
    stats::setNames(as.list(5L * seq_along(cats)), cats))
  ref <- morie_tps_csi_per_year(counts, variant = "total", rebase_to_year = 2023L)
  rows <- r$tables[[1L]]$rows
  expect_identical(vapply(rows, `[`, "", 1L), c("2023", "2024", "2025"))
  expect_identical(rows[[2L]][4L], as.character(round(ref$csi_per_capita[ref$year == 2024], 2)))
  expect_identical(rows[[1L]][5L], "100")
  none <- morie_otis_analyze_a01_with_csi_context(a01, tps_data = list())
  expect_match(none$warnings, "CSI context unavailable", all = FALSE)
})

test_that("an OTIS table that cannot be loaded says to pass data", {
  testthat::local_mocked_bindings(morie_load_dataset = function(...) stop("offline"),
                                  .package = "rmorie")
  expect_error(morie_otis_analyze_b02_ruhela_formulations(), "pass data = the OTIS b02 table")
})
