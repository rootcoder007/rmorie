# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2RR: tests for internal helpers across bootstrap_methods.R,
# hawkes_fit.R, semipar_bridge.R, diagnostics.R, and siu_parser.R.

# ========================================================== bootstrap_methods.R

test_that(".new_bootstrap_result builds a morie_bootstrap_result list", {
  out <- morie:::.new_bootstrap_result(
    estimate = 0.5, se = 0.1,
    ci_lower = 0.3, ci_upper = 0.7, bias = 0.01,
    n_boot = 1000L, method = "nonparametric",
    ci_method = "percentile",
    boot_distribution = stats::rnorm(1000, 0.5, 0.1),
    original_estimate = 0.5)
  expect_type(out, "list")
  expect_s3_class(out, "morie_bootstrap_result")
})

test_that(".new_jackknife_result builds a morie_jackknife_result list", {
  out <- morie:::.new_jackknife_result(
    estimate = 0.5, se = 0.1,
    ci_lower = 0.3, ci_upper = 0.7, bias = 0.01,
    n = 100L, jackknife_estimates = stats::rnorm(100),
    pseudovalues = stats::rnorm(100),
    influence_values = stats::rnorm(100))
  expect_type(out, "list")
  expect_s3_class(out, "morie_jackknife_result")
})

test_that(".new_permutation_test_result has class + default NA CI", {
  out <- morie:::.new_permutation_test_result(
    observed_statistic = 2.5,
    p_value = 0.01,
    null_distribution = stats::rnorm(1000),
    n_permutations = 1000L,
    alternative = "two-sided")
  expect_s3_class(out, "morie_permutation_test_result")
  expect_true(is.na(out$ci_lower))
  expect_true(is.na(out$ci_upper))
})

test_that(".new_cv_result builds a morie_cv_result list", {
  out <- morie:::.new_cv_result(
    scores = c(0.8, 0.85, 0.82),
    mean_score = 0.823, se_score = 0.025,
    ci_lower = 0.78, ci_upper = 0.87,
    n_folds = 3L, metric = "accuracy",
    fold_sizes = c(30L, 30L, 30L))
  expect_type(out, "list")
  expect_s3_class(out, "morie_cv_result")
})

test_that(".pct equals stats::quantile type-7 linear interp", {
  x <- 1:100
  expect_equal(morie:::.pct(x, 50), 50.5)
  expect_equal(morie:::.pct(x, 95), 95.05)
})

test_that(".idx subsets a vector by integer index", {
  expect_equal(morie:::.idx(c(10, 20, 30, 40), c(1L, 3L)),
               c(10, 30))
})

test_that(".idx subsets a matrix preserving columns", {
  M <- matrix(1:12, nrow = 4L, ncol = 3L)
  out <- morie:::.idx(M, c(1L, 3L))
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(2L, 3L))
})

test_that(".nrow_like uses nrow for matrix/df, length otherwise", {
  expect_equal(morie:::.nrow_like(matrix(1:9, 3L, 3L)), 3L)
  expect_equal(morie:::.nrow_like(data.frame(a = 1:5, b = 6:10)), 5L)
  expect_equal(morie:::.nrow_like(c(1, 2, 3, 4, 5)), 5L)
})

# =============================================================== hawkes_fit.R

test_that(".hawkes_param_names returns 3-vec for exp / 4-vec otherwise", {
  expect_equal(morie:::.hawkes_param_names("exponential"),
               c("a0", "eta", "beta"))
  expect_equal(morie:::.hawkes_param_names("weibull"),
               c("a0", "eta", "alpha", "lambda"))
  expect_equal(morie:::.hawkes_param_names("lomax"),
               c("a0", "eta", "alpha", "c"))
  expect_equal(morie:::.hawkes_param_names("gamma"),
               c("a0", "eta", "alpha", "beta"))
})

test_that(".hawkes_param_names errors on unknown kernel", {
  expect_error(morie:::.hawkes_param_names("garch"),
               regexp = "unknown kernel")
})

test_that(".hawkes_to_theta + .hawkes_to_phi round-trip", {
  phi <- c(-1.0, 0.5, log(2), log(3))
  theta <- morie:::.hawkes_to_theta(phi)
  back <- morie:::.hawkes_to_phi(theta)
  expect_equal(back, phi, tolerance = 1e-10)
})

test_that(".hawkes_to_theta maps phi[2] -> (0,1) via plogis", {
  theta <- morie:::.hawkes_to_theta(c(0, 0, 0, 0))
  # plogis(0) = 0.5
  expect_equal(theta[2], 0.5, tolerance = 1e-10)
  # exp(0) = 1 for the shape params
  expect_equal(theta[3], 1)
})

test_that(".hawkes_loglik_poisson matches the closed-form", {
  # logLik = n * log(n/T) - n
  expect_equal(morie:::.hawkes_loglik_poisson(100L, 1000),
               100 * log(100 / 1000) - 100)
})

test_that(".hawkes_start returns the right-length init for each kernel", {
  set.seed(1L); t <- sort(stats::runif(50L, 0, 100))
  expect_length(morie:::.hawkes_start("exponential", t, 100), 3L)
  expect_length(morie:::.hawkes_start("weibull", t, 100), 4L)
  expect_length(morie:::.hawkes_start("lomax", t, 100), 4L)
  expect_length(morie:::.hawkes_start("gamma", t, 100), 4L)
})

test_that(".hawkes_kernel_funs returns g + G closures for exp kernel", {
  funs <- morie:::.hawkes_kernel_funs("exponential",
                                        c(0, 0.5, 2.0))
  expect_type(funs, "list")
  expect_true(is.function(funs$g) && is.function(funs$G))
  # g(0) = beta * exp(0) = 2; G(Inf) -> 1
  expect_equal(funs$g(0), 2)
  expect_equal(funs$G(1e9), 1, tolerance = 1e-10)
})

test_that(".hawkes_kernel_funs returns NULL on infeasible param", {
  expect_null(morie:::.hawkes_kernel_funs("exponential",
                                           c(0, 0.5, 0)))
})

test_that(".hawkes_nll_pureR returns penalty on infeasible eta", {
  out <- morie:::.hawkes_nll_pureR(theta = c(0, 0, 1.0),
                                     times = c(1, 2, 3),
                                     end_time = 10,
                                     kernel = "exponential")
  expect_equal(out, 1e12)
})

# ============================================================ semipar_bridge.R

test_that(".kernel_gaussian peaks at u = 0", {
  expect_equal(morie:::.kernel_gaussian(0),
               1 / sqrt(2 * pi), tolerance = 1e-10)
  # Symmetric around 0.
  expect_equal(morie:::.kernel_gaussian(1),
               morie:::.kernel_gaussian(-1), tolerance = 1e-10)
})

test_that(".kernel_epanechnikov is 0 outside [-1,1] and 0.75 at u=0", {
  expect_equal(morie:::.kernel_epanechnikov(0), 0.75)
  expect_equal(morie:::.kernel_epanechnikov(1.5), 0)
  expect_equal(morie:::.kernel_epanechnikov(-1.5), 0)
})

test_that(".kernel_uniform is 0.5 inside [-1,1] and 0 outside", {
  expect_equal(morie:::.kernel_uniform(0), 0.5)
  expect_equal(morie:::.kernel_uniform(0.5), 0.5)
  expect_equal(morie:::.kernel_uniform(1.5), 0)
})

test_that(".kernel_triangular is 1 at u=0 and 0 outside [-1,1]", {
  expect_equal(morie:::.kernel_triangular(0), 1)
  expect_equal(morie:::.kernel_triangular(0.5), 0.5)
  expect_equal(morie:::.kernel_triangular(1.5), 0)
})

test_that(".kernel_biweight is (15/16) at u=0 and 0 outside [-1,1]", {
  expect_equal(morie:::.kernel_biweight(0), 15 / 16,
               tolerance = 1e-10)
  expect_equal(morie:::.kernel_biweight(1.5), 0)
})

test_that(".kernel_fn returns each numbered kernel + Gaussian fallback", {
  # 0 -> gaussian; 1 -> epanechnikov; 2 -> uniform; 3 -> triangular;
  # 4 -> biweight; 5 -> gaussian fallback. (Out-of-range > 5 returns
  # NULL from switch(); the codebase only ever dispatches 0..5.)
  expect_equal(morie:::.kernel_fn(0L)(0), 1 / sqrt(2 * pi),
               tolerance = 1e-10)
  expect_equal(morie:::.kernel_fn(1L)(0), 0.75)
  expect_equal(morie:::.kernel_fn(2L)(0), 0.5)
  expect_equal(morie:::.kernel_fn(3L)(0), 1)
  expect_equal(morie:::.kernel_fn(4L)(0), 15 / 16, tolerance = 1e-10)
  expect_equal(morie:::.kernel_fn(5L)(0), 1 / sqrt(2 * pi),
               tolerance = 1e-10)
})

test_that(".resolve_kernel maps a name to a code via .kernel_name_map", {
  out <- morie:::.resolve_kernel("gaussian")
  expect_type(out, "integer")
})

test_that(".resolve_kernel passes a numeric code through", {
  expect_equal(morie:::.resolve_kernel(2), 2L)
})

# ============================================================== diagnostics.R

test_that(".new_residual_diag stamps morie_residual_diagnostics", {
  out <- morie:::.new_residual_diag(
    raw = 1:5, std = 1:5, student = 1:5, deviance = 1:5,
    pearson = 1:5, fitted = 1:5,
    normality = list(), hetero = list(), autoc = list(),
    outlier_indices = c(2L, 4L))
  expect_s3_class(out, "morie_residual_diagnostics")
  expect_equal(out$n_outliers, 2L)
})

test_that(".new_influence_diag stamps morie_influence_diagnostics", {
  out <- morie:::.new_influence_diag(
    h = 1:5, cooks = 1:5, dffits = 1:5,
    dfbetas = matrix(1, 5, 2),
    covratio = 1:5,
    influential = c(2L, 3L),
    high_lev = integer(0), high_cook = c(3L))
  expect_s3_class(out, "morie_influence_diagnostics")
  expect_equal(out$n_influential, 2L)
})

test_that(".new_collin_diag stamps morie_collinearity_diagnostics", {
  out <- morie:::.new_collin_diag(
    vif = c(1.2, 1.5), cond_num = 10, cond_idx = c(1, 10),
    var_decomp = matrix(0.5, 2, 2),
    eigvals = c(1.5, 0.5),
    n_collin = 0L, pairs = list())
  expect_s3_class(out, "morie_collinearity_diagnostics")
  expect_equal(out$n_collinear, 0L)
})

test_that(".new_spec_test stamps morie_specification_test", {
  out <- morie:::.new_spec_test(name = "RESET", statistic = 2.5,
                                  p_value = 0.04, df = 2L,
                                  conclusion = "borderline")
  expect_s3_class(out, "morie_specification_test")
})

test_that(".new_gof stamps morie_goodness_of_fit", {
  out <- morie:::.new_gof(r_squared = 0.8, adj_r_squared = 0.78,
                            pseudo_r_squared = NA,
                            aic = 100, bic = 110,
                            log_likelihood = -45, deviance = 90,
                            pearson_chi2 = 88, df_model = 3L,
                            df_residual = 96L, f_statistic = 25,
                            f_pvalue = 0.001, n_obs = 100L)
  expect_s3_class(out, "morie_goodness_of_fit")
})

test_that(".new_diag_report stamps morie_diagnostic_report", {
  out <- morie:::.new_diag_report(
    residuals = list(), influence = list(),
    collinearity = list(), gof = list(),
    spec_tests = list(), assessment = "ok")
  expect_s3_class(out, "morie_diagnostic_report")
})

test_that(".safe_solve inverts a non-singular matrix", {
  M <- diag(c(2, 3, 4))
  inv <- morie:::.safe_solve(M)
  expect_equal(M %*% inv, diag(3L), tolerance = 1e-10)
})

test_that(".safe_solve falls back to ginv on a singular matrix", {
  v <- c(1, 2, 3)
  M <- v %*% t(v)  # rank-1 singular
  out <- morie:::.safe_solve(M)
  expect_true(all(is.finite(out)))
  expect_equal(dim(out), c(3L, 3L))
})

# ================================================================ siu_parser.R

test_that(".siu_p_has_rvest returns logical", {
  expect_type(morie:::.siu_p_has_rvest(), "logical")
})

test_that(".siu_p_blank_row returns the canonical NA-row template", {
  row <- morie:::.siu_p_blank_row()
  expect_type(row, "list")
  for (k in c("drid", "nrid", "case_number", "police_service"))
    expect_true(k %in% names(row))
})

test_that(".siu_p_re_escape escapes regex metacharacters", {
  esc <- morie:::.siu_p_re_escape("a.b+c*d?")
  # Each metachar should be preceded by a backslash in the output.
  expect_match(esc, "a\\\\\\.b\\\\\\+c\\\\\\*d\\\\\\?")
})

test_that(".siu_p_parse_drid_from_url extracts numeric drid", {
  expect_equal(
    morie:::.siu_p_parse_drid_from_url(
      "https://www.siu.on.ca/en/news_template.php?drid=4321"),
    4321L)
  expect_true(is.na(morie:::.siu_p_parse_drid_from_url(NULL)))
  expect_true(is.na(morie:::.siu_p_parse_drid_from_url("")))
  expect_true(is.na(morie:::.siu_p_parse_drid_from_url(
    "https://example.com/no-drid")))
})

test_that(".siu_p_parse_nrid_from_url extracts numeric nrid", {
  expect_equal(
    morie:::.siu_p_parse_nrid_from_url(
      "https://www.siu.on.ca/en/news_template.php?nrid=7777"),
    7777L)
  expect_true(is.na(morie:::.siu_p_parse_nrid_from_url(NULL)))
})

test_that(".siu_p_normalise_sex maps synonyms to canonical labels", {
  expect_equal(morie:::.siu_p_normalise_sex("woman"), "Female")
  expect_equal(morie:::.siu_p_normalise_sex("F"),     "Female")
  expect_equal(morie:::.siu_p_normalise_sex("Man"),   "Male")
  expect_equal(morie:::.siu_p_normalise_sex("x"),     "Non-binary")
  expect_true(is.na(morie:::.siu_p_normalise_sex(NA)))
  expect_true(is.na(morie:::.siu_p_normalise_sex("")))
})

test_that(".siu_p_normalise_yes_no maps to TRUE/FALSE/NA", {
  expect_true(morie:::.siu_p_normalise_yes_no("Yes"))
  expect_true(morie:::.siu_p_normalise_yes_no("1"))
  expect_false(morie:::.siu_p_normalise_yes_no("No"))
  expect_false(morie:::.siu_p_normalise_yes_no("0"))
  expect_true(is.na(morie:::.siu_p_normalise_yes_no(NA)))
  expect_true(is.na(morie:::.siu_p_normalise_yes_no("maybe")))
})

test_that(".siu_p_parse_date parses the canonical formats to ISO", {
  out <- morie:::.siu_p_parse_date("January 5, 2024")
  expect_equal(out$iso, "2024-01-05")
  out2 <- morie:::.siu_p_parse_date("2024-01-05")
  expect_equal(out2$iso, "2024-01-05")
})

test_that(".siu_p_parse_date returns NA on unparseable input", {
  out <- morie:::.siu_p_parse_date("not-a-date")
  expect_true(is.na(out$iso))
  out2 <- morie:::.siu_p_parse_date(NA)
  expect_true(is.na(out2$iso))
})

test_that(".siu_p_find_case_number finds the SIU case-number pattern", {
  expect_equal(
    morie:::.siu_p_find_case_number(
      "Incident occurred. Case number 24-OFD-001 was opened."),
    "24-OFD-001")
  expect_true(is.na(morie:::.siu_p_find_case_number("no case here")))
})

test_that(".siu_p_label_value extracts a labelled value", {
  text <- "Location of call: 123 Main Street\nPolice service: TPS"
  out <- morie:::.siu_p_label_value(text, "Location of call")
  expect_equal(out, "123 Main Street")
})

test_that(".siu_p_label_int extracts the first integer after the label", {
  text <- "Number of officers involved: 3 (one subject)"
  expect_equal(morie:::.siu_p_label_int(text, "Number of officers involved"),
               3L)
  expect_true(is.na(morie:::.siu_p_label_int("no match", "label")))
})

test_that(".siu_p_detect_language returns 'en' / 'fr' / 'unknown'", {
  out <- morie:::.siu_p_detect_language("plain ASCII text with no markers")
  expect_true(out %in% c("en", "fr", "unknown"))
})
