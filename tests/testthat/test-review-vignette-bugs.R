# Regression tests for the vignette-review source bugs (numbered as in the review).

test_that("bug 4: mrm_siu_outcome_classifier derives the outcome from charges_recommended", {
  siu <- data.frame(
    police_service = c("A", "A", "A", "B"),
    charges_recommended = c(TRUE, FALSE, "No Charges to Issue", NA)
  )
  out <- mrm_siu_outcome_classifier(siu)
  expect_equal(out$n_cases[out$service == "A" & out$outcome == "no charges"], 2L)
  expect_equal(out$n_cases[out$service == "A" & out$outcome == "charges"], 1L)
  expect_false("B" %in% out$service) # an unreadable decision is dropped
  expect_error(mrm_siu_outcome_classifier(data.frame(police_service = "A", x = 1)),
               "no outcome column.*charges_recommended")
  expect_error(mrm_siu_outcome_classifier(data.frame(outcome = "x")), "service column")
  skip_if_not_installed("rmoriedata")
  corpus <- utils::read.csv(suppressMessages(morie_fetch_siu(lang = "en")))
  res <- mrm_siu_outcome_classifier(corpus)
  expect_setequal(unique(res$outcome), c("charges", "no charges"))
  expect_gt(sum(res$n_cases), 2000)
})

test_that("bug 5: morie_siu_sanity_check does not flag missing values", {
  df <- data.frame(
    case_number = c("19-TCI-001", "19-TCI-002"),
    date_of_incident_iso = c(NA, "2019-01-02"),
    number_of_affected_persons = c(NA_real_, 2),
    age_affected = c(NA_integer_, 30L),
    charges_recommended = c(NA, FALSE),
    police_service = c("Toronto Police Service", "Toronto Police Service"),
    narrative_summary = strrep("x", 120),
    stringsAsFactors = FALSE
  )
  out <- morie_siu_sanity_check(df)
  expect_equal(out$issues_count, c(0L, 0L))
  chr <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  chr[is.na(chr)] <- ""
  expect_identical(morie_siu_sanity_check(chr)$issues, out$issues)
  skip_if_not_installed("rmoriedata")
  f <- suppressMessages(morie_fetch_siu(lang = "en"))
  a <- morie_siu_sanity_check(utils::read.csv(f))
  b <- morie_siu_sanity_check(utils::read.csv(f, colClasses = "character"))
  expect_lt(sum(a$issues_count > 0), nrow(a) / 10)
  expect_identical(a$issues[order(a$case_number)], b$issues[order(b$case_number)])
})

test_that("bug 6: the bundled b05 and c11 samples use the real OTIS duration bins", {
  # the OTIS samples are left out of the build tarball (.Rbuildignore), so a check of it has none
  f05 <- system.file("extdata", "otis_b05_seg_consecutive_lengths_sample.csv", package = "rmorie")
  f11 <- system.file("extdata", "otis_c11_aggregate_lengths_sample.csv", package = "rmorie")
  skip_if(!nzchar(f05) || !nzchar(f11), "the OTIS samples are not in this build")
  b05 <- utils::read.csv(f05)
  expect_false(any(.otis_classify_bin(b05$Consecutive_Duration) == "Unknown"))
  res <- morie_otis_analyze_b05_mandela_classification(b05)
  expect_length(res$payload$rows, length(unique(b05$EndFiscalYear)))
  expect_false(identical(res$summary_lines[["Latest provincial torture rate"]], "n/a"))
  c11 <- utils::read.csv(f11)
  expect_false(any(.otis_classify_bin(c11$Aggregate_Duration) == "Unknown"))
})

test_that("bug 7: TPS spatial functions drop (0, 0) and out-of-Toronto points and report it", {
  set.seed(7)
  n <- 120L
  df <- data.frame(
    OCC_DATE = format(as.Date("2020-01-01") + seq_len(n + 6L)),
    LAT_WGS84 = c(43.65 + stats::rnorm(n, 0, 0.03), 0, 0, 0, 45.4, NA, 43.7),
    LONG_WGS84 = c(-79.40 + stats::rnorm(n, 0, 0.03), 0, 0, 0, -75.7, -79.4, NA)
  )
  m <- mrm_tps_moran_clustering(df, grid_resolution = 10L)
  expect_equal(m$n_dropped_coords, 6L)
  expect_equal(m$n_events, n)
  expect_equal(mrm_tps_levy_scaling(df)$n_dropped_coords, 6L)
  expect_equal(morie_tps_ripley_k(df)$n_dropped_coords, 6L)
  expect_equal(morie_tps_dbscan_clusters(df)$n_dropped_coords, 6L)
  expect_equal(morie_tps_kde_density(df)$n_dropped_coords, 6L)
  expect_equal(morie_tps_spatial_summary(df)$summary_lines[["Geocoded incidents"]], n)
  skip_if_not_installed("rmoriedata")
  t <- morie_sample("tps_assault")
  r <- mrm_tps_moran_clustering(t, grid_resolution = 20L)
  expect_equal(r$n_dropped_coords, sum(t$LAT_WGS84 == 0 & t$LONG_WGS84 == 0))
  expect_gt(r$morans_I, 0.5)
  expect_gt(r$morans_z, 10)
})

test_that("bug 8: analyze_doob_table1_releases computes its headline from its own numbers", {
  res <- analyze_doob_table1_releases()
  tbl <- CCRSO_TABLE1_RELEASES
  v <- 100 * sum(tbl$revoke_violent) / sum(tbl$total)
  expect_equal(res$payload$violent_revocation_rate_pct, v)
  expect_equal(res$payload$not_violent_pct, 100 - v)
  head_txt <- res$summary_lines[[5]][2]
  expect_match(head_txt, sprintf("%.2f%%", 100 - v), fixed = TRUE)
  expect_false(grepl("99.4", head_txt, fixed = TRUE))
  t2 <- analyze_doob_table2_flow()
  expect_equal(t2$payload$early_release_pct,
               100 * 6 * t2$payload$monthly_releases / t2$payload$avg_count)
  t3 <- analyze_doob_table3_age_overrepresentation()
  old <- CCRSO_TABLE3_AGE$age_group != "18-49"
  expect_equal(t3$summary_lines[[2]][2],
               sprintf("%.1f%%", sum(CCRSO_TABLE3_AGE$canada_adult_pop_pct[old])))
})

test_that("bug 9: morie_e_value returns e_value, and sibling keys lost the morie_ prefix", {
  r <- morie_e_value(rr = 3.9, rr_lower = 2.4)
  expect_named(r, c("e_value", "e_value_ci"))
  expect_equal(r$e_value, 3.9 + sqrt(3.9 * 2.9))
  expect_equal(r$e_value_ci, 2.4 + sqrt(2.4 * 1.4))
  expect_error(morie_e_value(3.9, rr_lower = c(2, 3)), "rr_lower must be a positive")
  set.seed(9)
  x <- stats::rnorm(64)
  y <- x + stats::rnorm(64)
  expect_true("coherence" %in% names(morie_coherence(x, y)))
  expect_true("coherence" %in% names(rgcoh(x, y)))
  expect_true("kendall_tau" %in% names(copul(x, y)))
  expect_true("spearman_rho" %in% names(morie_prediction_accuracy(x, y)))
})

test_that("bug 10: morie_dag_estimate returns unique names for every method", {
  set.seed(10)
  z <- stats::rnorm(300)
  x <- stats::rbinom(300, 1, stats::plogis(z))
  y <- 0.8 * x + z + stats::rnorm(300)
  d <- data.frame(z = z, x = x, y = y)
  g <- morie_dag(c("z -> x", "z -> y", "x -> y"), "x", "y")
  for (m in c("backdoor.aipw", "backdoor.linear", "backdoor.dml")) {
    r <- morie_dag_estimate(g, d, m)
    expect_false(anyDuplicated(names(r)) > 0, info = m)
    expect_identical(r$method, paste0(m, " (rmorie native)"))
    expect_identical(r$estimand, "backdoor")
  }
  expect_true("estimator_method" %in% names(morie_dag_estimate(g, d, "backdoor.dml")))
})

test_that("bug 11: negative Delta_MH favours the reference group (Holland-Thayer)", {
  # stratum 1: reference 8/10 right, focal 5/10; stratum 2: reference 6/10, focal 3/10
  y <- c(rep(1, 8), rep(0, 2), rep(1, 5), rep(0, 5),
         rep(1, 6), rep(0, 4), rep(1, 3), rep(0, 7))
  g <- rep(rep(c("r", "f"), each = 10), 2)
  k <- rep(1:2, each = 20)
  a <- Difmh(y, g, item = k, reference = "r")$alpha_MH
  expect_equal(a, (8 * 5 / 20 + 6 * 7 / 20) / (2 * 5 / 20 + 4 * 3 / 20))
  d <- morie_dif_delta_mh(a)
  expect_lt(d$delta_mh, 0)
  expect_identical(d$favours, "reference")
  expect_identical(morie_dif_delta_mh(1 / a)$favours, "focal")
})

test_that("bug 12: morie_run_propensity_ipw_analysis applies the survey weight", {
  set.seed(12)
  n <- 300
  cp <- data.frame(
    weight = stats::runif(n, 0.2, 3),
    alcohol_past12m = 1L,
    heavy_drinking_30d = stats::rbinom(n, 1, 0.3),
    ebac_tot = 0.05, ebac_legal = 1L,
    cannabis_any_use = stats::rbinom(n, 1, 0.4),
    age_group = sample(1:3, n, TRUE), gender = sample(1:2, n, TRUE),
    province_region = sample(1:3, n, TRUE), mental_health = sample(1:3, n, TRUE),
    physical_health = sample(1:3, n, TRUE)
  )
  res <- morie_run_propensity_ipw_analysis(cp)
  f <- res$analysis_frame
  # hand computation from the frame's own ipw_trimmed and the survey weight
  w <- f$weight * f$ipw_trimmed
  t1 <- f$cannabis_any_use == 1
  m1 <- sum(w[t1] * f$heavy_drinking_30d[t1]) / sum(w[t1])
  m0 <- sum(w[!t1] * f$heavy_drinking_30d[!t1]) / sum(w[!t1])
  v1 <- sum((w[t1] * (f$heavy_drinking_30d[t1] - m1))^2) / sum(w[t1])^2
  v0 <- sum((w[!t1] * (f$heavy_drinking_30d[!t1] - m0))^2) / sum(w[!t1])^2
  expect_equal(res$ipw_results$estimate, m1 - m0)
  expect_equal(res$ipw_results$se, sqrt(v1 + v0))
  expect_equal(res$ipw_results$ci_lower, m1 - m0 - 1.96 * sqrt(v1 + v0))
  cp1 <- cp
  cp1$weight <- 1
  expect_false(isTRUE(all.equal(morie_run_propensity_ipw_analysis(cp1)$ipw_results$estimate,
                                res$ipw_results$estimate)))
})

test_that("bug 13: mrm_otis_seg_duration_km names the 75th percentile q75_days", {
  d <- data.frame(NumberConsecutiveDays_Segregation = c(1:20, NA))
  r <- mrm_otis_seg_duration_km(d)
  expect_false("q25_days" %in% names(r))
  expect_equal(r$q75_days, unname(stats::quantile(1:20, 0.75)))
  expect_equal(r$q75_days, r$days_at_S25)
})

test_that("bug 14: morie_otis_irr_glmm_vm uses the Ruhela builder's T and Y", {
  df <- data.frame(
    UniqueIndividual_ID = c("p1", "p1", "p2", "p3", "p3"),
    EndFiscalYear = c(2023, 2023, 2023, 2024, 2024),
    Gender = c("M", "M", "F", "M", "M"),
    Age_Category = c("18-24", "18-24", "25-34", "18-24", "18-24"),
    Region_AtTimeOfPlacement = c("Toronto", "Toronto", "Eastern", "Central", "Western"),
    Region_MostRecentPlacement = c("Toronto", "Eastern", "Eastern", "Central", "Western"),
    MentalHealth_Alert = c("No", "Yes", "No", "Yes", "Yes"),
    SuicideRisk_Alert = c("No", "No", "No", "No", "Yes"),
    SuicideWatch_Alert = "No"
  )
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(df)
  # p1: combos {0, 4} -> ac 2 (the no-alert combo counts); one within-row region change
  # p2: one combo, no change; p3: combos {4, 6}, one row-to-row region change
  expect_equal(pair$data$T_high_ac, c(1L, 0L, 1L))
  expect_equal(pair$data$Y_vm_count, c(1L, 0L, 1L))
  res <- morie_otis_irr_glmm_vm(df)
  expect_equal(res$payload$mean_vm, mean(pair$data$Y_vm_count))
  expect_identical(res$summary_lines[["T_high_ac prevalence"]],
                   sprintf("%.1f%%", 100 * mean(pair$data$T_high_ac)))
  expect_match(res$summary_lines[["Outcome Y"]], "region changes")
  expect_match(morie_otis_irr_glmm_vm(df[, -6])$warnings, "Region_MostRecentPlacement")
})

test_that("bug 15: morie_siu_compare errors and names unmatched field_map entries", {
  local_mocked_bindings(
    morie_siu_audit_case = function(...) {
      list(row = data.frame(case_number = "17-OVI-201", number_of_subject_officials = "1",
                            stringsAsFactors = FALSE),
           report_text = "")
    },
    .package = "rmorie"
  )
  ext <- data.frame(case_id = "17-OVI-201", n_officers = 1L)
  expect_error(
    morie_siu_compare("17-OVI-201", ext,
                      field_map = c(number_of_subject_officials = "n_officers"),
                      external_case_col = "case_id"),
    "external column 'number_of_subject_officials' not found"
  )
  ok <- morie_siu_compare("17-OVI-201", ext,
                          field_map = c(n_officers = "number_of_subject_officials",
                                        missing_col = "x"),
                          external_case_col = "case_id")
  expect_equal(nrow(ok), 1L)
  expect_true(ok$agree)
  expect_identical(attr(ok, "unmatched_fields"), "external column 'missing_col' not found")
})
