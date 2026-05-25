# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for R/sprott_doob.R --- Sprott & Doob (CRIMSL) replication
# analyzers + Mandela classifier + chi-square verifier.

set.seed(1)

# ---------------------------------------------------------------------------
# Mandela classifier --- the three-way decision
# ---------------------------------------------------------------------------

test_that("classify_mandela returns Solitary for short <=2hr stays", {
  set.seed(1)
  r <- morie_siu_classify_mandela(10, 1.5, 100)
  expect_equal(r$category, "Solitary Confinement")
  expect_match(r$rule, "Rule 44")
  expect_match(r$reason, "<=2 hrs")
})

test_that("classify_mandela returns Torture for long <=2hr stays", {
  set.seed(1)
  r <- morie_siu_classify_mandela(20, 1.5, 100)
  expect_equal(r$category, "Torture")
  expect_match(r$rule, "Rules 43\\+44")
  expect_match(r$reason, "prolonged")
})

test_that("classify_mandela boundary day=15 is Solitary", {
  set.seed(1)
  r <- morie_siu_classify_mandela(15, 2, 100)
  expect_equal(r$category, "Solitary Confinement")
})

test_that("classify_mandela boundary day=16 is Torture", {
  set.seed(1)
  r <- morie_siu_classify_mandela(16, 2, 100)
  expect_equal(r$category, "Torture")
})

test_that("classify_mandela returns All other when >2 hrs out of cell", {
  set.seed(1)
  r <- morie_siu_classify_mandela(20, 5, 100)
  expect_equal(r$category, "All other")
  expect_equal(r$rule, "--")
})

test_that("classify_mandela returns All other when missed-pct below 100", {
  set.seed(1)
  r <- morie_siu_classify_mandela(20, 1, 80)
  expect_equal(r$category, "All other")
})

test_that("classify_mandela returns All other on boundary missed-pct=99.99", {
  set.seed(1)
  r <- morie_siu_classify_mandela(20, 1, 99.99)
  expect_equal(r$category, "All other")
})

test_that("classify_mandela reason names hours and days", {
  set.seed(1)
  r <- morie_siu_classify_mandela(30, 1.5, 100)
  expect_match(r$reason, "1.5")
  expect_match(r$reason, "30")
})


# ---------------------------------------------------------------------------
# Table 13 -- regional rates
# ---------------------------------------------------------------------------

test_that("sprott_doob_table13 returns the canonical RichResult shape", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table13()
  expect_s3_class(r, "morie_siu_result")
  expect_s3_class(r, "morie_rich_result")
  expect_match(r$title, "Table 13")
  expect_true(is.list(r$summary_lines))
  expect_true(is.list(r$tables))
  expect_match(r$interpretation, "Quebec")
})

test_that("sprott_doob_table13 ratio matches the 'almost 10 times' claim", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table13()
  expect_true(r$payload$qc_on_short_stay_ratio > 9)
  expect_true(r$payload$qc_on_short_stay_ratio < 11)
})

test_that("sprott_doob_table13 payload has the 6-region data frame", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table13()
  expect_s3_class(r$payload$table13, "data.frame")
  expect_equal(nrow(r$payload$table13), 6L)
  expect_true("region" %in% names(r$payload$table13))
})


# ---------------------------------------------------------------------------
# Table 19 -- Mandela classification
# ---------------------------------------------------------------------------

test_that("sprott_doob_table19 returns ~38% problematic", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table19()
  expect_s3_class(r, "morie_siu_result")
  expect_true(r$payload$pct_problematic > 37)
  expect_true(r$payload$pct_problematic < 39)
  expect_equal(r$payload$n_problematic, 556L + 195L)
})

test_that("sprott_doob_table19 summary mentions both Solitary and Torture", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table19()
  # Summary names + values both carry the headline categories.
  s <- paste(c(names(r$summary_lines), unlist(r$summary_lines)), collapse = " ")
  expect_match(s, "Solitary")
  expect_match(s, "Torture")
})


# ---------------------------------------------------------------------------
# Table 23 -- regional torture rates
# ---------------------------------------------------------------------------

test_that("sprott_doob_table23 pac_on ratio = 22.x", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table23()
  expect_true(r$payload$pac_on_torture_ratio > 22)
  expect_true(r$payload$pac_on_torture_ratio < 23)
})

test_that("sprott_doob_table23 interpretation mentions Pacific", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table23()
  expect_match(r$interpretation, "Pacific")
})


# ---------------------------------------------------------------------------
# Table 4 -- length of stay
# ---------------------------------------------------------------------------

test_that("sprott_doob_table4 returns the 5-bin distribution", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table4()
  expect_s3_class(r, "morie_siu_result")
  expect_equal(nrow(r$payload$table4), 5L)
  expect_match(r$interpretation, "20\\.8%")
})

test_that("sprott_doob_table4 summary N = 1983", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table4()
  expect_equal(r$summary_lines$N, 1983L)
})


# ---------------------------------------------------------------------------
# Table 11, 12, 15, 22 -- secondary tables
# ---------------------------------------------------------------------------

test_that("sprott_doob_table11 carries chi2 = 201.00", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table11()
  expect_equal(r$payload$chisq$chi2, 201.00)
  expect_equal(r$payload$chisq$df, 16L)
})

test_that("sprott_doob_table12 computes over_under_ratio per region", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table12()
  expect_true("over_under_ratio" %in% names(r$payload$table12))
  qc <- r$payload$table12[r$payload$table12$region == "Quebec", ]
  expect_true(qc$over_under_ratio > 1.5)
})

test_that("sprott_doob_table15 chi2 = 27.51 df=4", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table15()
  expect_equal(r$payload$chisq$chi2, 27.51)
  expect_equal(r$payload$chisq$df, 4L)
})

test_that("sprott_doob_table22 chi2 = 208.54 df=8", {
  set.seed(1)
  r <- morie_siu_sprott_doob_table22()
  expect_equal(r$payload$chisq$chi2, 208.54)
  expect_equal(r$payload$chisq$df, 8L)
})


# ---------------------------------------------------------------------------
# Iftene May 2021 tables
# ---------------------------------------------------------------------------

test_that("sprott_doob_iftene_table1 returns the 4 demographic blocks", {
  set.seed(1)
  r <- morie_siu_sprott_doob_iftene_table1()
  expect_equal(length(r$tables), 4L)
  expect_equal(r$payload$table1_iedm_population$n_total, 265L)
})

test_that("sprott_doob_iftene_table9 sums to 380 reviews", {
  set.seed(1)
  r <- morie_siu_sprott_doob_iftene_table9()
  expect_equal(sum(r$payload$table9$n), 380L)
})

test_that("sprott_doob_iftene_table10 finds min remain = 37.5%", {
  set.seed(1)
  r <- morie_siu_sprott_doob_iftene_table10()
  expect_equal(r$payload$min_remain_pct, 37.5)
  expect_equal(r$payload$max_remain_pct, 85.7)
})

test_that("sprott_doob_iftene_table15 long-stay-no-iedm = 105", {
  set.seed(1)
  r <- morie_siu_sprott_doob_iftene_table15()
  expect_equal(r$payload$long_stay_no_iedm, 105L)
  expect_match(r$interpretation, "structural failure")
})


# ---------------------------------------------------------------------------
# Chi-square verifier
# ---------------------------------------------------------------------------

test_that("verify_chi2 returns 0 for uniform 2x2", {
  set.seed(1)
  v <- morie_siu_verify_chi2(matrix(c(10, 10, 10, 10), nrow = 2))
  expect_equal(v$chi2, 0)
  expect_equal(v$df, 1L)
  expect_equal(v$n, 40L)
})

test_that("verify_chi2 detects 2x2 imbalance", {
  set.seed(1)
  v <- morie_siu_verify_chi2(matrix(c(20, 5, 5, 20), nrow = 2))
  expect_true(v$chi2 > 10)
  expect_true(v$p_value < 0.05)
  expect_equal(v$n, 50L)
})

test_that("verify_chi2 expected has correct dims", {
  set.seed(1)
  v <- morie_siu_verify_chi2(matrix(c(10, 20, 30, 40), nrow = 2))
  expect_equal(dim(v$expected), c(2L, 2L))
})

test_that("verify_chi2 errors on negative counts", {
  set.seed(1)
  expect_error(
    morie_siu_verify_chi2(matrix(c(-1, 5, 5, 5), nrow = 2)),
    "non-negative"
  )
})

test_that("verify_chi2 accepts data.frame input", {
  set.seed(1)
  d <- data.frame(a = c(10, 5), b = c(5, 10))
  v <- morie_siu_verify_chi2(d)
  expect_equal(v$df, 1L)
  expect_true(v$chi2 > 0)
})

test_that("verify_chi2 single-cell df=0 returns p=1", {
  set.seed(1)
  v <- morie_siu_verify_chi2(matrix(c(10, 10), nrow = 1))
  expect_equal(v$df, 0L)
  expect_equal(v$p_value, 1)
})


# ---------------------------------------------------------------------------
# verify_published_chi_squares
# ---------------------------------------------------------------------------

test_that("verify_published_chi_squares evaluates 5 tables", {
  set.seed(1)
  v <- morie_siu_verify_published_chi_squares()
  expect_s3_class(v, "morie_siu_result")
  expect_equal(v$payload$n_total, 5L)
  expect_true(v$payload$n_pass >= 3L)
})

test_that("verify_published_chi_squares returns a verification table", {
  set.seed(1)
  v <- morie_siu_verify_published_chi_squares()
  vt <- v$payload$verification_table
  expect_s3_class(vt, "data.frame")
  expect_equal(nrow(vt), 5L)
  expect_true(all(c("source", "recomputed_chi2", "published_chi2",
                    "pass") %in% names(vt)))
})

test_that("verify_published_chi_squares warnings are character", {
  set.seed(1)
  v <- morie_siu_verify_published_chi_squares()
  expect_type(v$warnings, "character")
})


# ---------------------------------------------------------------------------
# sprott_doob_feb2021 bundle
# ---------------------------------------------------------------------------

test_that("sprott_doob_feb2021 bundle has 3 sections + headline payload", {
  set.seed(1)
  r <- morie_siu_sprott_doob_feb2021()
  expect_s3_class(r, "morie_siu_result")
  expect_equal(length(r$tables), 3L)
  expect_equal(r$payload$headline_findings$n_total_stays, 1960L)
  expect_match(r$title, "Sprott")
  expect_match(r$title, "Doob")
})

test_that("sprott_doob_feb2021 interpretation cites the 22.6x figure", {
  set.seed(1)
  r <- morie_siu_sprott_doob_feb2021()
  expect_match(r$interpretation, "22\\.6")
})

test_that("sprott_doob_feb2021 summary names both authors", {
  set.seed(1)
  r <- morie_siu_sprott_doob_feb2021()
  expect_match(r$summary_lines$Authors, "Sprott")
  expect_match(r$summary_lines$Authors, "Doob")
})