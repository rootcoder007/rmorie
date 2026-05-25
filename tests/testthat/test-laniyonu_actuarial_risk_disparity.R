# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/laniyonu_actuarial_risk_disparity.R

set.seed(2026L)

mk_ord_data <- function(n = 200L) {
  black <- rbinom(n, 1, 0.3)
  asian <- rbinom(n, 1, 0.2)
  indig <- rbinom(n, 1, 0.15)
  gender <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.7, 0.3))
  age <- rnorm(n)
  priors <- rpois(n, 1)
  # Stronger bias at low->med threshold than med->high
  z1 <- 0.5 * black + 0.3 * asian + 0.4 * indig + 0.2 * age + rnorm(n)
  cuts <- quantile(z1, c(0.33, 0.66))
  static_score <- ifelse(z1 < cuts[1], "low",
                  ifelse(z1 < cuts[2], "medium", "high"))
  dynamic_score <- static_score
  offender_security_level <- static_score
  reintegration_potential <- static_score
  data.frame(
    static_score = static_score,
    dynamic_score = dynamic_score,
    offender_security_level = offender_security_level,
    reintegration_potential = reintegration_potential,
    black = black, asian = asian, indig = indig,
    gender = gender, age = age, priors = priors,
    stringsAsFactors = FALSE
  )
}

mk_binary_data <- function(n = 200L) {
  d <- mk_ord_data(n)
  d$reintegration_score_numeric <- rnorm(n)
  d$osl_score_numeric <- rnorm(n)
  z <- 0.4 * d$black + 0.3 * d$reintegration_score_numeric + rnorm(n)
  d$parole_granted <- as.integer(plogis(z) > runif(n))
  d$housing_level <- as.integer(plogis(z + 0.1 * d$asian) > runif(n))
  d
}

test_that("morie_laniyonu_actuarial_risk_disparity ordinal path (split_by_gender)", {
  df <- mk_ord_data(200L)
  res <- suppressWarnings(
    morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "static",
      race_cols = c("black", "asian"),
      gender_col = "gender",
      control_cols = c("age"),
      split_by_gender = TRUE
    )
  )
  expect_s3_class(res, "morie_laniyonu_ard_result")
  expect_equal(res$outcome_kind, "ordinal")
  expect_true(res$n_obs > 0L)
  expect_true(is.character(res$interpretation))
})

test_that("morie_laniyonu_actuarial_risk_disparity ordinal pooled (split_by_gender=FALSE)", {
  df <- mk_ord_data(200L)
  res <- suppressWarnings(
    morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "dynamic",
      race_cols = c("black"),
      gender_col = "gender",
      control_cols = character(0),
      split_by_gender = FALSE
    )
  )
  expect_s3_class(res, "morie_laniyonu_ard_result")
  expect_true("pooled" %in% names(res$ordinal_result))
})

test_that("morie_laniyonu_actuarial_risk_disparity all ordinal outcomes accepted", {
  df <- mk_ord_data(200L)
  for (oc in c("static", "dynamic", "osl", "reintegration")) {
    res <- suppressWarnings(
      morie_laniyonu_actuarial_risk_disparity(
        df, outcome = oc,
        race_cols = c("black"),
        gender_col = "gender",
        control_cols = character(0),
        split_by_gender = TRUE
      )
    )
    expect_s3_class(res, "morie_laniyonu_ard_result")
  }
})

test_that("morie_laniyonu_actuarial_risk_disparity binary (parole) path", {
  df <- mk_binary_data(200L)
  res <- suppressWarnings(
    morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "parole",
      race_cols = c("black"),
      gender_col = "gender",
      score_col = "reintegration_score_numeric",
      control_cols = c("age"),
      bootstrap_replicates = 10L,
      split_by_gender = TRUE
    )
  )
  expect_s3_class(res, "morie_laniyonu_ard_result")
})

test_that("morie_laniyonu_actuarial_risk_disparity binary (housing) pooled", {
  df <- mk_binary_data(200L)
  res <- suppressWarnings(
    morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "housing",
      race_cols = c("black"),
      gender_col = "gender",
      score_col = "osl_score_numeric",
      control_cols = character(0),
      bootstrap_replicates = 5L,
      split_by_gender = FALSE
    )
  )
  expect_s3_class(res, "morie_laniyonu_ard_result")
})

test_that("morie_laniyonu_actuarial_risk_disparity unknown outcome errors", {
  df <- mk_ord_data(40L)
  expect_error(
    suppressWarnings(morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "weird_outcome", race_cols = "black"
    )),
    "unknown outcome"
  )
})

test_that("morie_laniyonu_actuarial_risk_disparity missing columns errors", {
  df <- mk_ord_data(40L)
  expect_error(
    suppressWarnings(morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "static",
      race_cols = c("missing_col"),
      gender_col = "gender",
      split_by_gender = FALSE
    )),
    "missing"
  )
})

test_that("morie_laniyonu_actuarial_risk_disparity binary needs score_col", {
  df <- mk_binary_data(40L)
  expect_error(
    suppressWarnings(morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "parole",
      race_cols = c("black"),
      gender_col = "gender",
      split_by_gender = FALSE
    )),
    "score_col is required"
  )
})

test_that("score_col warns for ordinal outcome", {
  df <- mk_ord_data(150L)
  expect_warning(
    morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "static",
      race_cols = c("black"),
      gender_col = "gender",
      score_col = "static_score",  # ignored
      split_by_gender = FALSE
    )
  )
})

test_that("outcome_col override works", {
  df <- mk_ord_data(150L)
  df$my_custom_ord <- df$static_score
  res <- suppressWarnings(morie_laniyonu_actuarial_risk_disparity(
    df, outcome = "static",
    race_cols = c("black"),
    gender_col = "gender",
    outcome_col = "my_custom_ord",
    split_by_gender = FALSE
  ))
  expect_s3_class(res, "morie_laniyonu_ard_result")
})

test_that("print.morie_laniyonu_ard_result emits header lines", {
  df <- mk_ord_data(150L)
  res <- suppressWarnings(morie_laniyonu_actuarial_risk_disparity(
    df, outcome = "static",
    race_cols = c("black"),
    gender_col = "gender",
    split_by_gender = FALSE
  ))
  out <- capture.output(print(res))
  expect_true(any(grepl("Laniyonu", out)))
  expect_true(any(grepl("CAVEAT|caveat", out, ignore.case = TRUE)))
})

test_that("realistic gender distribution exercises both M and F strata cleanly", {
  # Vee 2026-05-25: synth data must reflect realistic gender
  # distribution -- in the carceral system there are far more than
  # 5 women involved with police, so a happy-path test should NOT
  # under-represent F. mk_ord_data(300L) yields ~30% F (~90 rows),
  # comfortably above the n>=30 stability threshold, so BOTH strata
  # get fit and the function runs warning-free.
  df <- mk_ord_data(300L)
  res <- morie_laniyonu_actuarial_risk_disparity(
    df, outcome = "static",
    race_cols = c("black"),
    gender_col = "gender",
    split_by_gender = TRUE
  )
  out <- capture.output(print(res))
  expect_true(any(grepl("Laniyonu", out)))
})

test_that("n<30 stratum guard fires for genuinely tiny groups", {
  # Edge-case test for the n>=30 stability threshold. We deliberately
  # construct a tiny F group (n=5) here to assert the function's
  # defensive warning behaviour -- this is the ONE place we under-
  # represent F on purpose; the realistic happy path lives above.
  df <- mk_ord_data(40L)
  df$gender <- "M"
  df$gender[1:5] <- "F"
  expect_warning(
    morie_laniyonu_actuarial_risk_disparity(
      df, outcome = "static",
      race_cols = c("black"),
      gender_col = "gender",
      split_by_gender = TRUE
    ),
    regexp = "stratum 'F' has only 5 rows"
  )
})
